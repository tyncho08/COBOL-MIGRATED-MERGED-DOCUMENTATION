"""
Journal Entry Service - GL020/GL070 migration
Handles manual journal entries and batch posting
"""
from typing import List, Optional, Dict, Tuple
from decimal import Decimal
from datetime import datetime
from sqlalchemy.orm import Session
from sqlalchemy import and_

from app.services.file_handlers.gl_handler import GLFileHandler
from app.services.file_handlers.system_handler import SystemFileHandler
from app.models.gl_accounts import GLLedgerRec, GLPostingRec, GLBatchRec
from app.core.security import log_user_action
from app.models.auth import User


class JournalEntryService:
    """
    Journal Entry management
    Implements GL020 (manual entries) and GL070 (batch processing)
    """
    
    def __init__(self, db: Session, current_user: Optional[User] = None):
        self.db = db
        self.current_user = current_user
        self.gl_handler = GLFileHandler(db)
        self.system_handler = SystemFileHandler(db)
        
    def create_journal_batch(self, description: str, source: str = "MANUAL") -> GLBatchRec:
        """Create new journal batch"""
        # Get next batch number
        system_rec, _ = self.system_handler.read_system_params()
        batch_no = system_rec.gl_next_batch if system_rec else 1
        
        batch = GLBatchRec(
            batch_no=batch_no,
            batch_date=int(datetime.now().strftime("%Y%m%d")),
            batch_period=self.system_handler.get_current_period(),
            batch_type='JE',
            batch_description=description,
            batch_source=source,
            batch_status='O'  # Open
        )
        
        self.db.add(batch)
        self.db.flush()
        
        # Update next batch number
        if system_rec:
            system_rec.gl_next_batch = batch_no + 1
            self.system_handler.update_system_params(system_rec)
            
        return batch
        
    def add_journal_line(self, batch_no: int, line_data: Dict) -> Tuple[GLPostingRec, Optional[str]]:
        """
        Add line to journal batch
        Returns (posting_record, error_message)
        """
        # Validate batch exists and is open
        batch = self.db.query(GLBatchRec).filter(
            GLBatchRec.batch_no == batch_no
        ).first()
        
        if not batch:
            return None, "Batch not found"
            
        if batch.batch_status != 'O':
            return None, "Batch is not open for posting"
            
        # Validate account
        account_no = line_data.get('account', 0)
        if not self.gl_handler.validate_account_number(account_no):
            return None, f"Invalid account {account_no}"
            
        # Get amounts
        debit = Decimal(str(line_data.get('debit', 0)))
        credit = Decimal(str(line_data.get('credit', 0)))
        
        # Validate only one side has value
        if (debit > 0 and credit > 0) or (debit == 0 and credit == 0):
            return None, "Entry must have either debit or credit, not both or neither"
            
        # Create posting
        posting = GLPostingRec(
            posting_batch=batch_no,
            posting_date=batch.batch_date,
            posting_period=batch.batch_period,
            posting_type='JE',
            posting_reference=line_data.get('reference', ''),
            posting_description=line_data.get('description', ''),
            posting_account=account_no,
            posting_debit=debit,
            posting_credit=credit,
            posting_analysis_1=line_data.get('analysis_1', ''),
            posting_analysis_2=line_data.get('analysis_2', ''),
            posting_analysis_3=line_data.get('analysis_3', ''),
            posting_status='U',  # Unposted
            posting_user=self.current_user.username if self.current_user else 'SYSTEM'
        )
        
        self.db.add(posting)
        
        # Update batch totals
        batch.batch_debits += debit
        batch.batch_credits += credit
        batch.batch_entries += 1
        
        self.db.flush()
        
        return posting, None
        
    def validate_batch(self, batch_no: int) -> List[str]:
        """
        Validate journal batch before posting
        Returns list of validation errors
        """
        errors = []
        
        # Get batch
        batch = self.db.query(GLBatchRec).filter(
            GLBatchRec.batch_no == batch_no
        ).first()
        
        if not batch:
            return ["Batch not found"]
            
        # Check if already posted
        if batch.batch_status == 'P':
            return ["Batch already posted"]
            
        # Check if balanced
        if abs(batch.batch_debits - batch.batch_credits) > Decimal('0.01'):
            errors.append(f"Batch not balanced: DR={batch.batch_debits} CR={batch.batch_credits}")
            
        # Check if has entries
        if batch.batch_entries == 0:
            errors.append("Batch has no entries")
            
        # Validate individual entries
        entries = self.db.query(GLPostingRec).filter(
            GLPostingRec.posting_batch == batch_no
        ).all()
        
        for entry in entries:
            # Validate account exists
            account, status = self.gl_handler.process(4, key_value=entry.posting_account)
            if status.fs_reply != "00":
                errors.append(f"Account {entry.posting_account} not found")
            elif account.ledger_active != 'Y':
                errors.append(f"Account {entry.posting_account} is inactive")
                
        return errors
        
    def post_batch(self, batch_no: int) -> Tuple[bool, Optional[str]]:
        """
        Post journal batch to GL
        Returns (success, error_message)
        """
        # Validate first
        errors = self.validate_batch(batch_no)
        if errors:
            return False, "; ".join(errors)
            
        # Get batch and entries
        batch = self.db.query(GLBatchRec).filter(
            GLBatchRec.batch_no == batch_no
        ).first()
        
        entries = self.db.query(GLPostingRec).filter(
            GLPostingRec.posting_batch == batch_no
        ).all()
        
        # Build entry list for handler
        entry_list = []
        for entry in entries:
            entry_dict = {
                'account': entry.posting_account,
                'debit': float(entry.posting_debit),
                'credit': float(entry.posting_credit),
                'reference': entry.posting_reference,
                'description': entry.posting_description
            }
            entry_list.append(entry_dict)
            
        # Post using GL handler
        status = self.gl_handler.post_journal_entry(batch_no, entry_list)
        
        if status.fs_reply != "00":
            return False, f"Posting failed: {status.fs_reply}"
            
        # Update batch status
        batch.batch_status = 'P'
        batch.batch_posted_date = int(datetime.now().strftime("%Y%m%d"))
        batch.batch_posted_by = self.current_user.username if self.current_user else 'SYSTEM'
        
        # Update entry status
        for entry in entries:
            entry.posting_status = 'P'
            
        self.db.commit()
        
        # Log posting
        if self.current_user:
            log_user_action(
                db=self.db,
                user=self.current_user,
                action="POST_JOURNAL_BATCH",
                table="glbatch_rec",
                key=str(batch_no),
                new_values={
                    'entries': batch.batch_entries,
                    'debits': float(batch.batch_debits),
                    'credits': float(batch.batch_credits)
                },
                module="GL"
            )
            
        return True, None
        
    def reverse_batch(self, original_batch_no: int, reversal_date: Optional[int] = None) -> Tuple[Optional[int], Optional[str]]:
        """
        Create reversal batch for posted batch
        Returns (new_batch_no, error_message)
        """
        # Get original batch
        original_batch = self.db.query(GLBatchRec).filter(
            GLBatchRec.batch_no == original_batch_no
        ).first()
        
        if not original_batch:
            return None, "Original batch not found"
            
        if original_batch.batch_status != 'P':
            return None, "Can only reverse posted batches"
            
        # Create reversal batch
        reversal_batch = self.create_journal_batch(
            f"Reversal of batch {original_batch_no}",
            "REVERSAL"
        )
        
        # Get original entries
        original_entries = self.db.query(GLPostingRec).filter(
            GLPostingRec.posting_batch == original_batch_no
        ).all()
        
        # Create reversal entries (swap debits and credits)
        for entry in original_entries:
            reversal_data = {
                'account': entry.posting_account,
                'debit': float(entry.posting_credit),  # Swap
                'credit': float(entry.posting_debit),   # Swap
                'reference': f"REV-{entry.posting_reference}",
                'description': f"Reversal: {entry.posting_description}",
                'analysis_1': entry.posting_analysis_1,
                'analysis_2': entry.posting_analysis_2,
                'analysis_3': entry.posting_analysis_3
            }
            
            _, error = self.add_journal_line(reversal_batch.batch_no, reversal_data)
            if error:
                return None, f"Failed to create reversal entry: {error}"
                
        return reversal_batch.batch_no, None
        
    def get_batch_list(self, filters: Optional[Dict] = None) -> List[GLBatchRec]:
        """Get list of journal batches with optional filters"""
        query = self.db.query(GLBatchRec)
        
        if filters:
            if 'status' in filters:
                query = query.filter(GLBatchRec.batch_status == filters['status'])
                
            if 'period' in filters:
                query = query.filter(GLBatchRec.batch_period == filters['period'])
                
            if 'date_from' in filters:
                query = query.filter(GLBatchRec.batch_date >= filters['date_from'])
                
            if 'date_to' in filters:
                query = query.filter(GLBatchRec.batch_date <= filters['date_to'])
                
            if 'source' in filters:
                query = query.filter(GLBatchRec.batch_source == filters['source'])
                
        return query.order_by(GLBatchRec.batch_no.desc()).all()
        
    def get_batch_details(self, batch_no: int) -> Dict:
        """Get batch with all entries"""
        batch = self.db.query(GLBatchRec).filter(
            GLBatchRec.batch_no == batch_no
        ).first()
        
        if not batch:
            return None
            
        entries = self.db.query(GLPostingRec).filter(
            GLPostingRec.posting_batch == batch_no
        ).order_by(GLPostingRec.posting_id).all()
        
        # Get account names
        entry_details = []
        for entry in entries:
            account, _ = self.gl_handler.process(4, key_value=entry.posting_account)
            entry_dict = {
                'posting_id': entry.posting_id,
                'account': entry.posting_account,
                'account_name': account.ledger_name if account else '',
                'debit': float(entry.posting_debit),
                'credit': float(entry.posting_credit),
                'reference': entry.posting_reference,
                'description': entry.posting_description,
                'analysis_1': entry.posting_analysis_1,
                'analysis_2': entry.posting_analysis_2,
                'analysis_3': entry.posting_analysis_3
            }
            entry_details.append(entry_dict)
            
        return {
            'batch': batch,
            'entries': entry_details,
            'is_balanced': abs(batch.batch_debits - batch.batch_credits) < Decimal('0.01')
        }
        
    def create_recurring_template(self, template_data: Dict) -> Dict:
        """Create template for recurring journal entries"""
        # This would store template in a separate table
        # For now, return template structure
        template = {
            'name': template_data.get('name'),
            'description': template_data.get('description'),
            'frequency': template_data.get('frequency', 'MONTHLY'),
            'entries': template_data.get('entries', [])
        }
        
        return template
        
    def apply_template(self, template: Dict, posting_date: Optional[int] = None) -> Tuple[Optional[int], Optional[str]]:
        """
        Apply journal template to create new batch
        Returns (batch_no, error_message)
        """
        # Create batch
        batch = self.create_journal_batch(
            template.get('description', 'Template Entry'),
            'TEMPLATE'
        )
        
        # Add entries from template
        for entry in template.get('entries', []):
            _, error = self.add_journal_line(batch.batch_no, entry)
            if error:
                return None, error
                
        return batch.batch_no, None