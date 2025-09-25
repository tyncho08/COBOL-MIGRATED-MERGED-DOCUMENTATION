"""
IRS Transaction Entry Service - IRS020 migration
Handles simplified transaction entry for tax purposes
"""
from typing import List, Optional, Dict, Tuple
from decimal import Decimal
from datetime import datetime, timedelta
from sqlalchemy.orm import Session
from sqlalchemy import and_, or_, func, desc

from app.services.file_handlers.system_handler import SystemFileHandler
from app.models.irs import IrsTransactionRec, IrsCompanyConfigRec
from app.core.security import log_user_action
from app.models.auth import User


class IrsTransactionService:
    """
    IRS Transaction Entry Service
    Implements IRS020 - simplified transaction entry for tax tracking
    """
    
    def __init__(self, db: Session, current_user: Optional[User] = None):
        self.db = db
        self.current_user = current_user
        self.system_handler = SystemFileHandler(db)
        
    def create_transaction(self, transaction_data: Dict) -> Tuple[bool, Optional[str]]:
        """
        Create IRS transaction record
        Returns (success, error_message or transaction_data)
        """
        company_code = transaction_data.get('company_code')
        trans_date = transaction_data.get('trans_date')
        trans_type = transaction_data.get('trans_type')
        category = transaction_data.get('category')
        amount = Decimal(str(transaction_data.get('amount', 0)))
        description = transaction_data.get('description')
        gl_account = transaction_data.get('gl_account')
        
        if not all([company_code, trans_date, trans_type, category, description, gl_account]):
            return False, "Company code, date, type, category, description, and GL account are required"
            
        if amount == 0:
            return False, "Transaction amount cannot be zero"
            
        # Validate company exists
        company = self.db.query(IrsCompanyConfigRec).filter(
            IrsCompanyConfigRec.config_company_code == company_code
        ).first()
        
        if not company:
            return False, f"Company {company_code} not configured"
            
        try:
            # Parse date if string
            if isinstance(trans_date, str):
                trans_date = int(trans_date.replace('-', ''))
                
            # Calculate deductible amount based on category
            deductible_amount = self._calculate_deductible_amount(trans_type, category, amount, transaction_data)
            
            transaction = IrsTransactionRec(
                trans_company_code=company_code,
                trans_date=trans_date,
                trans_type=trans_type,
                trans_category=category,
                trans_subcategory=transaction_data.get('subcategory', ''),
                trans_amount=amount,
                trans_description=description,
                trans_reference=transaction_data.get('reference', ''),
                trans_gl_account=gl_account,
                trans_deductible_amount=deductible_amount,
                trans_depreciation_method=transaction_data.get('depreciation_method', ''),
                trans_depreciation_life=transaction_data.get('depreciation_life', 0),
                trans_section_code=transaction_data.get('section_code', ''),
                trans_schedule=transaction_data.get('schedule', ''),
                trans_form_line=transaction_data.get('form_line', ''),
                trans_created_date=int(datetime.now().strftime("%Y%m%d")),
                trans_created_by=self.current_user.username if self.current_user else 'SYSTEM'
            )
            
            self.db.add(transaction)
            self.db.commit()
            
            # Log transaction creation
            if self.current_user:
                log_user_action(
                    db=self.db,
                    user=self.current_user,
                    action="CREATE_IRS_TRANSACTION",
                    table="irs_transaction_rec",
                    key=str(transaction.trans_id),
                    new_values={
                        'company_code': company_code,
                        'type': trans_type,
                        'category': category,
                        'amount': float(amount),
                        'description': description
                    },
                    module="IRS"
                )
                
            return True, {
                'trans_id': transaction.trans_id,
                'company_code': company_code,
                'trans_date': trans_date,
                'trans_type': trans_type,
                'category': category,
                'amount': float(amount),
                'deductible_amount': float(deductible_amount),
                'description': description
            }
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def update_transaction(self, trans_id: int, update_data: Dict) -> Tuple[bool, Optional[str]]:
        """
        Update IRS transaction record
        Returns (success, error_message)
        """
        transaction = self.db.query(IrsTransactionRec).filter(
            IrsTransactionRec.trans_id == trans_id
        ).first()
        
        if not transaction:
            return False, f"Transaction {trans_id} not found"
            
        try:
            # Store old values for audit
            old_values = {
                'type': transaction.trans_type,
                'category': transaction.trans_category,
                'amount': float(transaction.trans_amount),
                'description': transaction.trans_description
            }
            
            # Update allowed fields
            updateable_fields = [
                'trans_type', 'trans_category', 'trans_subcategory', 'trans_amount',
                'trans_description', 'trans_reference', 'trans_gl_account',
                'trans_depreciation_method', 'trans_depreciation_life',
                'trans_section_code', 'trans_schedule', 'trans_form_line'
            ]
            
            new_values = {}
            for field, value in update_data.items():
                if f'trans_{field}' in updateable_fields:
                    setattr(transaction, f'trans_{field}', value)
                    new_values[field] = value
                    
            # Recalculate deductible amount if amount or category changed
            if 'amount' in update_data or 'category' in update_data:
                transaction.trans_deductible_amount = self._calculate_deductible_amount(
                    transaction.trans_type,
                    transaction.trans_category,
                    transaction.trans_amount,
                    update_data
                )
                new_values['deductible_amount'] = float(transaction.trans_deductible_amount)
                
            transaction.trans_updated_date = int(datetime.now().strftime("%Y%m%d"))
            transaction.trans_updated_by = self.current_user.username if self.current_user else 'SYSTEM'
            
            self.db.commit()
            
            # Log update
            if self.current_user:
                log_user_action(
                    db=self.db,
                    user=self.current_user,
                    action="UPDATE_IRS_TRANSACTION",
                    table="irs_transaction_rec",
                    key=str(trans_id),
                    old_values=old_values,
                    new_values=new_values,
                    module="IRS"
                )
                
            return True, f"Transaction {trans_id} updated successfully"
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def delete_transaction(self, trans_id: int) -> Tuple[bool, Optional[str]]:
        """
        Delete IRS transaction record
        Returns (success, error_message)
        """
        transaction = self.db.query(IrsTransactionRec).filter(
            IrsTransactionRec.trans_id == trans_id
        ).first()
        
        if not transaction:
            return False, f"Transaction {trans_id} not found"
            
        try:
            # Store values for audit
            old_values = {
                'company_code': transaction.trans_company_code,
                'type': transaction.trans_type,
                'category': transaction.trans_category,
                'amount': float(transaction.trans_amount),
                'description': transaction.trans_description
            }
            
            self.db.delete(transaction)
            self.db.commit()
            
            # Log deletion
            if self.current_user:
                log_user_action(
                    db=self.db,
                    user=self.current_user,
                    action="DELETE_IRS_TRANSACTION",
                    table="irs_transaction_rec",
                    key=str(trans_id),
                    old_values=old_values,
                    module="IRS"
                )
                
            return True, f"Transaction {trans_id} deleted successfully"
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def get_transactions(self, filters: Optional[Dict] = None) -> Dict:
        """Get IRS transactions with filtering"""
        filters = filters or {}
        
        try:
            query = self.db.query(IrsTransactionRec)
            
            # Apply filters
            if filters.get('company_code'):
                query = query.filter(IrsTransactionRec.trans_company_code == filters['company_code'])
                
            if filters.get('trans_type'):
                query = query.filter(IrsTransactionRec.trans_type == filters['trans_type'])
                
            if filters.get('category'):
                query = query.filter(IrsTransactionRec.trans_category == filters['category'])
                
            if filters.get('date_from'):
                date_from = int(str(filters['date_from']).replace('-', ''))
                query = query.filter(IrsTransactionRec.trans_date >= date_from)
                
            if filters.get('date_to'):
                date_to = int(str(filters['date_to']).replace('-', ''))
                query = query.filter(IrsTransactionRec.trans_date <= date_to)
                
            if filters.get('gl_account'):
                query = query.filter(IrsTransactionRec.trans_gl_account == filters['gl_account'])
                
            if filters.get('min_amount'):
                query = query.filter(IrsTransactionRec.trans_amount >= Decimal(str(filters['min_amount'])))
                
            if filters.get('max_amount'):
                query = query.filter(IrsTransactionRec.trans_amount <= Decimal(str(filters['max_amount'])))
                
            # Apply ordering
            query = query.order_by(desc(IrsTransactionRec.trans_date), desc(IrsTransactionRec.trans_id))
            
            # Apply pagination if specified
            page = filters.get('page', 1)
            page_size = filters.get('page_size', 100)
            offset = (page - 1) * page_size
            
            total_count = query.count()
            transactions = query.offset(offset).limit(page_size).all()
            
            return {
                'transactions': [
                    {
                        'trans_id': trans.trans_id,
                        'company_code': trans.trans_company_code,
                        'trans_date': trans.trans_date,
                        'trans_date_formatted': self._format_date(trans.trans_date),
                        'trans_type': trans.trans_type,
                        'category': trans.trans_category,
                        'subcategory': trans.trans_subcategory,
                        'amount': float(trans.trans_amount),
                        'deductible_amount': float(trans.trans_deductible_amount),
                        'description': trans.trans_description,
                        'reference': trans.trans_reference,
                        'gl_account': trans.trans_gl_account,
                        'depreciation_method': trans.trans_depreciation_method,
                        'depreciation_life': trans.trans_depreciation_life,
                        'section_code': trans.trans_section_code,
                        'schedule': trans.trans_schedule,
                        'form_line': trans.trans_form_line,
                        'created_date': trans.trans_created_date,
                        'created_by': trans.trans_created_by
                    }
                    for trans in transactions
                ],
                'pagination': {
                    'page': page,
                    'page_size': page_size,
                    'total_count': total_count,
                    'total_pages': (total_count + page_size - 1) // page_size
                },
                'summary': self._calculate_transaction_summary(transactions)
            }
            
        except Exception as e:
            return {'error': str(e)}
            
    def get_transaction_summary_by_category(self, company_code: str, year: int) -> Dict:
        """Get transaction summary by tax category"""
        try:
            date_from = int(f"{year}0101")
            date_to = int(f"{year}1231")
            
            # Get transactions for the year
            transactions = self.db.query(IrsTransactionRec).filter(
                and_(
                    IrsTransactionRec.trans_company_code == company_code,
                    IrsTransactionRec.trans_date >= date_from,
                    IrsTransactionRec.trans_date <= date_to
                )
            ).all()
            
            # Group by category and type
            summary_by_category = {}
            summary_by_type = {}
            
            total_income = Decimal('0')
            total_expenses = Decimal('0')
            total_deductible = Decimal('0')
            
            for trans in transactions:
                category = trans.trans_category
                trans_type = trans.trans_type
                amount = trans.trans_amount
                deductible = trans.trans_deductible_amount
                
                # Category summary
                if category not in summary_by_category:
                    summary_by_category[category] = {
                        'total_amount': Decimal('0'),
                        'total_deductible': Decimal('0'),
                        'transaction_count': 0
                    }
                    
                summary_by_category[category]['total_amount'] += amount
                summary_by_category[category]['total_deductible'] += deductible
                summary_by_category[category]['transaction_count'] += 1
                
                # Type summary
                if trans_type not in summary_by_type:
                    summary_by_type[trans_type] = {
                        'total_amount': Decimal('0'),
                        'total_deductible': Decimal('0'),
                        'transaction_count': 0
                    }
                    
                summary_by_type[trans_type]['total_amount'] += amount
                summary_by_type[trans_type]['total_deductible'] += deductible
                summary_by_type[trans_type]['transaction_count'] += 1
                
                # Overall totals
                if trans_type == 'INCOME':
                    total_income += amount
                else:
                    total_expenses += amount
                    
                total_deductible += deductible
                
            # Convert to regular dict with float values
            return {
                'company_code': company_code,
                'tax_year': year,
                'summary_by_category': {
                    cat: {
                        'total_amount': float(data['total_amount']),
                        'total_deductible': float(data['total_deductible']),
                        'transaction_count': data['transaction_count']
                    }
                    for cat, data in summary_by_category.items()
                },
                'summary_by_type': {
                    typ: {
                        'total_amount': float(data['total_amount']),
                        'total_deductible': float(data['total_deductible']),
                        'transaction_count': data['transaction_count']
                    }
                    for typ, data in summary_by_type.items()
                },
                'totals': {
                    'total_income': float(total_income),
                    'total_expenses': float(total_expenses),
                    'net_income': float(total_income - total_expenses),
                    'total_deductible': float(total_deductible),
                    'total_transactions': len(transactions)
                }
            }
            
        except Exception as e:
            return {'error': str(e)}
            
    def bulk_categorize_transactions(self, categorization_data: Dict) -> Tuple[bool, Optional[str]]:
        """
        Bulk categorize transactions based on description patterns
        Returns (success, error_message or results)
        """
        company_code = categorization_data.get('company_code')
        rules = categorization_data.get('rules', [])
        
        if not company_code or not rules:
            return False, "Company code and categorization rules are required"
            
        try:
            transactions_updated = 0
            
            for rule in rules:
                pattern = rule.get('pattern', '')
                category = rule.get('category', '')
                schedule = rule.get('schedule', '')
                section_code = rule.get('section_code', '')
                
                if not pattern or not category:
                    continue
                    
                # Find transactions matching pattern
                transactions = self.db.query(IrsTransactionRec).filter(
                    and_(
                        IrsTransactionRec.trans_company_code == company_code,
                        IrsTransactionRec.trans_description.ilike(f'%{pattern}%'),
                        IrsTransactionRec.trans_category == ''  # Only uncategorized
                    )
                ).all()
                
                for trans in transactions:
                    trans.trans_category = category
                    if schedule:
                        trans.trans_schedule = schedule
                    if section_code:
                        trans.trans_section_code = section_code
                        
                    # Recalculate deductible amount
                    trans.trans_deductible_amount = self._calculate_deductible_amount(
                        trans.trans_type, category, trans.trans_amount, {}
                    )
                    
                    trans.trans_updated_date = int(datetime.now().strftime("%Y%m%d"))
                    trans.trans_updated_by = self.current_user.username if self.current_user else 'SYSTEM'
                    
                    transactions_updated += 1
                    
            self.db.commit()
            
            return True, {
                'transactions_updated': transactions_updated,
                'rules_applied': len(rules)
            }
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def _calculate_deductible_amount(self, trans_type: str, category: str, 
                                   amount: Decimal, transaction_data: Dict) -> Decimal:
        """Calculate deductible amount based on transaction type and category"""
        
        # Income is generally not deductible
        if trans_type == 'INCOME':
            return Decimal('0')
            
        # Business expense deductibility rules
        fully_deductible_categories = [
            'OFFICE_SUPPLIES', 'UTILITIES', 'RENT', 'PROFESSIONAL_SERVICES',
            'ADVERTISING', 'INSURANCE', 'REPAIRS_MAINTENANCE', 'TRAVEL',
            'TELEPHONE', 'POSTAGE', 'BANK_CHARGES', 'INTEREST_EXPENSE'
        ]
        
        partially_deductible_categories = {
            'MEALS_ENTERTAINMENT': 0.50,  # 50% deductible
            'HOME_OFFICE': 0.30,  # Simplified calculation
            'VEHICLE_EXPENSE': 0.75  # Business use percentage
        }
        
        non_deductible_categories = [
            'PERSONAL_EXPENSE', 'CAPITAL_EXPENDITURE', 'PENALTIES_FINES',
            'POLITICAL_CONTRIBUTIONS', 'LOBBYING'
        ]
        
        if category in fully_deductible_categories:
            return amount
        elif category in partially_deductible_categories:
            return amount * Decimal(str(partially_deductible_categories[category]))
        elif category in non_deductible_categories:
            return Decimal('0')
        else:
            # Default to fully deductible for business expenses
            return amount if trans_type == 'EXPENSE' else Decimal('0')
            
    def _calculate_transaction_summary(self, transactions: List[IrsTransactionRec]) -> Dict:
        """Calculate summary statistics for transaction list"""
        if not transactions:
            return {'total_amount': 0, 'total_deductible': 0, 'count': 0}
            
        total_amount = sum(float(trans.trans_amount) for trans in transactions)
        total_deductible = sum(float(trans.trans_deductible_amount) for trans in transactions)
        
        return {
            'total_amount': total_amount,
            'total_deductible': total_deductible,
            'transaction_count': len(transactions),
            'average_amount': total_amount / len(transactions) if transactions else 0
        }
        
    def _format_date(self, date_int: int) -> str:
        """Format integer date to readable format"""
        try:
            date_str = str(date_int)
            return f"{date_str[4:6]}/{date_str[6:8]}/{date_str[:4]}"
        except:
            return str(date_int)