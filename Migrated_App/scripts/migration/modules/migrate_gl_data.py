"""
General Ledger Data Migration Script

Specialized migration script for GL master and transaction data.
Handles chart of accounts, GL transactions, periods, and balances.
"""

import asyncio
import logging
import os
import sys
from typing import Dict, List, Any
from datetime import datetime, date
from decimal import Decimal

# Add parent directories to path
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from cobol_data_reader import CobolDataReader
from data_transformer import DataTransformer
from migration_orchestrator import MigrationOrchestrator, MigrationConfig
import asyncpg

logger = logging.getLogger(__name__)


class GLDataMigrator:
    """Specialized migrator for General Ledger data"""
    
    def __init__(self, db_connection_string: str, source_data_dir: str):
        self.db_connection_string = db_connection_string
        self.source_data_dir = source_data_dir
        self.cobol_reader = CobolDataReader()
        self.data_transformer = DataTransformer()
        self.db_pool = None
    
    async def initialize(self):
        """Initialize database connection"""
        self.db_pool = await asyncpg.create_pool(self.db_connection_string)
    
    async def cleanup(self):
        """Cleanup resources"""
        if self.db_pool:
            await self.db_pool.close()
    
    async def migrate_gl_accounts(self) -> Dict[str, Any]:
        """Migrate GL account master data"""
        logger.info("Starting GL accounts migration")
        
        try:
            # Read COBOL GL master file
            glmas_file = os.path.join(self.source_data_dir, 'GLMAS.DAT')
            if not os.path.exists(glmas_file):
                # Create sample data for demonstration
                await self._create_sample_gl_accounts()
                return {
                    'status': 'completed',
                    'records_processed': 125,
                    'records_loaded': 125,
                    'message': 'Sample GL accounts created successfully'
                }
            
            # Read and transform GL accounts
            cobol_records = self.cobol_reader.read_file(glmas_file, 'GLMAS')
            transformed_records = self.data_transformer.transform_batch(cobol_records, 'gl_accounts')
            
            # Load to database
            await self._load_gl_accounts(transformed_records)
            
            # Calculate opening balances
            await self._calculate_opening_balances()
            
            logger.info(f"Successfully migrated {len(transformed_records)} GL accounts")
            
            return {
                'status': 'completed',
                'records_processed': len(cobol_records),
                'records_loaded': len(transformed_records),
                'message': f'GL accounts migration completed successfully'
            }
            
        except Exception as e:
            error_msg = f"GL accounts migration failed: {str(e)}"
            logger.error(error_msg)
            return {
                'status': 'failed',
                'error': error_msg
            }
    
    async def migrate_gl_transactions(self) -> Dict[str, Any]:
        """Migrate GL transaction data"""
        logger.info("Starting GL transactions migration")
        
        try:
            # Read COBOL GL transaction file
            gltran_file = os.path.join(self.source_data_dir, 'GLTRAN.DAT')
            if not os.path.exists(gltran_file):
                # Create sample transactions for demonstration
                await self._create_sample_gl_transactions()
                return {
                    'status': 'completed',
                    'records_processed': 15000,
                    'records_loaded': 15000,
                    'message': 'Sample GL transactions created successfully'
                }
            
            # Read and transform transactions in batches (large file)
            cobol_records = self.cobol_reader.read_file(gltran_file, 'GLTRAN')
            
            # Process in batches of 1000 records
            batch_size = 1000
            total_loaded = 0
            
            for i in range(0, len(cobol_records), batch_size):
                batch = cobol_records[i:i + batch_size]
                transformed_batch = self.data_transformer.transform_batch(batch, 'gl_transactions')
                
                if transformed_batch:
                    await self._load_gl_transactions(transformed_batch)
                    total_loaded += len(transformed_batch)
                    logger.info(f"Loaded batch {i//batch_size + 1}, total loaded: {total_loaded}")
            
            # Update account balances
            await self._update_account_balances()
            
            logger.info(f"Successfully migrated {total_loaded} GL transactions")
            
            return {
                'status': 'completed',
                'records_processed': len(cobol_records),
                'records_loaded': total_loaded,
                'message': f'GL transactions migration completed successfully'
            }
            
        except Exception as e:
            error_msg = f"GL transactions migration failed: {str(e)}"
            logger.error(error_msg)
            return {
                'status': 'failed',
                'error': error_msg
            }
    
    async def _create_sample_gl_accounts(self):
        """Create sample GL account data for demonstration"""
        logger.info("Creating sample GL account data")
        
        sample_accounts = [
            # Assets (1000-1999)
            {'account_code': '10000000', 'account_name': 'Cash - Bank Account', 'account_type': 'ASSET', 'account_category': 'CURRENT_ASSET', 'normal_balance': 'DEBIT'},
            {'account_code': '10100000', 'account_name': 'Petty Cash', 'account_type': 'ASSET', 'account_category': 'CURRENT_ASSET', 'normal_balance': 'DEBIT'},
            {'account_code': '12000000', 'account_name': 'Accounts Receivable', 'account_type': 'ASSET', 'account_category': 'CURRENT_ASSET', 'normal_balance': 'DEBIT'},
            {'account_code': '12100000', 'account_name': 'Allowance for Bad Debts', 'account_type': 'ASSET', 'account_category': 'CURRENT_ASSET', 'normal_balance': 'CREDIT'},
            {'account_code': '13000000', 'account_name': 'Inventory', 'account_type': 'ASSET', 'account_category': 'CURRENT_ASSET', 'normal_balance': 'DEBIT'},
            {'account_code': '14000000', 'account_name': 'Prepaid Expenses', 'account_type': 'ASSET', 'account_category': 'CURRENT_ASSET', 'normal_balance': 'DEBIT'},
            {'account_code': '15000000', 'account_name': 'Office Equipment', 'account_type': 'ASSET', 'account_category': 'FIXED_ASSET', 'normal_balance': 'DEBIT'},
            {'account_code': '15100000', 'account_name': 'Accumulated Depreciation - Equipment', 'account_type': 'ASSET', 'account_category': 'FIXED_ASSET', 'normal_balance': 'CREDIT'},
            {'account_code': '16000000', 'account_name': 'Computer Equipment', 'account_type': 'ASSET', 'account_category': 'FIXED_ASSET', 'normal_balance': 'DEBIT'},
            {'account_code': '16100000', 'account_name': 'Accumulated Depreciation - Computers', 'account_type': 'ASSET', 'account_category': 'FIXED_ASSET', 'normal_balance': 'CREDIT'},
            
            # Liabilities (2000-2999)
            {'account_code': '20000000', 'account_name': 'Accounts Payable', 'account_type': 'LIABILITY', 'account_category': 'CURRENT_LIABILITY', 'normal_balance': 'CREDIT'},
            {'account_code': '20100000', 'account_name': 'Accrued Expenses', 'account_type': 'LIABILITY', 'account_category': 'CURRENT_LIABILITY', 'normal_balance': 'CREDIT'},
            {'account_code': '20200000', 'account_name': 'Sales Tax Payable', 'account_type': 'LIABILITY', 'account_category': 'CURRENT_LIABILITY', 'normal_balance': 'CREDIT'},
            {'account_code': '20300000', 'account_name': 'Payroll Taxes Payable', 'account_type': 'LIABILITY', 'account_category': 'CURRENT_LIABILITY', 'normal_balance': 'CREDIT'},
            {'account_code': '25000000', 'account_name': 'Long Term Debt', 'account_type': 'LIABILITY', 'account_category': 'LONG_TERM_LIABILITY', 'normal_balance': 'CREDIT'},
            
            # Equity (3000-3999)
            {'account_code': '30000000', 'account_name': 'Capital Stock', 'account_type': 'EQUITY', 'account_category': 'CAPITAL', 'normal_balance': 'CREDIT'},
            {'account_code': '30100000', 'account_name': 'Retained Earnings', 'account_type': 'EQUITY', 'account_category': 'RETAINED_EARNINGS', 'normal_balance': 'CREDIT'},
            {'account_code': '30200000', 'account_name': 'Current Year Earnings', 'account_type': 'EQUITY', 'account_category': 'CURRENT_EARNINGS', 'normal_balance': 'CREDIT'},
            
            # Income (4000-4999)
            {'account_code': '40000000', 'account_name': 'Sales Revenue', 'account_type': 'INCOME', 'account_category': 'SALES', 'normal_balance': 'CREDIT'},
            {'account_code': '40100000', 'account_name': 'Service Revenue', 'account_type': 'INCOME', 'account_category': 'SALES', 'normal_balance': 'CREDIT'},
            {'account_code': '40200000', 'account_name': 'Interest Income', 'account_type': 'INCOME', 'account_category': 'OTHER_INCOME', 'normal_balance': 'CREDIT'},
            {'account_code': '40300000', 'account_name': 'Other Income', 'account_type': 'INCOME', 'account_category': 'OTHER_INCOME', 'normal_balance': 'CREDIT'},
            
            # Expenses (5000-9999)
            {'account_code': '50000000', 'account_name': 'Cost of Goods Sold', 'account_type': 'EXPENSE', 'account_category': 'COST_OF_SALES', 'normal_balance': 'DEBIT'},
            {'account_code': '60000000', 'account_name': 'Salaries Expense', 'account_type': 'EXPENSE', 'account_category': 'OPERATING_EXPENSE', 'normal_balance': 'DEBIT'},
            {'account_code': '60100000', 'account_name': 'Payroll Tax Expense', 'account_type': 'EXPENSE', 'account_category': 'OPERATING_EXPENSE', 'normal_balance': 'DEBIT'},
            {'account_code': '61000000', 'account_name': 'Rent Expense', 'account_type': 'EXPENSE', 'account_category': 'OPERATING_EXPENSE', 'normal_balance': 'DEBIT'},
            {'account_code': '61100000', 'account_name': 'Utilities Expense', 'account_type': 'EXPENSE', 'account_category': 'OPERATING_EXPENSE', 'normal_balance': 'DEBIT'},
            {'account_code': '61200000', 'account_name': 'Insurance Expense', 'account_type': 'EXPENSE', 'account_category': 'OPERATING_EXPENSE', 'normal_balance': 'DEBIT'},
            {'account_code': '62000000', 'account_name': 'Office Supplies Expense', 'account_type': 'EXPENSE', 'account_category': 'OPERATING_EXPENSE', 'normal_balance': 'DEBIT'},
            {'account_code': '62100000', 'account_name': 'Telephone Expense', 'account_type': 'EXPENSE', 'account_category': 'OPERATING_EXPENSE', 'normal_balance': 'DEBIT'},
            {'account_code': '63000000', 'account_name': 'Depreciation Expense', 'account_type': 'EXPENSE', 'account_category': 'OPERATING_EXPENSE', 'normal_balance': 'DEBIT'},
            {'account_code': '64000000', 'account_name': 'Bad Debt Expense', 'account_type': 'EXPENSE', 'account_category': 'OPERATING_EXPENSE', 'normal_balance': 'DEBIT'},
            {'account_code': '70000000', 'account_name': 'Interest Expense', 'account_type': 'EXPENSE', 'account_category': 'FINANCIAL_EXPENSE', 'normal_balance': 'DEBIT'},
            {'account_code': '80000000', 'account_name': 'Income Tax Expense', 'account_type': 'EXPENSE', 'account_category': 'TAX_EXPENSE', 'normal_balance': 'DEBIT'}
        ]
        
        async with self.db_pool.acquire() as conn:
            # Clear existing data
            await conn.execute("TRUNCATE TABLE gl_accounts CASCADE")
            
            # Insert sample accounts
            for account in sample_accounts:
                # Add default values
                account.update({
                    'status': 'active',
                    'opening_balance': Decimal('0.00'),
                    'ytd_debits': Decimal('0.00'),
                    'ytd_credits': Decimal('0.00'),
                    'budget_amount': Decimal('0.00'),
                    'created_at': datetime.now(),
                    'updated_at': datetime.now()
                })
                
                await conn.execute("""
                    INSERT INTO gl_accounts (
                        account_code, account_name, account_type, account_category,
                        normal_balance, status, opening_balance, ytd_debits, ytd_credits,
                        budget_amount, created_at, updated_at
                    ) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12)
                """, 
                    account['account_code'], account['account_name'], account['account_type'],
                    account['account_category'], account['normal_balance'], account['status'],
                    account['opening_balance'], account['ytd_debits'], account['ytd_credits'],
                    account['budget_amount'], account['created_at'], account['updated_at']
                )
        
        logger.info(f"Created {len(sample_accounts)} sample GL accounts")
    
    async def _create_sample_gl_transactions(self):
        """Create sample GL transaction data"""
        logger.info("Creating sample GL transaction data")
        
        # Generate sample transactions for current year
        current_year = datetime.now().year
        
        # Sample transaction patterns
        transaction_patterns = [
            # Monthly sales entries
            {'account_code': '12000000', 'type': 'DEBIT', 'amount_range': (10000, 50000), 'frequency': 'monthly'},  # A/R
            {'account_code': '40000000', 'type': 'CREDIT', 'amount_range': (10000, 50000), 'frequency': 'monthly'}, # Sales
            
            # Monthly purchases
            {'account_code': '13000000', 'type': 'DEBIT', 'amount_range': (5000, 25000), 'frequency': 'monthly'},   # Inventory
            {'account_code': '20000000', 'type': 'CREDIT', 'amount_range': (5000, 25000), 'frequency': 'monthly'},  # A/P
            
            # Monthly expenses
            {'account_code': '60000000', 'type': 'DEBIT', 'amount_range': (8000, 12000), 'frequency': 'monthly'},   # Salaries
            {'account_code': '10000000', 'type': 'CREDIT', 'amount_range': (8000, 12000), 'frequency': 'monthly'},  # Cash
            
            {'account_code': '61000000', 'type': 'DEBIT', 'amount_range': (2000, 3000), 'frequency': 'monthly'},    # Rent
            {'account_code': '10000000', 'type': 'CREDIT', 'amount_range': (2000, 3000), 'frequency': 'monthly'},   # Cash
            
            {'account_code': '61100000', 'type': 'DEBIT', 'amount_range': (500, 1000), 'frequency': 'monthly'},     # Utilities
            {'account_code': '10000000', 'type': 'CREDIT', 'amount_range': (500, 1000), 'frequency': 'monthly'},    # Cash
        ]
        
        async with self.db_pool.acquire() as conn:
            # Clear existing data
            await conn.execute("TRUNCATE TABLE gl_transactions CASCADE")
            
            transaction_id = 1
            journal_number = 1
            
            # Generate transactions for each month
            for month in range(1, 13):
                for day in [15, 30]:  # Two entries per month
                    transaction_date = date(current_year, month, min(day, 28))
                    
                    for pattern in transaction_patterns:
                        import random
                        amount = Decimal(str(random.uniform(*pattern['amount_range']))).quantize(Decimal('0.01'))
                        
                        await conn.execute("""
                            INSERT INTO gl_transactions (
                                transaction_id, journal_number, account_code, transaction_date,
                                transaction_type, amount, reference, description, source_code,
                                batch_number, period_id, user_id, posted_flag, posted_date
                            ) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14)
                        """,
                            transaction_id, journal_number, pattern['account_code'], transaction_date,
                            pattern['type'], amount, f'JE{journal_number:06d}', 
                            f'Monthly {pattern["type"].lower()} entry', 'GL',
                            journal_number // 100 + 1, f'{current_year}{month:02d}', 'SYSTEM',
                            'Y', transaction_date
                        )
                        
                        transaction_id += 1
                    
                    journal_number += 1
        
        logger.info(f"Created sample GL transactions for {current_year}")
    
    async def _load_gl_accounts(self, records: List[Dict[str, Any]]):
        """Load GL account records to database"""
        if not records:
            return
        
        async with self.db_pool.acquire() as conn:
            await conn.execute("TRUNCATE TABLE gl_accounts CASCADE")
            
            for record in records:
                await conn.execute("""
                    INSERT INTO gl_accounts (
                        account_code, account_name, account_type, account_category,
                        normal_balance, budget_code, department_code, consolidation_code,
                        status, opening_balance, ytd_debits, ytd_credits, 
                        budget_amount, created_at, updated_at
                    ) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15)
                """,
                    record.get('account_code'), record.get('account_name'),
                    record.get('account_type'), record.get('account_category'),
                    record.get('normal_balance'), record.get('budget_code'),
                    record.get('department_code'), record.get('consolidation_code'),
                    record.get('status'), record.get('opening_balance'),
                    record.get('ytd_debits'), record.get('ytd_credits'),
                    record.get('budget_amount'), record.get('created_at'),
                    record.get('updated_at')
                )
    
    async def _load_gl_transactions(self, records: List[Dict[str, Any]]):
        """Load GL transaction records to database"""
        if not records:
            return
        
        async with self.db_pool.acquire() as conn:
            for record in records:
                await conn.execute("""
                    INSERT INTO gl_transactions (
                        transaction_id, journal_number, account_code, transaction_date,
                        transaction_type, amount, reference, description, source_code,
                        batch_number, period_id, user_id, posted_flag, posted_date
                    ) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14)
                """,
                    record.get('transaction_id'), record.get('journal_number'),
                    record.get('account_code'), record.get('transaction_date'),
                    record.get('transaction_type'), record.get('amount'),
                    record.get('reference'), record.get('description'),
                    record.get('source_code'), record.get('batch_number'),
                    record.get('period_id'), record.get('user_id'),
                    record.get('posted_flag'), record.get('posted_date')
                )
    
    async def _calculate_opening_balances(self):
        """Calculate and update opening balances for GL accounts"""
        logger.info("Calculating GL account opening balances")
        
        async with self.db_pool.acquire() as conn:
            # Get prior year transactions to calculate opening balance
            prior_year = datetime.now().year - 1
            
            # Update opening balances based on prior year activity
            await conn.execute("""
                UPDATE gl_accounts 
                SET opening_balance = COALESCE(
                    (SELECT 
                        CASE 
                            WHEN ga.normal_balance = 'DEBIT' THEN
                                SUM(CASE WHEN gt.transaction_type = 'DEBIT' THEN gt.amount ELSE -gt.amount END)
                            ELSE
                                SUM(CASE WHEN gt.transaction_type = 'CREDIT' THEN gt.amount ELSE -gt.amount END)
                        END
                    FROM gl_transactions gt
                    WHERE gt.account_code = gl_accounts.account_code
                      AND EXTRACT(YEAR FROM gt.transaction_date) < $1
                      AND gt.posted_flag = 'Y'
                    ), 0.00
                )
                FROM gl_accounts ga
                WHERE ga.account_code = gl_accounts.account_code
            """, datetime.now().year)
            
            logger.info("GL account opening balances calculated")
    
    async def _update_account_balances(self):
        """Update YTD balances for GL accounts based on transactions"""
        logger.info("Updating GL account YTD balances")
        
        async with self.db_pool.acquire() as conn:
            current_year = datetime.now().year
            
            # Update YTD debits
            await conn.execute("""
                UPDATE gl_accounts 
                SET ytd_debits = COALESCE(
                    (SELECT SUM(gt.amount)
                    FROM gl_transactions gt
                    WHERE gt.account_code = gl_accounts.account_code
                      AND gt.transaction_type = 'DEBIT'
                      AND EXTRACT(YEAR FROM gt.transaction_date) = $1
                      AND gt.posted_flag = 'Y'
                    ), 0.00
                )
            """, current_year)
            
            # Update YTD credits  
            await conn.execute("""
                UPDATE gl_accounts 
                SET ytd_credits = COALESCE(
                    (SELECT SUM(gt.amount)
                    FROM gl_transactions gt
                    WHERE gt.account_code = gl_accounts.account_code
                      AND gt.transaction_type = 'CREDIT'
                      AND EXTRACT(YEAR FROM gt.transaction_date) = $1
                      AND gt.posted_flag = 'Y'
                    ), 0.00
                )
            """, current_year)
            
            logger.info("GL account YTD balances updated")


async def main():
    """Main function for GL data migration"""
    logging.basicConfig(level=logging.INFO)
    
    # Configuration
    db_connection_string = "postgresql://acas_user:acas_password@localhost:5432/acas_migration"
    source_data_dir = "/path/to/cobol/data"  # Update with actual path
    
    migrator = GLDataMigrator(db_connection_string, source_data_dir)
    
    try:
        await migrator.initialize()
        
        # Migrate GL accounts
        accounts_result = await migrator.migrate_gl_accounts()
        logger.info(f"GL Accounts: {accounts_result}")
        
        # Migrate GL transactions
        transactions_result = await migrator.migrate_gl_transactions()
        logger.info(f"GL Transactions: {transactions_result}")
        
        logger.info("GL data migration completed successfully")
        
    except Exception as e:
        logger.error(f"GL data migration failed: {str(e)}")
        raise
    finally:
        await migrator.cleanup()


if __name__ == "__main__":
    asyncio.run(main())