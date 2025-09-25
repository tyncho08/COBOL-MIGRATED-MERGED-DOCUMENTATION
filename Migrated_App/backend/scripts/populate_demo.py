#!/usr/bin/env python3
"""
ACAS Demo Data Population Script
Populates database with realistic business demo data
"""
import sys
from pathlib import Path
from decimal import Decimal
from datetime import datetime, date
import random

# Add parent directory to path
sys.path.append(str(Path(__file__).parent.parent))

from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker
from app.core.config import settings
from app.models.customer import SalesLedgerRec
from app.models.supplier import PurchaseLedgerRec
from app.models.stock import StockRec
from app.models.gl_accounts import GLLedgerRec
import logging

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

def create_demo_customers(session):
    """Create demo customers"""
    customers = [
        {
            'sales_key': 'CUST001',
            'sales_name': 'ACME Corporation Ltd',
            'sales_address_1': '123 Business Street',
            'sales_address_2': 'Commercial District',
            'sales_address_3': 'London',
            'sales_address_4': 'England',
            'sales_address_5': 'SW1A 1AA',
            'sales_contact': 'John Smith',
            'sales_phone': '020 7123 4567',
            'sales_email': 'accounts@acmecorp.com',
            'sales_credit_limit': Decimal('10000.00'),
            'sales_discount_rate': Decimal('2.50'),
            'sales_payment_terms': '30',
            'sales_tax_code': 'VSTD',
            'sales_balance': Decimal('2500.00'),
            'sales_ytd_turnover': Decimal('25000.00')
        },
        {
            'sales_key': 'CUST002',
            'sales_name': 'Global Industries PLC',
            'sales_address_1': '456 Industrial Estate',
            'sales_address_2': 'Manufacturing Zone',
            'sales_address_3': 'Birmingham',
            'sales_address_4': 'West Midlands',
            'sales_address_5': 'B1 2CD',
            'sales_contact': 'Sarah Johnson',
            'sales_phone': '0121 234 5678',
            'sales_email': 'finance@globalind.com',
            'sales_credit_limit': Decimal('15000.00'),
            'sales_discount_rate': Decimal('5.00'),
            'sales_payment_terms': '60',
            'sales_tax_code': 'VSTD',
            'sales_balance': Decimal('7500.00'),
            'sales_ytd_turnover': Decimal('45000.00')
        },
        {
            'sales_key': 'CUST003',
            'sales_name': 'Tech Solutions Ltd',
            'sales_address_1': '789 Technology Park',
            'sales_address_2': 'Innovation Centre',
            'sales_address_3': 'Cambridge',
            'sales_address_4': 'Cambridgeshire',
            'sales_address_5': 'CB1 3EF',
            'sales_contact': 'Mike Williams',
            'sales_phone': '01223 345 678',
            'sales_email': 'procurement@techsol.com',
            'sales_credit_limit': Decimal('5000.00'),
            'sales_discount_rate': Decimal('0.00'),
            'sales_payment_terms': '30',
            'sales_tax_code': 'VSTD',
            'sales_balance': Decimal('1200.00'),
            'sales_ytd_turnover': Decimal('12000.00')
        }
    ]
    
    for customer_data in customers:
        customer = SalesLedgerRec(**customer_data)
        session.add(customer)
    
    logger.info(f"Created {len(customers)} demo customers")

def create_demo_suppliers(session):
    """Create demo suppliers"""
    suppliers = [
        {
            'purch_key': 'SUPP001',
            'purch_name': 'Office Supplies Direct',
            'purch_address_1': '321 Supplier Street',
            'purch_address_2': 'Warehouse District',
            'purch_address_3': 'Manchester',
            'purch_address_4': 'Greater Manchester',
            'purch_address_5': 'M1 4GH',
            'purch_contact': 'Emma Brown',
            'purch_phone': '0161 123 4567',
            'purch_email': 'sales@officesupplies.com',
            'purch_payment_terms': '30',
            'purch_tax_code': 'VSTD',
            'purch_balance': Decimal('1500.00'),
            'purch_ytd_turnover': Decimal('18000.00')
        },
        {
            'purch_key': 'SUPP002',
            'purch_name': 'Manufacturing Components Ltd',
            'purch_address_1': '654 Industrial Avenue',
            'purch_address_2': 'Factory Estate',
            'purch_address_3': 'Sheffield',
            'purch_address_4': 'South Yorkshire',
            'purch_address_5': 'S2 5JK',
            'purch_contact': 'David Wilson',
            'purch_phone': '0114 234 5678',
            'purch_email': 'orders@mancomp.com',
            'purch_payment_terms': '60',
            'purch_tax_code': 'VSTD',
            'purch_balance': Decimal('3200.00'),
            'purch_ytd_turnover': Decimal('38000.00')
        }
    ]
    
    for supplier_data in suppliers:
        supplier = PurchaseLedgerRec(**supplier_data)
        session.add(supplier)
    
    logger.info(f"Created {len(suppliers)} demo suppliers")

def create_demo_stock_items(session):
    """Create demo stock items"""
    stock_items = [
        {
            'stock_key': 'ITEM001',
            'stock_desc': 'Office Desk Standard',
            'stock_abrev_key': 'DESK-STD',
            'stock_location': 'A01',
            'stock_qty_on_hand': Decimal('50.000'),
            'stock_qty_allocated': Decimal('10.000'),
            'stock_qty_available': Decimal('40.000'),
            'stock_reorder_point': Decimal('20.000'),
            'stock_reorder_qty': Decimal('100.000'),
            'stock_std_cost': Decimal('125.0000'),
            'stock_avg_cost': Decimal('127.5000'),
            'stock_list_price': Decimal('199.9900'),
            'stock_costing_method': 'A',
            'stock_product_group': 'FUR',
            'stock_unit_of_measure': 'EA',
            'stock_tax_code': 'VSTD'
        },
        {
            'stock_key': 'ITEM002',
            'stock_desc': 'Laptop Computer Pro',
            'stock_abrev_key': 'LAP-PRO',
            'stock_location': 'B02',
            'stock_qty_on_hand': Decimal('25.000'),
            'stock_qty_allocated': Decimal('5.000'),
            'stock_qty_available': Decimal('20.000'),
            'stock_reorder_point': Decimal('10.000'),
            'stock_reorder_qty': Decimal('50.000'),
            'stock_std_cost': Decimal('750.0000'),
            'stock_avg_cost': Decimal('762.5000'),
            'stock_list_price': Decimal('1299.9900'),
            'stock_costing_method': 'F',
            'stock_product_group': 'TEC',
            'stock_unit_of_measure': 'EA',
            'stock_tax_code': 'VSTD'
        },
        {
            'stock_key': 'ITEM003',
            'stock_desc': 'Printer Paper A4',
            'stock_abrev_key': 'PAP-A4',
            'stock_location': 'C03',
            'stock_qty_on_hand': Decimal('500.000'),
            'stock_qty_allocated': Decimal('50.000'),
            'stock_qty_available': Decimal('450.000'),
            'stock_reorder_point': Decimal('100.000'),
            'stock_reorder_qty': Decimal('1000.000'),
            'stock_std_cost': Decimal('3.5000'),
            'stock_avg_cost': Decimal('3.4500'),
            'stock_list_price': Decimal('5.9900'),
            'stock_costing_method': 'A',
            'stock_product_group': 'OFF',
            'stock_unit_of_measure': 'RM',
            'stock_tax_code': 'VSTD'
        }
    ]
    
    for stock_data in stock_items:
        stock_item = StockRec(**stock_data)
        session.add(stock_item)
    
    logger.info(f"Created {len(stock_items)} demo stock items")

def create_demo_gl_accounts(session):
    """Create demo GL accounts"""
    gl_accounts = [
        # Assets
        {
            'ledger_key': 10010000,
            'ledger_type': 1,
            'ledger_place': 'B',
            'ledger_level': 4,
            'ledger_name': 'Petty Cash',
            'ledger_balance': Decimal('500.00')
        },
        {
            'ledger_key': 10020000,
            'ledger_type': 1,
            'ledger_place': 'B',
            'ledger_level': 4,
            'ledger_name': 'Bank Current Account',
            'ledger_balance': Decimal('25000.00')
        },
        {
            'ledger_key': 11010000,
            'ledger_type': 1,
            'ledger_place': 'B',
            'ledger_level': 4,
            'ledger_name': 'Trade Debtors Control',
            'ledger_balance': Decimal('11200.00')
        },
        {
            'ledger_key': 12010000,
            'ledger_type': 1,
            'ledger_place': 'B',
            'ledger_level': 4,
            'ledger_name': 'Stock - Finished Goods',
            'ledger_balance': Decimal('47500.00')
        },
        # Liabilities
        {
            'ledger_key': 20010000,
            'ledger_type': 2,
            'ledger_place': 'B',
            'ledger_level': 4,
            'ledger_name': 'Trade Creditors Control',
            'ledger_balance': Decimal('4700.00')
        },
        {
            'ledger_key': 20020000,
            'ledger_type': 2,
            'ledger_place': 'B',
            'ledger_level': 4,
            'ledger_name': 'VAT Output Tax',
            'ledger_balance': Decimal('2240.00')
        },
        # Capital
        {
            'ledger_key': 30010000,
            'ledger_type': 3,
            'ledger_place': 'B',
            'ledger_level': 4,
            'ledger_name': 'Share Capital',
            'ledger_balance': Decimal('50000.00')
        },
        {
            'ledger_key': 30020000,
            'ledger_type': 3,
            'ledger_place': 'B',
            'ledger_level': 4,
            'ledger_name': 'Retained Earnings',
            'ledger_balance': Decimal('27260.00')
        },
        # Income
        {
            'ledger_key': 40010000,
            'ledger_type': 4,
            'ledger_place': 'P',
            'ledger_level': 4,
            'ledger_name': 'Sales Revenue',
            'ledger_balance': Decimal('82000.00')
        },
        # Expenses
        {
            'ledger_key': 50010000,
            'ledger_type': 5,
            'ledger_place': 'P',
            'ledger_level': 4,
            'ledger_name': 'Cost of Sales',
            'ledger_balance': Decimal('49200.00')
        },
        {
            'ledger_key': 60010000,
            'ledger_type': 5,
            'ledger_place': 'P',
            'ledger_level': 4,
            'ledger_name': 'Office Expenses',
            'ledger_balance': Decimal('3500.00')
        }
    ]
    
    for gl_data in gl_accounts:
        gl_account = GLLedgerRec(**gl_data)
        session.add(gl_account)
    
    logger.info(f"Created {len(gl_accounts)} demo GL accounts")

def populate_demo_data():
    """Main function to populate all demo data"""
    try:
        engine = create_engine(settings.DATABASE_URL)
        SessionLocal = sessionmaker(autocommit=False, autoflush=False, bind=engine)
        
        session = SessionLocal()
        
        logger.info("Starting demo data population...")
        
        # Create demo data
        create_demo_customers(session)
        create_demo_suppliers(session)
        create_demo_stock_items(session)
        create_demo_gl_accounts(session)
        
        # Commit all changes
        session.commit()
        session.close()
        
        logger.info("Demo data population completed successfully")
        
    except Exception as e:
        logger.error(f"Demo data population failed: {e}")
        if 'session' in locals():
            session.rollback()
            session.close()
        raise

if __name__ == "__main__":
    populate_demo_data()