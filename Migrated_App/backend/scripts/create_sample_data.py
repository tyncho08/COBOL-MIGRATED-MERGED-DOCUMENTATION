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

from sqlalchemy import create_engine, text
from sqlalchemy.orm import sessionmaker
from app.core.config import settings
from app.models.customer import SalesLedgerRec
from app.models.supplier import PurchaseLedgerRec
from app.models.stock import StockRec, StockAuditRec
from app.models.gl_accounts import GLLedgerRec
from app.models.customer import SalesInvoiceRec
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

def create_demo_invoices(session):
    """Create demo sales invoices with historical data"""
    from datetime import datetime, timedelta
    import random
    
    # Base date for generating historical invoices
    base_date = datetime.now()
    
    # Customer list for random assignment (must match created customers)
    customers = ['CUST001', 'CUST002', 'CUST003']
    
    # Use raw SQL to insert invoices with correct data types
    insert_query = text("""
        INSERT INTO acas.sainvoice_rec (
            invoice_key,
            invoice_customer,
            invoice_date,
            invoice_type,
            invoice_status,
            invoice_goods_amount,
            invoice_vat_amount,
            invoice_total_amount,
            invoice_paid_amount,
            invoice_balance,
            invoice_discount_amount,
            invoice_order_no,
            invoice_terms,
            invoice_printed,
            invoice_emailed,
            invoice_period
        ) VALUES (
            :invoice_key,
            :invoice_customer,
            :invoice_date,
            :invoice_type,
            :invoice_status,
            :invoice_goods_amount,
            :invoice_vat_amount,
            :invoice_total_amount,
            :invoice_paid_amount,
            :invoice_balance,
            :invoice_discount_amount,
            :invoice_order_no,
            :invoice_terms,
            :invoice_printed,
            :invoice_emailed,
            :invoice_period
        )
    """)
    
    invoice_counter = 10001  # Start with a higher number
    invoices_created = 0
    
    # Generate invoices for the last 12 months
    for months_ago in range(12):
        # Generate 5-8 invoices per month
        num_invoices = random.randint(5, 8)
        
        for _ in range(num_invoices):
            # Calculate invoice date
            invoice_date = base_date - timedelta(days=months_ago * 30 + random.randint(0, 29))
            
            # Random invoice amount between 500 and 10000
            invoice_amount = Decimal(str(round(random.uniform(500, 10000), 2)))
            
            # Random payment status
            is_paid = random.random() > 0.3  # 70% chance of being paid
            
            if is_paid:
                # Paid invoice
                balance = Decimal('0.00')
            else:
                # Outstanding invoice  
                balance = invoice_amount
            
            # Insert invoice
            session.execute(insert_query, {
                'invoice_key': invoice_counter,
                'invoice_customer': random.choice(customers),
                'invoice_date': int(invoice_date.strftime('%Y%m%d')),
                'invoice_type': 'I',  # Invoice
                'invoice_status': 'P' if is_paid else 'O',  # Paid or Outstanding
                'invoice_goods_amount': float(invoice_amount * Decimal('0.80')),
                'invoice_vat_amount': float(invoice_amount * Decimal('0.20')),
                'invoice_total_amount': float(invoice_amount),
                'invoice_paid_amount': float(invoice_amount - balance),
                'invoice_balance': float(balance),
                'invoice_discount_amount': 0.00,
                'invoice_order_no': f'ORD-{invoice_counter}',
                'invoice_terms': '30',
                'invoice_printed': 'Y',
                'invoice_emailed': 'N',
                'invoice_period': invoice_date.month
            })
            
            invoice_counter += 1
            invoices_created += 1
    
    # Add recent outstanding invoices for testing
    recent_date = datetime.now()
    for i in range(5):  # Create 5 recent invoices
        days_ago = random.randint(1, 15)
        invoice_date = recent_date - timedelta(days=days_ago)
        invoice_amount = Decimal(str(round(random.uniform(2000, 8000), 2)))
        
        session.execute(insert_query, {
            'invoice_key': invoice_counter,
            'invoice_customer': random.choice(customers),
            'invoice_date': int(invoice_date.strftime('%Y%m%d')),
            'invoice_type': 'I',
            'invoice_status': 'O',  # All recent ones are outstanding
            'invoice_goods_amount': float(invoice_amount * Decimal('0.80')),
            'invoice_vat_amount': float(invoice_amount * Decimal('0.20')),
            'invoice_total_amount': float(invoice_amount),
            'invoice_paid_amount': 0.00,
            'invoice_balance': float(invoice_amount),
            'invoice_discount_amount': 0.00,
            'invoice_order_no': f'ORD-{invoice_counter}',
            'invoice_terms': '30',
            'invoice_printed': 'Y' if random.random() > 0.5 else 'N',
            'invoice_emailed': 'Y' if random.random() > 0.7 else 'N',
            'invoice_period': invoice_date.month
        })
        
        invoice_counter += 1
        invoices_created += 1
    
    logger.info(f"Created {invoices_created} demo sales invoices")

def create_demo_stock_movements(session):
    """Create demo stock movements (audit records)"""
    from datetime import datetime, timedelta
    import random
    
    # Stock items we have
    stock_items = ['ITEM001', 'ITEM002', 'ITEM003']
    
    # Movement types
    movement_types = ['R', 'I', 'A', 'T', 'C']  # Receipt, Issue, Adjust, Transfer, Count
    sources = ['PO', 'SO', 'ADJ', 'TRF', 'CNT', 'WO']
    
    # Generate movements for the last 30 days
    base_date = datetime.now()
    
    for days_ago in range(30, 0, -1):
        movement_date = base_date - timedelta(days=days_ago)
        movement_time = random.randint(80000, 170000)  # 8am to 5pm in HHMMSS format
        
        # Generate 1-3 movements per day
        num_movements = random.randint(1, 3)
        
        for _ in range(num_movements):
            stock_item = random.choice(stock_items)
            movement_type = random.choice(movement_types)
            
            # Set quantities based on movement type
            if movement_type == 'R':  # Receipt
                qty_before = random.uniform(10, 100)
                qty_change = random.uniform(10, 50)
                qty_after = qty_before + qty_change
                source = 'PO'
                reference = f'PO-{random.randint(1000, 9999)}'
                reason = 'Goods receipt from purchase order'
            elif movement_type == 'I':  # Issue
                qty_before = random.uniform(50, 150)
                qty_change = -random.uniform(5, 20)
                qty_after = qty_before + qty_change
                source = 'SO'
                reference = f'SO-{random.randint(1000, 9999)}'
                reason = 'Stock issue for sales order'
            elif movement_type == 'A':  # Adjustment
                qty_before = random.uniform(20, 100)
                qty_change = random.uniform(-10, 10)
                qty_after = qty_before + qty_change
                source = 'ADJ'
                reference = f'ADJ-{random.randint(100, 999)}'
                reason = random.choice(['Stock count adjustment', 'Damaged goods', 'Quality inspection'])
            elif movement_type == 'T':  # Transfer
                qty_before = random.uniform(30, 80)
                qty_change = 0  # Transfers don't change total qty
                qty_after = qty_before
                source = 'TRF'
                reference = f'TRF-{random.randint(100, 999)}'
                reason = 'Location transfer'
            else:  # 'C' - Count
                qty_before = random.uniform(40, 100)
                qty_change = random.uniform(-5, 5)
                qty_after = qty_before + qty_change
                source = 'CNT'
                reference = f'CNT-{movement_date.strftime("%Y%m")}'
                reason = 'Physical stock count'
            
            # Use raw SQL to insert with correct column names
            insert_query = text("""
                INSERT INTO acas.stockaudit_rec (
                    audit_date,
                    audit_time,
                    audit_stock_code,
                    audit_type,
                    audit_reference,
                    audit_source,
                    audit_qty,
                    audit_qty_before,
                    audit_qty_after,
                    audit_cost,
                    audit_value,
                    audit_user,
                    audit_reason
                ) VALUES (
                    :audit_date,
                    :audit_time,
                    :audit_stock_code,
                    :audit_type,
                    :audit_reference,
                    :audit_source,
                    :audit_qty,
                    :audit_qty_before,
                    :audit_qty_after,
                    :audit_cost,
                    :audit_value,
                    :audit_user,
                    :audit_reason
                )
            """)
            
            # Get cost from stock item (simplified)
            cost = 100.00 if stock_item == 'ITEM001' else 750.00 if stock_item == 'ITEM002' else 3.50
            
            session.execute(insert_query, {
                'audit_date': int(movement_date.strftime('%Y%m%d')),
                'audit_time': movement_time,
                'audit_stock_code': stock_item,
                'audit_type': movement_type,
                'audit_reference': reference,
                'audit_source': source,
                'audit_qty': abs(qty_change),
                'audit_qty_before': round(qty_before, 3),
                'audit_qty_after': round(qty_after, 3),
                'audit_cost': cost,
                'audit_value': round(abs(qty_change) * cost, 2),
                'audit_user': random.choice(['ADMIN', 'WAREHOUSE', 'SALES01', 'PURCH01']),
                'audit_reason': reason
            })
    
    # Add recent movements for today
    today = int(datetime.now().strftime('%Y%m%d'))
    current_time = int(datetime.now().strftime('%H%M%S'))
    
    recent_movements = [
        {
            'audit_date': today,
            'audit_time': current_time - 10000,
            'audit_stock_code': 'ITEM001',
            'audit_type': 'R',
            'audit_reference': 'PO-2025-001',
            'audit_source': 'PO',
            'audit_qty': 50.000,
            'audit_qty_before': 45.000,
            'audit_qty_after': 95.000,
            'audit_cost': 125.00,
            'audit_value': 6250.00,
            'audit_user': 'ADMIN',
            'audit_reason': 'Purchase order receipt - Restock'
        },
        {
            'audit_date': today,
            'audit_time': current_time - 5000,
            'audit_stock_code': 'ITEM002',
            'audit_type': 'I',
            'audit_reference': 'SO-2025-045',
            'audit_source': 'SO',
            'audit_qty': 5.000,
            'audit_qty_before': 30.000,
            'audit_qty_after': 25.000,
            'audit_cost': 750.00,
            'audit_value': 3750.00,
            'audit_user': 'SALES01',
            'audit_reason': 'Customer order shipment'
        },
        {
            'audit_date': today,
            'audit_time': current_time - 2000,
            'audit_stock_code': 'ITEM003',
            'audit_type': 'A',
            'audit_reference': 'ADJ-2025-012',
            'audit_source': 'ADJ',
            'audit_qty': 2.000,
            'audit_qty_before': 500.000,
            'audit_qty_after': 498.000,
            'audit_cost': 3.50,
            'audit_value': 7.00,
            'audit_user': 'WAREHOUSE',
            'audit_reason': 'Damaged items written off'
        }
    ]
    
    # Insert recent movements
    for movement in recent_movements:
        session.execute(insert_query, movement)
    
    logger.info(f"Created {30*2 + len(recent_movements)} demo stock movements")

def populate_demo_data():
    """Main function to populate all demo data"""
    try:
        engine = create_engine(settings.DATABASE_URL)
        SessionLocal = sessionmaker(autocommit=False, autoflush=False, bind=engine)
        
        logger.info("Starting demo data population...")
        
        # Create demo data with separate transactions for each type
        session = SessionLocal()
        try:
            create_demo_customers(session)
            session.commit()
            logger.info("Customers committed successfully")
        except Exception as e:
            logger.warning(f"Could not create customers (may already exist): {e}")
            session.rollback()
        finally:
            session.close()
        
        session = SessionLocal()
        try:
            create_demo_suppliers(session)
            session.commit()
            logger.info("Suppliers committed successfully")
        except Exception as e:
            logger.warning(f"Could not create suppliers (may already exist): {e}")
            session.rollback()
        finally:
            session.close()
        
        session = SessionLocal()
        try:
            create_demo_stock_items(session)
            session.commit()
            logger.info("Stock items committed successfully")
        except Exception as e:
            logger.warning(f"Could not create stock items (may already exist): {e}")
            session.rollback()
        finally:
            session.close()
        
        session = SessionLocal()
        try:
            create_demo_gl_accounts(session)
            session.commit()
            logger.info("GL accounts committed successfully")
        except Exception as e:
            logger.warning(f"Could not create GL accounts (may already exist): {e}")
            session.rollback()
        finally:
            session.close()
        
        session = SessionLocal()
        try:
            create_demo_invoices(session)
            session.commit()
            logger.info("Invoices committed successfully")
        except Exception as e:
            logger.warning(f"Could not create invoices (may already exist): {e}")
            session.rollback()
        finally:
            session.close()
        
        session = SessionLocal()
        try:
            create_demo_stock_movements(session)
            session.commit()
            logger.info("Stock movements committed successfully")
        except Exception as e:
            logger.warning(f"Could not create stock movements: {e}")
            session.rollback()
        finally:
            session.close()
        
        logger.info("Demo data population completed")
        
    except Exception as e:
        logger.error(f"Demo data population failed: {e}")
        if 'session' in locals():
            session.rollback()
            session.close()
        raise

if __name__ == "__main__":
    populate_demo_data()