#!/usr/bin/env python3
"""
ACAS Sample Data Creation Script
Creates sample data compatible with the real database schema
"""
import sys
import os
from pathlib import Path

# Add parent directory to path
sys.path.append(str(Path(__file__).parent.parent))

import psycopg2
import logging
from datetime import datetime

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

def create_sample_data():
    """Create sample data directly in PostgreSQL using the correct schema"""
    try:
        # Get database connection parameters from environment
        user = os.environ.get('PGUSER', 'postgres')
        password = os.environ.get('PGPASSWORD', '')
        host = os.environ.get('PGHOST', 'localhost')
        port = os.environ.get('PGPORT', '5432')
        dbname = 'acas_db'
        
        logger.info(f"Connecting to PostgreSQL at {host}:{port} as user {user}")
        
        # Connect to database
        conn = psycopg2.connect(
            host=host,
            port=port,
            database=dbname,
            user=user,
            password=password
        )
        cursor = conn.cursor()
        
        logger.info("Creating sample data in database with corrected audit system...")
        
        # Create sample customers
        logger.info("Creating sample customers...")
        customers = [
            {
                'sales_key': 'CUST001',
                'sales_name': 'ACME Corporation Ltd',
                'sales_address_1': '123 Business Street',
                'sales_address_2': 'Commercial District',
                'sales_address_3': 'London',
                'sales_address_4': 'England',
                'sales_address_5': 'SW1A 1AA',
                'sales_country': 'United Kingdom',
                'sales_contact': 'John Smith',
                'sales_phone': '020 7123 4567',
                'sales_email': 'accounts@acmecorp.com',
                'sales_credit_limit': 10000.00,
                'sales_balance': 2500.00,
                'sales_ytd_turnover': 25000.00,
                'sales_payment_terms': '30',
                'sales_discount_rate': 2.50,
                'sales_tax_code': 'VSTD'
            },
            {
                'sales_key': 'CUST002',
                'sales_name': 'Global Industries PLC',
                'sales_address_1': '456 Industrial Estate',
                'sales_address_2': 'Manufacturing Zone',
                'sales_address_3': 'Birmingham',
                'sales_address_4': 'West Midlands',
                'sales_address_5': 'B1 2CD',
                'sales_country': 'United Kingdom',
                'sales_contact': 'Sarah Johnson',
                'sales_phone': '0121 234 5678',
                'sales_email': 'finance@globalind.com',
                'sales_credit_limit': 15000.00,
                'sales_balance': 7500.00,
                'sales_ytd_turnover': 45000.00,
                'sales_payment_terms': '30',
                'sales_discount_rate': 5.00,
                'sales_tax_code': 'VSTD'
            },
            {
                'sales_key': 'CUST003',
                'sales_name': 'Tech Solutions Ltd',
                'sales_address_1': '789 Technology Park',
                'sales_address_2': 'Innovation Centre',
                'sales_address_3': 'Cambridge',
                'sales_address_4': 'Cambridgeshire',
                'sales_address_5': 'CB1 3EF',
                'sales_country': 'United Kingdom',
                'sales_contact': 'Mike Williams',
                'sales_phone': '01223 345 678',
                'sales_email': 'procurement@techsol.com',
                'sales_credit_limit': 5000.00,
                'sales_balance': 1200.00,
                'sales_ytd_turnover': 12000.00,
                'sales_payment_terms': '30',
                'sales_discount_rate': 0.00,
                'sales_tax_code': 'VSTD'
            }
        ]
        
        for customer in customers:
            cursor.execute("""
                INSERT INTO acas.saledger_rec (
                    sales_key, sales_name, sales_address_1, sales_address_2, sales_address_3,
                    sales_address_4, sales_address_5, sales_country, sales_contact, sales_phone,
                    sales_email, sales_credit_limit, sales_balance, sales_ytd_turnover,
                    sales_payment_terms, sales_discount_rate, sales_tax_code
                ) VALUES (
                    %(sales_key)s, %(sales_name)s, %(sales_address_1)s, %(sales_address_2)s, %(sales_address_3)s,
                    %(sales_address_4)s, %(sales_address_5)s, %(sales_country)s, %(sales_contact)s, %(sales_phone)s,
                    %(sales_email)s, %(sales_credit_limit)s, %(sales_balance)s, %(sales_ytd_turnover)s,
                    %(sales_payment_terms)s, %(sales_discount_rate)s, %(sales_tax_code)s
                ) ON CONFLICT (sales_key) DO NOTHING
            """, customer)
        
        logger.info(f"Created {len(customers)} sample customers")
        
        # Create sample suppliers
        logger.info("Creating sample suppliers...")
        suppliers = [
            {
                'purch_key': 'SUPP001',
                'purch_name': 'Office Supplies Direct Ltd',
                'purch_address_1': '100 Warehouse Road',
                'purch_address_2': 'Industrial Park',
                'purch_address_3': 'Manchester',
                'purch_address_4': 'Greater Manchester',
                'purch_address_5': 'M1 1AA',
                'purch_country': 'United Kingdom',
                'purch_contact': 'David Brown',
                'purch_phone': '0161 555 0123',
                'purch_email': 'orders@officesupplies.com',
                'purch_balance': -1500.00,
                'purch_ytd_turnover': 18000.00,
                'purch_payment_terms': '30',
                'purch_tax_code': 'VSTD'
            },
            {
                'purch_key': 'SUPP002',
                'purch_name': 'Tech Hardware Solutions',
                'purch_address_1': '250 Technology Drive',
                'purch_address_2': 'Business Quarter',
                'purch_address_3': 'Reading',
                'purch_address_4': 'Berkshire',
                'purch_address_5': 'RG1 2BB',
                'purch_country': 'United Kingdom',
                'purch_contact': 'Lisa Chen',
                'purch_phone': '0118 555 0456',
                'purch_email': 'sales@techhw.com',
                'purch_balance': -3200.00,
                'purch_ytd_turnover': 35000.00,
                'purch_payment_terms': '45',
                'purch_tax_code': 'VSTD'
            }
        ]
        
        for supplier in suppliers:
            cursor.execute("""
                INSERT INTO acas.puledger_rec (
                    purch_key, purch_name, purch_address_1, purch_address_2, purch_address_3,
                    purch_address_4, purch_address_5, purch_country, purch_contact, purch_phone,
                    purch_email, purch_balance, purch_ytd_turnover, purch_payment_terms, purch_tax_code
                ) VALUES (
                    %(purch_key)s, %(purch_name)s, %(purch_address_1)s, %(purch_address_2)s, %(purch_address_3)s,
                    %(purch_address_4)s, %(purch_address_5)s, %(purch_country)s, %(purch_contact)s, %(purch_phone)s,
                    %(purch_email)s, %(purch_balance)s, %(purch_ytd_turnover)s, %(purch_payment_terms)s, %(purch_tax_code)s
                ) ON CONFLICT (purch_key) DO NOTHING
            """, supplier)
        
        logger.info(f"Created {len(suppliers)} sample suppliers")
        
        # Create sample stock items
        logger.info("Creating sample stock items...")
        stock_items = [
            {
                'stock_key': 'LAPTOP001',
                'stock_desc': 'Business Laptop - 15 inch',
                'stock_abrev_key': 'LAP001',
                'stock_location': 'WH001',
                'stock_bin': 'A01-001',
                'stock_qty_on_hand': 25.000,
                'stock_qty_available': 20.000,
                'stock_reorder_point': 10.000,
                'stock_reorder_qty': 20.000,
                'stock_std_cost': 750.0000,
                'stock_avg_cost': 780.0000,
                'stock_last_cost': 800.0000,
                'stock_list_price': 1200.0000,
                'stock_price_1': 1100.0000,
                'stock_price_2': 1050.0000,
                'stock_product_group': 'COMP',
                'stock_unit_of_measure': 'EA',
                'stock_tax_code': 'VSTD',
                'stock_primary_supplier': 'SUPP002'
            },
            {
                'stock_key': 'DESK001',
                'stock_desc': 'Executive Office Desk',
                'stock_abrev_key': 'DESK01',
                'stock_location': 'WH001',
                'stock_bin': 'B02-001',
                'stock_qty_on_hand': 15.000,
                'stock_qty_available': 12.000,
                'stock_reorder_point': 5.000,
                'stock_reorder_qty': 10.000,
                'stock_std_cost': 200.0000,
                'stock_avg_cost': 215.0000,
                'stock_last_cost': 220.0000,
                'stock_list_price': 350.0000,
                'stock_price_1': 320.0000,
                'stock_price_2': 300.0000,
                'stock_product_group': 'FURN',
                'stock_unit_of_measure': 'EA',
                'stock_tax_code': 'VSTD',
                'stock_primary_supplier': 'SUPP001'
            },
            {
                'stock_key': 'PAPER001',
                'stock_desc': 'A4 Copy Paper - Box of 5 Reams',
                'stock_abrev_key': 'PAP001',
                'stock_location': 'WH001',
                'stock_bin': 'C01-001',
                'stock_qty_on_hand': 100.000,
                'stock_qty_available': 85.000,
                'stock_reorder_point': 25.000,
                'stock_reorder_qty': 50.000,
                'stock_std_cost': 15.0000,
                'stock_avg_cost': 16.5000,
                'stock_last_cost': 17.0000,
                'stock_list_price': 25.0000,
                'stock_price_1': 22.0000,
                'stock_price_2': 20.0000,
                'stock_product_group': 'STAT',
                'stock_unit_of_measure': 'BOX',
                'stock_tax_code': 'VSTD',
                'stock_primary_supplier': 'SUPP001'
            }
        ]
        
        for item in stock_items:
            cursor.execute("""
                INSERT INTO acas.stock_rec (
                    stock_key, stock_desc, stock_abrev_key, stock_location, stock_bin,
                    stock_qty_on_hand, stock_qty_available, stock_reorder_point, stock_reorder_qty,
                    stock_std_cost, stock_avg_cost, stock_last_cost, stock_list_price, stock_price_1, stock_price_2,
                    stock_product_group, stock_unit_of_measure, stock_tax_code, stock_primary_supplier
                ) VALUES (
                    %(stock_key)s, %(stock_desc)s, %(stock_abrev_key)s, %(stock_location)s, %(stock_bin)s,
                    %(stock_qty_on_hand)s, %(stock_qty_available)s, %(stock_reorder_point)s, %(stock_reorder_qty)s,
                    %(stock_std_cost)s, %(stock_avg_cost)s, %(stock_last_cost)s, %(stock_list_price)s, %(stock_price_1)s, %(stock_price_2)s,
                    %(stock_product_group)s, %(stock_unit_of_measure)s, %(stock_tax_code)s, %(stock_primary_supplier)s
                ) ON CONFLICT (stock_key) DO NOTHING
            """, item)
        
        logger.info(f"Created {len(stock_items)} sample stock items")
        
        # Commit all changes
        conn.commit()
        cursor.close()
        conn.close()
        
        logger.info("Sample data creation completed successfully!")
        
        # Verify data was created
        logger.info("Verifying sample data...")
        conn = psycopg2.connect(
            host=host,
            port=port,
            database=dbname,
            user=user,
            password=password
        )
        cursor = conn.cursor()
        
        cursor.execute("SELECT COUNT(*) FROM acas.saledger_rec")
        customer_count = cursor.fetchone()[0]
        logger.info(f"Total customers in database: {customer_count}")
        
        cursor.execute("SELECT COUNT(*) FROM acas.puledger_rec")
        supplier_count = cursor.fetchone()[0]
        logger.info(f"Total suppliers in database: {supplier_count}")
        
        cursor.execute("SELECT COUNT(*) FROM acas.stock_rec")
        stock_count = cursor.fetchone()[0]
        logger.info(f"Total stock items in database: {stock_count}")
        
        cursor.close()
        conn.close()
        
    except Exception as e:
        logger.error(f"Sample data creation failed: {e}")
        raise

if __name__ == "__main__":
    create_sample_data()