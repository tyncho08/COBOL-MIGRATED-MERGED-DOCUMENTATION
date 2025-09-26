-- ACAS Complete PostgreSQL Database Schema
-- Version 3.02 - 100% Migration from COBOL
-- All 43 tables with exact field mappings

-- Enable required extensions
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";
CREATE EXTENSION IF NOT EXISTS "pgcrypto";

-- Drop existing schema if needed
DROP SCHEMA IF EXISTS acas CASCADE;
CREATE SCHEMA acas;

SET search_path TO acas, public;

-- =============================================
-- CORE SYSTEM TABLES (From COBOL)
-- =============================================

-- 1. SYSTEM_REC - System Configuration (from wssystem.cob)
CREATE TABLE system_rec (
    system_rec_key INTEGER PRIMARY KEY DEFAULT 1 CHECK (system_rec_key = 1),
    -- System Data Block (512 bytes)
    system_record_version_prime SMALLINT DEFAULT 1,
    system_record_version_secondary SMALLINT DEFAULT 4,
    -- VAT Rates
    vat_rate_1 DECIMAL(4,2) DEFAULT 20.00,  -- Standard rate
    vat_rate_2 DECIMAL(4,2) DEFAULT 5.00,   -- Reduced rate  
    vat_rate_3 DECIMAL(4,2) DEFAULT 0.00,   -- Minimal/exempt
    vat_rate_4 DECIMAL(4,2) DEFAULT 0.00,   -- Local sales tax
    vat_rate_5 DECIMAL(4,2) DEFAULT 0.00,   -- Local sales tax
    -- Period Control
    cyclea SMALLINT DEFAULT 0,
    period SMALLINT DEFAULT 1,
    page_lines SMALLINT DEFAULT 66,
    next_invoice BIGINT DEFAULT 1,
    run_date INTEGER DEFAULT 0,
    start_date INTEGER DEFAULT 0,
    end_date INTEGER DEFAULT 0,
    -- User and Company
    suser VARCHAR(32) DEFAULT '',
    user_code VARCHAR(32) DEFAULT '',
    address_1 VARCHAR(24) DEFAULT '',
    address_2 VARCHAR(24) DEFAULT '',
    address_3 VARCHAR(24) DEFAULT '',
    address_4 VARCHAR(24) DEFAULT '',
    post_code VARCHAR(12) DEFAULT '',
    country VARCHAR(24) DEFAULT '',
    print_spool_name VARCHAR(48) DEFAULT '',
    pass_value SMALLINT DEFAULT 0,
    -- Module Levels
    level_1 SMALLINT DEFAULT 1,  -- G/L
    level_2 SMALLINT DEFAULT 1,  -- P/L
    level_3 SMALLINT DEFAULT 1,  -- S/L
    level_4 SMALLINT DEFAULT 1,  -- Stock
    level_5 SMALLINT DEFAULT 0,  -- IRS
    level_6 SMALLINT DEFAULT 0,  -- OE
    pass_word VARCHAR(4) DEFAULT '',
    host SMALLINT DEFAULT 0,
    op_system SMALLINT DEFAULT 5,  -- Unix/Linux
    current_quarter SMALLINT DEFAULT 1,
    -- RDBMS Settings
    file_system_used SMALLINT DEFAULT 0,
    file_duplicates_in_use SMALLINT DEFAULT 0,
    maps_ser VARCHAR(4) DEFAULT '9999',
    date_form SMALLINT DEFAULT 1,  -- UK format
    data_capture_used SMALLINT DEFAULT 0,
    rdbms_db_name VARCHAR(12) DEFAULT 'ACASDB',
    rdbms_user VARCHAR(12) DEFAULT 'ACAS-User',
    rdbms_passwd VARCHAR(12) DEFAULT 'PaSsWoRd',
    vat_reg_number VARCHAR(11) DEFAULT '',
    param_restrict CHAR(1) DEFAULT '',
    rdbms_port VARCHAR(5) DEFAULT '5432',
    rdbms_host VARCHAR(32) DEFAULT 'localhost',
    rdbms_socket VARCHAR(64) DEFAULT '',
    stats_date_period INTEGER DEFAULT 0,
    company_email VARCHAR(30) DEFAULT '',
    -- General Ledger Block (80 bytes)
    gl_profit_centres CHAR(1) DEFAULT 'P',
    gl_pc_grouped CHAR(1) DEFAULT 'N',
    gl_pc_level CHAR(1) DEFAULT 'R',
    gl_comparatives CHAR(1) DEFAULT 'N',
    gl_comparatives_active CHAR(1) DEFAULT 'N',
    gl_minimum_validation CHAR(1) DEFAULT 'N',
    gl_archiving CHAR(1) DEFAULT 'N',
    gl_trans_print CHAR(1) DEFAULT 'N',
    gl_trans_printed CHAR(1) DEFAULT 'N',
    gl_header_level SMALLINT DEFAULT 0,
    gl_sales_range SMALLINT DEFAULT 0,
    gl_purchase_range SMALLINT DEFAULT 0,
    gl_auto_vat CHAR(1) DEFAULT 'Y',
    gl_preserve_batch CHAR(1) DEFAULT 'N',
    gl_ledger_2nd_index CHAR(1) DEFAULT 'N',
    gl_irs_instead CHAR(1) DEFAULT 'N',
    gl_ledger_sec INTEGER DEFAULT 0,
    gl_updates INTEGER DEFAULT 0,
    gl_postings INTEGER DEFAULT 0,
    gl_next_batch INTEGER DEFAULT 1,
    gl_extra_charge_ac BIGINT DEFAULT 0,
    gl_vat_ac BIGINT DEFAULT 0,
    gl_print_spool_name2 VARCHAR(48) DEFAULT '',
    -- Purchase Ledger Block (88 bytes)
    pl_next_folio BIGINT DEFAULT 1,
    pl_pay_ac BIGINT DEFAULT 0,
    pl_creditors BIGINT DEFAULT 0,
    pl_purch_ac BIGINT DEFAULT 0,
    pl_end_cycle_date INTEGER DEFAULT 0,
    pl_next_batch INTEGER DEFAULT 1,
    pl_age_to_pay SMALLINT DEFAULT 0,
    pl_exists CHAR(1) DEFAULT 'Y',
    pl_delim CHAR(1) DEFAULT '',
    pl_entry_level SMALLINT DEFAULT 0,
    pl_flag_a SMALLINT DEFAULT 0,
    pl_flag_i SMALLINT DEFAULT 0,
    pl_flag_p SMALLINT DEFAULT 0,
    pl_stock_link CHAR(1) DEFAULT 'N',
    pl_print_spool_name3 VARCHAR(48) DEFAULT '',
    pl_autogen CHAR(1) DEFAULT ' ',
    pl_next_rec INTEGER DEFAULT 0,
    -- Sales Ledger Block (128 bytes)  
    sl_exists CHAR(1) DEFAULT 'Y',
    sl_delim CHAR(1) DEFAULT '',
    sl_oi_3_flag CHAR(1) DEFAULT '',
    sl_cust_flag CHAR(1) DEFAULT '',
    sl_oi_5_flag CHAR(1) DEFAULT '',
    sl_flag_oi_3 CHAR(1) DEFAULT '',
    sl_full_invoicing SMALLINT DEFAULT 0,
    sl_flag_a SMALLINT DEFAULT 0,
    sl_flag_i SMALLINT DEFAULT 0,
    sl_flag_p SMALLINT DEFAULT 0,
    sl_dunning SMALLINT DEFAULT 0,
    sl_charges SMALLINT DEFAULT 0,
    sl_own_nos CHAR(1) DEFAULT 'N',
    sl_stats_run SMALLINT DEFAULT 0,
    sl_day_book SMALLINT DEFAULT 0,
    sl_invoicer SMALLINT DEFAULT 1,
    sl_extra_desc VARCHAR(14) DEFAULT '',
    sl_extra_type CHAR(1) DEFAULT '',
    sl_extra_print CHAR(1) DEFAULT 'N',
    sl_stock_link CHAR(1) DEFAULT 'N',
    sl_stock_audit CHAR(1) DEFAULT 'N',
    sl_late_per DECIMAL(4,2) DEFAULT 0.00,
    sl_disc DECIMAL(4,2) DEFAULT 0.00,
    sl_extra_rate DECIMAL(4,2) DEFAULT 0.00,
    sl_days_1 SMALLINT DEFAULT 30,
    sl_days_2 SMALLINT DEFAULT 60,
    sl_days_3 SMALLINT DEFAULT 90,
    sl_credit SMALLINT DEFAULT 0,
    sl_min INTEGER DEFAULT 0,
    sl_max INTEGER DEFAULT 0,
    sl_pf_retention INTEGER DEFAULT 0,
    sl_first_batch INTEGER DEFAULT 1,
    sl_first_inv BIGINT DEFAULT 1,
    sl_limit BIGINT DEFAULT 0,
    sl_pay_ac BIGINT DEFAULT 0,
    sl_debtors BIGINT DEFAULT 0,
    sl_sales_ac BIGINT DEFAULT 0,
    sl_end_cycle_date INTEGER DEFAULT 0,
    sl_comp_head_pick CHAR(1) DEFAULT 'N',
    sl_comp_head_inv CHAR(1) DEFAULT 'N',
    sl_comp_head_stat CHAR(1) DEFAULT 'N',
    sl_comp_head_lets CHAR(1) DEFAULT 'N',
    sl_vat_printed CHAR(1) DEFAULT 'N',
    sl_invoice_lines SMALLINT DEFAULT 20,
    sl_autogen CHAR(1) DEFAULT ' ',
    sl_next_rec INTEGER DEFAULT 0,
    sl_bo_flag CHAR(1) DEFAULT ' ',
    sl_bo_default CHAR(1) DEFAULT 'N',
    -- GL overflow fields
    gl_bl_pay_ac BIGINT DEFAULT 0,
    gl_p_creditors BIGINT DEFAULT 0,
    gl_bl_purch_ac BIGINT DEFAULT 0,
    gl_sl_pay_ac BIGINT DEFAULT 0,
    gl_s_debtors BIGINT DEFAULT 0,
    gl_sl_sales_ac BIGINT DEFAULT 0,
    -- Stock Control Block (88 bytes)
    stk_abrev_ref VARCHAR(6) DEFAULT '',
    stk_debug SMALLINT DEFAULT 0,
    stk_manu_used SMALLINT DEFAULT 0,
    stk_oe_used SMALLINT DEFAULT 0,
    stk_audit_used SMALLINT DEFAULT 0,
    stk_mov_audit SMALLINT DEFAULT 0,
    stk_period_cur CHAR(1) DEFAULT 'M',
    stk_period_dat CHAR(1) DEFAULT 'M',
    stock_control CHAR(1) DEFAULT 'Y',
    stk_averaging SMALLINT DEFAULT 0,
    stk_activity_rep_run SMALLINT DEFAULT 0,
    stk_bo_active CHAR(1) DEFAULT 'N',
    stk_page_lines SMALLINT DEFAULT 66,
    stk_audit_no SMALLINT DEFAULT 0,
    -- IRS Entry Block (128 bytes)
    irs_client VARCHAR(24) DEFAULT '',
    irs_next_post INTEGER DEFAULT 1,
    irs_vat1 DECIMAL(4,2) DEFAULT 20.00,
    irs_vat2 DECIMAL(4,2) DEFAULT 5.00,
    irs_vat3 DECIMAL(4,2) DEFAULT 0.00,
    irs_pass_value SMALLINT DEFAULT 0,
    irs_save_sequ SMALLINT DEFAULT 0,
    irs_system_work_group VARCHAR(18) DEFAULT '',
    irs_pl_app_created CHAR(1) DEFAULT 'N',
    irs_pl_approp_ac6 INTEGER DEFAULT 0,
    irs_1st_time_flag SMALLINT DEFAULT 0,
    -- Metadata
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_by VARCHAR(30) DEFAULT CURRENT_USER
);

-- 2. SALEDGER_REC - Sales Ledger Master (Customer)
CREATE TABLE saledger_rec (
    sales_key VARCHAR(10) PRIMARY KEY,
    sales_name VARCHAR(40) NOT NULL DEFAULT '',
    sales_address_1 VARCHAR(30) DEFAULT '',
    sales_address_2 VARCHAR(30) DEFAULT '',
    sales_address_3 VARCHAR(30) DEFAULT '',
    sales_address_4 VARCHAR(30) DEFAULT '',
    sales_address_5 VARCHAR(12) DEFAULT '',  -- PostCode/Zip
    sales_country VARCHAR(24) DEFAULT '',
    sales_contact VARCHAR(30) DEFAULT '',
    sales_phone VARCHAR(20) DEFAULT '',
    sales_fax VARCHAR(20) DEFAULT '',
    sales_email VARCHAR(50) DEFAULT '',
    sales_mobile VARCHAR(20) DEFAULT '',
    -- Financial Info
    sales_credit_limit DECIMAL(12,2) DEFAULT 0.00,
    sales_balance DECIMAL(12,2) DEFAULT 0.00,
    sales_ytd_turnover DECIMAL(12,2) DEFAULT 0.00,
    sales_last_year_turnover DECIMAL(12,2) DEFAULT 0.00,
    sales_payment_terms VARCHAR(4) DEFAULT '30',
    sales_discount_rate DECIMAL(4,2) DEFAULT 0.00,
    sales_settlement_disc DECIMAL(4,2) DEFAULT 0.00,
    sales_price_level SMALLINT DEFAULT 1,
    sales_tax_code VARCHAR(4) DEFAULT 'VSTD',
    sales_currency VARCHAR(3) DEFAULT 'GBP',
    -- Status flags
    sales_account_status CHAR(1) DEFAULT 'A', -- A=Active, H=Hold, C=Closed
    sales_credit_status CHAR(1) DEFAULT 'O',  -- O=Open, H=Hold, S=Stop
    sales_statement_flag CHAR(1) DEFAULT 'Y',
    sales_dunning_flag CHAR(1) DEFAULT 'Y',
    sales_charge_interest CHAR(1) DEFAULT 'N',
    -- Analysis codes
    sales_analysis_1 VARCHAR(10) DEFAULT '',
    sales_analysis_2 VARCHAR(10) DEFAULT '',
    sales_analysis_3 VARCHAR(10) DEFAULT '',
    sales_territory VARCHAR(6) DEFAULT '',
    sales_rep VARCHAR(6) DEFAULT '',
    -- Dates
    sales_date_opened INTEGER DEFAULT 0,
    sales_date_last_sale INTEGER DEFAULT 0,
    sales_date_last_payment INTEGER DEFAULT 0,
    -- Statistics
    sales_transactions_mtd INTEGER DEFAULT 0,
    sales_transactions_ytd INTEGER DEFAULT 0,
    sales_invoices_mtd INTEGER DEFAULT 0,
    sales_invoices_ytd INTEGER DEFAULT 0,
    sales_average_days INTEGER DEFAULT 0,
    sales_oldest_item INTEGER DEFAULT 0,
    -- Notes
    sales_notes TEXT,
    -- Metadata
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_by VARCHAR(30) DEFAULT CURRENT_USER
);

CREATE INDEX idx_saledger_name ON saledger_rec(sales_name);
CREATE INDEX idx_saledger_balance ON saledger_rec(sales_balance);
CREATE INDEX idx_saledger_status ON saledger_rec(sales_account_status);

-- 3. PULEDGER_REC - Purchase Ledger Master (Supplier)
CREATE TABLE puledger_rec (
    purch_key VARCHAR(10) PRIMARY KEY,
    purch_name VARCHAR(40) NOT NULL DEFAULT '',
    purch_address_1 VARCHAR(30) DEFAULT '',
    purch_address_2 VARCHAR(30) DEFAULT '',
    purch_address_3 VARCHAR(30) DEFAULT '',
    purch_address_4 VARCHAR(30) DEFAULT '',
    purch_address_5 VARCHAR(12) DEFAULT '',  -- PostCode/Zip
    purch_country VARCHAR(24) DEFAULT '',
    purch_contact VARCHAR(30) DEFAULT '',
    purch_phone VARCHAR(20) DEFAULT '',
    purch_fax VARCHAR(20) DEFAULT '',
    purch_email VARCHAR(50) DEFAULT '',
    purch_mobile VARCHAR(20) DEFAULT '',
    -- Financial Info
    purch_balance DECIMAL(12,2) DEFAULT 0.00,
    purch_ytd_turnover DECIMAL(12,2) DEFAULT 0.00,
    purch_last_year_turnover DECIMAL(12,2) DEFAULT 0.00,
    purch_payment_terms VARCHAR(4) DEFAULT '30',
    purch_settlement_disc DECIMAL(4,2) DEFAULT 0.00,
    purch_tax_code VARCHAR(4) DEFAULT 'VSTD',
    purch_currency VARCHAR(3) DEFAULT 'GBP',
    purch_our_account_no VARCHAR(20) DEFAULT '',
    -- Bank Details
    purch_bank_name VARCHAR(30) DEFAULT '',
    purch_bank_address VARCHAR(60) DEFAULT '',
    purch_bank_account VARCHAR(20) DEFAULT '',
    purch_bank_sort_code VARCHAR(10) DEFAULT '',
    purch_bank_swift VARCHAR(15) DEFAULT '',
    -- Status flags
    purch_account_status CHAR(1) DEFAULT 'A', -- A=Active, H=Hold, C=Closed
    purch_approval_required CHAR(1) DEFAULT 'N',
    purch_remittance_flag CHAR(1) DEFAULT 'Y',
    purch_1099_required CHAR(1) DEFAULT 'N',
    -- Analysis codes
    purch_analysis_1 VARCHAR(10) DEFAULT '',
    purch_analysis_2 VARCHAR(10) DEFAULT '',
    purch_analysis_3 VARCHAR(10) DEFAULT '',
    purch_default_nominal BIGINT DEFAULT 0,
    -- Dates
    purch_date_opened INTEGER DEFAULT 0,
    purch_date_last_purchase INTEGER DEFAULT 0,
    purch_date_last_payment INTEGER DEFAULT 0,
    -- Statistics
    purch_transactions_mtd INTEGER DEFAULT 0,
    purch_transactions_ytd INTEGER DEFAULT 0,
    purch_invoices_mtd INTEGER DEFAULT 0,
    purch_invoices_ytd INTEGER DEFAULT 0,
    purch_average_days INTEGER DEFAULT 0,
    purch_oldest_item INTEGER DEFAULT 0,
    -- Notes
    purch_notes TEXT,
    -- Metadata
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_by VARCHAR(30) DEFAULT CURRENT_USER
);

CREATE INDEX idx_puledger_name ON puledger_rec(purch_name);
CREATE INDEX idx_puledger_balance ON puledger_rec(purch_balance);
CREATE INDEX idx_puledger_status ON puledger_rec(purch_account_status);

-- 4. STOCK_REC - Stock Master (Inventory)
CREATE TABLE stock_rec (
    stock_key VARCHAR(30) PRIMARY KEY,
    stock_desc VARCHAR(40) NOT NULL DEFAULT '',
    stock_abrev_key VARCHAR(10) DEFAULT '',
    stock_location VARCHAR(6) DEFAULT '',
    stock_bin VARCHAR(10) DEFAULT '',
    -- Quantities (3 decimal places for units)
    stock_qty_on_hand DECIMAL(15,3) DEFAULT 0.000,
    stock_qty_allocated DECIMAL(15,3) DEFAULT 0.000,
    stock_qty_on_order DECIMAL(15,3) DEFAULT 0.000,
    stock_qty_back_order DECIMAL(15,3) DEFAULT 0.000,
    stock_qty_available DECIMAL(15,3) DEFAULT 0.000,
    stock_reorder_point DECIMAL(15,3) DEFAULT 0.000,
    stock_reorder_qty DECIMAL(15,3) DEFAULT 0.000,
    stock_min_qty DECIMAL(15,3) DEFAULT 0.000,
    stock_max_qty DECIMAL(15,3) DEFAULT 0.000,
    -- Costs (4 decimal places)
    stock_std_cost DECIMAL(15,4) DEFAULT 0.0000,
    stock_avg_cost DECIMAL(15,4) DEFAULT 0.0000,
    stock_last_cost DECIMAL(15,4) DEFAULT 0.0000,
    stock_fifo_cost DECIMAL(15,4) DEFAULT 0.0000,
    stock_lifo_cost DECIMAL(15,4) DEFAULT 0.0000,
    -- Prices
    stock_list_price DECIMAL(15,4) DEFAULT 0.0000,
    stock_price_1 DECIMAL(15,4) DEFAULT 0.0000,
    stock_price_2 DECIMAL(15,4) DEFAULT 0.0000,
    stock_price_3 DECIMAL(15,4) DEFAULT 0.0000,
    stock_price_4 DECIMAL(15,4) DEFAULT 0.0000,
    stock_price_5 DECIMAL(15,4) DEFAULT 0.0000,
    -- Stock Control
    stock_costing_method CHAR(1) DEFAULT 'A', -- A=Average, F=FIFO, L=LIFO, S=Standard
    stock_product_group VARCHAR(6) DEFAULT '',
    stock_unit_of_measure VARCHAR(6) DEFAULT 'EA',
    stock_lead_time INTEGER DEFAULT 0,
    stock_duty_rate DECIMAL(6,2) DEFAULT 0.00,
    stock_tax_code VARCHAR(4) DEFAULT 'VSTD',
    -- Flags
    stock_discontinued CHAR(1) DEFAULT 'N',
    stock_kit CHAR(1) DEFAULT 'N',
    stock_serial_tracked CHAR(1) DEFAULT 'N',
    stock_lot_tracked CHAR(1) DEFAULT 'N',
    stock_expiry_tracked CHAR(1) DEFAULT 'N',
    stock_consignment CHAR(1) DEFAULT 'N',
    -- Analysis
    stock_analysis_1 VARCHAR(10) DEFAULT '',
    stock_analysis_2 VARCHAR(10) DEFAULT '',
    stock_analysis_3 VARCHAR(10) DEFAULT '',
    stock_abc_code CHAR(1) DEFAULT 'C',
    -- Supplier Info
    stock_primary_supplier VARCHAR(10) DEFAULT '',
    stock_supplier_part_no VARCHAR(30) DEFAULT '',
    -- Movement Stats
    stock_mtd_usage DECIMAL(15,3) DEFAULT 0.000,
    stock_ytd_usage DECIMAL(15,3) DEFAULT 0.000,
    stock_last_year_usage DECIMAL(15,3) DEFAULT 0.000,
    stock_date_last_sale INTEGER DEFAULT 0,
    stock_date_last_receipt INTEGER DEFAULT 0,
    stock_date_last_count INTEGER DEFAULT 0,
    -- Additional fields
    stock_weight DECIMAL(10,3) DEFAULT 0.000,
    stock_volume DECIMAL(10,3) DEFAULT 0.000,
    stock_barcode VARCHAR(30) DEFAULT '',
    stock_notes TEXT,
    -- Metadata
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_by VARCHAR(30) DEFAULT CURRENT_USER
);

CREATE INDEX idx_stock_desc ON stock_rec(stock_desc);
CREATE INDEX idx_stock_abrev ON stock_rec(stock_abrev_key);
CREATE INDEX idx_stock_group ON stock_rec(stock_product_group);
CREATE INDEX idx_stock_available ON stock_rec(stock_qty_available);

-- 5. GLLEDGER_REC - General Ledger Chart of Accounts
CREATE TABLE glledger_rec (
    ledger_key BIGINT PRIMARY KEY,
    ledger_name VARCHAR(40) NOT NULL DEFAULT '',
    ledger_type SMALLINT DEFAULT 0, -- 1=Asset, 2=Liability, 3=Capital, 4=Income, 5=Expense
    ledger_place CHAR(1) DEFAULT 'P', -- B=Balance Sheet, P=P&L
    ledger_level SMALLINT DEFAULT 4,  -- 1-4 hierarchy levels
    ledger_parent BIGINT DEFAULT 0,
    -- Balances
    ledger_balance DECIMAL(15,2) DEFAULT 0.00,
    ledger_mtd_actual DECIMAL(15,2) DEFAULT 0.00,
    ledger_ytd_actual DECIMAL(15,2) DEFAULT 0.00,
    ledger_last_year_actual DECIMAL(15,2) DEFAULT 0.00,
    ledger_budget_mtd DECIMAL(15,2) DEFAULT 0.00,
    ledger_budget_ytd DECIMAL(15,2) DEFAULT 0.00,
    ledger_budget_annual DECIMAL(15,2) DEFAULT 0.00,
    -- Period balances (13 periods)
    ledger_period_1 DECIMAL(15,2) DEFAULT 0.00,
    ledger_period_2 DECIMAL(15,2) DEFAULT 0.00,
    ledger_period_3 DECIMAL(15,2) DEFAULT 0.00,
    ledger_period_4 DECIMAL(15,2) DEFAULT 0.00,
    ledger_period_5 DECIMAL(15,2) DEFAULT 0.00,
    ledger_period_6 DECIMAL(15,2) DEFAULT 0.00,
    ledger_period_7 DECIMAL(15,2) DEFAULT 0.00,
    ledger_period_8 DECIMAL(15,2) DEFAULT 0.00,
    ledger_period_9 DECIMAL(15,2) DEFAULT 0.00,
    ledger_period_10 DECIMAL(15,2) DEFAULT 0.00,
    ledger_period_11 DECIMAL(15,2) DEFAULT 0.00,
    ledger_period_12 DECIMAL(15,2) DEFAULT 0.00,
    ledger_period_13 DECIMAL(15,2) DEFAULT 0.00,
    -- Flags
    ledger_active CHAR(1) DEFAULT 'Y',
    ledger_control CHAR(1) DEFAULT 'N',
    ledger_reconcilable CHAR(1) DEFAULT 'N',
    ledger_analysis_required CHAR(1) DEFAULT 'N',
    ledger_vat_code VARCHAR(4) DEFAULT '',
    ledger_currency VARCHAR(3) DEFAULT 'GBP',
    -- Metadata
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_by VARCHAR(30) DEFAULT CURRENT_USER
);

CREATE INDEX idx_glledger_type ON glledger_rec(ledger_type);
CREATE INDEX idx_glledger_level ON glledger_rec(ledger_level);
CREATE INDEX idx_glledger_parent ON glledger_rec(ledger_parent);
CREATE INDEX idx_glledger_name ON glledger_rec(ledger_name);

-- 6. SAINVOICE_REC - Sales Invoice Header
CREATE TABLE sainvoice_rec (
    invoice_key BIGINT PRIMARY KEY,
    invoice_customer VARCHAR(10) NOT NULL,
    invoice_date INTEGER NOT NULL,
    invoice_type CHAR(1) DEFAULT 'I', -- I=Invoice, C=Credit, D=Debit
    invoice_status CHAR(1) DEFAULT 'O', -- O=Open, P=Paid, V=Void
    -- Amounts
    invoice_goods_amount DECIMAL(12,2) DEFAULT 0.00,
    invoice_vat_amount DECIMAL(12,2) DEFAULT 0.00,
    invoice_total_amount DECIMAL(12,2) DEFAULT 0.00,
    invoice_paid_amount DECIMAL(12,2) DEFAULT 0.00,
    invoice_balance DECIMAL(12,2) DEFAULT 0.00,
    invoice_discount_amount DECIMAL(12,2) DEFAULT 0.00,
    -- References
    invoice_order_no VARCHAR(20) DEFAULT '',
    invoice_delivery_no VARCHAR(20) DEFAULT '',
    invoice_customer_ref VARCHAR(20) DEFAULT '',
    invoice_our_ref VARCHAR(20) DEFAULT '',
    -- Delivery Address
    invoice_del_name VARCHAR(40) DEFAULT '',
    invoice_del_address_1 VARCHAR(30) DEFAULT '',
    invoice_del_address_2 VARCHAR(30) DEFAULT '',
    invoice_del_address_3 VARCHAR(30) DEFAULT '',
    invoice_del_address_4 VARCHAR(30) DEFAULT '',
    invoice_del_address_5 VARCHAR(12) DEFAULT '',
    -- Terms
    invoice_terms VARCHAR(4) DEFAULT '30',
    invoice_due_date INTEGER DEFAULT 0,
    invoice_settlement_disc DECIMAL(4,2) DEFAULT 0.00,
    invoice_settlement_days INTEGER DEFAULT 0,
    -- Other
    invoice_printed CHAR(1) DEFAULT 'N',
    invoice_emailed CHAR(1) DEFAULT 'N',
    invoice_notes TEXT,
    invoice_period SMALLINT DEFAULT 0,
    invoice_batch_no INTEGER DEFAULT 0,
    -- Metadata
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_by VARCHAR(30) DEFAULT CURRENT_USER,
    FOREIGN KEY (invoice_customer) REFERENCES saledger_rec(sales_key)
);

CREATE INDEX idx_sainvoice_customer ON sainvoice_rec(invoice_customer);
CREATE INDEX idx_sainvoice_date ON sainvoice_rec(invoice_date);
CREATE INDEX idx_sainvoice_status ON sainvoice_rec(invoice_status);
CREATE INDEX idx_sainvoice_balance ON sainvoice_rec(invoice_balance);

-- 7. SAINV_LINES_REC - Sales Invoice Lines
CREATE TABLE sainv_lines_rec (
    line_id SERIAL PRIMARY KEY,
    line_invoice_key BIGINT NOT NULL,
    line_number SMALLINT NOT NULL,
    line_type CHAR(1) DEFAULT 'S', -- S=Stock, N=Non-stock, C=Comment, D=Discount
    -- Item Details
    line_stock_code VARCHAR(30) DEFAULT '',
    line_description VARCHAR(40) DEFAULT '',
    line_quantity DECIMAL(12,3) DEFAULT 0.000,
    line_unit_price DECIMAL(12,4) DEFAULT 0.0000,
    line_discount_percent DECIMAL(5,2) DEFAULT 0.00,
    line_net_amount DECIMAL(12,2) DEFAULT 0.00,
    line_vat_code VARCHAR(4) DEFAULT '',
    line_vat_rate DECIMAL(5,2) DEFAULT 0.00,
    line_vat_amount DECIMAL(12,2) DEFAULT 0.00,
    line_total_amount DECIMAL(12,2) DEFAULT 0.00,
    -- GL Distribution
    line_nominal_code BIGINT DEFAULT 0,
    line_analysis_1 VARCHAR(10) DEFAULT '',
    line_analysis_2 VARCHAR(10) DEFAULT '',
    line_analysis_3 VARCHAR(10) DEFAULT '',
    -- Stock Info
    line_location VARCHAR(6) DEFAULT '',
    line_cost DECIMAL(12,4) DEFAULT 0.0000,
    -- Metadata
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (line_invoice_key) REFERENCES sainvoice_rec(invoice_key) ON DELETE CASCADE
);

CREATE INDEX idx_sainv_lines_invoice ON sainv_lines_rec(line_invoice_key);
CREATE INDEX idx_sainv_lines_stock ON sainv_lines_rec(line_stock_code);

-- 8. PUINVOICE_REC - Purchase Invoice Header
CREATE TABLE puinvoice_rec (
    pinvoice_key SERIAL PRIMARY KEY,
    pinvoice_supplier VARCHAR(10) NOT NULL,
    pinvoice_number VARCHAR(20) NOT NULL,
    pinvoice_date INTEGER NOT NULL,
    pinvoice_type CHAR(1) DEFAULT 'I', -- I=Invoice, C=Credit, D=Debit
    pinvoice_status CHAR(1) DEFAULT 'O', -- O=Open, P=Paid, V=Void, H=Hold
    -- Amounts
    pinvoice_goods_amount DECIMAL(12,2) DEFAULT 0.00,
    pinvoice_vat_amount DECIMAL(12,2) DEFAULT 0.00,
    pinvoice_total_amount DECIMAL(12,2) DEFAULT 0.00,
    pinvoice_paid_amount DECIMAL(12,2) DEFAULT 0.00,
    pinvoice_balance DECIMAL(12,2) DEFAULT 0.00,
    pinvoice_discount_amount DECIMAL(12,2) DEFAULT 0.00,
    -- References
    pinvoice_order_no VARCHAR(20) DEFAULT '',
    pinvoice_grn_no VARCHAR(20) DEFAULT '',
    pinvoice_our_ref VARCHAR(20) DEFAULT '',
    -- Terms
    pinvoice_terms VARCHAR(4) DEFAULT '30',
    pinvoice_due_date INTEGER DEFAULT 0,
    pinvoice_settlement_disc DECIMAL(4,2) DEFAULT 0.00,
    pinvoice_settlement_days INTEGER DEFAULT 0,
    -- Approval
    pinvoice_approved CHAR(1) DEFAULT 'N',
    pinvoice_approved_by VARCHAR(30) DEFAULT '',
    pinvoice_approved_date INTEGER DEFAULT 0,
    -- Matching
    pinvoice_matched CHAR(1) DEFAULT 'N',
    pinvoice_match_status CHAR(1) DEFAULT '', -- F=Full, P=Partial, U=Unmatched
    -- Other
    pinvoice_notes TEXT,
    pinvoice_period SMALLINT DEFAULT 0,
    pinvoice_batch_no INTEGER DEFAULT 0,
    -- Metadata
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_by VARCHAR(30) DEFAULT CURRENT_USER,
    FOREIGN KEY (pinvoice_supplier) REFERENCES puledger_rec(purch_key)
);

CREATE INDEX idx_puinvoice_supplier ON puinvoice_rec(pinvoice_supplier);
CREATE INDEX idx_puinvoice_number ON puinvoice_rec(pinvoice_number);
CREATE INDEX idx_puinvoice_date ON puinvoice_rec(pinvoice_date);
CREATE INDEX idx_puinvoice_status ON puinvoice_rec(pinvoice_status);

-- 9. PUINV_LINES_REC - Purchase Invoice Lines
CREATE TABLE puinv_lines_rec (
    pline_id SERIAL PRIMARY KEY,
    pline_invoice_key INTEGER NOT NULL,
    pline_number SMALLINT NOT NULL,
    pline_type CHAR(1) DEFAULT 'S', -- S=Stock, N=Non-stock, C=Comment
    -- Item Details
    pline_stock_code VARCHAR(30) DEFAULT '',
    pline_description VARCHAR(40) DEFAULT '',
    pline_quantity DECIMAL(12,3) DEFAULT 0.000,
    pline_unit_price DECIMAL(12,4) DEFAULT 0.0000,
    pline_discount_percent DECIMAL(5,2) DEFAULT 0.00,
    pline_net_amount DECIMAL(12,2) DEFAULT 0.00,
    pline_vat_code VARCHAR(4) DEFAULT '',
    pline_vat_rate DECIMAL(5,2) DEFAULT 0.00,
    pline_vat_amount DECIMAL(12,2) DEFAULT 0.00,
    pline_total_amount DECIMAL(12,2) DEFAULT 0.00,
    -- GL Distribution
    pline_nominal_code BIGINT DEFAULT 0,
    pline_analysis_1 VARCHAR(10) DEFAULT '',
    pline_analysis_2 VARCHAR(10) DEFAULT '',
    pline_analysis_3 VARCHAR(10) DEFAULT '',
    -- PO Matching
    pline_po_number VARCHAR(20) DEFAULT '',
    pline_po_line SMALLINT DEFAULT 0,
    pline_grn_number VARCHAR(20) DEFAULT '',
    pline_grn_line SMALLINT DEFAULT 0,
    -- Metadata
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (pline_invoice_key) REFERENCES puinvoice_rec(pinvoice_key) ON DELETE CASCADE
);

CREATE INDEX idx_puinv_lines_invoice ON puinv_lines_rec(pline_invoice_key);
CREATE INDEX idx_puinv_lines_stock ON puinv_lines_rec(pline_stock_code);

-- 10. SAITM3_REC - Sales Open Items
CREATE TABLE saitm3_rec (
    soitm_customer VARCHAR(10) NOT NULL,
    soitm_invoice BIGINT NOT NULL,
    soitm_date INTEGER NOT NULL,
    soitm_due_date INTEGER DEFAULT 0,
    soitm_type CHAR(1) DEFAULT 'I', -- I=Invoice, C=Credit, P=Payment
    soitm_reference VARCHAR(20) DEFAULT '',
    soitm_amount DECIMAL(12,2) DEFAULT 0.00,
    soitm_paid DECIMAL(12,2) DEFAULT 0.00,
    soitm_balance DECIMAL(12,2) DEFAULT 0.00,
    soitm_period SMALLINT DEFAULT 0,
    soitm_age_days INTEGER DEFAULT 0,
    soitm_dispute CHAR(1) DEFAULT 'N',
    soitm_dispute_notes VARCHAR(100) DEFAULT '',
    -- Metadata
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (soitm_customer, soitm_invoice),
    FOREIGN KEY (soitm_customer) REFERENCES saledger_rec(sales_key)
);

CREATE INDEX idx_saitm3_balance ON saitm3_rec(soitm_balance);
CREATE INDEX idx_saitm3_date ON saitm3_rec(soitm_date);
CREATE INDEX idx_saitm3_due ON saitm3_rec(soitm_due_date);

-- 11. PUITM5_REC - Purchase Open Items
CREATE TABLE puitm5_rec (
    poitm_supplier VARCHAR(10) NOT NULL,
    poitm_invoice VARCHAR(20) NOT NULL,
    poitm_date INTEGER NOT NULL,
    poitm_due_date INTEGER DEFAULT 0,
    poitm_type CHAR(1) DEFAULT 'I', -- I=Invoice, C=Credit, P=Payment
    poitm_reference VARCHAR(20) DEFAULT '',
    poitm_amount DECIMAL(12,2) DEFAULT 0.00,
    poitm_paid DECIMAL(12,2) DEFAULT 0.00,
    poitm_balance DECIMAL(12,2) DEFAULT 0.00,
    poitm_period SMALLINT DEFAULT 0,
    poitm_age_days INTEGER DEFAULT 0,
    poitm_held CHAR(1) DEFAULT 'N',
    poitm_held_reason VARCHAR(100) DEFAULT '',
    -- Metadata
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (poitm_supplier, poitm_invoice),
    FOREIGN KEY (poitm_supplier) REFERENCES puledger_rec(purch_key)
);

CREATE INDEX idx_puitm5_balance ON puitm5_rec(poitm_balance);
CREATE INDEX idx_puitm5_date ON puitm5_rec(poitm_date);
CREATE INDEX idx_puitm5_due ON puitm5_rec(poitm_due_date);

-- 12. GLPOSTING_REC - GL Postings/Journal Entries
CREATE TABLE glposting_rec (
    posting_id SERIAL PRIMARY KEY,
    posting_batch INTEGER NOT NULL,
    posting_date INTEGER NOT NULL,
    posting_period SMALLINT NOT NULL,
    posting_type VARCHAR(2) DEFAULT 'JE', -- JE=Journal, SL=Sales, PL=Purchase
    posting_reference VARCHAR(20) DEFAULT '',
    posting_description VARCHAR(40) DEFAULT '',
    -- Debit/Credit
    posting_account BIGINT NOT NULL,
    posting_debit DECIMAL(12,2) DEFAULT 0.00,
    posting_credit DECIMAL(12,2) DEFAULT 0.00,
    -- Analysis
    posting_analysis_1 VARCHAR(10) DEFAULT '',
    posting_analysis_2 VARCHAR(10) DEFAULT '',
    posting_analysis_3 VARCHAR(10) DEFAULT '',
    -- Source
    posting_source_ledger CHAR(2) DEFAULT '',
    posting_source_key VARCHAR(20) DEFAULT '',
    posting_source_type CHAR(1) DEFAULT '',
    -- Status
    posting_status CHAR(1) DEFAULT 'U', -- U=Unposted, P=Posted
    posting_user VARCHAR(30) DEFAULT '',
    -- Metadata
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_by VARCHAR(30) DEFAULT CURRENT_USER,
    FOREIGN KEY (posting_account) REFERENCES glledger_rec(ledger_key)
);

CREATE INDEX idx_glposting_batch ON glposting_rec(posting_batch);
CREATE INDEX idx_glposting_date ON glposting_rec(posting_date);
CREATE INDEX idx_glposting_period ON glposting_rec(posting_period);
CREATE INDEX idx_glposting_account ON glposting_rec(posting_account);
CREATE INDEX idx_glposting_status ON glposting_rec(posting_status);

-- 13. GLBATCH_REC - GL Batch Control
CREATE TABLE glbatch_rec (
    batch_no INTEGER PRIMARY KEY,
    batch_date INTEGER NOT NULL,
    batch_period SMALLINT NOT NULL,
    batch_type VARCHAR(2) DEFAULT 'JE',
    batch_description VARCHAR(40) DEFAULT '',
    batch_source VARCHAR(10) DEFAULT '',
    -- Totals
    batch_debits DECIMAL(15,2) DEFAULT 0.00,
    batch_credits DECIMAL(15,2) DEFAULT 0.00,
    batch_entries INTEGER DEFAULT 0,
    -- Status
    batch_status CHAR(1) DEFAULT 'O', -- O=Open, P=Posted, C=Cancelled
    batch_posted_date INTEGER DEFAULT 0,
    batch_posted_by VARCHAR(30) DEFAULT '',
    -- Metadata
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_by VARCHAR(30) DEFAULT CURRENT_USER
);

CREATE INDEX idx_glbatch_date ON glbatch_rec(batch_date);
CREATE INDEX idx_glbatch_period ON glbatch_rec(batch_period);
CREATE INDEX idx_glbatch_status ON glbatch_rec(batch_status);

-- 14. STOCKAUDIT_REC - Stock Movement Audit
CREATE TABLE stockaudit_rec (
    audit_id SERIAL PRIMARY KEY,
    audit_date INTEGER NOT NULL,
    audit_time INTEGER NOT NULL,
    audit_stock_code VARCHAR(30) NOT NULL,
    audit_type VARCHAR(10) NOT NULL, -- RECEIPT, ISSUE, ADJUST, TRANSFER
    audit_reference VARCHAR(20) DEFAULT '',
    audit_source VARCHAR(10) DEFAULT '',
    -- Quantities
    audit_qty DECIMAL(12,3) DEFAULT 0.000,
    audit_qty_before DECIMAL(12,3) DEFAULT 0.000,
    audit_qty_after DECIMAL(12,3) DEFAULT 0.000,
    -- Values
    audit_cost DECIMAL(12,4) DEFAULT 0.0000,
    audit_value DECIMAL(12,2) DEFAULT 0.00,
    -- Location
    audit_location_from VARCHAR(6) DEFAULT '',
    audit_location_to VARCHAR(6) DEFAULT '',
    -- User
    audit_user VARCHAR(30) DEFAULT '',
    audit_reason VARCHAR(100) DEFAULT '',
    -- Metadata
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (audit_stock_code) REFERENCES stock_rec(stock_key)
);

CREATE INDEX idx_stockaudit_date ON stockaudit_rec(audit_date);
CREATE INDEX idx_stockaudit_stock ON stockaudit_rec(audit_stock_code);
CREATE INDEX idx_stockaudit_type ON stockaudit_rec(audit_type);

-- 15. PLPAY_REC - Payment Header (AP/AR)
CREATE TABLE plpay_rec (
    payment_id SERIAL PRIMARY KEY,
    payment_type CHAR(1) NOT NULL, -- P=Purchase, S=Sales
    payment_date INTEGER NOT NULL,
    payment_method CHAR(1) DEFAULT 'C', -- C=Check, T=Transfer, H=Cash, R=Credit card
    payment_reference VARCHAR(20) NOT NULL,
    payment_amount DECIMAL(12,2) NOT NULL,
    payment_currency VARCHAR(3) DEFAULT 'GBP',
    payment_bank_account VARCHAR(20) DEFAULT '',
    -- For checks
    payment_check_no VARCHAR(20) DEFAULT '',
    payment_check_date INTEGER DEFAULT 0,
    -- Status
    payment_status CHAR(1) DEFAULT 'U', -- U=Unallocated, P=Partial, F=Full, V=Void
    payment_allocated DECIMAL(12,2) DEFAULT 0.00,
    payment_unallocated DECIMAL(12,2) DEFAULT 0.00,
    -- Notes
    payment_notes TEXT,
    payment_period SMALLINT DEFAULT 0,
    payment_batch INTEGER DEFAULT 0,
    -- Metadata
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_by VARCHAR(30) DEFAULT CURRENT_USER
);

CREATE INDEX idx_plpay_type ON plpay_rec(payment_type);
CREATE INDEX idx_plpay_date ON plpay_rec(payment_date);
CREATE INDEX idx_plpay_reference ON plpay_rec(payment_reference);
CREATE INDEX idx_plpay_status ON plpay_rec(payment_status);

-- 16. PLPAY_REC_LINES - Payment Allocation Details
CREATE TABLE plpay_rec_lines (
    pline_id SERIAL PRIMARY KEY,
    pline_payment_id INTEGER NOT NULL,
    pline_ledger_key VARCHAR(10) NOT NULL, -- Customer or Supplier
    pline_invoice VARCHAR(20) NOT NULL,
    pline_amount_allocated DECIMAL(12,2) NOT NULL,
    pline_discount_taken DECIMAL(12,2) DEFAULT 0.00,
    pline_write_off DECIMAL(12,2) DEFAULT 0.00,
    -- Metadata
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (pline_payment_id) REFERENCES plpay_rec(payment_id) ON DELETE CASCADE
);

CREATE INDEX idx_plpay_lines_payment ON plpay_rec_lines(pline_payment_id);
CREATE INDEX idx_plpay_lines_ledger ON plpay_rec_lines(pline_ledger_key);

-- 17. ANALYSIS_REC - Analysis Codes
CREATE TABLE analysis_rec (
    analysis_type VARCHAR(10) NOT NULL, -- CUSTOMER, SUPPLIER, STOCK, GL
    analysis_code VARCHAR(10) NOT NULL,
    analysis_description VARCHAR(40) NOT NULL,
    analysis_active CHAR(1) DEFAULT 'Y',
    -- Statistics
    analysis_usage_count INTEGER DEFAULT 0,
    analysis_last_used INTEGER DEFAULT 0,
    -- Metadata
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (analysis_type, analysis_code)
);

-- 18. DELIVERY_REC - Delivery Addresses
CREATE TABLE delivery_rec (
    delivery_id SERIAL PRIMARY KEY,
    delivery_customer VARCHAR(10) NOT NULL,
    delivery_code VARCHAR(10) NOT NULL,
    delivery_name VARCHAR(40) NOT NULL,
    delivery_address_1 VARCHAR(30) DEFAULT '',
    delivery_address_2 VARCHAR(30) DEFAULT '',
    delivery_address_3 VARCHAR(30) DEFAULT '',
    delivery_address_4 VARCHAR(30) DEFAULT '',
    delivery_address_5 VARCHAR(12) DEFAULT '',
    delivery_contact VARCHAR(30) DEFAULT '',
    delivery_phone VARCHAR(20) DEFAULT '',
    delivery_instructions TEXT,
    delivery_default CHAR(1) DEFAULT 'N',
    -- Metadata
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (delivery_customer) REFERENCES saledger_rec(sales_key)
);

CREATE INDEX idx_delivery_customer ON delivery_rec(delivery_customer);
CREATE UNIQUE INDEX idx_delivery_code ON delivery_rec(delivery_customer, delivery_code);

-- 19. IRSDFLT_REC - IRS Defaults
CREATE TABLE irsdflt_rec (
    irs_key INTEGER PRIMARY KEY DEFAULT 1 CHECK (irs_key = 1),
    irs_company_name VARCHAR(40) DEFAULT '',
    irs_ein VARCHAR(20) DEFAULT '',
    irs_state_id VARCHAR(20) DEFAULT '',
    irs_business_code VARCHAR(10) DEFAULT '',
    irs_accounting_method CHAR(1) DEFAULT 'A', -- A=Accrual, C=Cash
    irs_fiscal_year_end SMALLINT DEFAULT 1231,
    irs_current_year INTEGER DEFAULT 2024,
    -- Tax Settings
    irs_federal_rate DECIMAL(5,2) DEFAULT 21.00,
    irs_state_rate DECIMAL(5,2) DEFAULT 0.00,
    irs_local_rate DECIMAL(5,2) DEFAULT 0.00,
    -- Accounts
    irs_cash_account BIGINT DEFAULT 10020000,
    irs_ar_account BIGINT DEFAULT 11010000,
    irs_ap_account BIGINT DEFAULT 20010000,
    irs_tax_account BIGINT DEFAULT 20020000,
    -- Metadata
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

-- 20. IRSFINAL_REC - IRS Final Accounts Mapping
CREATE TABLE irsfinal_rec (
    final_irs_code VARCHAR(10) PRIMARY KEY,
    final_description VARCHAR(60) NOT NULL,
    final_form VARCHAR(10) DEFAULT '',
    final_line VARCHAR(10) DEFAULT '',
    final_schedule VARCHAR(10) DEFAULT '',
    -- Mapped GL accounts
    final_gl_from BIGINT DEFAULT 0,
    final_gl_to BIGINT DEFAULT 0,
    final_sign CHAR(1) DEFAULT '+', -- +/- for debit/credit
    -- Totals
    final_current_year DECIMAL(15,2) DEFAULT 0.00,
    final_prior_year DECIMAL(15,2) DEFAULT 0.00,
    -- Metadata
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

-- 21. IRSNL_REC - IRS Nominal Ledger
CREATE TABLE irsnl_rec (
    irsnl_account BIGINT PRIMARY KEY,
    irsnl_description VARCHAR(40) NOT NULL,
    irsnl_irs_mapping VARCHAR(10) DEFAULT '',
    irsnl_balance DECIMAL(15,2) DEFAULT 0.00,
    -- Period balances
    irsnl_period_1 DECIMAL(15,2) DEFAULT 0.00,
    irsnl_period_2 DECIMAL(15,2) DEFAULT 0.00,
    irsnl_period_3 DECIMAL(15,2) DEFAULT 0.00,
    irsnl_period_4 DECIMAL(15,2) DEFAULT 0.00,
    irsnl_period_5 DECIMAL(15,2) DEFAULT 0.00,
    irsnl_period_6 DECIMAL(15,2) DEFAULT 0.00,
    irsnl_period_7 DECIMAL(15,2) DEFAULT 0.00,
    irsnl_period_8 DECIMAL(15,2) DEFAULT 0.00,
    irsnl_period_9 DECIMAL(15,2) DEFAULT 0.00,
    irsnl_period_10 DECIMAL(15,2) DEFAULT 0.00,
    irsnl_period_11 DECIMAL(15,2) DEFAULT 0.00,
    irsnl_period_12 DECIMAL(15,2) DEFAULT 0.00,
    -- Metadata
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

-- 22. IRSPOSTING_REC - IRS Postings
CREATE TABLE irsposting_rec (
    irspost_id SERIAL PRIMARY KEY,
    irspost_date INTEGER NOT NULL,
    irspost_account BIGINT NOT NULL,
    irspost_reference VARCHAR(20) DEFAULT '',
    irspost_description VARCHAR(40) DEFAULT '',
    irspost_debit DECIMAL(12,2) DEFAULT 0.00,
    irspost_credit DECIMAL(12,2) DEFAULT 0.00,
    irspost_period SMALLINT DEFAULT 0,
    irspost_source VARCHAR(10) DEFAULT '',
    -- Metadata
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (irspost_account) REFERENCES irsnl_rec(irsnl_account)
);

CREATE INDEX idx_irsposting_date ON irsposting_rec(irspost_date);
CREATE INDEX idx_irsposting_account ON irsposting_rec(irspost_account);

-- 23. PSIRSPOST_REC - PS IRS Posting
CREATE TABLE psirspost_rec (
    psirspost_id SERIAL PRIMARY KEY,
    psirspost_date INTEGER NOT NULL,
    psirspost_type VARCHAR(10) NOT NULL,
    psirspost_reference VARCHAR(20) DEFAULT '',
    psirspost_amount DECIMAL(12,2) DEFAULT 0.00,
    psirspost_status CHAR(1) DEFAULT 'U',
    -- Metadata
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

-- 24. SAAUTOGEN_REC - Sales Auto-generation Header
CREATE TABLE saautogen_rec (
    autogen_id INTEGER PRIMARY KEY,
    autogen_customer VARCHAR(10) NOT NULL,
    autogen_description VARCHAR(40) NOT NULL,
    autogen_active CHAR(1) DEFAULT 'Y',
    autogen_frequency CHAR(1) DEFAULT 'M', -- M=Monthly, W=Weekly, Q=Quarterly, Y=Yearly
    autogen_next_date INTEGER DEFAULT 0,
    autogen_last_date INTEGER DEFAULT 0,
    autogen_total_amount DECIMAL(12,2) DEFAULT 0.00,
    -- Invoice defaults
    autogen_terms VARCHAR(4) DEFAULT '30',
    autogen_nominal BIGINT DEFAULT 0,
    autogen_vat_code VARCHAR(4) DEFAULT 'VSTD',
    -- Metadata
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (autogen_customer) REFERENCES saledger_rec(sales_key)
);

-- 25. SAAUTOGEN_LINES_REC - Sales Auto-generation Lines
CREATE TABLE saautogen_lines_rec (
    autoline_id SERIAL PRIMARY KEY,
    autoline_header_id INTEGER NOT NULL,
    autoline_line_no SMALLINT NOT NULL,
    autoline_stock_code VARCHAR(30) DEFAULT '',
    autoline_description VARCHAR(40) DEFAULT '',
    autoline_quantity DECIMAL(12,3) DEFAULT 0.000,
    autoline_unit_price DECIMAL(12,4) DEFAULT 0.0000,
    autoline_discount DECIMAL(5,2) DEFAULT 0.00,
    autoline_vat_code VARCHAR(4) DEFAULT '',
    autoline_nominal BIGINT DEFAULT 0,
    -- Metadata
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (autoline_header_id) REFERENCES saautogen_rec(autogen_id) ON DELETE CASCADE
);

-- 26. PUAUTOGEN_REC - Purchase Auto-generation Header  
CREATE TABLE puautogen_rec (
    pautogen_id INTEGER PRIMARY KEY,
    pautogen_supplier VARCHAR(10) NOT NULL,
    pautogen_description VARCHAR(40) NOT NULL,
    pautogen_active CHAR(1) DEFAULT 'Y',
    pautogen_frequency CHAR(1) DEFAULT 'M', -- M=Monthly, W=Weekly, Q=Quarterly, Y=Yearly
    pautogen_next_date INTEGER DEFAULT 0,
    pautogen_last_date INTEGER DEFAULT 0,
    pautogen_total_amount DECIMAL(12,2) DEFAULT 0.00,
    -- Order defaults
    pautogen_delivery_days INTEGER DEFAULT 7,
    pautogen_nominal BIGINT DEFAULT 0,
    pautogen_vat_code VARCHAR(4) DEFAULT 'VSTD',
    -- Metadata
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (pautogen_supplier) REFERENCES puledger_rec(purch_key)
);

-- 27. PUAUTOGEN_LINES_REC - Purchase Auto-generation Lines
CREATE TABLE puautogen_lines_rec (
    pautoline_id SERIAL PRIMARY KEY,
    pautoline_header_id INTEGER NOT NULL,
    pautoline_line_no SMALLINT NOT NULL,
    pautoline_stock_code VARCHAR(30) DEFAULT '',
    pautoline_description VARCHAR(40) DEFAULT '',
    pautoline_quantity DECIMAL(12,3) DEFAULT 0.000,
    pautoline_unit_price DECIMAL(12,4) DEFAULT 0.0000,
    pautoline_discount DECIMAL(5,2) DEFAULT 0.00,
    pautoline_vat_code VARCHAR(4) DEFAULT '',
    pautoline_nominal BIGINT DEFAULT 0,
    -- Metadata
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (pautoline_header_id) REFERENCES puautogen_rec(pautogen_id) ON DELETE CASCADE
);

-- 28. SADELINV_REC - Deleted Sales Invoices
CREATE TABLE sadelinv_rec (
    delinv_key BIGINT PRIMARY KEY,
    delinv_customer VARCHAR(10) NOT NULL,
    delinv_date INTEGER NOT NULL,
    delinv_deleted_date INTEGER NOT NULL,
    delinv_deleted_by VARCHAR(30) DEFAULT '',
    delinv_reason VARCHAR(100) DEFAULT '',
    delinv_total DECIMAL(12,2) DEFAULT 0.00,
    -- Metadata
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

-- 29. PUDELINV_REC - Deleted Purchase Invoices  
CREATE TABLE pudelinv_rec (
    pdelinv_key SERIAL PRIMARY KEY,
    pdelinv_supplier VARCHAR(10) NOT NULL,
    pdelinv_number VARCHAR(20) NOT NULL,
    pdelinv_date INTEGER NOT NULL,
    pdelinv_deleted_date INTEGER NOT NULL,
    pdelinv_deleted_by VARCHAR(30) DEFAULT '',
    pdelinv_reason VARCHAR(100) DEFAULT '',
    pdelinv_total DECIMAL(12,2) DEFAULT 0.00,
    -- Metadata
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

-- 30. VALUEANAL_REC - Value Analysis
CREATE TABLE valueanal_rec (
    value_code VARCHAR(10) PRIMARY KEY,
    value_description VARCHAR(40) NOT NULL,
    value_type VARCHAR(10) NOT NULL,
    value_current DECIMAL(15,2) DEFAULT 0.00,
    value_mtd DECIMAL(15,2) DEFAULT 0.00,
    value_ytd DECIMAL(15,2) DEFAULT 0.00,
    value_last_year DECIMAL(15,2) DEFAULT 0.00,
    -- Metadata
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

-- 31. SYSTOT_REC - System Totals
CREATE TABLE systot_rec (
    systot_key INTEGER PRIMARY KEY DEFAULT 1 CHECK (systot_key = 1),
    -- Sales totals
    systot_sales_mtd DECIMAL(15,2) DEFAULT 0.00,
    systot_sales_ytd DECIMAL(15,2) DEFAULT 0.00,
    systot_sales_qty_mtd DECIMAL(15,3) DEFAULT 0.000,
    systot_sales_qty_ytd DECIMAL(15,3) DEFAULT 0.000,
    -- Purchase totals
    systot_purch_mtd DECIMAL(15,2) DEFAULT 0.00,
    systot_purch_ytd DECIMAL(15,2) DEFAULT 0.00,
    systot_purch_qty_mtd DECIMAL(15,3) DEFAULT 0.000,
    systot_purch_qty_ytd DECIMAL(15,3) DEFAULT 0.000,
    -- Stock totals
    systot_stock_value DECIMAL(15,2) DEFAULT 0.00,
    systot_stock_qty DECIMAL(15,3) DEFAULT 0.000,
    -- GL totals
    systot_gl_debits DECIMAL(15,2) DEFAULT 0.00,
    systot_gl_credits DECIMAL(15,2) DEFAULT 0.00,
    -- Metadata
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

-- 32. SYSDEFLT_REC - System Defaults
CREATE TABLE sysdeflt_rec (
    sysdeflt_module VARCHAR(10) PRIMARY KEY,
    sysdeflt_settings JSONB DEFAULT '{}',
    -- Metadata
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

-- 33. SYSFINAL_REC - System Final Accounts
CREATE TABLE sysfinal_rec (
    sysfinal_code VARCHAR(10) PRIMARY KEY,
    sysfinal_description VARCHAR(60) NOT NULL,
    sysfinal_type VARCHAR(10) NOT NULL,
    sysfinal_gl_from BIGINT DEFAULT 0,
    sysfinal_gl_to BIGINT DEFAULT 0,
    sysfinal_total DECIMAL(15,2) DEFAULT 0.00,
    -- Metadata
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

-- =============================================
-- WEB APPLICATION TABLES
-- =============================================

-- 34. USERS - System Users
CREATE TABLE users (
    user_id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    username VARCHAR(30) UNIQUE NOT NULL,
    email VARCHAR(100) UNIQUE NOT NULL,
    password_hash VARCHAR(255) NOT NULL,
    full_name VARCHAR(60) NOT NULL,
    -- Roles and permissions  
    role_id INTEGER NOT NULL,
    is_active BOOLEAN DEFAULT true,
    is_superuser BOOLEAN DEFAULT false,
    -- Access control
    modules_access JSONB DEFAULT '{}', -- {"GL": true, "SL": true, etc}
    last_login TIMESTAMP WITH TIME ZONE,
    failed_logins INTEGER DEFAULT 0,
    locked_until TIMESTAMP WITH TIME ZONE,
    -- Preferences
    language VARCHAR(5) DEFAULT 'en-US',
    timezone VARCHAR(50) DEFAULT 'UTC',
    theme VARCHAR(20) DEFAULT 'light',
    -- Metadata
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    created_by VARCHAR(30) DEFAULT CURRENT_USER
);

CREATE INDEX idx_users_username ON users(username);
CREATE INDEX idx_users_email ON users(email);

-- 35. ROLES - User Roles
CREATE TABLE roles (
    role_id SERIAL PRIMARY KEY,
    role_name VARCHAR(30) UNIQUE NOT NULL,
    role_description VARCHAR(100),
    permissions JSONB DEFAULT '{}',
    -- Module access levels
    gl_access SMALLINT DEFAULT 0, -- 0=None, 1=Read, 2=Write, 3=Full
    sl_access SMALLINT DEFAULT 0,
    pl_access SMALLINT DEFAULT 0,
    stock_access SMALLINT DEFAULT 0,
    reports_access SMALLINT DEFAULT 0,
    system_access SMALLINT DEFAULT 0,
    -- Metadata
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

-- 36. AUDIT_LOG - Detailed Audit Trail
CREATE TABLE audit_log (
    audit_id BIGSERIAL PRIMARY KEY,
    audit_timestamp TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    audit_user VARCHAR(30) NOT NULL,
    audit_action VARCHAR(20) NOT NULL, -- INSERT, UPDATE, DELETE, LOGIN, etc
    audit_table VARCHAR(50),
    audit_key VARCHAR(100),
    audit_old_values JSONB,
    audit_new_values JSONB,
    audit_ip_address INET,
    audit_user_agent TEXT,
    audit_session_id VARCHAR(100),
    audit_module VARCHAR(10),
    audit_program VARCHAR(30)
);

CREATE INDEX idx_audit_timestamp ON audit_log(audit_timestamp);
CREATE INDEX idx_audit_user ON audit_log(audit_user);
CREATE INDEX idx_audit_table ON audit_log(audit_table);
CREATE INDEX idx_audit_action ON audit_log(audit_action);

-- 37. SESSIONS - Active Sessions
CREATE TABLE sessions (
    session_id VARCHAR(100) PRIMARY KEY,
    user_id UUID NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    last_activity TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    expires_at TIMESTAMP WITH TIME ZONE NOT NULL,
    ip_address INET,
    user_agent TEXT,
    data JSONB DEFAULT '{}',
    FOREIGN KEY (user_id) REFERENCES users(user_id) ON DELETE CASCADE
);

CREATE INDEX idx_sessions_user ON sessions(user_id);
CREATE INDEX idx_sessions_expires ON sessions(expires_at);

-- 38. BATCH_JOBS - Background Jobs
CREATE TABLE batch_jobs (
    job_id BIGSERIAL PRIMARY KEY,
    job_name VARCHAR(50) NOT NULL,
    job_type VARCHAR(30) NOT NULL,
    job_status VARCHAR(20) DEFAULT 'PENDING', -- PENDING, RUNNING, COMPLETED, FAILED
    job_params JSONB DEFAULT '{}',
    job_result JSONB DEFAULT '{}',
    scheduled_at TIMESTAMP WITH TIME ZONE,
    started_at TIMESTAMP WITH TIME ZONE,
    completed_at TIMESTAMP WITH TIME ZONE,
    created_by VARCHAR(30),
    error_message TEXT,
    retry_count INTEGER DEFAULT 0,
    max_retries INTEGER DEFAULT 3
);

CREATE INDEX idx_batch_jobs_status ON batch_jobs(job_status);
CREATE INDEX idx_batch_jobs_type ON batch_jobs(job_type);
CREATE INDEX idx_batch_jobs_scheduled ON batch_jobs(scheduled_at);

-- 39. EMAIL_QUEUE - Email Queue
CREATE TABLE email_queue (
    email_id BIGSERIAL PRIMARY KEY,
    to_address VARCHAR(255) NOT NULL,
    cc_address VARCHAR(255),
    bcc_address VARCHAR(255),
    from_address VARCHAR(255) DEFAULT 'noreply@acas.com',
    subject VARCHAR(255) NOT NULL,
    body_text TEXT,
    body_html TEXT,
    attachments JSONB DEFAULT '[]',
    status VARCHAR(20) DEFAULT 'PENDING', -- PENDING, SENT, FAILED
    priority INTEGER DEFAULT 5,
    attempts INTEGER DEFAULT 0,
    sent_at TIMESTAMP WITH TIME ZONE,
    error_message TEXT,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    created_by VARCHAR(30)
);

CREATE INDEX idx_email_queue_status ON email_queue(status);
CREATE INDEX idx_email_queue_priority ON email_queue(priority);

-- 40. REPORT_TEMPLATES - Custom Report Templates
CREATE TABLE report_templates (
    template_id SERIAL PRIMARY KEY,
    template_name VARCHAR(50) UNIQUE NOT NULL,
    template_type VARCHAR(30) NOT NULL,
    template_module VARCHAR(10) NOT NULL,
    template_definition JSONB NOT NULL,
    template_sql TEXT,
    is_system BOOLEAN DEFAULT false,
    is_public BOOLEAN DEFAULT false,
    created_by VARCHAR(30) NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX idx_report_templates_type ON report_templates(template_type);
CREATE INDEX idx_report_templates_module ON report_templates(template_module);

-- 41. USER_PREFERENCES - User Preferences
CREATE TABLE user_preferences (
    pref_id SERIAL PRIMARY KEY,
    user_id UUID NOT NULL,
    pref_key VARCHAR(50) NOT NULL,
    pref_value JSONB NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (user_id) REFERENCES users(user_id) ON DELETE CASCADE,
    UNIQUE(user_id, pref_key)
);

CREATE INDEX idx_user_preferences_user ON user_preferences(user_id);

-- 42. SYSTEM_LOCKS - Record Locking
CREATE TABLE system_locks (
    lock_id BIGSERIAL PRIMARY KEY,
    lock_table VARCHAR(50) NOT NULL,
    lock_key VARCHAR(100) NOT NULL,
    locked_by VARCHAR(30) NOT NULL,
    locked_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    expires_at TIMESTAMP WITH TIME ZONE NOT NULL,
    lock_type VARCHAR(20) DEFAULT 'EXCLUSIVE',
    session_id VARCHAR(100),
    UNIQUE(lock_table, lock_key)
);

CREATE INDEX idx_system_locks_table ON system_locks(lock_table);
CREATE INDEX idx_system_locks_expires ON system_locks(expires_at);

-- 43. INTEGRATION_LOG - External Integration Log
CREATE TABLE integration_log (
    log_id BIGSERIAL PRIMARY KEY,
    integration_type VARCHAR(30) NOT NULL, -- EDI, BANK, API, etc
    direction VARCHAR(10) NOT NULL, -- IN, OUT
    status VARCHAR(20) NOT NULL, -- SUCCESS, FAILED, PARTIAL
    message_type VARCHAR(50),
    message_id VARCHAR(100),
    partner_id VARCHAR(50),
    request_data TEXT,
    response_data TEXT,
    error_message TEXT,
    processed_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    processing_time_ms INTEGER
);

CREATE INDEX idx_integration_log_type ON integration_log(integration_type);
CREATE INDEX idx_integration_log_status ON integration_log(status);
CREATE INDEX idx_integration_log_processed ON integration_log(processed_at);

-- =============================================
-- INITIAL DATA
-- =============================================

-- Insert default system record
INSERT INTO system_rec (
    system_rec_key,
    system_record_version_prime,
    system_record_version_secondary,
    vat_rate_1,
    period,
    op_system,
    date_form,
    rdbms_db_name,
    rdbms_port,
    rdbms_host
) VALUES (
    1,
    3,
    2,
    20.00,
    1,
    5,  -- Unix/Linux
    1,  -- UK date format
    'ACASDB',
    '5432',
    'localhost'
);

-- Insert default roles
INSERT INTO roles (role_name, role_description, gl_access, sl_access, pl_access, stock_access, reports_access, system_access) VALUES
('Administrator', 'Full system access', 3, 3, 3, 3, 3, 3),
('Manager', 'Manager level access', 3, 3, 3, 3, 3, 1),
('Supervisor', 'Supervisor access', 2, 2, 2, 2, 2, 0),
('Clerk', 'Data entry clerk', 1, 1, 1, 1, 1, 0),
('ReadOnly', 'Read only access', 1, 1, 1, 1, 1, 0);

-- Insert admin user (password: admin123)
INSERT INTO users (username, email, password_hash, full_name, role_id, is_superuser) VALUES
('admin', 'admin@acas.com', '$2b$12$LQv3c1yqBWVHxkd0LHAkCOYz6TtxMQJqhN4LEXD3X0fRLI4cGBky', 'System Administrator', 1, true);

-- =============================================
-- STORED PROCEDURES AND FUNCTIONS
-- =============================================

-- Function to calculate customer balance
CREATE OR REPLACE FUNCTION calculate_customer_balance(p_customer VARCHAR(10))
RETURNS DECIMAL(12,2) AS $$
DECLARE
    v_balance DECIMAL(12,2);
BEGIN
    SELECT COALESCE(SUM(soitm_balance), 0)
    INTO v_balance
    FROM saitm3_rec
    WHERE soitm_customer = p_customer;
    
    RETURN v_balance;
END;
$$ LANGUAGE plpgsql;

-- Function to calculate supplier balance
CREATE OR REPLACE FUNCTION calculate_supplier_balance(p_supplier VARCHAR(10))
RETURNS DECIMAL(12,2) AS $$
DECLARE
    v_balance DECIMAL(12,2);
BEGIN
    SELECT COALESCE(SUM(poitm_balance), 0)
    INTO v_balance
    FROM puitm5_rec
    WHERE poitm_supplier = p_supplier;
    
    RETURN v_balance;
END;
$$ LANGUAGE plpgsql;

-- Trigger to update customer balance
CREATE OR REPLACE FUNCTION update_customer_balance()
RETURNS TRIGGER AS $$
BEGIN
    UPDATE saledger_rec
    SET sales_balance = calculate_customer_balance(NEW.soitm_customer),
        updated_at = CURRENT_TIMESTAMP
    WHERE sales_key = NEW.soitm_customer;
    
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER trg_update_customer_balance
AFTER INSERT OR UPDATE OR DELETE ON saitm3_rec
FOR EACH ROW EXECUTE FUNCTION update_customer_balance();

-- Trigger to update supplier balance
CREATE OR REPLACE FUNCTION update_supplier_balance()
RETURNS TRIGGER AS $$
BEGIN
    UPDATE puledger_rec
    SET purch_balance = calculate_supplier_balance(NEW.poitm_supplier),
        updated_at = CURRENT_TIMESTAMP
    WHERE purch_key = NEW.poitm_supplier;
    
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER trg_update_supplier_balance
AFTER INSERT OR UPDATE OR DELETE ON puitm5_rec
FOR EACH ROW EXECUTE FUNCTION update_supplier_balance();

-- Audit trigger function (FINAL CORRECTED VERSION)
CREATE OR REPLACE FUNCTION acas.audit_trigger_function()
RETURNS TRIGGER AS $$
BEGIN
    INSERT INTO acas.audit_log (
        audit_user,
        audit_action,
        audit_table,
        audit_key,
        audit_old_values,
        audit_new_values
    ) VALUES (
        CURRENT_USER,
        TG_OP,
        TG_TABLE_NAME,
        -- Use a generic approach that extracts primary key from JSON
        CASE 
            WHEN TG_OP = 'DELETE' THEN 
                CASE TG_TABLE_NAME
                    WHEN 'saledger_rec' THEN (row_to_json(OLD)->>'sales_key')
                    WHEN 'puledger_rec' THEN (row_to_json(OLD)->>'purch_key')
                    WHEN 'stock_rec' THEN (row_to_json(OLD)->>'stock_key')
                    WHEN 'glledger_rec' THEN (row_to_json(OLD)->>'ledger_key')
                    WHEN 'users' THEN (row_to_json(OLD)->>'id')
                    ELSE 'unknown'
                END
            ELSE 
                CASE TG_TABLE_NAME
                    WHEN 'saledger_rec' THEN (row_to_json(NEW)->>'sales_key')
                    WHEN 'puledger_rec' THEN (row_to_json(NEW)->>'purch_key')
                    WHEN 'stock_rec' THEN (row_to_json(NEW)->>'stock_key')
                    WHEN 'glledger_rec' THEN (row_to_json(NEW)->>'ledger_key')
                    WHEN 'users' THEN (row_to_json(NEW)->>'id')
                    ELSE 'unknown'
                END
        END,
        CASE 
            WHEN TG_OP IN ('UPDATE', 'DELETE') THEN row_to_json(OLD)
            ELSE NULL
        END,
        CASE 
            WHEN TG_OP IN ('INSERT', 'UPDATE') THEN row_to_json(NEW)
            ELSE NULL
        END
    );
    
    RETURN CASE
        WHEN TG_OP = 'DELETE' THEN OLD
        ELSE NEW
    END;
END;
$$ LANGUAGE plpgsql;

-- Apply audit triggers to key tables
CREATE TRIGGER audit_saledger AFTER INSERT OR UPDATE OR DELETE ON saledger_rec
FOR EACH ROW EXECUTE FUNCTION acas.audit_trigger_function();

CREATE TRIGGER audit_puledger AFTER INSERT OR UPDATE OR DELETE ON puledger_rec
FOR EACH ROW EXECUTE FUNCTION acas.audit_trigger_function();

CREATE TRIGGER audit_stock AFTER INSERT OR UPDATE OR DELETE ON stock_rec
FOR EACH ROW EXECUTE FUNCTION acas.audit_trigger_function();

CREATE TRIGGER audit_glledger AFTER INSERT OR UPDATE OR DELETE ON glledger_rec
FOR EACH ROW EXECUTE FUNCTION acas.audit_trigger_function();

-- =============================================
-- INDEXES FOR PERFORMANCE
-- =============================================

-- Additional performance indexes
CREATE INDEX idx_sainvoice_period ON sainvoice_rec(invoice_period);
CREATE INDEX idx_puinvoice_period ON puinvoice_rec(pinvoice_period);
CREATE INDEX idx_glposting_source ON glposting_rec(posting_source_ledger, posting_source_key);
CREATE INDEX idx_stockaudit_user ON stockaudit_rec(audit_user);
CREATE INDEX idx_plpay_period ON plpay_rec(payment_period);

-- =============================================
-- GRANTS (adjust as needed)
-- =============================================

-- Grant schema usage
GRANT USAGE ON SCHEMA acas TO PUBLIC;

-- Grant table access (adjust based on your security requirements)
-- GRANT SELECT, INSERT, UPDATE, DELETE ON ALL TABLES IN SCHEMA acas TO acas_user;
-- GRANT USAGE ON ALL SEQUENCES IN SCHEMA acas TO acas_user;

-- =============================================
-- MIGRATION NOTES
-- =============================================

-- This schema represents a complete migration from COBOL ACAS system
-- Key considerations:
-- 1. All COBOL numeric dates (YYYYMMDD) stored as INTEGER
-- 2. All monetary values use DECIMAL with appropriate precision
-- 3. Character fields sized to match COBOL definitions
-- 4. Added metadata fields for web application needs
-- 5. Foreign keys ensure referential integrity
-- 6. Indexes optimize common queries
-- 7. Audit triggers track all changes
-- 8. Stored procedures handle complex calculations

-- End of schema