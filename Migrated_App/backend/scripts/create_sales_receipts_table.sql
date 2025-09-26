-- Create missing sales_receipts table
-- Based on SalesReceiptRec model requirements

CREATE TABLE IF NOT EXISTS acas.sales_receipts (
    receipt_id SERIAL PRIMARY KEY,
    receipt_number VARCHAR(20) UNIQUE NOT NULL,
    sales_key VARCHAR(10) NOT NULL,
    receipt_date INTEGER NOT NULL,
    payment_method VARCHAR(10) NOT NULL,
    amount DECIMAL(12,2) NOT NULL,
    bank_account VARCHAR(20),
    check_number VARCHAR(20),
    check_date INTEGER,
    status CHAR(1) DEFAULT 'U',
    currency VARCHAR(3) DEFAULT 'USD',
    exchange_rate DECIMAL(8,4) DEFAULT 1.0000,
    customer_ref VARCHAR(30),
    narrative VARCHAR(100),
    received_by VARCHAR(30) NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (sales_key) REFERENCES acas.saledger_rec(sales_key)
);

-- Create indexes for performance
CREATE INDEX IF NOT EXISTS idx_sales_receipts_customer ON acas.sales_receipts(sales_key);
CREATE INDEX IF NOT EXISTS idx_sales_receipts_date ON acas.sales_receipts(receipt_date);
CREATE INDEX IF NOT EXISTS idx_sales_receipts_status ON acas.sales_receipts(status);
CREATE INDEX IF NOT EXISTS idx_sales_receipts_number ON acas.sales_receipts(receipt_number);

-- Insert some sample data for testing
INSERT INTO acas.sales_receipts (
    receipt_number, sales_key, receipt_date, payment_method, 
    amount, received_by, narrative
) VALUES 
('REC001', 'CUST001', 20250915, 'TRANSFER', 2500.00, 'system', 'Payment received'),
('REC002', 'CUST002', 20250920, 'CHECK', 1200.00, 'system', 'Check payment'),
('REC003', 'CUST003', 20250922, 'TRANSFER', 750.00, 'system', 'Bank transfer')
ON CONFLICT (receipt_number) DO NOTHING;