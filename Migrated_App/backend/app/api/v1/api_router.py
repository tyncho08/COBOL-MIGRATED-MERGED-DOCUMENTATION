"""
Main API Router - Consolidates all module routes
"""

from fastapi import APIRouter
from app.api.v1 import (
    auth,
    customers,
    gl,
    sl,
    pl,
    stock,
    # reports,  # Temporarily disabled - has import issues
    admin,
    dashboard
)
from app.api.v1.irs import (
    company_router,
    transactions_router,
    bank_reconciliation_router,
    tax_calculations_router,
    tax_returns_router,
    electronic_filing_router,
    audit_trail_router
)

api_router = APIRouter()

# Dashboard routes
api_router.include_router(dashboard.router, prefix="/dashboard", tags=["dashboard"])

# Authentication routes
api_router.include_router(auth.router, prefix="/auth", tags=["authentication"])

# Sales Ledger routes
api_router.include_router(customers.router, prefix="/customers", tags=["customers"])
api_router.include_router(sl.invoices.router, prefix="/sl/invoices", tags=["sales-invoices"])
api_router.include_router(sl.payments.router, prefix="/sl/payments", tags=["sales-payments"])
api_router.include_router(sl.statements.router, prefix="/sl/statements", tags=["sales-statements"])
api_router.include_router(sl.credit_control.router, prefix="/sl/credit-control", tags=["credit-control"])

# Purchase Ledger routes
api_router.include_router(pl.suppliers.router, prefix="/suppliers", tags=["suppliers"])
api_router.include_router(pl.purchase_orders.router, prefix="/pl/orders", tags=["purchase-orders"])
api_router.include_router(pl.purchase_invoices.router, prefix="/pl/invoices", tags=["purchase-invoices"])
api_router.include_router(pl.purchase_payments.router, prefix="/pl/payments", tags=["purchase-payments"])
# Simplified supplier routes without auth
from app.api.v1.pl import suppliers_summary
api_router.include_router(suppliers_summary.router, prefix="/pl", tags=["purchase-ledger"])

# General Ledger routes
api_router.include_router(gl.accounts.router, prefix="/gl/accounts", tags=["gl-accounts"])
api_router.include_router(gl.journals.router, prefix="/gl/journals", tags=["gl-journals"])
api_router.include_router(gl.postings.router, prefix="/gl/postings", tags=["gl-postings"])
api_router.include_router(gl.periods.router, prefix="/gl/periods", tags=["gl-periods"])
api_router.include_router(gl.budgets.router, prefix="/gl/budgets", tags=["gl-budgets"])
# Simplified GL routes without auth
from app.api.v1.gl import gl_summary
api_router.include_router(gl_summary.router, prefix="/gl", tags=["general-ledger"])

# Stock Control routes
api_router.include_router(stock.items.router, prefix="/stock/items", tags=["stock-items"])
api_router.include_router(stock.movements.router, prefix="/stock/movements", tags=["stock-movements"])
api_router.include_router(stock.locations.router, prefix="/stock/locations", tags=["stock-locations"])
api_router.include_router(stock.valuation.router, prefix="/stock/valuation", tags=["stock-valuation"])
api_router.include_router(stock.orders.router, prefix="/stock/orders", tags=["stock-orders"])
# Simplified stock routes without auth
from app.api.v1.stock import stock_summary
api_router.include_router(stock_summary.router, prefix="/stock", tags=["stock-control"])

# IRS Module routes
api_router.include_router(company_router, prefix="/irs/company", tags=["irs-company"])
api_router.include_router(transactions_router, prefix="/irs/transactions", tags=["irs-transactions"])
api_router.include_router(bank_reconciliation_router, prefix="/irs/bank-reconciliation", tags=["irs-bank-reconciliation"])
api_router.include_router(tax_calculations_router, prefix="/irs/tax-calculations", tags=["irs-tax-calculations"])
api_router.include_router(tax_returns_router, prefix="/irs/returns", tags=["irs-returns"])
api_router.include_router(electronic_filing_router, prefix="/irs/efiling", tags=["irs-efiling"])
api_router.include_router(audit_trail_router, prefix="/irs/audit", tags=["irs-audit"])

# Sales Module routes (consolidated)
from app.api.v1 import sales
api_router.include_router(sales.customers.router, prefix="/sales/customers", tags=["sales-customers"])
api_router.include_router(sales.outstanding.router, prefix="/sales/outstanding", tags=["sales-outstanding"])
api_router.include_router(sales.reports.router, prefix="/sales/reports", tags=["sales-reports"])
api_router.include_router(sales.analytics.router, prefix="/sales/analytics", tags=["sales-analytics"])

# Reports routes
from app.api.v1 import reports
api_router.include_router(reports.router, prefix="/reports", tags=["reports"])

# Payments routes
from app.api.v1 import payments
api_router.include_router(payments.router, prefix="/payments", tags=["payments"])

# Admin routes
api_router.include_router(admin.users.router, prefix="/admin/users", tags=["admin-users"])
api_router.include_router(admin.system.router, prefix="/admin/system", tags=["admin-system"])
api_router.include_router(admin.backup.router, prefix="/admin/backup", tags=["admin-backup"])
api_router.include_router(admin.settings.router, prefix="/admin", tags=["admin-settings"])