"""
IRS Module Models - Phase 8 migration
Tax processing, IRS compliance and reporting models
"""
from sqlalchemy import Column, Integer, String, Decimal, Date, DateTime, Text, Boolean, ForeignKey
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm import relationship
from datetime import datetime

Base = declarative_base()


class IrsCompanyConfigRec(Base):
    """IRS company configuration - IRS010"""
    __tablename__ = "irs_company_config_rec"
    
    config_id = Column(Integer, primary_key=True, autoincrement=True)
    config_company_code = Column(String(10), nullable=False, index=True)
    config_company_name = Column(String(100), nullable=False)
    config_ein = Column(String(12), nullable=False)  # Employer Identification Number
    config_tax_year = Column(Integer, nullable=False)
    config_accounting_method = Column(String(10), nullable=False)  # CASH, ACCRUAL
    config_business_type = Column(String(20), nullable=False)  # CORP, SCORP, PARTNERSHIP, LLC
    config_fiscal_year_end = Column(String(4), nullable=False)  # MMDD format
    config_address_line1 = Column(String(100), nullable=False)
    config_address_line2 = Column(String(100))
    config_city = Column(String(50), nullable=False)
    config_state = Column(String(2), nullable=False)
    config_zip_code = Column(String(10), nullable=False)
    config_phone = Column(String(15))
    config_fax = Column(String(15))
    config_email = Column(String(100))
    config_preparer_name = Column(String(100))
    config_preparer_ein = Column(String(12))
    config_preparer_phone = Column(String(15))
    config_quarterly_filing = Column(String(1), default='Y')
    config_electronic_filing = Column(String(1), default='Y')
    config_estimated_payments = Column(String(1), default='Y')
    config_created_date = Column(Integer, nullable=False)
    config_created_by = Column(String(20), nullable=False)
    config_updated_date = Column(Integer)
    config_updated_by = Column(String(20))


class IrsTransactionRec(Base):
    """IRS transaction records - IRS020"""
    __tablename__ = "irs_transaction_rec"
    
    trans_id = Column(Integer, primary_key=True, autoincrement=True)
    trans_company_code = Column(String(10), nullable=False, index=True)
    trans_date = Column(Integer, nullable=False, index=True)  # YYYYMMDD
    trans_type = Column(String(10), nullable=False)  # INCOME, EXPENSE, PAYROLL
    trans_category = Column(String(20), nullable=False)  # Tax category
    trans_subcategory = Column(String(20))
    trans_amount = Column(Decimal(15, 2), nullable=False)
    trans_description = Column(String(100), nullable=False)
    trans_reference = Column(String(20))
    trans_gl_account = Column(String(10), nullable=False)
    trans_deductible_amount = Column(Decimal(15, 2), default=0)
    trans_depreciation_method = Column(String(10))  # For assets
    trans_depreciation_life = Column(Integer)  # Years
    trans_section_code = Column(String(10))  # IRS section code
    trans_schedule = Column(String(5))  # Schedule A, B, C, etc.
    trans_form_line = Column(String(10))  # Form line reference
    trans_created_date = Column(Integer, nullable=False)
    trans_created_by = Column(String(20), nullable=False)
    trans_updated_date = Column(Integer)
    trans_updated_by = Column(String(20))


class IrsBankReconciliationRec(Base):
    """IRS bank reconciliation - IRS030"""
    __tablename__ = "irs_bank_reconciliation_rec"
    
    recon_id = Column(Integer, primary_key=True, autoincrement=True)
    recon_company_code = Column(String(10), nullable=False, index=True)
    recon_bank_account = Column(String(20), nullable=False)
    recon_statement_date = Column(Integer, nullable=False, index=True)  # YYYYMMDD
    recon_statement_balance = Column(Decimal(15, 2), nullable=False)
    recon_book_balance = Column(Decimal(15, 2), nullable=False)
    recon_outstanding_checks = Column(Decimal(15, 2), default=0)
    recon_deposits_in_transit = Column(Decimal(15, 2), default=0)
    recon_bank_charges = Column(Decimal(15, 2), default=0)
    recon_interest_earned = Column(Decimal(15, 2), default=0)
    recon_nsf_items = Column(Decimal(15, 2), default=0)
    recon_other_adjustments = Column(Decimal(15, 2), default=0)
    recon_reconciled_balance = Column(Decimal(15, 2), nullable=False)
    recon_status = Column(String(10), default='OPEN')  # OPEN, RECONCILED
    recon_reconciled_by = Column(String(20))
    recon_reconciled_date = Column(Integer)
    recon_notes = Column(Text)
    recon_created_date = Column(Integer, nullable=False)
    recon_created_by = Column(String(20), nullable=False)


class IrsTaxCalculationRec(Base):
    """IRS tax calculations - IRS040"""
    __tablename__ = "irs_tax_calculation_rec"
    
    calc_id = Column(Integer, primary_key=True, autoincrement=True)
    calc_company_code = Column(String(10), nullable=False, index=True)
    calc_tax_year = Column(Integer, nullable=False, index=True)
    calc_tax_period = Column(String(2), nullable=False)  # Q1, Q2, Q3, Q4, YR
    calc_calculation_date = Column(Integer, nullable=False)
    calc_gross_income = Column(Decimal(15, 2), default=0)
    calc_total_deductions = Column(Decimal(15, 2), default=0)
    calc_taxable_income = Column(Decimal(15, 2), default=0)
    calc_federal_tax = Column(Decimal(15, 2), default=0)
    calc_state_tax = Column(Decimal(15, 2), default=0)
    calc_alternative_minimum_tax = Column(Decimal(15, 2), default=0)
    calc_self_employment_tax = Column(Decimal(15, 2), default=0)
    calc_estimated_tax_payments = Column(Decimal(15, 2), default=0)
    calc_withholding_tax = Column(Decimal(15, 2), default=0)
    calc_credits = Column(Decimal(15, 2), default=0)
    calc_penalties = Column(Decimal(15, 2), default=0)
    calc_interest = Column(Decimal(15, 2), default=0)
    calc_refund_due = Column(Decimal(15, 2), default=0)
    calc_tax_due = Column(Decimal(15, 2), default=0)
    calc_effective_rate = Column(Decimal(5, 2), default=0)
    calc_marginal_rate = Column(Decimal(5, 2), default=0)
    calc_status = Column(String(10), default='DRAFT')  # DRAFT, FINAL, FILED
    calc_created_date = Column(Integer, nullable=False)
    calc_created_by = Column(String(20), nullable=False)


class IrsTaxTableRec(Base):
    """IRS tax tables - IRS045"""
    __tablename__ = "irs_tax_table_rec"
    
    table_id = Column(Integer, primary_key=True, autoincrement=True)
    table_tax_year = Column(Integer, nullable=False, index=True)
    table_filing_status = Column(String(10), nullable=False)  # SINGLE, MFJ, MFS, HOH
    table_income_min = Column(Decimal(15, 2), nullable=False)
    table_income_max = Column(Decimal(15, 2), nullable=False)
    table_tax_base = Column(Decimal(15, 2), default=0)
    table_tax_rate = Column(Decimal(5, 4), nullable=False)
    table_bracket_number = Column(Integer, nullable=False)
    table_standard_deduction = Column(Decimal(15, 2), default=0)
    table_personal_exemption = Column(Decimal(15, 2), default=0)
    table_created_date = Column(Integer, nullable=False)
    table_created_by = Column(String(20), nullable=False)


class IrsTaxReturnRec(Base):
    """IRS tax return preparation - IRS060"""
    __tablename__ = "irs_tax_return_rec"
    
    return_id = Column(Integer, primary_key=True, autoincrement=True)
    return_company_code = Column(String(10), nullable=False, index=True)
    return_tax_year = Column(Integer, nullable=False, index=True)
    return_form_type = Column(String(10), nullable=False)  # 1120, 1120S, 1065, etc.
    return_filing_status = Column(String(10))
    return_preparation_date = Column(Integer, nullable=False)
    return_due_date = Column(Integer, nullable=False)
    return_extended_due_date = Column(Integer)
    return_gross_receipts = Column(Decimal(15, 2), default=0)
    return_total_income = Column(Decimal(15, 2), default=0)
    return_total_deductions = Column(Decimal(15, 2), default=0)
    return_taxable_income = Column(Decimal(15, 2), default=0)
    return_total_tax = Column(Decimal(15, 2), default=0)
    return_total_payments = Column(Decimal(15, 2), default=0)
    return_refund_amount = Column(Decimal(15, 2), default=0)
    return_balance_due = Column(Decimal(15, 2), default=0)
    return_amended_return = Column(String(1), default='N')
    return_original_return_date = Column(Integer)
    return_status = Column(String(15), default='DRAFT')  # DRAFT, READY, FILED, AMENDED
    return_filed_date = Column(Integer)
    return_confirmation_number = Column(String(20))
    return_preparer_name = Column(String(100))
    return_preparer_signature_date = Column(Integer)
    return_created_date = Column(Integer, nullable=False)
    return_created_by = Column(String(20), nullable=False)


class IrsScheduleRec(Base):
    """IRS schedules - IRS065"""
    __tablename__ = "irs_schedule_rec"
    
    schedule_id = Column(Integer, primary_key=True, autoincrement=True)
    schedule_return_id = Column(Integer, ForeignKey('irs_tax_return_rec.return_id'), nullable=False, index=True)
    schedule_type = Column(String(5), nullable=False)  # A, B, C, D, E, etc.
    schedule_line_number = Column(String(10), nullable=False)
    schedule_description = Column(String(200), nullable=False)
    schedule_amount = Column(Decimal(15, 2), default=0)
    schedule_percentage = Column(Decimal(5, 2), default=0)
    schedule_carryover_amount = Column(Decimal(15, 2), default=0)
    schedule_prior_year_amount = Column(Decimal(15, 2), default=0)
    schedule_notes = Column(Text)
    schedule_supporting_docs = Column(String(1), default='N')
    schedule_created_date = Column(Integer, nullable=False)
    schedule_created_by = Column(String(20), nullable=False)
    
    # Relationship
    tax_return = relationship("IrsTaxReturnRec", back_populates="schedules")


class IrsEstimatedPaymentRec(Base):
    """IRS estimated payments - IRS085"""
    __tablename__ = "irs_estimated_payment_rec"
    
    payment_id = Column(Integer, primary_key=True, autoincrement=True)
    payment_company_code = Column(String(10), nullable=False, index=True)
    payment_tax_year = Column(Integer, nullable=False, index=True)
    payment_quarter = Column(String(2), nullable=False)  # Q1, Q2, Q3, Q4
    payment_due_date = Column(Integer, nullable=False)
    payment_amount = Column(Decimal(15, 2), nullable=False)
    payment_paid_date = Column(Integer)
    payment_paid_amount = Column(Decimal(15, 2), default=0)
    payment_method = Column(String(10))  # CHECK, EFTPS, ONLINE
    payment_confirmation = Column(String(20))
    payment_penalty = Column(Decimal(15, 2), default=0)
    payment_interest = Column(Decimal(15, 2), default=0)
    payment_status = Column(String(10), default='DUE')  # DUE, PAID, LATE, PENALTY
    payment_voucher_printed = Column(String(1), default='N')
    payment_created_date = Column(Integer, nullable=False)
    payment_created_by = Column(String(20), nullable=False)


class IrsFiscalCloseRec(Base):
    """IRS fiscal year close - IRS090"""
    __tablename__ = "irs_fiscal_close_rec"
    
    close_id = Column(Integer, primary_key=True, autoincrement=True)
    close_company_code = Column(String(10), nullable=False, index=True)
    close_tax_year = Column(Integer, nullable=False, index=True)
    close_start_date = Column(Integer, nullable=False)
    close_end_date = Column(Integer, nullable=False)
    close_status = Column(String(10), default='OPEN')  # OPEN, CLOSED, REOPENED
    close_closed_date = Column(Integer)
    close_closed_by = Column(String(20))
    close_total_income = Column(Decimal(15, 2), default=0)
    close_total_expenses = Column(Decimal(15, 2), default=0)
    close_net_income = Column(Decimal(15, 2), default=0)
    close_depreciation_taken = Column(Decimal(15, 2), default=0)
    close_section179_deduction = Column(Decimal(15, 2), default=0)
    close_bonus_depreciation = Column(Decimal(15, 2), default=0)
    close_carryforward_losses = Column(Decimal(15, 2), default=0)
    close_carryforward_credits = Column(Decimal(15, 2), default=0)
    close_adjustments_made = Column(Integer, default=0)
    close_final_return_prepared = Column(String(1), default='N')
    close_books_closed = Column(String(1), default='N')
    close_audit_trail_locked = Column(String(1), default='N')
    close_backup_completed = Column(String(1), default='N')
    close_notes = Column(Text)
    close_created_date = Column(Integer, nullable=False)
    close_created_by = Column(String(20), nullable=False)


class IrsDepreciationRec(Base):
    """IRS depreciation tracking"""
    __tablename__ = "irs_depreciation_rec"
    
    depreciation_id = Column(Integer, primary_key=True, autoincrement=True)
    depreciation_company_code = Column(String(10), nullable=False, index=True)
    depreciation_asset_code = Column(String(20), nullable=False, index=True)
    depreciation_asset_description = Column(String(100), nullable=False)
    depreciation_placed_in_service = Column(Integer, nullable=False)  # YYYYMMDD
    depreciation_cost = Column(Decimal(15, 2), nullable=False)
    depreciation_method = Column(String(10), nullable=False)  # SL, MACRS, DDB
    depreciation_life_years = Column(Integer, nullable=False)
    depreciation_convention = Column(String(10), default='HY')  # HY, MQ, MM
    depreciation_section179_amount = Column(Decimal(15, 2), default=0)
    depreciation_bonus_amount = Column(Decimal(15, 2), default=0)
    depreciation_basis_for_depreciation = Column(Decimal(15, 2), nullable=False)
    depreciation_current_year = Column(Decimal(15, 2), default=0)
    depreciation_prior_years = Column(Decimal(15, 2), default=0)
    depreciation_accumulated = Column(Decimal(15, 2), default=0)
    depreciation_remaining_basis = Column(Decimal(15, 2), nullable=False)
    depreciation_disposed_date = Column(Integer)  # YYYYMMDD
    depreciation_disposal_amount = Column(Decimal(15, 2), default=0)
    depreciation_gain_loss = Column(Decimal(15, 2), default=0)
    depreciation_status = Column(String(10), default='ACTIVE')  # ACTIVE, DISPOSED, FULLY_DEPRECIATED
    depreciation_created_date = Column(Integer, nullable=False)
    depreciation_created_by = Column(String(20), nullable=False)


class IrsAuditTrailRec(Base):
    """IRS audit trail - IRS075"""
    __tablename__ = "irs_audit_trail_rec"
    
    audit_id = Column(Integer, primary_key=True, autoincrement=True)
    audit_company_code = Column(String(10), nullable=False, index=True)
    audit_tax_year = Column(Integer, nullable=False, index=True)
    audit_date = Column(Integer, nullable=False, index=True)  # YYYYMMDD
    audit_time = Column(Integer, nullable=False)  # HHMMSS
    audit_user = Column(String(20), nullable=False, index=True)
    audit_program = Column(String(10), nullable=False)
    audit_function = Column(String(20), nullable=False)
    audit_table_name = Column(String(50))
    audit_record_key = Column(String(50))
    audit_action = Column(String(10), nullable=False)  # CREATE, UPDATE, DELETE, VIEW
    audit_old_values = Column(Text)
    audit_new_values = Column(Text)
    audit_ip_address = Column(String(15))
    audit_session_id = Column(String(50))
    audit_notes = Column(String(200))


class IrsElectronicFileRec(Base):
    """IRS electronic filing - IRS080"""
    __tablename__ = "irs_electronic_file_rec"
    
    efile_id = Column(Integer, primary_key=True, autoincrement=True)
    efile_company_code = Column(String(10), nullable=False, index=True)
    efile_return_id = Column(Integer, ForeignKey('irs_tax_return_rec.return_id'), nullable=False, index=True)
    efile_submission_id = Column(String(50), nullable=False)
    efile_transmission_date = Column(Integer, nullable=False)  # YYYYMMDD
    efile_transmission_time = Column(Integer, nullable=False)  # HHMMSS
    efile_file_name = Column(String(100), nullable=False)
    efile_file_size = Column(Integer, nullable=False)
    efile_file_format = Column(String(10), default='XML')
    efile_batch_number = Column(String(20))
    efile_acknowledgment_date = Column(Integer)
    efile_acknowledgment_status = Column(String(10), default='PENDING')  # PENDING, ACCEPTED, REJECTED
    efile_reject_codes = Column(Text)
    efile_correction_required = Column(String(1), default='N')
    efile_resubmission_count = Column(Integer, default=0)
    efile_final_status = Column(String(10), default='PENDING')  # PENDING, ACCEPTED, REJECTED, CORRECTED
    efile_created_date = Column(Integer, nullable=False)
    efile_created_by = Column(String(20), nullable=False)
    
    # Relationship
    tax_return = relationship("IrsTaxReturnRec")


# Add back_populates to IrsTaxReturnRec
IrsTaxReturnRec.schedules = relationship("IrsScheduleRec", back_populates="tax_return")