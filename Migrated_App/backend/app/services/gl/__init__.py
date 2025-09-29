"""General Ledger Services - COBOL GL programs migration"""

# Import service classes
from .chart_of_accounts import ChartOfAccountsService
from .budget_actual import BudgetActualService
from .period_close import PeriodCloseService
from .trial_balance import TrialBalanceService
from .journal_entry import JournalEntryService
from .financial_statements import FinancialStatementsService
from .account_analysis import AccountAnalysisService
from .custom_reports import CustomReportsService
from .gl_integration import GLIntegrationService
from .year_end_processing import YearEndProcessingService

# Export service instances/modules for API compatibility
chart_of_accounts = ChartOfAccountsService
budget_actual = BudgetActualService
period_close = PeriodCloseService
trial_balance = TrialBalanceService
journal_entry = JournalEntryService
financial_statements = FinancialStatementsService
account_analysis = AccountAnalysisService
custom_reports = CustomReportsService
gl_integration = GLIntegrationService
year_end = YearEndProcessingService

# Export all for convenience
__all__ = [
    'ChartOfAccountsService',
    'BudgetActualService', 
    'PeriodCloseService',
    'TrialBalanceService',
    'JournalEntryService',
    'FinancialStatementsService',
    'AccountAnalysisService',
    'CustomReportsService',
    'GLIntegrationService',
    'YearEndProcessingService',
    'chart_of_accounts',
    'budget_actual',
    'period_close',
    'trial_balance',
    'journal_entry',
    'financial_statements',
    'account_analysis',
    'custom_reports',
    'gl_integration',
    'year_end'
]