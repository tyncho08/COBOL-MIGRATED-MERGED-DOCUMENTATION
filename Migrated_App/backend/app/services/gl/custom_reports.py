"""
Custom GL Reports Service - GL120 migration
Provides customizable GL reporting capabilities
"""
from typing import List, Optional, Dict, Any, Tuple
from decimal import Decimal
from datetime import datetime, date
from sqlalchemy.orm import Session
from sqlalchemy import and_, or_, func, case, extract
import json

from app.services.file_handlers.gl_handler import GLFileHandler
from app.services.file_handlers.system_handler import SystemFileHandler
from app.models.gl_accounts import GLLedgerRec, GLPostingRec
from app.models.gl_reports import GLReportDefinition, GLReportSchedule
from app.core.security import log_user_action
from app.models.auth import User


class CustomReportsService:
    """
    Custom GL Reports functionality
    Implements GL120 - user-defined report builder
    """
    
    def __init__(self, db: Session, current_user: Optional[User] = None):
        self.db = db
        self.current_user = current_user
        self.gl_handler = GLFileHandler(db)
        self.system_handler = SystemFileHandler(db)
        
    def create_report_definition(self, report_data: Dict) -> Tuple[GLReportDefinition, Optional[str]]:
        """
        Create new custom report definition
        Returns (report_definition, error_message)
        """
        # Validate report type
        valid_types = ['DETAIL', 'SUMMARY', 'COMPARISON', 'TREND', 'MATRIX', 'CUSTOM']
        if report_data.get('report_type') not in valid_types:
            return None, f"Invalid report type. Must be one of: {valid_types}"
            
        # Create report definition
        report_def = GLReportDefinition(
            report_code=report_data.get('report_code'),
            report_name=report_data.get('report_name'),
            report_type=report_data.get('report_type'),
            report_category=report_data.get('report_category', 'CUSTOM'),
            report_description=report_data.get('report_description', ''),
            report_status='ACTIVE',
            created_by=self.current_user.username if self.current_user else 'SYSTEM',
            created_date=int(datetime.now().strftime("%Y%m%d"))
        )
        
        # Set report configuration
        config = {
            'columns': report_data.get('columns', []),
            'filters': report_data.get('filters', {}),
            'grouping': report_data.get('grouping', []),
            'sorting': report_data.get('sorting', []),
            'calculations': report_data.get('calculations', []),
            'formatting': report_data.get('formatting', {})
        }
        report_def.report_config = json.dumps(config)
        
        # Set account selection criteria
        selection = report_data.get('account_selection', {})
        report_def.account_selection = json.dumps(selection)
        
        # Set period options
        period_options = report_data.get('period_options', {})
        report_def.period_options = json.dumps(period_options)
        
        self.db.add(report_def)
        self.db.flush()
        
        # Log creation
        if self.current_user:
            log_user_action(
                db=self.db,
                user=self.current_user,
                action="CREATE_CUSTOM_REPORT",
                module="GL",
                new_values={
                    'report_code': report_def.report_code,
                    'report_name': report_def.report_name,
                    'report_type': report_def.report_type
                }
            )
            
        return report_def, None
        
    def generate_custom_report(self, report_code: str, parameters: Optional[Dict] = None) -> Dict:
        """Generate custom report based on definition"""
        # Get report definition
        report_def = self.db.query(GLReportDefinition).filter(
            GLReportDefinition.report_code == report_code
        ).first()
        
        if not report_def:
            return {"error": "Report definition not found"}
            
        parameters = parameters or {}
        config = json.loads(report_def.report_config)
        
        # Determine report type and generate accordingly
        if report_def.report_type == 'DETAIL':
            return self._generate_detail_report(report_def, parameters)
        elif report_def.report_type == 'SUMMARY':
            return self._generate_summary_report(report_def, parameters)
        elif report_def.report_type == 'COMPARISON':
            return self._generate_comparison_report(report_def, parameters)
        elif report_def.report_type == 'TREND':
            return self._generate_trend_report(report_def, parameters)
        elif report_def.report_type == 'MATRIX':
            return self._generate_matrix_report(report_def, parameters)
        else:
            return self._generate_custom_layout_report(report_def, parameters)
            
    def _generate_detail_report(self, report_def: GLReportDefinition, parameters: Dict) -> Dict:
        """Generate detailed transaction report"""
        config = json.loads(report_def.report_config)
        selection = json.loads(report_def.account_selection)
        
        # Build account filter
        query = self.db.query(GLPostingRec)
        
        # Apply account selection
        if 'account_range' in selection:
            query = query.filter(
                and_(
                    GLPostingRec.posting_account >= selection['account_range']['from'],
                    GLPostingRec.posting_account <= selection['account_range']['to']
                )
            )
        elif 'account_list' in selection:
            query = query.filter(
                GLPostingRec.posting_account.in_(selection['account_list'])
            )
        elif 'account_type' in selection:
            # Join with account master
            query = query.join(GLLedgerRec).filter(
                GLLedgerRec.ledger_type.in_(selection['account_type'])
            )
            
        # Apply period filter
        period_from = parameters.get('period_from', 1)
        period_to = parameters.get('period_to', 13)
        query = query.filter(
            and_(
                GLPostingRec.posting_period >= period_from,
                GLPostingRec.posting_period <= period_to
            )
        )
        
        # Apply date filter if specified
        if 'date_from' in parameters:
            query = query.filter(GLPostingRec.posting_date >= parameters['date_from'])
        if 'date_to' in parameters:
            query = query.filter(GLPostingRec.posting_date <= parameters['date_to'])
            
        # Apply additional filters from config
        if 'amount_min' in config['filters']:
            min_amt = Decimal(str(config['filters']['amount_min']))
            query = query.filter(
                or_(
                    GLPostingRec.posting_debit >= min_amt,
                    GLPostingRec.posting_credit >= min_amt
                )
            )
            
        # Get transactions
        transactions = query.order_by(
            GLPostingRec.posting_date,
            GLPostingRec.posting_account
        ).all()
        
        # Format data based on columns configuration
        detail_data = []
        for trans in transactions:
            # Get account details
            account = self.db.query(GLLedgerRec).filter(
                GLLedgerRec.ledger_key == trans.posting_account
            ).first()
            
            row = self._build_detail_row(trans, account, config['columns'])
            detail_data.append(row)
            
        # Apply grouping if specified
        if config.get('grouping'):
            detail_data = self._apply_grouping(detail_data, config['grouping'])
            
        # Calculate totals
        totals = self._calculate_totals(detail_data, config.get('calculations', []))
        
        return {
            'report_name': report_def.report_name,
            'report_type': 'DETAIL',
            'generated_date': datetime.now().strftime("%Y-%m-%d %H:%M"),
            'parameters': parameters,
            'data': detail_data,
            'totals': totals,
            'row_count': len(detail_data),
            'config': config
        }
        
    def _generate_summary_report(self, report_def: GLReportDefinition, parameters: Dict) -> Dict:
        """Generate summary report by account"""
        config = json.loads(report_def.report_config)
        selection = json.loads(report_def.account_selection)
        
        # Get accounts based on selection
        account_query = self.db.query(GLLedgerRec)
        
        if 'account_range' in selection:
            account_query = account_query.filter(
                and_(
                    GLLedgerRec.ledger_key >= selection['account_range']['from'],
                    GLLedgerRec.ledger_key <= selection['account_range']['to']
                )
            )
        elif 'account_list' in selection:
            account_query = account_query.filter(
                GLLedgerRec.ledger_key.in_(selection['account_list'])
            )
        elif 'account_type' in selection:
            account_query = account_query.filter(
                GLLedgerRec.ledger_type.in_(selection['account_type'])
            )
            
        accounts = account_query.filter(GLLedgerRec.ledger_active == 'Y').all()
        
        # Build summary data
        summary_data = []
        for account in accounts:
            # Get period activity
            period_activity = self._get_account_period_activity(
                account.ledger_key,
                parameters.get('period_from', 1),
                parameters.get('period_to', 13)
            )
            
            if period_activity['transaction_count'] == 0 and not parameters.get('include_zero', False):
                continue
                
            summary_row = {
                'account': account.ledger_key,
                'name': account.ledger_name,
                'type': account.ledger_type,
                'opening_balance': float(period_activity['opening_balance']),
                'total_debits': float(period_activity['total_debits']),
                'total_credits': float(period_activity['total_credits']),
                'net_movement': float(period_activity['net_movement']),
                'closing_balance': float(period_activity['closing_balance']),
                'transaction_count': period_activity['transaction_count']
            }
            
            summary_data.append(summary_row)
            
        # Apply sorting
        if config.get('sorting'):
            summary_data = self._apply_sorting(summary_data, config['sorting'])
            
        # Calculate grand totals
        grand_totals = {
            'total_debits': sum(r['total_debits'] for r in summary_data),
            'total_credits': sum(r['total_credits'] for r in summary_data),
            'net_movement': sum(r['net_movement'] for r in summary_data),
            'account_count': len(summary_data)
        }
        
        return {
            'report_name': report_def.report_name,
            'report_type': 'SUMMARY',
            'generated_date': datetime.now().strftime("%Y-%m-%d %H:%M"),
            'parameters': parameters,
            'data': summary_data,
            'grand_totals': grand_totals,
            'config': config
        }
        
    def _generate_comparison_report(self, report_def: GLReportDefinition, parameters: Dict) -> Dict:
        """Generate comparison report (actual vs budget, period vs period, etc.)"""
        config = json.loads(report_def.report_config)
        comparison_type = config.get('comparison_type', 'PERIOD')
        
        if comparison_type == 'BUDGET':
            return self._generate_budget_comparison(report_def, parameters)
        elif comparison_type == 'PERIOD':
            return self._generate_period_comparison(report_def, parameters)
        elif comparison_type == 'YEAR':
            return self._generate_year_comparison(report_def, parameters)
        else:
            return {"error": f"Unknown comparison type: {comparison_type}"}
            
    def _generate_trend_report(self, report_def: GLReportDefinition, parameters: Dict) -> Dict:
        """Generate trend analysis report"""
        config = json.loads(report_def.report_config)
        selection = json.loads(report_def.account_selection)
        
        # Determine trend periods
        trend_periods = config.get('trend_periods', 12)
        current_period = self.system_handler.get_current_period()
        
        # Get accounts
        accounts = self._get_selected_accounts(selection)
        
        trend_data = []
        for account in accounts:
            account_trend = {
                'account': account.ledger_key,
                'name': account.ledger_name,
                'periods': []
            }
            
            # Get data for each period
            for i in range(trend_periods):
                period = current_period - i
                if period < 1:
                    period += 13  # Wrap to previous year
                    
                period_data = self._get_period_data(account.ledger_key, period)
                account_trend['periods'].append({
                    'period': period,
                    'balance': float(period_data['balance']),
                    'movement': float(period_data['movement'])
                })
                
            # Calculate trend metrics
            trend_metrics = self._calculate_trend_metrics(account_trend['periods'])
            account_trend.update(trend_metrics)
            
            trend_data.append(account_trend)
            
        return {
            'report_name': report_def.report_name,
            'report_type': 'TREND',
            'generated_date': datetime.now().strftime("%Y-%m-%d %H:%M"),
            'parameters': parameters,
            'data': trend_data,
            'config': config
        }
        
    def _generate_matrix_report(self, report_def: GLReportDefinition, parameters: Dict) -> Dict:
        """Generate matrix/pivot report"""
        config = json.loads(report_def.report_config)
        
        # Matrix dimensions
        row_dimension = config.get('row_dimension', 'ACCOUNT')
        column_dimension = config.get('column_dimension', 'PERIOD')
        value_field = config.get('value_field', 'BALANCE')
        
        # Build matrix query based on dimensions
        if row_dimension == 'ACCOUNT' and column_dimension == 'PERIOD':
            return self._generate_account_period_matrix(report_def, parameters)
        elif row_dimension == 'ACCOUNT' and column_dimension == 'ANALYSIS':
            return self._generate_account_analysis_matrix(report_def, parameters)
        else:
            return {"error": f"Unsupported matrix dimensions: {row_dimension} x {column_dimension}"}
            
    def _generate_custom_layout_report(self, report_def: GLReportDefinition, parameters: Dict) -> Dict:
        """Generate report with custom layout definition"""
        config = json.loads(report_def.report_config)
        layout = config.get('layout', {})
        
        # Build report sections based on layout
        sections = []
        for section_def in layout.get('sections', []):
            section = self._build_report_section(section_def, parameters)
            sections.append(section)
            
        return {
            'report_name': report_def.report_name,
            'report_type': 'CUSTOM',
            'generated_date': datetime.now().strftime("%Y-%m-%d %H:%M"),
            'parameters': parameters,
            'sections': sections,
            'config': config
        }
        
    def schedule_report(self, report_code: str, schedule_data: Dict) -> Tuple[GLReportSchedule, Optional[str]]:
        """Schedule report for automatic generation"""
        # Validate report exists
        report_def = self.db.query(GLReportDefinition).filter(
            GLReportDefinition.report_code == report_code
        ).first()
        
        if not report_def:
            return None, "Report definition not found"
            
        # Create schedule
        schedule = GLReportSchedule(
            report_code=report_code,
            schedule_type=schedule_data.get('schedule_type', 'MONTHLY'),
            schedule_frequency=schedule_data.get('frequency', 1),
            schedule_day=schedule_data.get('day_of_month', 1),
            schedule_time=schedule_data.get('time', '08:00'),
            schedule_parameters=json.dumps(schedule_data.get('parameters', {})),
            output_format=schedule_data.get('output_format', 'PDF'),
            email_recipients=schedule_data.get('email_recipients', ''),
            schedule_status='ACTIVE',
            created_by=self.current_user.username if self.current_user else 'SYSTEM',
            next_run_date=self._calculate_next_run(schedule_data)
        )
        
        self.db.add(schedule)
        self.db.flush()
        
        return schedule, None
        
    def get_available_reports(self, category: Optional[str] = None) -> List[Dict]:
        """Get list of available custom reports"""
        query = self.db.query(GLReportDefinition).filter(
            GLReportDefinition.report_status == 'ACTIVE'
        )
        
        if category:
            query = query.filter(GLReportDefinition.report_category == category)
            
        reports = query.order_by(GLReportDefinition.report_category, GLReportDefinition.report_name).all()
        
        return [
            {
                'code': r.report_code,
                'name': r.report_name,
                'type': r.report_type,
                'category': r.report_category,
                'description': r.report_description,
                'created_by': r.created_by,
                'created_date': r.created_date,
                'last_run': r.last_run_date
            }
            for r in reports
        ]
        
    def _build_detail_row(self, posting: GLPostingRec, account: GLLedgerRec, columns: List[str]) -> Dict:
        """Build detail row based on column configuration"""
        row = {}
        
        for col in columns:
            if col == 'date':
                row['date'] = posting.posting_date
            elif col == 'period':
                row['period'] = posting.posting_period
            elif col == 'account':
                row['account'] = posting.posting_account
            elif col == 'account_name':
                row['account_name'] = account.ledger_name if account else ''
            elif col == 'reference':
                row['reference'] = posting.posting_reference
            elif col == 'description':
                row['description'] = posting.posting_description
            elif col == 'debit':
                row['debit'] = float(posting.posting_debit)
            elif col == 'credit':
                row['credit'] = float(posting.posting_credit)
            elif col == 'amount':
                row['amount'] = float(posting.posting_debit - posting.posting_credit)
            elif col == 'analysis_1':
                row['analysis_1'] = posting.posting_analysis_1
            elif col == 'analysis_2':
                row['analysis_2'] = posting.posting_analysis_2
            elif col == 'analysis_3':
                row['analysis_3'] = posting.posting_analysis_3
            elif col == 'user':
                row['user'] = posting.posting_user
                
        return row
        
    def _get_account_period_activity(self, account_no: int, period_from: int, period_to: int) -> Dict:
        """Get account activity for period range"""
        # Get opening balance (sum of all periods before period_from)
        opening_trans = self.db.query(
            func.sum(GLPostingRec.posting_debit).label('total_dr'),
            func.sum(GLPostingRec.posting_credit).label('total_cr')
        ).filter(
            and_(
                GLPostingRec.posting_account == account_no,
                GLPostingRec.posting_period < period_from,
                GLPostingRec.posting_status == 'P'
            )
        ).first()
        
        opening_dr = opening_trans.total_dr or Decimal('0')
        opening_cr = opening_trans.total_cr or Decimal('0')
        
        # Get account type to determine natural balance
        account = self.db.query(GLLedgerRec).filter(
            GLLedgerRec.ledger_key == account_no
        ).first()
        
        if account and account.ledger_type in [1, 5]:  # Assets & Expenses
            opening_balance = opening_dr - opening_cr
        else:
            opening_balance = opening_cr - opening_dr
            
        # Get period activity
        period_trans = self.db.query(
            func.count(GLPostingRec.posting_id).label('count'),
            func.sum(GLPostingRec.posting_debit).label('total_dr'),
            func.sum(GLPostingRec.posting_credit).label('total_cr')
        ).filter(
            and_(
                GLPostingRec.posting_account == account_no,
                GLPostingRec.posting_period >= period_from,
                GLPostingRec.posting_period <= period_to,
                GLPostingRec.posting_status == 'P'
            )
        ).first()
        
        total_dr = period_trans.total_dr or Decimal('0')
        total_cr = period_trans.total_cr or Decimal('0')
        
        if account and account.ledger_type in [1, 5]:  # Assets & Expenses
            net_movement = total_dr - total_cr
        else:
            net_movement = total_cr - total_dr
            
        return {
            'opening_balance': opening_balance,
            'total_debits': total_dr,
            'total_credits': total_cr,
            'net_movement': net_movement,
            'closing_balance': opening_balance + net_movement,
            'transaction_count': period_trans.count or 0
        }
        
    def _get_selected_accounts(self, selection: Dict) -> List[GLLedgerRec]:
        """Get accounts based on selection criteria"""
        query = self.db.query(GLLedgerRec)
        
        if 'account_range' in selection:
            query = query.filter(
                and_(
                    GLLedgerRec.ledger_key >= selection['account_range']['from'],
                    GLLedgerRec.ledger_key <= selection['account_range']['to']
                )
            )
        elif 'account_list' in selection:
            query = query.filter(
                GLLedgerRec.ledger_key.in_(selection['account_list'])
            )
        elif 'account_type' in selection:
            query = query.filter(
                GLLedgerRec.ledger_type.in_(selection['account_type'])
            )
            
        return query.filter(GLLedgerRec.ledger_active == 'Y').all()
        
    def _apply_grouping(self, data: List[Dict], grouping: List[str]) -> List[Dict]:
        """Apply grouping to data"""
        # Simple grouping implementation
        # Would be enhanced for complex grouping requirements
        return data
        
    def _apply_sorting(self, data: List[Dict], sorting: List[Dict]) -> List[Dict]:
        """Apply sorting to data"""
        for sort_spec in reversed(sorting):
            field = sort_spec.get('field')
            reverse = sort_spec.get('descending', False)
            if field:
                data.sort(key=lambda x: x.get(field, ''), reverse=reverse)
        return data
        
    def _calculate_totals(self, data: List[Dict], calculations: List[Dict]) -> Dict:
        """Calculate totals based on configuration"""
        totals = {}
        
        for calc in calculations:
            field = calc.get('field')
            calc_type = calc.get('type', 'SUM')
            
            if calc_type == 'SUM':
                totals[field] = sum(row.get(field, 0) for row in data)
            elif calc_type == 'AVG':
                values = [row.get(field, 0) for row in data if field in row]
                totals[field] = sum(values) / len(values) if values else 0
            elif calc_type == 'COUNT':
                totals[field] = len([row for row in data if field in row])
                
        return totals
        
    def _calculate_next_run(self, schedule_data: Dict) -> int:
        """Calculate next run date for scheduled report"""
        # Simple implementation - would be enhanced for complex schedules
        return int((datetime.now().date() + timedelta(days=30)).strftime("%Y%m%d"))
        
    def export_custom_report(self, report_data: Dict, format: str = 'excel') -> bytes:
        """Export custom report to various formats"""
        if format == 'csv':
            return self._export_to_csv(report_data)
        elif format == 'excel':
            return self._export_to_excel(report_data)
        elif format == 'pdf':
            return self._export_to_pdf(report_data)
        else:
            return b''
            
    def _export_to_csv(self, report_data: Dict) -> bytes:
        """Export report to CSV format"""
        import csv
        import io
        
        output = io.StringIO()
        writer = csv.writer(output)
        
        # Write header
        writer.writerow([report_data['report_name']])
        writer.writerow([f"Generated: {report_data['generated_date']}"])
        writer.writerow([])
        
        # Write data based on report type
        if report_data['report_type'] == 'DETAIL':
            # Write column headers
            if report_data['data']:
                writer.writerow(report_data['data'][0].keys())
                
            # Write data rows
            for row in report_data['data']:
                writer.writerow(row.values())
                
        return output.getvalue().encode('utf-8')