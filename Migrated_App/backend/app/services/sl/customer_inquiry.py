"""
Customer Inquiry Service - SL020 migration
Provides comprehensive customer information and analysis
"""
from typing import List, Optional, Dict, Tuple
from decimal import Decimal
from datetime import datetime, date, timedelta
from sqlalchemy.orm import Session
from sqlalchemy import and_, or_, func, case

from app.services.file_handlers.customer_handler import CustomerFileHandler
from app.services.file_handlers.system_handler import SystemFileHandler
from app.services.file_handlers.open_items_handler import SalesOpenItemsHandler
from app.models.customer import SalesLedgerRec
from app.models.sales import SalesOpenItemRec, SalesHistoryRec
from app.models.auth import User


class CustomerInquiryService:
    """
    Customer Inquiry functionality
    Implements SL020 - comprehensive customer information
    """
    
    def __init__(self, db: Session, current_user: Optional[User] = None):
        self.db = db
        self.current_user = current_user
        self.customer_handler = CustomerFileHandler(db)
        self.system_handler = SystemFileHandler(db)
        self.open_items_handler = SalesOpenItemsHandler(db)
        
    def get_customer_summary(self, customer_no: str) -> Dict:
        """Get comprehensive customer summary"""
        # Get customer master
        customer, status = self.customer_handler.process(4, key_value=customer_no)
        if status.fs_reply != "00":
            return {"error": "Customer not found"}
            
        # Get open items summary
        open_items = self._get_open_items_summary(customer_no)
        
        # Get payment history
        payment_history = self._get_payment_history(customer_no)
        
        # Get sales analysis
        sales_analysis = self._get_sales_analysis(customer_no)
        
        # Get aging summary
        aging = self._get_aging_summary(customer_no)
        
        # Calculate key metrics
        metrics = self._calculate_customer_metrics(customer, open_items)
        
        return {
            'customer': {
                'number': customer.sales_cust,
                'name': customer.sales_name,
                'status': {
                    'active': customer.sales_active == 'Y',
                    'on_stop': customer.sales_on_stop == 'Y',
                    'stop_date': customer.sales_stop_date if customer.sales_on_stop == 'Y' else None,
                    'stop_reason': customer.sales_stop_reason if customer.sales_on_stop == 'Y' else None
                },
                'contact': {
                    'address': f"{customer.sales_add1} {customer.sales_add2}".strip(),
                    'city': customer.sales_add3,
                    'postcode': customer.sales_postcode,
                    'phone': customer.sales_telno,
                    'email': customer.sales_email,
                    'contact_name': customer.sales_contact
                },
                'financial': {
                    'balance': float(customer.sales_balance),
                    'credit_limit': float(customer.sales_credit_lim),
                    'available_credit': float(customer.sales_credit_lim - customer.sales_balance - customer.sales_os_orders),
                    'open_orders': float(customer.sales_os_orders),
                    'payment_terms': customer.sales_payment_cd,
                    'currency': customer.sales_currency
                }
            },
            'open_items': open_items,
            'aging': aging,
            'payment_history': payment_history,
            'sales_analysis': sales_analysis,
            'metrics': metrics
        }
        
    def get_customer_transactions(self, customer_no: str, filters: Optional[Dict] = None) -> List[Dict]:
        """Get customer transaction history"""
        filters = filters or {}
        
        # Build query for open items
        query = self.db.query(SalesOpenItemRec).filter(
            SalesOpenItemRec.sales_oi_cust == customer_no
        )
        
        # Apply filters
        if filters.get('date_from'):
            query = query.filter(SalesOpenItemRec.sales_oi_date >= filters['date_from'])
        if filters.get('date_to'):
            query = query.filter(SalesOpenItemRec.sales_oi_date <= filters['date_to'])
        if filters.get('type'):
            query = query.filter(SalesOpenItemRec.sales_oi_type == filters['type'])
        if filters.get('unpaid_only'):
            query = query.filter(SalesOpenItemRec.sales_oi_amount != 0)
            
        # Get transactions
        transactions = query.order_by(SalesOpenItemRec.sales_oi_date.desc()).all()
        
        # Format transactions
        trans_list = []
        for trans in transactions:
            trans_dict = {
                'date': trans.sales_oi_date,
                'type': trans.sales_oi_type,
                'type_desc': self._get_transaction_type_desc(trans.sales_oi_type),
                'reference': trans.sales_oi_our_ref,
                'their_ref': trans.sales_oi_their_ref,
                'amount': float(trans.sales_oi_gross),
                'tax': float(trans.sales_oi_tax),
                'balance': float(trans.sales_oi_amount),
                'due_date': trans.sales_oi_due_date,
                'days_overdue': self._calculate_days_overdue(trans.sales_oi_due_date) if trans.sales_oi_amount != 0 else 0,
                'period': trans.sales_oi_period,
                'status': 'PAID' if trans.sales_oi_amount == 0 else 'OPEN'
            }
            
            # Add allocations if partially paid
            if trans.sales_oi_gross != trans.sales_oi_amount:
                trans_dict['allocations'] = self._get_transaction_allocations(trans)
                
            trans_list.append(trans_dict)
            
        return trans_list
        
    def get_customer_statement(self, customer_no: str, options: Optional[Dict] = None) -> Dict:
        """Generate customer statement data"""
        options = options or {}
        
        # Get customer
        customer, status = self.customer_handler.process(4, key_value=customer_no)
        if status.fs_reply != "00":
            return {"error": "Customer not found"}
            
        # Determine statement type
        statement_type = options.get('type', customer.sales_stmt_type)
        
        if statement_type == 'OPEN':
            return self._generate_open_item_statement(customer, options)
        elif statement_type == 'BALANCE':
            return self._generate_balance_forward_statement(customer, options)
        else:
            return self._generate_full_statement(customer, options)
            
    def get_credit_status(self, customer_no: str) -> Dict:
        """Get detailed credit status"""
        customer, status = self.customer_handler.process(4, key_value=customer_no)
        if status.fs_reply != "00":
            return {"error": "Customer not found"}
            
        # Calculate exposure
        total_exposure = customer.sales_balance + customer.sales_os_orders
        available = customer.sales_credit_lim - total_exposure
        utilization = (total_exposure / customer.sales_credit_lim * 100) if customer.sales_credit_lim > 0 else 0
        
        # Get overdue analysis
        overdue = self._get_overdue_analysis(customer_no)
        
        # Get payment performance
        performance = self._get_payment_performance(customer_no)
        
        # Credit recommendation
        recommendation = self._calculate_credit_recommendation(customer, overdue, performance)
        
        return {
            'customer': customer_no,
            'credit_limit': float(customer.sales_credit_lim),
            'exposure': {
                'balance': float(customer.sales_balance),
                'orders': float(customer.sales_os_orders),
                'total': float(total_exposure),
                'available': float(available),
                'utilization_pct': float(utilization)
            },
            'overdue': overdue,
            'performance': performance,
            'status': {
                'on_stop': customer.sales_on_stop == 'Y',
                'credit_exceeded': total_exposure > customer.sales_credit_lim,
                'has_overdue': overdue['total_overdue'] > 0
            },
            'recommendation': recommendation
        }
        
    def get_sales_history(self, customer_no: str, months: int = 12) -> Dict:
        """Get sales history and trends"""
        # Calculate date range
        end_date = datetime.now().date()
        start_date = end_date - timedelta(days=months * 30)
        
        # Get monthly sales
        monthly_sales = self.db.query(
            func.extract('year', func.to_date(SalesHistoryRec.hist_date.cast(String), 'YYYYMMDD')).label('year'),
            func.extract('month', func.to_date(SalesHistoryRec.hist_date.cast(String), 'YYYYMMDD')).label('month'),
            func.sum(SalesHistoryRec.hist_amount).label('sales'),
            func.sum(SalesHistoryRec.hist_cost).label('cost'),
            func.count(SalesHistoryRec.hist_id).label('count')
        ).filter(
            and_(
                SalesHistoryRec.hist_customer == customer_no,
                SalesHistoryRec.hist_type == 'INV',
                SalesHistoryRec.hist_date >= int(start_date.strftime("%Y%m%d"))
            )
        ).group_by('year', 'month').all()
        
        # Get product analysis
        product_sales = self.db.query(
            SalesHistoryRec.hist_product,
            func.sum(SalesHistoryRec.hist_quantity).label('quantity'),
            func.sum(SalesHistoryRec.hist_amount).label('amount')
        ).filter(
            and_(
                SalesHistoryRec.hist_customer == customer_no,
                SalesHistoryRec.hist_type == 'INV',
                SalesHistoryRec.hist_date >= int(start_date.strftime("%Y%m%d"))
            )
        ).group_by(SalesHistoryRec.hist_product).order_by(func.sum(SalesHistoryRec.hist_amount).desc()).limit(10).all()
        
        # Calculate trends
        sales_trend = self._calculate_sales_trend(monthly_sales)
        
        return {
            'period': f"{start_date.strftime('%Y-%m')} to {end_date.strftime('%Y-%m')}",
            'monthly_sales': [
                {
                    'year': int(m.year),
                    'month': int(m.month),
                    'sales': float(m.sales),
                    'cost': float(m.cost),
                    'margin': float(m.sales - m.cost),
                    'margin_pct': float((m.sales - m.cost) / m.sales * 100) if m.sales > 0 else 0,
                    'invoice_count': m.count
                }
                for m in monthly_sales
            ],
            'top_products': [
                {
                    'product': p.hist_product,
                    'quantity': float(p.quantity),
                    'amount': float(p.amount)
                }
                for p in product_sales
            ],
            'trends': sales_trend,
            'totals': {
                'sales': sum(float(m.sales) for m in monthly_sales),
                'cost': sum(float(m.cost) for m in monthly_sales),
                'margin': sum(float(m.sales - m.cost) for m in monthly_sales),
                'invoices': sum(m.count for m in monthly_sales)
            }
        }
        
    def _get_open_items_summary(self, customer_no: str) -> Dict:
        """Get summary of open items"""
        open_items = self.db.query(
            SalesOpenItemRec.sales_oi_type,
            func.count(SalesOpenItemRec.sales_oi_id).label('count'),
            func.sum(SalesOpenItemRec.sales_oi_amount).label('total')
        ).filter(
            and_(
                SalesOpenItemRec.sales_oi_cust == customer_no,
                SalesOpenItemRec.sales_oi_amount != 0
            )
        ).group_by(SalesOpenItemRec.sales_oi_type).all()
        
        summary = {
            'total_open': 0,
            'by_type': {}
        }
        
        for item in open_items:
            summary['by_type'][item.sales_oi_type] = {
                'count': item.count,
                'amount': float(item.total)
            }
            summary['total_open'] += float(item.total)
            
        return summary
        
    def _get_payment_history(self, customer_no: str) -> List[Dict]:
        """Get recent payment history"""
        # Get last 10 payments
        payments = self.db.query(SalesOpenItemRec).filter(
            and_(
                SalesOpenItemRec.sales_oi_cust == customer_no,
                SalesOpenItemRec.sales_oi_type == 'PAY'
            )
        ).order_by(SalesOpenItemRec.sales_oi_date.desc()).limit(10).all()
        
        return [
            {
                'date': p.sales_oi_date,
                'amount': float(abs(p.sales_oi_gross)),
                'reference': p.sales_oi_our_ref,
                'method': p.sales_oi_pay_method
            }
            for p in payments
        ]
        
    def _get_sales_analysis(self, customer_no: str) -> Dict:
        """Get sales analysis for customer"""
        customer, _ = self.customer_handler.process(4, key_value=customer_no)
        
        # YTD comparison
        ytd_growth = 0
        if customer.sales_last_yr_sales > 0:
            ytd_growth = ((customer.sales_ytd_sales - customer.sales_last_yr_sales) / 
                         customer.sales_last_yr_sales * 100)
            
        # Calculate margin
        ytd_margin = 0
        if customer.sales_ytd_sales > 0:
            ytd_margin = ((customer.sales_ytd_sales - customer.sales_ytd_cost) / 
                         customer.sales_ytd_sales * 100)
            
        return {
            'ytd_sales': float(customer.sales_ytd_sales),
            'ytd_cost': float(customer.sales_ytd_cost),
            'ytd_margin': float(ytd_margin),
            'last_year_sales': float(customer.sales_last_yr_sales),
            'ytd_growth': float(ytd_growth),
            'highest_balance': float(customer.sales_highest_bal),
            'average_days': customer.sales_average_days,
            'invoice_count': customer.sales_invoice_cnt
        }
        
    def _get_aging_summary(self, customer_no: str) -> Dict:
        """Get aging summary"""
        today = int(datetime.now().strftime("%Y%m%d"))
        
        # Define aging buckets
        aging_buckets = [
            ('current', 0, 30),
            ('30_days', 31, 60),
            ('60_days', 61, 90),
            ('90_days', 91, 120),
            ('over_120', 121, 9999)
        ]
        
        aging = {}
        total = Decimal('0')
        
        for bucket_name, min_days, max_days in aging_buckets:
            # Calculate date range
            date_from = today - (max_days * 10000)  # Approximate conversion
            date_to = today - (min_days * 10000)
            
            # Get amount for bucket
            result = self.db.query(
                func.sum(SalesOpenItemRec.sales_oi_amount)
            ).filter(
                and_(
                    SalesOpenItemRec.sales_oi_cust == customer_no,
                    SalesOpenItemRec.sales_oi_type == 'INV',
                    SalesOpenItemRec.sales_oi_amount != 0,
                    SalesOpenItemRec.sales_oi_due_date >= date_from,
                    SalesOpenItemRec.sales_oi_due_date < date_to
                )
            ).scalar()
            
            amount = result or Decimal('0')
            aging[bucket_name] = float(amount)
            total += amount
            
        aging['total'] = float(total)
        
        # Calculate percentages
        if total > 0:
            for bucket in aging_buckets:
                bucket_name = bucket[0]
                aging[f"{bucket_name}_pct"] = float(
                    Decimal(str(aging[bucket_name])) / total * 100
                )
                
        return aging
        
    def _calculate_customer_metrics(self, customer: SalesLedgerRec, open_items: Dict) -> Dict:
        """Calculate key customer metrics"""
        # DSO (Days Sales Outstanding)
        dso = 0
        if customer.sales_ytd_sales > 0:
            daily_sales = customer.sales_ytd_sales / 365
            if daily_sales > 0:
                dso = float(customer.sales_balance / daily_sales)
                
        # Payment performance
        on_time_pct = 100 - (customer.sales_average_days / 30 * 100) if customer.sales_average_days > 0 else 100
        
        return {
            'dso': dso,
            'payment_performance': float(on_time_pct),
            'credit_utilization': float(
                customer.sales_balance / customer.sales_credit_lim * 100
            ) if customer.sales_credit_lim > 0 else 0,
            'risk_score': self._calculate_risk_score(customer)
        }
        
    def _get_transaction_type_desc(self, trans_type: str) -> str:
        """Get description for transaction type"""
        types = {
            'INV': 'Invoice',
            'CRN': 'Credit Note',
            'PAY': 'Payment',
            'ADJ': 'Adjustment',
            'JNL': 'Journal'
        }
        return types.get(trans_type, trans_type)
        
    def _calculate_days_overdue(self, due_date: int) -> int:
        """Calculate days overdue"""
        if due_date == 0:
            return 0
            
        today = datetime.now().date()
        due = datetime.strptime(str(due_date), "%Y%m%d").date()
        
        if due >= today:
            return 0
            
        return (today - due).days
        
    def _get_transaction_allocations(self, transaction: SalesOpenItemRec) -> List[Dict]:
        """Get allocations for partially paid transaction"""
        # This would query allocation records
        # For now, return calculated allocation
        paid = float(transaction.sales_oi_gross - transaction.sales_oi_amount)
        
        return [{
            'date': transaction.sales_oi_last_pay,
            'amount': paid,
            'type': 'Payment'
        }]
        
    def _generate_open_item_statement(self, customer: SalesLedgerRec, options: Dict) -> Dict:
        """Generate open item statement"""
        # Get all open items
        open_items = self.db.query(SalesOpenItemRec).filter(
            and_(
                SalesOpenItemRec.sales_oi_cust == customer.sales_cust,
                SalesOpenItemRec.sales_oi_amount != 0
            )
        ).order_by(SalesOpenItemRec.sales_oi_date).all()
        
        # Group by age
        statement_lines = []
        total_due = Decimal('0')
        
        for item in open_items:
            days_overdue = self._calculate_days_overdue(item.sales_oi_due_date)
            
            statement_lines.append({
                'date': item.sales_oi_date,
                'type': item.sales_oi_type,
                'reference': item.sales_oi_our_ref,
                'amount': float(item.sales_oi_amount),
                'due_date': item.sales_oi_due_date,
                'days_overdue': days_overdue,
                'age_bucket': self._get_age_bucket(days_overdue)
            })
            
            total_due += item.sales_oi_amount
            
        return {
            'customer': {
                'number': customer.sales_cust,
                'name': customer.sales_name,
                'address': [
                    customer.sales_add1,
                    customer.sales_add2,
                    customer.sales_add3,
                    customer.sales_add4,
                    customer.sales_postcode
                ]
            },
            'statement_date': datetime.now().strftime("%Y-%m-%d"),
            'statement_type': 'Open Items',
            'items': statement_lines,
            'total_due': float(total_due),
            'aging': self._get_aging_summary(customer.sales_cust)
        }
        
    def _generate_balance_forward_statement(self, customer: SalesLedgerRec, options: Dict) -> Dict:
        """Generate balance forward statement"""
        # Implementation for balance forward statements
        pass
        
    def _generate_full_statement(self, customer: SalesLedgerRec, options: Dict) -> Dict:
        """Generate full activity statement"""
        # Implementation for full statements
        pass
        
    def _get_overdue_analysis(self, customer_no: str) -> Dict:
        """Get detailed overdue analysis"""
        today = int(datetime.now().strftime("%Y%m%d"))
        
        overdue_items = self.db.query(SalesOpenItemRec).filter(
            and_(
                SalesOpenItemRec.sales_oi_cust == customer_no,
                SalesOpenItemRec.sales_oi_type == 'INV',
                SalesOpenItemRec.sales_oi_amount != 0,
                SalesOpenItemRec.sales_oi_due_date < today
            )
        ).all()
        
        total_overdue = sum(item.sales_oi_amount for item in overdue_items)
        oldest_date = min((item.sales_oi_due_date for item in overdue_items), default=0)
        
        return {
            'count': len(overdue_items),
            'total_overdue': float(total_overdue),
            'oldest_date': oldest_date,
            'oldest_days': self._calculate_days_overdue(oldest_date) if oldest_date else 0
        }
        
    def _get_payment_performance(self, customer_no: str) -> Dict:
        """Analyze payment performance"""
        # Get paid invoices from history
        # This is simplified - would need more detailed tracking
        customer, _ = self.customer_handler.process(4, key_value=customer_no)
        
        return {
            'average_days': customer.sales_average_days,
            'on_time_pct': 100 - (customer.sales_average_days / 30 * 100) if customer.sales_average_days > 0 else 100,
            'last_payment': customer.sales_date_last_pay
        }
        
    def _calculate_credit_recommendation(self, customer: SalesLedgerRec, 
                                       overdue: Dict, performance: Dict) -> Dict:
        """Calculate credit recommendation"""
        risk_score = self._calculate_risk_score(customer)
        
        # Determine recommendation
        if customer.sales_on_stop == 'Y':
            action = 'MAINTAIN_STOP'
            reason = 'Customer currently on stop'
        elif overdue['total_overdue'] > customer.sales_credit_lim * 0.5:
            action = 'REDUCE'
            reason = 'Significant overdue balance'
        elif performance['average_days'] > 60:
            action = 'REVIEW'
            reason = 'Poor payment performance'
        elif risk_score < 30:
            action = 'INCREASE'
            reason = 'Low risk, good payment history'
        else:
            action = 'MAINTAIN'
            reason = 'Current limit appropriate'
            
        return {
            'action': action,
            'reason': reason,
            'risk_score': risk_score,
            'current_limit': float(customer.sales_credit_lim),
            'suggested_limit': self._calculate_suggested_limit(customer, risk_score)
        }
        
    def _calculate_risk_score(self, customer: SalesLedgerRec) -> float:
        """Calculate customer risk score (0-100, lower is better)"""
        score = 0
        
        # Payment performance (0-40 points)
        if customer.sales_average_days > 60:
            score += 40
        elif customer.sales_average_days > 45:
            score += 30
        elif customer.sales_average_days > 30:
            score += 20
        elif customer.sales_average_days > 15:
            score += 10
            
        # Credit utilization (0-30 points)
        if customer.sales_credit_lim > 0:
            utilization = customer.sales_balance / customer.sales_credit_lim
            if utilization > 1.0:
                score += 30
            elif utilization > 0.8:
                score += 20
            elif utilization > 0.6:
                score += 10
                
        # Account age (0-20 points)
        if customer.sales_date_opened > 0:
            days_open = (datetime.now().date() - 
                        datetime.strptime(str(customer.sales_date_opened), "%Y%m%d").date()).days
            if days_open < 180:
                score += 20
            elif days_open < 365:
                score += 10
                
        # Activity (0-10 points)
        if customer.sales_date_last_inv == 0:
            score += 10
        else:
            days_since_invoice = (datetime.now().date() - 
                                 datetime.strptime(str(customer.sales_date_last_inv), "%Y%m%d").date()).days
            if days_since_invoice > 180:
                score += 10
            elif days_since_invoice > 90:
                score += 5
                
        return score
        
    def _calculate_suggested_limit(self, customer: SalesLedgerRec, risk_score: float) -> float:
        """Calculate suggested credit limit based on risk"""
        # Base on average monthly sales
        if customer.sales_ytd_sales > 0:
            monthly_avg = customer.sales_ytd_sales / 12
            
            # Adjust based on risk
            if risk_score < 20:
                multiplier = 3.0  # 3 months
            elif risk_score < 40:
                multiplier = 2.0  # 2 months
            elif risk_score < 60:
                multiplier = 1.5  # 1.5 months
            else:
                multiplier = 1.0  # 1 month
                
            return float(monthly_avg * multiplier)
            
        return float(customer.sales_credit_lim)
        
    def _get_age_bucket(self, days: int) -> str:
        """Get age bucket for days overdue"""
        if days <= 0:
            return 'Current'
        elif days <= 30:
            return '1-30 days'
        elif days <= 60:
            return '31-60 days'
        elif days <= 90:
            return '61-90 days'
        else:
            return 'Over 90 days'
            
    def _calculate_sales_trend(self, monthly_sales: List) -> Dict:
        """Calculate sales trend analysis"""
        if len(monthly_sales) < 2:
            return {'trend': 'INSUFFICIENT_DATA'}
            
        # Simple linear regression for trend
        x_values = list(range(len(monthly_sales)))
        y_values = [float(m.sales) for m in monthly_sales]
        
        n = len(x_values)
        if n == 0:
            return {'trend': 'NO_DATA'}
            
        sum_x = sum(x_values)
        sum_y = sum(y_values)
        sum_xy = sum(x * y for x, y in zip(x_values, y_values))
        sum_x_squared = sum(x ** 2 for x in x_values)
        
        # Calculate slope
        if (n * sum_x_squared - sum_x ** 2) != 0:
            slope = (n * sum_xy - sum_x * sum_y) / (n * sum_x_squared - sum_x ** 2)
        else:
            slope = 0
            
        # Determine trend
        avg_sales = sum_y / n if n > 0 else 0
        trend_pct = (slope / avg_sales * 100) if avg_sales > 0 else 0
        
        if trend_pct > 5:
            trend = 'INCREASING'
        elif trend_pct < -5:
            trend = 'DECREASING'
        else:
            trend = 'STABLE'
            
        return {
            'trend': trend,
            'trend_pct': float(trend_pct),
            'average_monthly': float(avg_sales)
        }