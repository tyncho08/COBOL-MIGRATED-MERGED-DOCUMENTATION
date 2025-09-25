"""
Supplier Inquiry Service - PL020 migration
Provides comprehensive supplier information and analysis
"""
from typing import List, Optional, Dict, Tuple
from decimal import Decimal
from datetime import datetime, date, timedelta
from sqlalchemy.orm import Session
from sqlalchemy import and_, or_, func, case

from app.services.file_handlers.supplier_handler import SupplierFileHandler
from app.services.file_handlers.system_handler import SystemFileHandler
from app.services.file_handlers.open_items_handler import PurchaseOpenItemsHandler
from app.models.supplier import PurchaseLedgerRec
from app.models.purchase import PurchaseOpenItemRec, PurchaseHistoryRec
from app.models.auth import User


class SupplierInquiryService:
    """
    Supplier Inquiry functionality
    Implements PL020 - comprehensive supplier information
    """
    
    def __init__(self, db: Session, current_user: Optional[User] = None):
        self.db = db
        self.current_user = current_user
        self.supplier_handler = SupplierFileHandler(db)
        self.system_handler = SystemFileHandler(db)
        self.open_items_handler = PurchaseOpenItemsHandler(db)
        
    def get_supplier_summary(self, supplier_no: str) -> Dict:
        """Get comprehensive supplier summary"""
        # Get supplier master
        supplier, status = self.supplier_handler.process(4, key_value=supplier_no)
        if status.fs_reply != "00":
            return {"error": "Supplier not found"}
            
        # Get open items summary
        open_items = self._get_open_items_summary(supplier_no)
        
        # Get payment history
        payment_history = self._get_payment_history(supplier_no)
        
        # Get purchase analysis
        purchase_analysis = self._get_purchase_analysis(supplier_no)
        
        # Get aging summary
        aging = self._get_aging_summary(supplier_no)
        
        # Calculate key metrics
        metrics = self._calculate_supplier_metrics(supplier, open_items)
        
        # Get compliance status
        from app.services.pl.supplier_master import SupplierMasterService
        master_service = SupplierMasterService(self.db, self.current_user)
        compliance = master_service.check_supplier_compliance(supplier_no)
        rating = master_service.calculate_supplier_rating(supplier_no)
        
        return {
            'supplier': {
                'number': supplier.purch_supp,
                'name': supplier.purch_name,
                'status': {
                    'active': supplier.purch_active == 'Y',
                    'on_stop': supplier.purch_on_stop == 'Y',
                    'stop_date': supplier.purch_stop_date if supplier.purch_on_stop == 'Y' else None,
                    'stop_reason': supplier.purch_stop_reason if supplier.purch_on_stop == 'Y' else None
                },
                'contact': {
                    'address': f"{supplier.purch_add1} {supplier.purch_add2}".strip(),
                    'city': supplier.purch_add3,
                    'postcode': supplier.purch_postcode,
                    'phone': supplier.purch_telno,
                    'email': supplier.purch_email,
                    'contact_name': supplier.purch_contact
                },
                'financial': {
                    'balance': float(supplier.purch_balance),
                    'credit_limit': float(supplier.purch_credit_lim),
                    'available_credit': float(supplier.purch_credit_lim - supplier.purch_balance - supplier.purch_os_orders),
                    'open_orders': float(supplier.purch_os_orders),
                    'payment_terms': supplier.purch_payment_cd,
                    'payment_method': supplier.purch_payment_method,
                    'currency': supplier.purch_currency
                }
            },
            'open_items': open_items,
            'aging': aging,
            'payment_history': payment_history,
            'purchase_analysis': purchase_analysis,
            'metrics': metrics,
            'compliance': compliance,
            'rating': rating
        }
        
    def get_supplier_transactions(self, supplier_no: str, filters: Optional[Dict] = None) -> List[Dict]:
        """Get supplier transaction history"""
        filters = filters or {}
        
        # Build query for open items
        query = self.db.query(PurchaseOpenItemRec).filter(
            PurchaseOpenItemRec.purch_oi_supp == supplier_no
        )
        
        # Apply filters
        if filters.get('date_from'):
            query = query.filter(PurchaseOpenItemRec.purch_oi_date >= filters['date_from'])
        if filters.get('date_to'):
            query = query.filter(PurchaseOpenItemRec.purch_oi_date <= filters['date_to'])
        if filters.get('type'):
            query = query.filter(PurchaseOpenItemRec.purch_oi_type == filters['type'])
        if filters.get('unpaid_only'):
            query = query.filter(PurchaseOpenItemRec.purch_oi_amount != 0)
            
        # Get transactions
        transactions = query.order_by(PurchaseOpenItemRec.purch_oi_date.desc()).all()
        
        # Format transactions
        trans_list = []
        for trans in transactions:
            trans_dict = {
                'date': trans.purch_oi_date,
                'type': trans.purch_oi_type,
                'type_desc': self._get_transaction_type_desc(trans.purch_oi_type),
                'reference': trans.purch_oi_our_ref,
                'their_ref': trans.purch_oi_their_ref,
                'amount': float(trans.purch_oi_gross),
                'tax': float(trans.purch_oi_tax),
                'balance': float(trans.purch_oi_amount),
                'due_date': trans.purch_oi_due_date,
                'days_overdue': self._calculate_days_overdue(trans.purch_oi_due_date) if trans.purch_oi_amount != 0 else 0,
                'period': trans.purch_oi_period,
                'status': 'PAID' if trans.purch_oi_amount == 0 else 'OPEN',
                'approved': trans.purch_oi_approved == 'Y',
                'disputed': trans.purch_oi_disputed == 'Y'
            }
            
            # Add allocations if partially paid
            if trans.purch_oi_gross != trans.purch_oi_amount:
                trans_dict['allocations'] = self._get_transaction_allocations(trans)
                
            trans_list.append(trans_dict)
            
        return trans_list
        
    def get_supplier_statement(self, supplier_no: str, options: Optional[Dict] = None) -> Dict:
        """Generate supplier statement data"""
        options = options or {}
        
        # Get supplier
        supplier, status = self.supplier_handler.process(4, key_value=supplier_no)
        if status.fs_reply != "00":
            return {"error": "Supplier not found"}
            
        # Get all open items
        open_items = self.db.query(PurchaseOpenItemRec).filter(
            and_(
                PurchaseOpenItemRec.purch_oi_supp == supplier.purch_supp,
                PurchaseOpenItemRec.purch_oi_amount != 0
            )
        ).order_by(PurchaseOpenItemRec.purch_oi_date).all()
        
        # Group by age
        statement_lines = []
        total_due = Decimal('0')
        
        for item in open_items:
            days_overdue = self._calculate_days_overdue(item.purch_oi_due_date)
            
            statement_lines.append({
                'date': item.purch_oi_date,
                'type': item.purch_oi_type,
                'reference': item.purch_oi_our_ref,
                'their_ref': item.purch_oi_their_ref,
                'amount': float(item.purch_oi_amount),
                'due_date': item.purch_oi_due_date,
                'days_overdue': days_overdue,
                'age_bucket': self._get_age_bucket(days_overdue),
                'approved': item.purch_oi_approved == 'Y'
            })
            
            total_due += item.purch_oi_amount
            
        return {
            'supplier': {
                'number': supplier.purch_supp,
                'name': supplier.purch_name,
                'address': [
                    supplier.purch_add1,
                    supplier.purch_add2,
                    supplier.purch_add3,
                    supplier.purch_add4,
                    supplier.purch_postcode
                ]
            },
            'statement_date': datetime.now().strftime("%Y-%m-%d"),
            'items': statement_lines,
            'total_due': float(total_due),
            'aging': self._get_aging_summary(supplier.purch_supp)
        }
        
    def get_purchase_history(self, supplier_no: str, months: int = 12) -> Dict:
        """Get purchase history and trends"""
        # Calculate date range
        end_date = datetime.now().date()
        start_date = end_date - timedelta(days=months * 30)
        
        # Get monthly purchases
        monthly_purchases = self.db.query(
            func.extract('year', func.to_date(PurchaseHistoryRec.hist_date.cast(String), 'YYYYMMDD')).label('year'),
            func.extract('month', func.to_date(PurchaseHistoryRec.hist_date.cast(String), 'YYYYMMDD')).label('month'),
            func.sum(PurchaseHistoryRec.hist_amount).label('purchases'),
            func.sum(PurchaseHistoryRec.hist_discount).label('discounts'),
            func.count(PurchaseHistoryRec.hist_id).label('count')
        ).filter(
            and_(
                PurchaseHistoryRec.hist_supplier == supplier_no,
                PurchaseHistoryRec.hist_type == 'INV',
                PurchaseHistoryRec.hist_date >= int(start_date.strftime("%Y%m%d"))
            )
        ).group_by('year', 'month').all()
        
        # Get product analysis
        product_purchases = self.db.query(
            PurchaseHistoryRec.hist_product,
            func.sum(PurchaseHistoryRec.hist_quantity).label('quantity'),
            func.sum(PurchaseHistoryRec.hist_amount).label('amount')
        ).filter(
            and_(
                PurchaseHistoryRec.hist_supplier == supplier_no,
                PurchaseHistoryRec.hist_type == 'INV',
                PurchaseHistoryRec.hist_date >= int(start_date.strftime("%Y%m%d"))
            )
        ).group_by(PurchaseHistoryRec.hist_product).order_by(func.sum(PurchaseHistoryRec.hist_amount).desc()).limit(10).all()
        
        # Calculate trends
        purchase_trend = self._calculate_purchase_trend(monthly_purchases)
        
        return {
            'period': f"{start_date.strftime('%Y-%m')} to {end_date.strftime('%Y-%m')}",
            'monthly_purchases': [
                {
                    'year': int(m.year),
                    'month': int(m.month),
                    'purchases': float(m.purchases),
                    'discounts': float(m.discounts),
                    'net': float(m.purchases - m.discounts),
                    'invoice_count': m.count
                }
                for m in monthly_purchases
            ],
            'top_products': [
                {
                    'product': p.hist_product,
                    'quantity': float(p.quantity),
                    'amount': float(p.amount)
                }
                for p in product_purchases
            ],
            'trends': purchase_trend,
            'totals': {
                'purchases': sum(float(m.purchases) for m in monthly_purchases),
                'discounts': sum(float(m.discounts) for m in monthly_purchases),
                'invoices': sum(m.count for m in monthly_purchases)
            }
        }
        
    def get_payment_performance(self, supplier_no: str) -> Dict:
        """Analyze supplier payment performance"""
        supplier, _ = self.supplier_handler.process(4, key_value=supplier_no)
        
        # Get paid invoices from last 12 months
        end_date = datetime.now().date()
        start_date = end_date - timedelta(days=365)
        
        # This would query payment history
        # Simplified for now
        performance = {
            'average_days': supplier.purch_average_days,
            'on_time_pct': 100 - (supplier.purch_average_days / 30 * 100) if supplier.purch_average_days > 0 else 100,
            'early_payment_discount_taken': 0,  # Would calculate from history
            'late_payments': 0,  # Count of late payments
            'disputes': 0  # Count of disputed invoices
        }
        
        # Payment method analysis
        payment_methods = {
            'CHQ': 'Cheque',
            'BACS': 'BACS',
            'DD': 'Direct Debit',
            'CC': 'Credit Card',
            'CASH': 'Cash'
        }
        
        performance['preferred_method'] = payment_methods.get(supplier.purch_payment_method, 'Other')
        
        return performance
        
    def get_supplier_documents(self, supplier_no: str) -> Dict:
        """Get supplier compliance documents"""
        # This would query document management system
        # For now, return structure
        documents = {
            'insurance': {
                'status': 'Valid',
                'expiry_date': None,
                'document_ref': None
            },
            'vat_certificate': {
                'status': 'Valid',
                'vat_number': None,
                'verified_date': None
            },
            'bank_verification': {
                'status': 'Pending',
                'verified_date': None
            },
            'quality_certification': {
                'status': 'Not Required',
                'expiry_date': None
            }
        }
        
        supplier, _ = self.supplier_handler.process(4, key_value=supplier_no)
        if supplier:
            documents['vat_certificate']['vat_number'] = supplier.purch_vat_no
            
        return documents
        
    def get_order_summary(self, supplier_no: str) -> Dict:
        """Get purchase order summary"""
        # This would query purchase order tables
        # For now, return summary structure
        supplier, _ = self.supplier_handler.process(4, key_value=supplier_no)
        
        return {
            'open_orders': {
                'count': 0,
                'value': float(supplier.purch_os_orders) if supplier else 0
            },
            'completed_orders_mtd': {
                'count': 0,
                'value': 0
            },
            'average_order_value': 0,
            'average_lead_time': supplier.purch_lead_time if supplier else 0,
            'on_time_delivery_pct': 85  # Would calculate from order history
        }
        
    def _get_open_items_summary(self, supplier_no: str) -> Dict:
        """Get summary of open items"""
        open_items = self.db.query(
            PurchaseOpenItemRec.purch_oi_type,
            func.count(PurchaseOpenItemRec.purch_oi_id).label('count'),
            func.sum(PurchaseOpenItemRec.purch_oi_amount).label('total')
        ).filter(
            and_(
                PurchaseOpenItemRec.purch_oi_supp == supplier_no,
                PurchaseOpenItemRec.purch_oi_amount != 0
            )
        ).group_by(PurchaseOpenItemRec.purch_oi_type).all()
        
        summary = {
            'total_open': 0,
            'by_type': {},
            'unapproved': 0,
            'disputed': 0
        }
        
        for item in open_items:
            summary['by_type'][item.purch_oi_type] = {
                'count': item.count,
                'amount': float(item.total)
            }
            summary['total_open'] += float(item.total)
            
        # Count unapproved and disputed
        unapproved = self.db.query(func.count(PurchaseOpenItemRec.purch_oi_id)).filter(
            and_(
                PurchaseOpenItemRec.purch_oi_supp == supplier_no,
                PurchaseOpenItemRec.purch_oi_type == 'INV',
                PurchaseOpenItemRec.purch_oi_approved != 'Y',
                PurchaseOpenItemRec.purch_oi_amount != 0
            )
        ).scalar()
        
        disputed = self.db.query(func.count(PurchaseOpenItemRec.purch_oi_id)).filter(
            and_(
                PurchaseOpenItemRec.purch_oi_supp == supplier_no,
                PurchaseOpenItemRec.purch_oi_disputed == 'Y',
                PurchaseOpenItemRec.purch_oi_amount != 0
            )
        ).scalar()
        
        summary['unapproved'] = unapproved or 0
        summary['disputed'] = disputed or 0
            
        return summary
        
    def _get_payment_history(self, supplier_no: str) -> List[Dict]:
        """Get recent payment history"""
        # Get last 10 payments
        payments = self.db.query(PurchaseOpenItemRec).filter(
            and_(
                PurchaseOpenItemRec.purch_oi_supp == supplier_no,
                PurchaseOpenItemRec.purch_oi_type == 'PAY'
            )
        ).order_by(PurchaseOpenItemRec.purch_oi_date.desc()).limit(10).all()
        
        return [
            {
                'date': p.purch_oi_date,
                'amount': float(abs(p.purch_oi_gross)),
                'reference': p.purch_oi_our_ref,
                'method': p.purch_oi_pay_method
            }
            for p in payments
        ]
        
    def _get_purchase_analysis(self, supplier_no: str) -> Dict:
        """Get purchase analysis for supplier"""
        supplier, _ = self.supplier_handler.process(4, key_value=supplier_no)
        
        # YTD comparison
        ytd_growth = 0
        if supplier.purch_last_yr_purch > 0:
            ytd_growth = ((supplier.purch_ytd_purch - supplier.purch_last_yr_purch) / 
                         supplier.purch_last_yr_purch * 100)
            
        return {
            'ytd_purchases': float(supplier.purch_ytd_purch),
            'ytd_returns': float(supplier.purch_ytd_returns),
            'last_year_purchases': float(supplier.purch_last_yr_purch),
            'ytd_growth': float(ytd_growth),
            'highest_balance': float(supplier.purch_highest_bal),
            'average_days': supplier.purch_average_days,
            'invoice_count': supplier.purch_invoice_cnt
        }
        
    def _get_aging_summary(self, supplier_no: str) -> Dict:
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
                func.sum(PurchaseOpenItemRec.purch_oi_amount)
            ).filter(
                and_(
                    PurchaseOpenItemRec.purch_oi_supp == supplier_no,
                    PurchaseOpenItemRec.purch_oi_type == 'INV',
                    PurchaseOpenItemRec.purch_oi_amount != 0,
                    PurchaseOpenItemRec.purch_oi_due_date >= date_from,
                    PurchaseOpenItemRec.purch_oi_due_date < date_to
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
        
    def _calculate_supplier_metrics(self, supplier: PurchaseLedgerRec, open_items: Dict) -> Dict:
        """Calculate key supplier metrics"""
        # DPO (Days Payable Outstanding)
        dpo = 0
        if supplier.purch_ytd_purch > 0:
            daily_purchases = supplier.purch_ytd_purch / 365
            if daily_purchases > 0:
                dpo = float(supplier.purch_balance / daily_purchases)
                
        # Payment performance
        on_time_pct = 100 - (supplier.purch_average_days / 30 * 100) if supplier.purch_average_days > 0 else 100
        
        # Credit utilization
        credit_util = 0
        if supplier.purch_credit_lim > 0:
            credit_util = float(supplier.purch_balance / supplier.purch_credit_lim * 100)
            
        return {
            'dpo': dpo,
            'payment_performance': float(on_time_pct),
            'credit_utilization': credit_util,
            'approval_pending': open_items.get('unapproved', 0),
            'disputes_active': open_items.get('disputed', 0)
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
        
    def _get_transaction_allocations(self, transaction: PurchaseOpenItemRec) -> List[Dict]:
        """Get allocations for partially paid transaction"""
        # This would query allocation records
        # For now, return calculated allocation
        paid = float(transaction.purch_oi_gross - transaction.purch_oi_amount)
        
        return [{
            'date': transaction.purch_oi_last_pay,
            'amount': paid,
            'type': 'Payment'
        }]
        
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
            
    def _calculate_purchase_trend(self, monthly_purchases: List) -> Dict:
        """Calculate purchase trend analysis"""
        if len(monthly_purchases) < 2:
            return {'trend': 'INSUFFICIENT_DATA'}
            
        # Simple linear regression for trend
        x_values = list(range(len(monthly_purchases)))
        y_values = [float(m.purchases) for m in monthly_purchases]
        
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
        avg_purchases = sum_y / n if n > 0 else 0
        trend_pct = (slope / avg_purchases * 100) if avg_purchases > 0 else 0
        
        if trend_pct > 5:
            trend = 'INCREASING'
        elif trend_pct < -5:
            trend = 'DECREASING'
        else:
            trend = 'STABLE'
            
        return {
            'trend': trend,
            'trend_pct': float(trend_pct),
            'average_monthly': float(avg_purchases)
        }