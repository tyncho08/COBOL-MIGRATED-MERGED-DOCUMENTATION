"""
Template Manager Service

Manages report templates, formatting, and custom layouts.
Handles template storage, retrieval, and rendering.
"""

import os
import json
from typing import Dict, List, Any, Optional
from datetime import datetime
from jinja2 import Environment, FileSystemLoader, Template
import logging

logger = logging.getLogger(__name__)


class TemplateManager:
    """Manages report templates and formatting"""
    
    def __init__(self, template_dir: str = "/app/templates/reports"):
        self.template_dir = template_dir
        self.env = None
        self._initialize_templates()
    
    def _initialize_templates(self):
        """Initialize Jinja2 template environment"""
        try:
            os.makedirs(self.template_dir, exist_ok=True)
            self.env = Environment(
                loader=FileSystemLoader(self.template_dir),
                autoescape=True
            )
            
            # Create default templates if they don't exist
            self._create_default_templates()
            
        except Exception as e:
            logger.error(f"Error initializing templates: {str(e)}")
            # Fallback to basic template environment
            self.env = Environment()
    
    def _create_default_templates(self):
        """Create default HTML templates for standard reports"""
        templates = {
            'trial_balance.html': self._get_trial_balance_template(),
            'profit_loss.html': self._get_profit_loss_template(),
            'balance_sheet.html': self._get_balance_sheet_template(),
            'customer_aging.html': self._get_aging_template('Customer'),
            'supplier_aging.html': self._get_aging_template('Supplier'),
            'stock_valuation.html': self._get_stock_valuation_template(),
            'sales_analysis.html': self._get_sales_analysis_template(),
            'purchase_analysis.html': self._get_purchase_analysis_template(),
            'base_report.html': self._get_base_template()
        }
        
        for filename, content in templates.items():
            filepath = os.path.join(self.template_dir, filename)
            if not os.path.exists(filepath):
                try:
                    with open(filepath, 'w', encoding='utf-8') as f:
                        f.write(content)
                except Exception as e:
                    logger.error(f"Error creating template {filename}: {str(e)}")
    
    def render_report(self, template_name: str, data: Dict[str, Any]) -> str:
        """
        Render report using specified template
        
        Args:
            template_name: Name of template to use
            data: Data to render in template
            
        Returns:
            Rendered HTML content
        """
        try:
            template = self.env.get_template(template_name)
            return template.render(**data)
            
        except Exception as e:
            logger.error(f"Error rendering template {template_name}: {str(e)}")
            # Return basic HTML fallback
            return self._render_fallback_template(data)
    
    def _render_fallback_template(self, data: Dict[str, Any]) -> str:
        """Render basic fallback template when main template fails"""
        html = f"""
        <!DOCTYPE html>
        <html>
        <head>
            <title>{data.get('title', 'Report')}</title>
            <style>
                body {{ font-family: Arial, sans-serif; margin: 20px; }}
                table {{ border-collapse: collapse; width: 100%; }}
                th, td {{ border: 1px solid #ddd; padding: 8px; text-align: left; }}
                th {{ background-color: #f2f2f2; }}
                .header {{ margin-bottom: 20px; }}
                .total {{ font-weight: bold; background-color: #f9f9f9; }}
            </style>
        </head>
        <body>
            <div class="header">
                <h1>{data.get('title', 'Report')}</h1>
                <p>Generated: {data.get('generated_at', datetime.now().strftime('%d/%m/%Y %H:%M:%S'))}</p>
            </div>
            <div class="content">
                <pre>{json.dumps(data, indent=2, default=str)}</pre>
            </div>
        </body>
        </html>
        """
        return html
    
    def _get_base_template(self) -> str:
        """Base HTML template for all reports"""
        return """
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>{{ title }}</title>
    <style>
        body {
            font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
            margin: 0;
            padding: 20px;
            background-color: #f5f5f5;
        }
        .report-container {
            max-width: 1200px;
            margin: 0 auto;
            background-color: white;
            padding: 30px;
            border-radius: 8px;
            box-shadow: 0 2px 10px rgba(0,0,0,0.1);
        }
        .report-header {
            border-bottom: 3px solid #3b82f6;
            padding-bottom: 20px;
            margin-bottom: 30px;
        }
        .report-title {
            font-size: 28px;
            font-weight: bold;
            color: #1f2937;
            margin: 0 0 10px 0;
        }
        .report-subtitle {
            font-size: 16px;
            color: #6b7280;
            margin: 0;
        }
        .report-meta {
            display: flex;
            justify-content: space-between;
            margin-bottom: 20px;
            font-size: 14px;
            color: #6b7280;
        }
        table {
            width: 100%;
            border-collapse: collapse;
            margin-bottom: 20px;
            font-size: 14px;
        }
        th {
            background-color: #3b82f6;
            color: white;
            padding: 12px 8px;
            text-align: left;
            font-weight: 600;
        }
        td {
            padding: 10px 8px;
            border-bottom: 1px solid #e5e7eb;
        }
        tr:nth-child(even) {
            background-color: #f9fafb;
        }
        tr:hover {
            background-color: #f3f4f6;
        }
        .number {
            text-align: right;
            font-family: 'Courier New', monospace;
        }
        .total-row {
            font-weight: bold;
            background-color: #e5e7eb !important;
            border-top: 2px solid #3b82f6;
        }
        .section-header {
            font-size: 18px;
            font-weight: bold;
            color: #1f2937;
            margin: 30px 0 15px 0;
            padding-bottom: 5px;
            border-bottom: 2px solid #e5e7eb;
        }
        .summary-grid {
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
            gap: 20px;
            margin-bottom: 30px;
        }
        .summary-card {
            background-color: #f8fafc;
            padding: 20px;
            border-radius: 6px;
            border-left: 4px solid #3b82f6;
        }
        .summary-card h3 {
            margin: 0 0 10px 0;
            font-size: 16px;
            color: #374151;
        }
        .summary-card .value {
            font-size: 24px;
            font-weight: bold;
            color: #1f2937;
        }
        @media print {
            body { background-color: white; }
            .report-container { box-shadow: none; }
        }
    </style>
</head>
<body>
    <div class="report-container">
        <div class="report-header">
            <h1 class="report-title">{{ title }}</h1>
            {% if subtitle %}<p class="report-subtitle">{{ subtitle }}</p>{% endif %}
        </div>
        
        <div class="report-meta">
            <div>
                <strong>Report ID:</strong> {{ report_id }}<br>
                <strong>Generated:</strong> {{ generated_at.strftime('%d/%m/%Y %H:%M:%S') if generated_at else 'N/A' }}
            </div>
            <div>
                {% if parameters %}
                    {% for key, value in parameters.items() %}
                        <strong>{{ key.title() }}:</strong> {{ value }}<br>
                    {% endfor %}
                {% endif %}
            </div>
        </div>
        
        {% block content %}{% endblock %}
    </div>
</body>
</html>
        """
    
    def _get_trial_balance_template(self) -> str:
        """Template for Trial Balance report"""
        return """
{% extends "base_report.html" %}
{% block content %}
    <table>
        <thead>
            <tr>
                <th>Account Code</th>
                <th>Account Name</th>
                <th>Account Type</th>
                <th class="number">Debit</th>
                <th class="number">Credit</th>
            </tr>
        </thead>
        <tbody>
            {% for row in data %}
                <tr {% if row.account_code == 'TOTAL' %}class="total-row"{% endif %}>
                    <td>{{ row.account_code }}</td>
                    <td>{{ row.account_name }}</td>
                    <td>{{ row.account_type }}</td>
                    <td class="number">
                        {% if row.debit_amount > 0 %}£{{ "{:,.2f}".format(row.debit_amount) }}{% endif %}
                    </td>
                    <td class="number">
                        {% if row.credit_amount > 0 %}£{{ "{:,.2f}".format(row.credit_amount) }}{% endif %}
                    </td>
                </tr>
            {% endfor %}
        </tbody>
    </table>
{% endblock %}
        """
    
    def _get_profit_loss_template(self) -> str:
        """Template for Profit & Loss report"""
        return """
{% extends "base_report.html" %}
{% block content %}
    <div class="summary-grid">
        <div class="summary-card">
            <h3>Total Income</h3>
            <div class="value">£{{ "{:,.2f}".format(data.income_total) }}</div>
        </div>
        <div class="summary-card">
            <h3>Total Expenses</h3>
            <div class="value">£{{ "{:,.2f}".format(data.expense_total) }}</div>
        </div>
        <div class="summary-card" style="border-left-color: {% if data.net_profit >= 0 %}#10b981{% else %}#ef4444{% endif %}">
            <h3>Net Profit</h3>
            <div class="value" style="color: {% if data.net_profit >= 0 %}#10b981{% else %}#ef4444{% endif %}">
                £{{ "{:,.2f}".format(data.net_profit) }}
            </div>
        </div>
    </div>

    <h2 class="section-header">Income</h2>
    <table>
        <thead>
            <tr>
                <th>Account Code</th>
                <th>Account Name</th>
                <th class="number">Amount</th>
            </tr>
        </thead>
        <tbody>
            {% for item in data.income %}
                <tr>
                    <td>{{ item.account_code }}</td>
                    <td>{{ item.account_name }}</td>
                    <td class="number">£{{ "{:,.2f}".format(item.amount) }}</td>
                </tr>
            {% endfor %}
            <tr class="total-row">
                <td colspan="2"><strong>Total Income</strong></td>
                <td class="number"><strong>£{{ "{:,.2f}".format(data.income_total) }}</strong></td>
            </tr>
        </tbody>
    </table>

    <h2 class="section-header">Expenses</h2>
    <table>
        <thead>
            <tr>
                <th>Account Code</th>
                <th>Account Name</th>
                <th class="number">Amount</th>
            </tr>
        </thead>
        <tbody>
            {% for item in data.expenses %}
                <tr>
                    <td>{{ item.account_code }}</td>
                    <td>{{ item.account_name }}</td>
                    <td class="number">£{{ "{:,.2f}".format(item.amount) }}</td>
                </tr>
            {% endfor %}
            <tr class="total-row">
                <td colspan="2"><strong>Total Expenses</strong></td>
                <td class="number"><strong>£{{ "{:,.2f}".format(data.expense_total) }}</strong></td>
            </tr>
        </tbody>
    </table>
{% endblock %}
        """
    
    def _get_balance_sheet_template(self) -> str:
        """Template for Balance Sheet report"""
        return """
{% extends "base_report.html" %}
{% block content %}
    <div style="display: grid; grid-template-columns: 1fr 1fr; gap: 40px;">
        <div>
            <h2 class="section-header">Assets</h2>
            <table>
                <thead>
                    <tr>
                        <th>Account Code</th>
                        <th>Account Name</th>
                        <th class="number">Amount</th>
                    </tr>
                </thead>
                <tbody>
                    {% for asset in data.assets %}
                        <tr>
                            <td>{{ asset.account_code }}</td>
                            <td>{{ asset.account_name }}</td>
                            <td class="number">£{{ "{:,.2f}".format(asset.amount) }}</td>
                        </tr>
                    {% endfor %}
                    <tr class="total-row">
                        <td colspan="2"><strong>Total Assets</strong></td>
                        <td class="number"><strong>£{{ "{:,.2f}".format(data.total_assets) }}</strong></td>
                    </tr>
                </tbody>
            </table>
        </div>
        
        <div>
            <h2 class="section-header">Liabilities</h2>
            <table>
                <thead>
                    <tr>
                        <th>Account Code</th>
                        <th>Account Name</th>
                        <th class="number">Amount</th>
                    </tr>
                </thead>
                <tbody>
                    {% for liability in data.liabilities %}
                        <tr>
                            <td>{{ liability.account_code }}</td>
                            <td>{{ liability.account_name }}</td>
                            <td class="number">£{{ "{:,.2f}".format(liability.amount) }}</td>
                        </tr>
                    {% endfor %}
                    <tr class="total-row">
                        <td colspan="2"><strong>Total Liabilities</strong></td>
                        <td class="number"><strong>£{{ "{:,.2f}".format(data.total_liabilities) }}</strong></td>
                    </tr>
                </tbody>
            </table>
            
            <h2 class="section-header">Equity</h2>
            <table>
                <thead>
                    <tr>
                        <th>Account Code</th>
                        <th>Account Name</th>
                        <th class="number">Amount</th>
                    </tr>
                </thead>
                <tbody>
                    {% for equity in data.equity %}
                        <tr>
                            <td>{{ equity.account_code }}</td>
                            <td>{{ equity.account_name }}</td>
                            <td class="number">£{{ "{:,.2f}".format(equity.amount) }}</td>
                        </tr>
                    {% endfor %}
                    <tr class="total-row">
                        <td colspan="2"><strong>Total Equity</strong></td>
                        <td class="number"><strong>£{{ "{:,.2f}".format(data.total_equity) }}</strong></td>
                    </tr>
                </tbody>
            </table>
        </div>
    </div>
{% endblock %}
        """
    
    def _get_aging_template(self, entity_type: str) -> str:
        """Template for aging reports (Customer/Supplier)"""
        return f"""
{{% extends "base_report.html" %}}
{{% block content %}}
    <table>
        <thead>
            <tr>
                <th>{entity_type} Code</th>
                <th>{entity_type} Name</th>
                <th class="number">Current</th>
                <th class="number">31-60 Days</th>
                <th class="number">61-90 Days</th>
                <th class="number">Over 90 Days</th>
                <th class="number">Total Outstanding</th>
            </tr>
        </thead>
        <tbody>
            {{% for row in data %}}
                <tr>
                    <td>{{{{ row.{entity_type.lower()}_code }}}}</td>
                    <td>{{{{ row.{entity_type.lower()}_name }}}}</td>
                    <td class="number">£{{{{ "{{:,.2f}}".format(row.current) }}}}</td>
                    <td class="number">£{{{{ "{{:,.2f}}".format(row.days_31_60) }}}}</td>
                    <td class="number">£{{{{ "{{:,.2f}}".format(row.days_61_90) }}}}</td>
                    <td class="number">£{{{{ "{{:,.2f}}".format(row.days_over_90) }}}}</td>
                    <td class="number">£{{{{ "{{:,.2f}}".format(row.total_outstanding) }}}}</td>
                </tr>
            {{% endfor %}}
        </tbody>
    </table>
{{% endblock %}}
        """
    
    def _get_stock_valuation_template(self) -> str:
        """Template for Stock Valuation report"""
        return """
{% extends "base_report.html" %}
{% block content %}
    <table>
        <thead>
            <tr>
                <th>Item Code</th>
                <th>Item Name</th>
                <th>Category</th>
                <th>Costing Method</th>
                <th class="number">Quantity</th>
                <th class="number">Unit Cost</th>
                <th class="number">Total Cost</th>
            </tr>
        </thead>
        <tbody>
            {% for row in data %}
                <tr {% if row.item_code == 'TOTAL' %}class="total-row"{% endif %}>
                    <td>{{ row.item_code }}</td>
                    <td>{{ row.item_name }}</td>
                    <td>{{ row.item_category }}</td>
                    <td>{{ row.costing_method }}</td>
                    <td class="number">{{ "{:,.0f}".format(row.quantity_on_hand) if row.item_code != 'TOTAL' else '' }}</td>
                    <td class="number">
                        {% if row.item_code != 'TOTAL' and row.unit_cost > 0 %}£{{ "{:,.2f}".format(row.unit_cost) }}{% endif %}
                    </td>
                    <td class="number">£{{ "{:,.2f}".format(row.total_cost) }}</td>
                </tr>
            {% endfor %}
        </tbody>
    </table>
{% endblock %}
        """
    
    def _get_sales_analysis_template(self) -> str:
        """Template for Sales Analysis report"""
        return """
{% extends "base_report.html" %}
{% block content %}
    <table>
        <thead>
            <tr>
                <th>Customer Code</th>
                <th>Customer Name</th>
                <th class="number">Invoice Count</th>
                <th class="number">Sales Total</th>
                <th class="number">Average Sale</th>
            </tr>
        </thead>
        <tbody>
            {% for row in data %}
                <tr>
                    <td>{{ row.customer_code }}</td>
                    <td>{{ row.customer_name }}</td>
                    <td class="number">{{ row.invoice_count }}</td>
                    <td class="number">£{{ "{:,.2f}".format(row.sales_total) }}</td>
                    <td class="number">£{{ "{:,.2f}".format(row.average_sale) }}</td>
                </tr>
            {% endfor %}
        </tbody>
    </table>
{% endblock %}
        """
    
    def _get_purchase_analysis_template(self) -> str:
        """Template for Purchase Analysis report"""
        return """
{% extends "base_report.html" %}
{% block content %}
    <table>
        <thead>
            <tr>
                <th>Supplier Code</th>
                <th>Supplier Name</th>
                <th class="number">Invoice Count</th>
                <th class="number">Purchase Total</th>
                <th class="number">Average Purchase</th>
            </tr>
        </thead>
        <tbody>
            {% for row in data %}
                <tr>
                    <td>{{ row.supplier_code }}</td>
                    <td>{{ row.supplier_name }}</td>
                    <td class="number">{{ row.invoice_count }}</td>
                    <td class="number">£{{ "{:,.2f}".format(row.purchase_total) }}</td>
                    <td class="number">£{{ "{:,.2f}".format(row.average_purchase) }}</td>
                </tr>
            {% endfor %}
        </tbody>
    </table>
{% endblock %}
        """