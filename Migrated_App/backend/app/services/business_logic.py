"""
ACAS Business Logic Services
Exact implementation of COBOL business calculations and rules
"""
from decimal import Decimal, ROUND_HALF_UP
from typing import List, Dict, Tuple, Optional
from enum import Enum
from dataclasses import dataclass
import logging

logger = logging.getLogger(__name__)

# Constants matching COBOL system
TAX_CALCULATION_PRECISION = 2
CURRENCY_PRECISION = 2
QUANTITY_PRECISION = 3
RATE_PRECISION = 4

class CostingMethod(Enum):
    """Stock costing methods"""
    AVERAGE = "A"
    FIFO = "F"  
    LIFO = "L"
    STANDARD = "S"

class AccountStatus(Enum):
    """Account status codes"""
    ACTIVE = "A"
    HOLD = "H"
    CLOSED = "C"

class CreditRating(Enum):
    """Credit rating codes"""
    EXCELLENT = "A"
    GOOD = "B" 
    FAIR = "C"
    POOR = "D"

@dataclass
class TaxCalculationResult:
    """Result of tax calculation"""
    net_amount: Decimal
    tax_amount: Decimal
    gross_amount: Decimal
    tax_rate: Decimal
    tax_code: str

@dataclass
class DiscountCalculationResult:
    """Result of discount calculation"""
    original_amount: Decimal
    trade_discount_amount: Decimal
    volume_discount_amount: Decimal
    promotional_discount_amount: Decimal
    settlement_discount_amount: Decimal
    total_discount_amount: Decimal
    final_amount: Decimal

@dataclass
class CreditCheckResult:
    """Result of credit limit check"""
    customer_code: str
    credit_limit: Decimal
    current_balance: Decimal
    open_orders_total: Decimal
    available_credit: Decimal
    credit_approved: bool
    approval_required: bool
    hold_reason: Optional[str]

@dataclass
class StockCostLayer:
    """FIFO/LIFO cost layer"""
    date: int  # YYYYMMDD
    quantity: Decimal
    unit_cost: Decimal
    remaining_quantity: Decimal

class TaxCalculationService:
    """
    Tax calculation service implementing exact COBOL logic
    
    Supports standard and reverse tax calculations with proper rounding
    """
    
    @staticmethod
    def calculate_standard_tax(net_amount: Decimal, tax_rate: Decimal) -> TaxCalculationResult:
        """
        Standard tax calculation: Tax Amount = (Net Amount × Tax Rate) / 100
        
        Args:
            net_amount: Net amount before tax
            tax_rate: Tax rate percentage (e.g., 20.000 for 20%)
            
        Returns:
            TaxCalculationResult with calculated amounts
        """
        # Ensure proper decimal precision
        net_amount = net_amount.quantize(Decimal('0.01'), rounding=ROUND_HALF_UP)
        tax_rate = tax_rate.quantize(Decimal('0.001'), rounding=ROUND_HALF_UP)
        
        # Calculate tax amount with exact COBOL formula
        tax_amount = (net_amount * tax_rate / Decimal('100')).quantize(
            Decimal('0.01'), rounding=ROUND_HALF_UP
        )
        
        # Calculate gross amount
        gross_amount = net_amount + tax_amount
        
        logger.debug(f"Standard tax calculation: net={net_amount}, rate={tax_rate}, tax={tax_amount}, gross={gross_amount}")
        
        return TaxCalculationResult(
            net_amount=net_amount,
            tax_amount=tax_amount,
            gross_amount=gross_amount,
            tax_rate=tax_rate,
            tax_code="CALC"
        )
    
    @staticmethod
    def calculate_reverse_tax(gross_amount: Decimal, tax_rate: Decimal) -> TaxCalculationResult:
        """
        Reverse tax calculation: Net Amount = Gross Amount / (1 + (Tax Rate / 100))
        
        Args:
            gross_amount: Gross amount including tax
            tax_rate: Tax rate percentage
            
        Returns:
            TaxCalculationResult with calculated amounts
        """
        # Ensure proper decimal precision
        gross_amount = gross_amount.quantize(Decimal('0.01'), rounding=ROUND_HALF_UP)
        tax_rate = tax_rate.quantize(Decimal('0.001'), rounding=ROUND_HALF_UP)
        
        # Calculate divisor
        divisor = Decimal('1') + (tax_rate / Decimal('100'))
        
        # Calculate net amount
        net_amount = (gross_amount / divisor).quantize(
            Decimal('0.01'), rounding=ROUND_HALF_UP
        )
        
        # Calculate tax amount as difference
        tax_amount = gross_amount - net_amount
        
        logger.debug(f"Reverse tax calculation: gross={gross_amount}, rate={tax_rate}, net={net_amount}, tax={tax_amount}")
        
        return TaxCalculationResult(
            net_amount=net_amount,
            tax_amount=tax_amount,
            gross_amount=gross_amount,
            tax_rate=tax_rate,
            tax_code="CALC"
        )
    
    @staticmethod
    def get_tax_rate_by_code(tax_code: str, system_config: dict) -> Decimal:
        """
        Get tax rate by tax code from system configuration
        
        Args:
            tax_code: Tax code to lookup
            system_config: System configuration dictionary
            
        Returns:
            Tax rate as Decimal
        """
        tax_rates = {
            system_config.get('vat_code_1', ''): system_config.get('vat_rate_1', Decimal('0.000')),
            system_config.get('vat_code_2', ''): system_config.get('vat_rate_2', Decimal('0.000')),
            system_config.get('vat_code_3', ''): system_config.get('vat_rate_3', Decimal('0.000')),
        }
        
        return Decimal(str(tax_rates.get(tax_code, '0.000')))

class DiscountCalculationService:
    """
    Discount calculation service implementing exact COBOL discount hierarchy
    
    Applies discounts in the correct order:
    1. Trade Discount (customer-specific)
    2. Volume Discount (quantity-based)
    3. Promotional Discount (time-limited)
    4. Settlement Discount (payment terms)
    """
    
    @staticmethod
    def calculate_discounts(
        list_price: Decimal,
        quantity: Decimal,
        trade_discount_rate: Decimal = Decimal('0.00'),
        volume_discount_rate: Decimal = Decimal('0.00'),
        promotional_discount_rate: Decimal = Decimal('0.00'),
        settlement_discount_rate: Decimal = Decimal('0.00')
    ) -> DiscountCalculationResult:
        """
        Calculate all discounts in hierarchy order
        
        Args:
            list_price: Original list price per unit
            quantity: Quantity being purchased
            trade_discount_rate: Trade discount percentage
            volume_discount_rate: Volume discount percentage
            promotional_discount_rate: Promotional discount percentage
            settlement_discount_rate: Settlement discount percentage
            
        Returns:
            DiscountCalculationResult with all calculations
        """
        # Calculate original line amount
        original_amount = (list_price * quantity).quantize(
            Decimal('0.01'), rounding=ROUND_HALF_UP
        )
        
        # Step 1: Apply trade discount
        trade_discount_amount = (original_amount * trade_discount_rate / Decimal('100')).quantize(
            Decimal('0.01'), rounding=ROUND_HALF_UP
        )
        after_trade = original_amount - trade_discount_amount
        
        # Step 2: Apply volume discount to amount after trade discount
        volume_discount_amount = (after_trade * volume_discount_rate / Decimal('100')).quantize(
            Decimal('0.01'), rounding=ROUND_HALF_UP
        )
        after_volume = after_trade - volume_discount_amount
        
        # Step 3: Apply promotional discount to amount after volume discount
        promotional_discount_amount = (after_volume * promotional_discount_rate / Decimal('100')).quantize(
            Decimal('0.01'), rounding=ROUND_HALF_UP
        )
        after_promotional = after_volume - promotional_discount_amount
        
        # Step 4: Settlement discount is applied at payment time, not here
        # But we calculate what it would be for information
        settlement_discount_amount = (after_promotional * settlement_discount_rate / Decimal('100')).quantize(
            Decimal('0.01'), rounding=ROUND_HALF_UP
        )
        
        # Total discount amount (excluding settlement which is applied at payment)
        total_discount_amount = trade_discount_amount + volume_discount_amount + promotional_discount_amount
        
        # Final amount (before settlement discount)
        final_amount = after_promotional
        
        logger.debug(f"Discount calculation: original={original_amount}, trade={trade_discount_amount}, "
                    f"volume={volume_discount_amount}, promo={promotional_discount_amount}, "
                    f"settlement={settlement_discount_amount}, final={final_amount}")
        
        return DiscountCalculationResult(
            original_amount=original_amount,
            trade_discount_amount=trade_discount_amount,
            volume_discount_amount=volume_discount_amount,
            promotional_discount_amount=promotional_discount_amount,
            settlement_discount_amount=settlement_discount_amount,
            total_discount_amount=total_discount_amount,
            final_amount=final_amount
        )

class CreditControlService:
    """
    Credit control service implementing exact COBOL credit checking logic
    """
    
    @staticmethod
    def check_credit_limit(
        customer_code: str,
        credit_limit: Decimal,
        current_balance: Decimal,
        open_orders_total: Decimal,
        new_order_amount: Decimal,
        credit_rating: str = "B",
        account_status: str = "A"
    ) -> CreditCheckResult:
        """
        Perform credit limit check with exact COBOL logic
        
        Args:
            customer_code: Customer identifier
            credit_limit: Customer credit limit
            current_balance: Current account balance
            open_orders_total: Total of unshipped orders
            new_order_amount: Amount of new order
            credit_rating: Customer credit rating (A/B/C/D)
            account_status: Account status (A/H/C)
            
        Returns:
            CreditCheckResult with approval decision
        """
        # Step 1: Check account status
        if account_status == AccountStatus.CLOSED.value:
            return CreditCheckResult(
                customer_code=customer_code,
                credit_limit=credit_limit,
                current_balance=current_balance,
                open_orders_total=open_orders_total,
                available_credit=Decimal('0.00'),
                credit_approved=False,
                approval_required=False,
                hold_reason="Account is closed"
            )
        
        if account_status == AccountStatus.HOLD.value:
            return CreditCheckResult(
                customer_code=customer_code,
                credit_limit=credit_limit,
                current_balance=current_balance,
                open_orders_total=open_orders_total,
                available_credit=Decimal('0.00'),
                credit_approved=False,
                approval_required=True,
                hold_reason="Account on hold - supervisor override required"
            )
        
        # Step 2: Calculate available credit
        available_credit = credit_limit - current_balance - open_orders_total
        
        # Step 3: Check if new order exceeds available credit
        if new_order_amount <= available_credit:
            # Order approved - within credit limit
            return CreditCheckResult(
                customer_code=customer_code,
                credit_limit=credit_limit,
                current_balance=current_balance,
                open_orders_total=open_orders_total,
                available_credit=available_credit,
                credit_approved=True,
                approval_required=False,
                hold_reason=None
            )
        
        # Step 4: Order exceeds limit - check credit rating for override rules
        if credit_rating == CreditRating.EXCELLENT.value:
            # Grade A customers get warning only
            return CreditCheckResult(
                customer_code=customer_code,
                credit_limit=credit_limit,
                current_balance=current_balance,
                open_orders_total=open_orders_total,
                available_credit=available_credit,
                credit_approved=True,
                approval_required=False,
                hold_reason="Warning: Order exceeds credit limit but approved due to excellent rating"
            )
        else:
            # Other customers require supervisor override
            excess_amount = new_order_amount - available_credit
            return CreditCheckResult(
                customer_code=customer_code,
                credit_limit=credit_limit,
                current_balance=current_balance,
                open_orders_total=open_orders_total,
                available_credit=available_credit,
                credit_approved=False,
                approval_required=True,
                hold_reason=f"Order exceeds credit limit by {excess_amount} - supervisor override required"
            )

class StockCostingService:
    """
    Stock costing service implementing all COBOL inventory valuation methods
    """
    
    def __init__(self):
        self.cost_layers: Dict[str, List[StockCostLayer]] = {}
    
    def calculate_average_cost(
        self,
        current_qty: Decimal,
        current_avg_cost: Decimal,
        receipt_qty: Decimal,
        receipt_cost: Decimal
    ) -> Decimal:
        """
        Calculate new weighted average cost after receipt
        
        Formula: New Average = ((Current Qty × Current Avg) + (Receipt Qty × Receipt Cost)) 
                              / (Current Qty + Receipt Qty)
        
        Args:
            current_qty: Current quantity on hand
            current_avg_cost: Current average cost
            receipt_qty: Quantity being received
            receipt_cost: Cost of received quantity
            
        Returns:
            New weighted average cost
        """
        if current_qty == 0:
            return receipt_cost
        
        current_value = current_qty * current_avg_cost
        receipt_value = receipt_qty * receipt_cost
        total_qty = current_qty + receipt_qty
        
        if total_qty == 0:
            return Decimal('0.0000')
        
        new_avg_cost = ((current_value + receipt_value) / total_qty).quantize(
            Decimal('0.0001'), rounding=ROUND_HALF_UP
        )
        
        logger.debug(f"Average cost calculation: current_qty={current_qty}, current_avg={current_avg_cost}, "
                    f"receipt_qty={receipt_qty}, receipt_cost={receipt_cost}, new_avg={new_avg_cost}")
        
        return new_avg_cost
    
    def add_fifo_layer(self, stock_key: str, date: int, quantity: Decimal, unit_cost: Decimal):
        """Add new FIFO cost layer"""
        if stock_key not in self.cost_layers:
            self.cost_layers[stock_key] = []
        
        layer = StockCostLayer(
            date=date,
            quantity=quantity,
            unit_cost=unit_cost,
            remaining_quantity=quantity
        )
        
        self.cost_layers[stock_key].append(layer)
        # Sort by date to maintain FIFO order
        self.cost_layers[stock_key].sort(key=lambda x: x.date)
    
    def calculate_fifo_issue_cost(self, stock_key: str, issue_quantity: Decimal) -> Tuple[Decimal, Decimal]:
        """
        Calculate FIFO issue cost and update layers
        
        Args:
            stock_key: Stock item key
            issue_quantity: Quantity being issued
            
        Returns:
            Tuple of (weighted_average_cost, total_value)
        """
        if stock_key not in self.cost_layers:
            return Decimal('0.0000'), Decimal('0.00')
        
        layers = self.cost_layers[stock_key]
        remaining_to_issue = issue_quantity
        total_cost = Decimal('0.00')
        
        for layer in layers:
            if remaining_to_issue <= 0:
                break
                
            if layer.remaining_quantity <= 0:
                continue
            
            # Take from this layer
            qty_from_layer = min(remaining_to_issue, layer.remaining_quantity)
            cost_from_layer = qty_from_layer * layer.unit_cost
            
            total_cost += cost_from_layer
            layer.remaining_quantity -= qty_from_layer
            remaining_to_issue -= qty_from_layer
        
        # Clean up empty layers
        self.cost_layers[stock_key] = [layer for layer in layers if layer.remaining_quantity > 0]
        
        # Calculate weighted average cost
        if issue_quantity > 0:
            avg_cost = (total_cost / issue_quantity).quantize(
                Decimal('0.0001'), rounding=ROUND_HALF_UP
            )
        else:
            avg_cost = Decimal('0.0000')
        
        return avg_cost, total_cost
    
    def get_current_fifo_cost(self, stock_key: str) -> Decimal:
        """Get current FIFO cost (oldest layer)"""
        if stock_key not in self.cost_layers or not self.cost_layers[stock_key]:
            return Decimal('0.0000')
        
        # Find first layer with remaining quantity
        for layer in self.cost_layers[stock_key]:
            if layer.remaining_quantity > 0:
                return layer.unit_cost
        
        return Decimal('0.0000')
    
    def calculate_inventory_value(
        self,
        stock_key: str,
        quantity_on_hand: Decimal,
        costing_method: CostingMethod,
        standard_cost: Decimal = Decimal('0.0000'),
        average_cost: Decimal = Decimal('0.0000')
    ) -> Decimal:
        """
        Calculate inventory value based on costing method
        
        Args:
            stock_key: Stock item key
            quantity_on_hand: Current quantity
            costing_method: Valuation method
            standard_cost: Standard cost (for standard costing)
            average_cost: Average cost (for average costing)
            
        Returns:
            Total inventory value
        """
        if quantity_on_hand <= 0:
            return Decimal('0.00')
        
        if costing_method == CostingMethod.STANDARD:
            unit_cost = standard_cost
        elif costing_method == CostingMethod.AVERAGE:
            unit_cost = average_cost
        elif costing_method == CostingMethod.FIFO:
            # For FIFO valuation, sum all remaining layers
            if stock_key not in self.cost_layers:
                unit_cost = average_cost  # Fallback
            else:
                total_value = sum(
                    layer.remaining_quantity * layer.unit_cost 
                    for layer in self.cost_layers[stock_key]
                    if layer.remaining_quantity > 0
                )
                return total_value.quantize(Decimal('0.01'), rounding=ROUND_HALF_UP)
        else:
            unit_cost = average_cost  # Default fallback
        
        total_value = (quantity_on_hand * unit_cost).quantize(
            Decimal('0.01'), rounding=ROUND_HALF_UP
        )
        
        return total_value