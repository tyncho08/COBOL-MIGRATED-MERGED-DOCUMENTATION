"""
ACAS System Configuration Schemas
Pydantic models for system configuration API
"""
from pydantic import BaseModel, Field, field_validator
from typing import Optional, Annotated
from decimal import Decimal
from datetime import datetime

class SystemConfigBase(BaseModel):
    """
    Base system configuration schema
    
    Contains core system settings that can be updated
    """
    # Company Information
    company_name: str = Field(..., max_length=40, description="Company name")
    company_address_1: str = Field("", max_length=40, description="Address line 1")
    company_address_2: str = Field("", max_length=40, description="Address line 2")
    company_address_3: str = Field("", max_length=40, description="Address line 3")
    company_address_4: str = Field("", max_length=40, description="Address line 4")
    company_address_5: str = Field("", max_length=40, description="Address line 5 (postcode)")
    
    # System Parameters
    current_period: int = Field(..., ge=1, le=13, description="Current accounting period (1-13)")
    year_end_date: int = Field(..., ge=19000101, le=29991231, description="Year end date (YYYYMMDD)")
    base_currency: str = Field("GBP", max_length=3, description="Base currency code (ISO 4217)")
    
    # VAT/Tax Configuration
    vat_rate_1: Annotated[Decimal, Field(ge=0, le=100)] = Decimal('0.000')
    vat_rate_2: Annotated[Decimal, Field(ge=0, le=100)] = Decimal('0.000')
    vat_rate_3: Annotated[Decimal, Field(ge=0, le=100)] = Decimal('0.000')
    vat_code_1: str = Field("", max_length=6, description="Primary VAT code")
    vat_code_2: str = Field("", max_length=6, description="Secondary VAT code")
    vat_code_3: str = Field("", max_length=6, description="Tertiary VAT code")
    
    # Module Activation
    gl_active: bool = Field(True, description="General Ledger module active")
    sl_active: bool = Field(True, description="Sales Ledger module active")
    pl_active: bool = Field(True, description="Purchase Ledger module active")
    stock_active: bool = Field(True, description="Stock Control module active")
    irs_active: bool = Field(False, description="IRS module active")
    
    # System Settings
    audit_trail: bool = Field(True, description="Audit trail enabled")
    backup_retention_days: int = Field(90, ge=1, description="Backup retention period in days")
    auto_post_gl: bool = Field(True, description="Auto-post transactions to GL")
    period_locked: bool = Field(False, description="Current period locked")
    
    @field_validator('base_currency')
    @classmethod
    def validate_currency_code(cls, v):
        """Validate currency code format"""
        if len(v) != 3 or not v.isupper():
            raise ValueError('Currency code must be 3 uppercase letters')
        return v
    
    @field_validator('year_end_date')
    @classmethod
    def validate_year_end_date(cls, v):
        """Validate year end date format"""
        date_str = str(v).zfill(8)
        try:
            year = int(date_str[:4])
            month = int(date_str[4:6])
            day = int(date_str[6:8])
            
            if month < 1 or month > 12:
                raise ValueError('Invalid month in year end date')
            if day < 1 or day > 31:
                raise ValueError('Invalid day in year end date')
                
        except ValueError as e:
            raise ValueError(f'Invalid year end date format: {str(e)}')
        
        return v
    
    model_config = {
        "json_schema_extra": {
            "example": {
                "company_name": "ACAS Demo Company Ltd",
                "company_address_1": "123 Business Street",
                "company_address_2": "Industrial Estate",
                "company_address_3": "Business Town",
                "company_address_4": "County",
                "company_address_5": "BT1 2CD",
                "current_period": 1,
                "year_end_date": 20241231,
                "base_currency": "GBP",
                "vat_rate_1": "20.000",
                "vat_code_1": "VSTD",
                "gl_active": True,
                "sl_active": True,
                "pl_active": True,
                "stock_active": True,
                "irs_active": False,
                "audit_trail": True,
                "backup_retention_days": 90,
                "auto_post_gl": True,
                "period_locked": False
            }
        }
    }

class SystemConfigCreate(SystemConfigBase):
    """
    System configuration creation schema
    
    Used for initializing system configuration
    """
    version: str = Field("3.02", max_length=20, description="System version")

class SystemConfigUpdate(BaseModel):
    """
    System configuration update schema
    
    All fields optional for partial updates
    """
    # Company Information
    company_name: Optional[str] = Field(None, max_length=40, description="Company name")
    company_address_1: Optional[str] = Field(None, max_length=40, description="Address line 1")
    company_address_2: Optional[str] = Field(None, max_length=40, description="Address line 2")
    company_address_3: Optional[str] = Field(None, max_length=40, description="Address line 3")
    company_address_4: Optional[str] = Field(None, max_length=40, description="Address line 4")
    company_address_5: Optional[str] = Field(None, max_length=40, description="Address line 5")
    
    # System Parameters
    current_period: Optional[int] = Field(None, ge=1, le=13, description="Current accounting period")
    year_end_date: Optional[int] = Field(None, ge=19000101, le=29991231, description="Year end date")
    base_currency: Optional[str] = Field(None, max_length=3, description="Base currency code")
    
    # VAT Configuration
    vat_rate_1: Optional[Annotated[Decimal, Field(ge=0, le=100)]] = None
    vat_rate_2: Optional[Annotated[Decimal, Field(ge=0, le=100)]] = None
    vat_rate_3: Optional[Annotated[Decimal, Field(ge=0, le=100)]] = None
    vat_code_1: Optional[str] = Field(None, max_length=6)
    vat_code_2: Optional[str] = Field(None, max_length=6)
    vat_code_3: Optional[str] = Field(None, max_length=6)
    
    # Module Settings
    gl_active: Optional[bool] = None
    sl_active: Optional[bool] = None
    pl_active: Optional[bool] = None
    stock_active: Optional[bool] = None
    irs_active: Optional[bool] = None
    
    # System Settings
    audit_trail: Optional[bool] = None
    backup_retention_days: Optional[int] = Field(None, ge=1)
    auto_post_gl: Optional[bool] = None
    period_locked: Optional[bool] = None

class SystemConfigResponse(SystemConfigBase):
    """
    System configuration response schema
    
    Includes read-only fields and system status
    """
    system_rec_key: int = Field(..., description="System record key (always 1)")
    
    # Control Totals (read-only)
    gl_total_dr: Annotated[Decimal, Field(description="GL total debits")]
    gl_total_cr: Annotated[Decimal, Field(description="GL total credits")]
    sl_total_balance: Annotated[Decimal, Field(description="Sales ledger total balance")]
    pl_total_balance: Annotated[Decimal, Field(description="Purchase ledger total balance")]
    stock_total_value: Annotated[Decimal, Field(description="Stock total value")]
    
    # Version Information
    version: str = Field(..., description="System version")
    last_update: datetime = Field(..., description="Last update timestamp")
    created_at: datetime = Field(..., description="Creation timestamp")
    
    # Calculated Properties
    gl_in_balance: bool = Field(..., description="Whether GL debits equal credits")
    is_period_open: bool = Field(..., description="Whether current period is open")
    
    model_config = {
        "from_attributes": True,
        "json_schema_extra": {
            "example": {
                "system_rec_key": 1,
                "company_name": "ACAS Demo Company Ltd",
                "company_address_1": "123 Business Street",
                "company_address_2": "Industrial Estate",
                "company_address_3": "Business Town",
                "company_address_4": "County",
                "company_address_5": "BT1 2CD",
                "current_period": 1,
                "year_end_date": 20241231,
                "base_currency": "GBP",
                "vat_rate_1": "20.000",
                "vat_rate_2": "5.000",
                "vat_rate_3": "0.000",
                "vat_code_1": "VSTD",
                "vat_code_2": "VRED",
                "vat_code_3": "VZRO",
                "gl_active": True,
                "sl_active": True,
                "pl_active": True,
                "stock_active": True,
                "irs_active": False,
                "audit_trail": True,
                "backup_retention_days": 90,
                "auto_post_gl": True,
                "period_locked": False,
                "gl_total_dr": "125000.00",
                "gl_total_cr": "125000.00",
                "sl_total_balance": "45000.00",
                "pl_total_balance": "23000.00",
                "stock_total_value": "78000.00",
                "version": "3.02",
                "last_update": "2024-12-01T14:30:00Z",
                "created_at": "2024-01-01T00:00:00Z",
                "gl_in_balance": True,
                "is_period_open": True
            }
        }
    }

class SystemStatus(BaseModel):
    """
    System status information
    
    Provides operational status and health checks
    """
    system_healthy: bool = Field(..., description="Overall system health")
    database_connected: bool = Field(..., description="Database connection status")
    gl_balanced: bool = Field(..., description="General Ledger in balance")
    period_open: bool = Field(..., description="Current period open for posting")
    modules_active: dict = Field(..., description="Active module status")
    last_backup: Optional[datetime] = Field(None, description="Last backup timestamp")
    system_load: dict = Field(..., description="System performance metrics")
    
    model_config = {
        "json_schema_extra": {
            "example": {
                "system_healthy": True,
                "database_connected": True,
                "gl_balanced": True,
                "period_open": True,
                "modules_active": {
                    "gl": True,
                    "sl": True,
                    "pl": True,
                    "stock": True,
                    "irs": False
                },
                "last_backup": "2024-12-01T02:00:00Z",
                "system_load": {
                    "cpu_usage": 25.5,
                    "memory_usage": 68.2,
                    "disk_usage": 45.8,
                    "active_users": 5
                }
            }
        }
    }

# System Configuration alias for backward compatibility
SystemConfig = SystemConfigBase