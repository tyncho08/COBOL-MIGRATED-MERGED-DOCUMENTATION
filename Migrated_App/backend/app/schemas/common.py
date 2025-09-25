"""
ACAS Common Schemas
Shared Pydantic models for common API structures
"""
from pydantic import BaseModel, Field, validator
from typing import Any, Dict, List, Optional, Generic, TypeVar
from decimal import Decimal
from datetime import datetime

# Generic type for paginated responses
T = TypeVar('T')

class PaginatedResponse(BaseModel, Generic[T]):
    """
    Generic paginated response model
    
    Used for all list endpoints that support pagination
    """
    items: List[T] = Field(..., description="List of items for current page")
    total: int = Field(..., description="Total number of items across all pages")
    page: int = Field(..., description="Current page number (1-based)")
    page_size: int = Field(..., description="Number of items per page")
    total_pages: int = Field(..., description="Total number of pages")
    has_next: bool = Field(..., description="Whether there is a next page")
    has_previous: bool = Field(..., description="Whether there is a previous page")
    
    class Config:
        schema_extra = {
            "example": {
                "items": [],
                "total": 150,
                "page": 1,
                "page_size": 25,
                "total_pages": 6,
                "has_next": True,
                "has_previous": False
            }
        }

class ErrorResponse(BaseModel):
    """
    Standard error response model
    
    Used for all API error responses
    """
    error: str = Field(..., description="Error type or category")
    message: str = Field(..., description="Human-readable error message")
    detail: Optional[str] = Field(None, description="Additional error details")
    code: Optional[str] = Field(None, description="Application-specific error code")
    field_errors: Optional[Dict[str, List[str]]] = Field(
        None, 
        description="Field-specific validation errors"
    )
    
    class Config:
        schema_extra = {
            "example": {
                "error": "validation_error",
                "message": "Invalid input data provided",
                "detail": "Customer code must be exactly 7 characters",
                "code": "CUST_001",
                "field_errors": {
                    "sales_key": ["Must be exactly 7 characters"],
                    "sales_credit_limit": ["Must be a positive number"]
                }
            }
        }

class SuccessResponse(BaseModel):
    """
    Standard success response model
    
    Used for operations that don't return specific data
    """
    success: bool = Field(True, description="Operation success flag")
    message: str = Field(..., description="Success message")
    data: Optional[Dict[str, Any]] = Field(None, description="Additional response data")
    
    class Config:
        schema_extra = {
            "example": {
                "success": True,
                "message": "Customer updated successfully",
                "data": {
                    "updated_at": "2024-12-01T10:30:00Z",
                    "affected_records": 1
                }
            }
        }

class DateRange(BaseModel):
    """
    Date range filter for queries
    
    Dates in YYYYMMDD format matching COBOL convention
    """
    from_date: Optional[int] = Field(
        None, 
        description="Start date in YYYYMMDD format",
        ge=19000101,
        le=29991231
    )
    to_date: Optional[int] = Field(
        None, 
        description="End date in YYYYMMDD format",
        ge=19000101,
        le=29991231
    )
    
    @validator('to_date')
    def validate_date_range(cls, v, values):
        """Ensure to_date is not before from_date"""
        if v and values.get('from_date') and v < values['from_date']:
            raise ValueError('to_date must be greater than or equal to from_date')
        return v
    
    class Config:
        schema_extra = {
            "example": {
                "from_date": 20240101,
                "to_date": 20241231
            }
        }

class AmountRange(BaseModel):
    """
    Amount range filter for financial queries
    
    Using Decimal for precise financial calculations
    """
    min_amount: Optional[Decimal] = Field(
        None, 
        description="Minimum amount",
        ge=0,
        decimal_places=2
    )
    max_amount: Optional[Decimal] = Field(
        None, 
        description="Maximum amount",
        decimal_places=2
    )
    
    @validator('max_amount')
    def validate_amount_range(cls, v, values):
        """Ensure max_amount is not less than min_amount"""
        if v and values.get('min_amount') and v < values['min_amount']:
            raise ValueError('max_amount must be greater than or equal to min_amount')
        return v
    
    class Config:
        schema_extra = {
            "example": {
                "min_amount": "0.00",
                "max_amount": "10000.00"
            }
        }

class StatusFilter(BaseModel):
    """
    Common status filters for various entities
    """
    active_only: Optional[bool] = Field(
        None, 
        description="Filter for active records only"
    )
    status: Optional[str] = Field(
        None, 
        description="Specific status value to filter by"
    )
    exclude_status: Optional[List[str]] = Field(
        None, 
        description="Status values to exclude"
    )
    
    class Config:
        schema_extra = {
            "example": {
                "active_only": True,
                "status": "A",
                "exclude_status": ["C", "H"]
            }
        }

class SortOrder(BaseModel):
    """
    Sorting parameters for list queries
    """
    sort_by: Optional[str] = Field(
        None, 
        description="Field name to sort by"
    )
    sort_direction: Optional[str] = Field(
        "asc", 
        description="Sort direction: asc or desc"
    )
    
    @validator('sort_direction')
    def validate_sort_direction(cls, v):
        """Ensure sort_direction is valid"""
        if v.lower() not in ['asc', 'desc']:
            raise ValueError('sort_direction must be either "asc" or "desc"')
        return v.lower()
    
    class Config:
        schema_extra = {
            "example": {
                "sort_by": "sales_name",
                "sort_direction": "asc"
            }
        }

class SearchFilter(BaseModel):
    """
    Text search parameters
    """
    search_term: Optional[str] = Field(
        None, 
        description="Text to search for",
        min_length=1,
        max_length=50
    )
    search_fields: Optional[List[str]] = Field(
        None, 
        description="Fields to search in"
    )
    exact_match: Optional[bool] = Field(
        False, 
        description="Whether to perform exact match search"
    )
    
    class Config:
        schema_extra = {
            "example": {
                "search_term": "ACME",
                "search_fields": ["sales_name", "sales_key"],
                "exact_match": False
            }
        }

class BaseFilterParams(BaseModel):
    """
    Base filter parameters for list queries
    
    Combines common filtering options
    """
    page: int = Field(1, description="Page number (1-based)", ge=1)
    page_size: int = Field(25, description="Number of items per page", ge=1, le=1000)
    date_range: Optional[DateRange] = Field(None, description="Date range filter")
    amount_range: Optional[AmountRange] = Field(None, description="Amount range filter")
    status_filter: Optional[StatusFilter] = Field(None, description="Status filter")
    sort_order: Optional[SortOrder] = Field(None, description="Sort parameters")
    search_filter: Optional[SearchFilter] = Field(None, description="Search parameters")
    
    class Config:
        schema_extra = {
            "example": {
                "page": 1,
                "page_size": 25,
                "date_range": {
                    "from_date": 20240101,
                    "to_date": 20241231
                },
                "status_filter": {
                    "active_only": True
                },
                "sort_order": {
                    "sort_by": "created_at",
                    "sort_direction": "desc"
                }
            }
        }

class AuditInfo(BaseModel):
    """
    Audit information for all records
    """
    created_at: datetime = Field(..., description="Record creation timestamp")
    updated_at: Optional[datetime] = Field(None, description="Last update timestamp")
    created_by: Optional[str] = Field(None, description="User who created record")
    updated_by: Optional[str] = Field(None, description="User who last updated record")
    
    class Config:
        schema_extra = {
            "example": {
                "created_at": "2024-12-01T10:30:00Z",
                "updated_at": "2024-12-01T14:15:30Z",
                "created_by": "admin",
                "updated_by": "manager"
            }
        }

class ValidationErrorDetail(BaseModel):
    """
    Detailed validation error information
    """
    field: str = Field(..., description="Field name with error")
    message: str = Field(..., description="Error message")
    rejected_value: Any = Field(..., description="Value that was rejected")
    constraint: Optional[str] = Field(None, description="Constraint that was violated")
    
    class Config:
        schema_extra = {
            "example": {
                "field": "sales_credit_limit",
                "message": "Must be a positive number",
                "rejected_value": -1000.00,
                "constraint": "greater_than_zero"
            }
        }