"""
Custom Application Exceptions
"""


class ACASException(Exception):
    """Base exception for ACAS application"""
    pass


class InsufficientPermissionsError(ACASException):
    """Raised when user lacks required permissions"""
    pass


class ValidationError(ACASException):
    """Raised when data validation fails"""
    pass


class BusinessLogicError(ACASException):
    """Raised when business rules are violated"""
    pass


class IntegrationError(ACASException):
    """Raised when external system integration fails"""
    pass