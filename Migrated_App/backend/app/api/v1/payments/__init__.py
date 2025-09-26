"""Payments API Module"""

from fastapi import APIRouter
from .payments_summary import router as payments_summary_router

router = APIRouter()
router.include_router(payments_summary_router, tags=["payments"])