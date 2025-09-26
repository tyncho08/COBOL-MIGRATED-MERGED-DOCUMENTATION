"""Reports API Module"""

from fastapi import APIRouter
from .reports_summary import router as reports_summary_router

router = APIRouter()
router.include_router(reports_summary_router, tags=["reports"])