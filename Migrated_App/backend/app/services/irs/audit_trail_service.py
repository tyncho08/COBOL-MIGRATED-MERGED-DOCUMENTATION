"""
IRS075: Audit Trail Service

This service provides comprehensive audit trail functionality for the IRS module,
tracking all changes to IRS records and transactions for compliance purposes.

Based on the ACAS audit trail infrastructure (acas010, auditMT, SEC_AUDIT).
"""

from datetime import datetime, timedelta
from decimal import Decimal
from typing import List, Dict, Any, Optional
from sqlalchemy import select, and_, or_, func, desc, delete
from sqlalchemy.orm import Session
from sqlalchemy.sql import text

from app.models import IrsAuditTrailRec as IRSAuditTrail
from app.models.auth import User
from app.core.logging import get_logger

logger = get_logger(__name__)


class AuditTrailService:
    """Service for managing IRS module audit trail functionality."""
    
    def __init__(self, db: Session):
        self.db = db
    
    def log_change(
        self,
        user_id: int,
        entity_type: str,
        entity_id: str,
        action: str,
        field_name: Optional[str] = None,
        old_value: Optional[str] = None,
        new_value: Optional[str] = None,
        description: Optional[str] = None,
        ip_address: Optional[str] = None
    ) -> IRSAuditTrail:
        """
        Log a change to the audit trail.
        
        Args:
            user_id: ID of the user making the change
            entity_type: Type of entity (e.g., 'transaction', 'tax_return', 'company_config')
            entity_id: ID of the entity being changed
            action: Action performed (CREATE, UPDATE, DELETE, VIEW, EXPORT, etc.)
            field_name: Name of the field changed (for updates)
            old_value: Previous value (for updates)
            new_value: New value (for updates)
            description: Additional description of the change
            ip_address: IP address of the user
            
        Returns:
            Created audit trail record
        """
        try:
            audit_record = IRSAuditTrail(
                user_id=user_id,
                entity_type=entity_type,
                entity_id=entity_id,
                action=action,
                field_name=field_name,
                old_value=old_value,
                new_value=new_value,
                description=description,
                ip_address=ip_address,
                timestamp=datetime.utcnow()
            )
            
            self.db.add(audit_record)
            self.db.commit()
            
            logger.info(
                f"Audit trail created: {action} on {entity_type} {entity_id} by user {user_id}"
            )
            
            return audit_record
            
        except Exception as e:
            logger.error(f"Error creating audit trail record: {str(e)}")
            self.db.rollback()
            raise
    
    def log_bulk_change(
        self,
        user_id: int,
        entity_type: str,
        action: str,
        changes: List[Dict[str, Any]],
        description: Optional[str] = None,
        ip_address: Optional[str] = None
    ) -> List[IRSAuditTrail]:
        """
        Log multiple changes in a single operation.
        
        Args:
            user_id: ID of the user making the changes
            entity_type: Type of entity being changed
            action: Action performed
            changes: List of change dictionaries containing entity_id, field_name, old_value, new_value
            description: Overall description of the bulk change
            ip_address: IP address of the user
            
        Returns:
            List of created audit trail records
        """
        try:
            audit_records = []
            timestamp = datetime.utcnow()
            
            for change in changes:
                audit_record = IRSAuditTrail(
                    user_id=user_id,
                    entity_type=entity_type,
                    entity_id=change.get('entity_id'),
                    action=action,
                    field_name=change.get('field_name'),
                    old_value=change.get('old_value'),
                    new_value=change.get('new_value'),
                    description=description,
                    ip_address=ip_address,
                    timestamp=timestamp
                )
                self.db.add(audit_record)
                audit_records.append(audit_record)
            
            self.db.commit()
            
            logger.info(
                f"Bulk audit trail created: {len(audit_records)} records for {action} on {entity_type}"
            )
            
            return audit_records
            
        except Exception as e:
            logger.error(f"Error creating bulk audit trail records: {str(e)}")
            self.db.rollback()
            raise
    
    def get_entity_history(
        self,
        entity_type: str,
        entity_id: str,
        start_date: Optional[datetime] = None,
        end_date: Optional[datetime] = None,
        limit: int = 100
    ) -> List[Dict[str, Any]]:
        """
        Get the complete audit history for a specific entity.
        
        Args:
            entity_type: Type of entity
            entity_id: ID of the entity
            start_date: Optional start date filter
            end_date: Optional end date filter
            limit: Maximum number of records to return
            
        Returns:
            List of audit trail records with user information
        """
        try:
            query = (
                select(IRSAuditTrail, User)
                .join(User, IRSAuditTrail.user_id == User.id)
                .filter(
                    and_(
                        IRSAuditTrail.entity_type == entity_type,
                        IRSAuditTrail.entity_id == entity_id
                    )
                )
            )
            
            if start_date:
                query = query.filter(IRSAuditTrail.timestamp >= start_date)
            if end_date:
                query = query.filter(IRSAuditTrail.timestamp <= end_date)
            
            query = query.order_by(desc(IRSAuditTrail.timestamp)).limit(limit)
            
            results = self.db.execute(query).all()
            
            return [
                {
                    'id': audit.id,
                    'timestamp': audit.timestamp.isoformat(),
                    'user_id': audit.user_id,
                    'username': user.username,
                    'user_full_name': f"{user.first_name} {user.last_name}",
                    'action': audit.action,
                    'field_name': audit.field_name,
                    'old_value': audit.old_value,
                    'new_value': audit.new_value,
                    'description': audit.description,
                    'ip_address': audit.ip_address
                }
                for audit, user in results
            ]
            
        except Exception as e:
            logger.error(f"Error retrieving entity history: {str(e)}")
            raise
    
    def get_user_activity(
        self,
        user_id: int,
        start_date: Optional[datetime] = None,
        end_date: Optional[datetime] = None,
        entity_type: Optional[str] = None,
        limit: int = 100
    ) -> List[Dict[str, Any]]:
        """
        Get all audit trail records for a specific user.
        
        Args:
            user_id: ID of the user
            start_date: Optional start date filter
            end_date: Optional end date filter
            entity_type: Optional entity type filter
            limit: Maximum number of records to return
            
        Returns:
            List of audit trail records
        """
        try:
            query = (
                select(IRSAuditTrail)
                .filter(IRSAuditTrail.user_id == user_id)
            )
            
            if start_date:
                query = query.filter(IRSAuditTrail.timestamp >= start_date)
            if end_date:
                query = query.filter(IRSAuditTrail.timestamp <= end_date)
            if entity_type:
                query = query.filter(IRSAuditTrail.entity_type == entity_type)
            
            query = query.order_by(desc(IRSAuditTrail.timestamp)).limit(limit)
            
            results = self.db.execute(query).scalars().all()
            
            return [
                {
                    'id': audit.id,
                    'timestamp': audit.timestamp.isoformat(),
                    'entity_type': audit.entity_type,
                    'entity_id': audit.entity_id,
                    'action': audit.action,
                    'field_name': audit.field_name,
                    'old_value': audit.old_value,
                    'new_value': audit.new_value,
                    'description': audit.description,
                    'ip_address': audit.ip_address
                }
                for audit in results
            ]
            
        except Exception as e:
            logger.error(f"Error retrieving user activity: {str(e)}")
            raise
    
    def search_audit_trail(
        self,
        search_criteria: Dict[str, Any],
        start_date: Optional[datetime] = None,
        end_date: Optional[datetime] = None,
        limit: int = 100
    ) -> List[Dict[str, Any]]:
        """
        Search audit trail records based on various criteria.
        
        Args:
            search_criteria: Dictionary of search parameters
            start_date: Optional start date filter
            end_date: Optional end date filter
            limit: Maximum number of records to return
            
        Returns:
            List of matching audit trail records
        """
        try:
            query = select(IRSAuditTrail, User).join(User, IRSAuditTrail.user_id == User.id)
            
            # Apply search filters
            filters = []
            
            if search_criteria.get('entity_type'):
                filters.append(IRSAuditTrail.entity_type == search_criteria['entity_type'])
            
            if search_criteria.get('action'):
                filters.append(IRSAuditTrail.action == search_criteria['action'])
            
            if search_criteria.get('user_id'):
                filters.append(IRSAuditTrail.user_id == search_criteria['user_id'])
            
            if search_criteria.get('entity_id'):
                filters.append(IRSAuditTrail.entity_id == search_criteria['entity_id'])
            
            if search_criteria.get('ip_address'):
                filters.append(IRSAuditTrail.ip_address == search_criteria['ip_address'])
            
            if search_criteria.get('field_name'):
                filters.append(IRSAuditTrail.field_name.like(f"%{search_criteria['field_name']}%"))
            
            if search_criteria.get('description'):
                filters.append(IRSAuditTrail.description.like(f"%{search_criteria['description']}%"))
            
            if start_date:
                filters.append(IRSAuditTrail.timestamp >= start_date)
            if end_date:
                filters.append(IRSAuditTrail.timestamp <= end_date)
            
            if filters:
                query = query.filter(and_(*filters))
            
            query = query.order_by(desc(IRSAuditTrail.timestamp)).limit(limit)
            
            results = self.db.execute(query).all()
            
            return [
                {
                    'id': audit.id,
                    'timestamp': audit.timestamp.isoformat(),
                    'user_id': audit.user_id,
                    'username': user.username,
                    'user_full_name': f"{user.first_name} {user.last_name}",
                    'entity_type': audit.entity_type,
                    'entity_id': audit.entity_id,
                    'action': audit.action,
                    'field_name': audit.field_name,
                    'old_value': audit.old_value,
                    'new_value': audit.new_value,
                    'description': audit.description,
                    'ip_address': audit.ip_address
                }
                for audit, user in results
            ]
            
        except Exception as e:
            logger.error(f"Error searching audit trail: {str(e)}")
            raise
    
    def generate_compliance_report(
        self,
        start_date: datetime,
        end_date: datetime,
        report_type: str = 'summary'
    ) -> Dict[str, Any]:
        """
        Generate compliance reports from audit trail data.
        
        Args:
            start_date: Report start date
            end_date: Report end date
            report_type: Type of report ('summary', 'detailed', 'user_activity', 'entity_changes')
            
        Returns:
            Dictionary containing report data
        """
        try:
            if report_type == 'summary':
                # Summary statistics
                total_changes = self.db.execute(
                    select(func.count(IRSAuditTrail.id))
                    .filter(
                        and_(
                            IRSAuditTrail.timestamp >= start_date,
                            IRSAuditTrail.timestamp <= end_date
                        )
                    )
                ).scalar()
                
                # Changes by action type
                changes_by_action = self.db.execute(
                    select(
                        IRSAuditTrail.action,
                        func.count(IRSAuditTrail.id).label('count')
                    )
                    .filter(
                        and_(
                            IRSAuditTrail.timestamp >= start_date,
                            IRSAuditTrail.timestamp <= end_date
                        )
                    )
                    .group_by(IRSAuditTrail.action)
                ).all()
                
                # Changes by entity type
                changes_by_entity = self.db.execute(
                    select(
                        IRSAuditTrail.entity_type,
                        func.count(IRSAuditTrail.id).label('count')
                    )
                    .filter(
                        and_(
                            IRSAuditTrail.timestamp >= start_date,
                            IRSAuditTrail.timestamp <= end_date
                        )
                    )
                    .group_by(IRSAuditTrail.entity_type)
                ).all()
                
                # Most active users
                active_users = self.db.execute(
                    select(
                        User.username,
                        func.count(IRSAuditTrail.id).label('count')
                    )
                    .join(User, IRSAuditTrail.user_id == User.id)
                    .filter(
                        and_(
                            IRSAuditTrail.timestamp >= start_date,
                            IRSAuditTrail.timestamp <= end_date
                        )
                    )
                    .group_by(User.username)
                    .order_by(desc('count'))
                    .limit(10)
                ).all()
                
                return {
                    'report_type': 'summary',
                    'period': {
                        'start': start_date.isoformat(),
                        'end': end_date.isoformat()
                    },
                    'total_changes': total_changes,
                    'changes_by_action': [
                        {'action': action, 'count': count}
                        for action, count in changes_by_action
                    ],
                    'changes_by_entity': [
                        {'entity_type': entity, 'count': count}
                        for entity, count in changes_by_entity
                    ],
                    'most_active_users': [
                        {'username': username, 'count': count}
                        for username, count in active_users
                    ]
                }
            
            elif report_type == 'user_activity':
                # Detailed user activity report
                user_activity_data = self.db.execute(
                    text("""
                        SELECT 
                            u.username,
                            u.first_name,
                            u.last_name,
                            COUNT(DISTINCT DATE(a.timestamp)) as active_days,
                            COUNT(a.id) as total_actions,
                            COUNT(DISTINCT a.entity_type) as entity_types_accessed,
                            COUNT(DISTINCT a.entity_id) as unique_entities_accessed,
                            MAX(a.timestamp) as last_activity
                        FROM irs_audit_trail a
                        JOIN users u ON a.user_id = u.id
                        WHERE a.timestamp >= :start_date 
                        AND a.timestamp <= :end_date
                        GROUP BY u.id, u.username, u.first_name, u.last_name
                        ORDER BY total_actions DESC
                    """),
                    {'start_date': start_date, 'end_date': end_date}
                ).all()
                
                return {
                    'report_type': 'user_activity',
                    'period': {
                        'start': start_date.isoformat(),
                        'end': end_date.isoformat()
                    },
                    'user_activity': [
                        {
                            'username': row.username,
                            'full_name': f"{row.first_name} {row.last_name}",
                            'active_days': row.active_days,
                            'total_actions': row.total_actions,
                            'entity_types_accessed': row.entity_types_accessed,
                            'unique_entities_accessed': row.unique_entities_accessed,
                            'last_activity': row.last_activity.isoformat() if row.last_activity else None
                        }
                        for row in user_activity_data
                    ]
                }
            
            else:
                raise ValueError(f"Unknown report type: {report_type}")
            
        except Exception as e:
            logger.error(f"Error generating compliance report: {str(e)}")
            raise
    
    def cleanup_old_records(self, retention_days: int = 2555) -> int:
        """
        Clean up old audit trail records based on retention policy.
        Default retention is 7 years (2555 days) for IRS compliance.
        
        Args:
            retention_days: Number of days to retain audit records
            
        Returns:
            Number of records deleted
        """
        try:
            cutoff_date = datetime.utcnow() - timedelta(days=retention_days)
            
            # Count records to be deleted
            count = self.db.execute(
                select(func.count(IRSAuditTrail.id))
                .filter(IRSAuditTrail.timestamp < cutoff_date)
            ).scalar()
            
            if count > 0:
                # Archive records before deletion (implement archiving if needed)
                logger.info(f"Archiving {count} audit trail records older than {cutoff_date}")
                
                # Delete old records
                self.db.execute(
                    delete(IRSAuditTrail)
                    .where(IRSAuditTrail.timestamp < cutoff_date)
                )
                self.db.commit()
                
                logger.info(f"Deleted {count} old audit trail records")
            
            return count
            
        except Exception as e:
            logger.error(f"Error cleaning up audit trail records: {str(e)}")
            self.db.rollback()
            raise