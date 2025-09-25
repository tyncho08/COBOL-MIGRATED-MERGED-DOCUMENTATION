"""
Batch Log Viewer and Management Service - Phase 7 migration
Provides comprehensive batch job log viewing and management capabilities
"""
from typing import List, Optional, Dict, Tuple, Any
from datetime import datetime, timedelta
from sqlalchemy.orm import Session
from sqlalchemy import and_, or_, func, desc, text, asc
import json
import os
from pathlib import Path

from app.services.file_handlers.system_handler import SystemFileHandler
from app.models.batch import (
    BatchJobRec, BatchExecutionRec, BatchLogRec, BatchScheduleRec,
    BatchMetricsRec, BatchAlertRec
)
from app.core.security import log_user_action
from app.models.auth import User


class BatchLogViewerService:
    """
    Batch Log Viewer and Management Service
    Provides comprehensive log viewing, filtering, and analysis capabilities
    """
    
    def __init__(self, db: Session, current_user: Optional[User] = None):
        self.db = db
        self.current_user = current_user
        self.system_handler = SystemFileHandler(db)
        
    def get_execution_logs(self, log_filters: Optional[Dict] = None) -> Dict:
        """
        Get batch execution logs with filtering and pagination
        Returns formatted log data
        """
        filters = log_filters or {}
        
        try:
            # Extract filter parameters
            execution_id = filters.get('execution_id')
            job_name = filters.get('job_name')
            date_from = filters.get('date_from')
            date_to = filters.get('date_to')
            log_level = filters.get('log_level')  # INFO, WARNING, ERROR
            search_text = filters.get('search_text')
            page = filters.get('page', 1)
            page_size = filters.get('page_size', 100)
            
            # Build base query
            query = self.db.query(BatchLogRec)
            
            # Apply filters
            conditions = []
            
            if execution_id:
                conditions.append(BatchLogRec.log_execution_id == execution_id)
                
            if job_name:
                # Join with execution to filter by job name
                query = query.join(BatchExecutionRec, 
                                 BatchLogRec.log_execution_id == BatchExecutionRec.exec_id)
                conditions.append(BatchExecutionRec.exec_job_name == job_name)
                
            if date_from:
                date_from_dt = datetime.strptime(str(date_from), "%Y%m%d")
                conditions.append(BatchLogRec.log_timestamp >= date_from_dt)
                
            if date_to:
                date_to_dt = datetime.strptime(str(date_to), "%Y%m%d") + timedelta(days=1)
                conditions.append(BatchLogRec.log_timestamp < date_to_dt)
                
            if log_level:
                conditions.append(BatchLogRec.log_level == log_level)
                
            if search_text:
                conditions.append(BatchLogRec.log_message.ilike(f'%{search_text}%'))
                
            # Apply all conditions
            if conditions:
                query = query.filter(and_(*conditions))
                
            # Get total count before pagination
            total_count = query.count()
            
            # Apply pagination and ordering
            offset = (page - 1) * page_size
            logs = query.order_by(desc(BatchLogRec.log_timestamp)).offset(offset).limit(page_size).all()
            
            # Format log entries
            formatted_logs = []
            for log in logs:
                # Get execution details if not already joined
                if not job_name:
                    execution = self.db.query(BatchExecutionRec).filter(
                        BatchExecutionRec.exec_id == log.log_execution_id
                    ).first()
                else:
                    execution = log.batch_execution_rec if hasattr(log, 'batch_execution_rec') else None
                
                formatted_logs.append({
                    'log_id': log.log_id,
                    'execution_id': log.log_execution_id,
                    'job_name': execution.exec_job_name if execution else 'Unknown',
                    'timestamp': log.log_timestamp.isoformat() if log.log_timestamp else None,
                    'level': log.log_level,
                    'message': log.log_message,
                    'formatted_time': log.log_timestamp.strftime('%Y-%m-%d %H:%M:%S') if log.log_timestamp else '',
                    'severity_color': self._get_severity_color(log.log_level)
                })
                
            return {
                'logs': formatted_logs,
                'pagination': {
                    'page': page,
                    'page_size': page_size,
                    'total_count': total_count,
                    'total_pages': (total_count + page_size - 1) // page_size,
                    'has_next': page * page_size < total_count,
                    'has_prev': page > 1
                },
                'filters_applied': {
                    'execution_id': execution_id,
                    'job_name': job_name,
                    'date_from': date_from,
                    'date_to': date_to,
                    'log_level': log_level,
                    'search_text': search_text
                }
            }
            
        except Exception as e:
            return {"error": str(e)}
            
    def get_job_execution_summary(self, job_filters: Optional[Dict] = None) -> Dict:
        """
        Get summary of job executions with performance metrics
        """
        filters = job_filters or {}
        
        try:
            job_name = filters.get('job_name')
            date_from = filters.get('date_from', int((datetime.now() - timedelta(days=30)).strftime("%Y%m%d")))
            date_to = filters.get('date_to', int(datetime.now().strftime("%Y%m%d")))
            status = filters.get('status')
            
            # Build query
            query = self.db.query(BatchExecutionRec).filter(
                and_(
                    BatchExecutionRec.exec_submitted_date >= date_from,
                    BatchExecutionRec.exec_submitted_date <= date_to
                )
            )
            
            if job_name:
                query = query.filter(BatchExecutionRec.exec_job_name == job_name)
            if status:
                query = query.filter(BatchExecutionRec.exec_status == status)
                
            executions = query.order_by(desc(BatchExecutionRec.exec_submitted_date),
                                      desc(BatchExecutionRec.exec_submitted_time)).all()
            
            # Calculate summary statistics
            total_executions = len(executions)
            completed_executions = len([e for e in executions if e.exec_status == 'COMPLETED'])
            failed_executions = len([e for e in executions if e.exec_status == 'FAILED'])
            running_executions = len([e for e in executions if e.exec_status == 'RUNNING'])
            
            # Calculate average runtime
            completed_with_runtime = [
                e for e in executions 
                if e.exec_status == 'COMPLETED' and e.exec_start_date and e.exec_end_date
            ]
            
            avg_runtime = 0
            if completed_with_runtime:
                total_runtime = sum(self._calculate_runtime_minutes(e) for e in completed_with_runtime)
                avg_runtime = total_runtime / len(completed_with_runtime)
                
            # Format execution details
            execution_details = []
            for execution in executions[:50]:  # Limit to latest 50
                runtime = self._calculate_runtime_minutes(execution)
                
                # Get error count from logs
                error_count = self.db.query(BatchLogRec).filter(
                    and_(
                        BatchLogRec.log_execution_id == execution.exec_id,
                        BatchLogRec.log_level == 'ERROR'
                    )
                ).count()
                
                execution_details.append({
                    'execution_id': execution.exec_id,
                    'job_name': execution.exec_job_name,
                    'status': execution.exec_status,
                    'submitted_date': execution.exec_submitted_date,
                    'submitted_time': f"{execution.exec_submitted_time:06d}" if execution.exec_submitted_time else '',
                    'start_date': execution.exec_start_date,
                    'start_time': f"{execution.exec_start_time:06d}" if execution.exec_start_time else '',
                    'end_date': execution.exec_end_date,
                    'end_time': f"{execution.exec_end_time:06d}" if execution.exec_end_time else '',
                    'runtime_minutes': runtime,
                    'runtime_formatted': self._format_runtime(runtime),
                    'return_code': execution.exec_return_code,
                    'error_message': execution.exec_error_message,
                    'error_count': error_count,
                    'retry_count': execution.exec_retry_count,
                    'priority': execution.exec_priority,
                    'parameters': json.loads(execution.exec_parameters or '{}'),
                    'status_color': self._get_status_color(execution.exec_status)
                })
                
            return {
                'period': {
                    'date_from': date_from,
                    'date_to': date_to,
                    'job_filter': job_name,
                    'status_filter': status
                },
                'summary': {
                    'total_executions': total_executions,
                    'completed_executions': completed_executions,
                    'failed_executions': failed_executions,
                    'running_executions': running_executions,
                    'success_rate': (completed_executions / total_executions * 100) if total_executions > 0 else 0,
                    'failure_rate': (failed_executions / total_executions * 100) if total_executions > 0 else 0,
                    'average_runtime_minutes': avg_runtime,
                    'average_runtime_formatted': self._format_runtime(avg_runtime)
                },
                'executions': execution_details
            }
            
        except Exception as e:
            return {"error": str(e)}
            
    def get_system_log_analysis(self, analysis_filters: Optional[Dict] = None) -> Dict:
        """
        Analyze system logs for patterns and issues
        """
        filters = analysis_filters or {}
        
        try:
            date_from = filters.get('date_from', int((datetime.now() - timedelta(days=7)).strftime("%Y%m%d")))
            date_to = filters.get('date_to', int(datetime.now().strftime("%Y%m%d")))
            
            date_from_dt = datetime.strptime(str(date_from), "%Y%m%d")
            date_to_dt = datetime.strptime(str(date_to), "%Y%m%d") + timedelta(days=1)
            
            # Get log level distribution
            log_level_stats = self.db.execute(text("""
                SELECT 
                    log_level,
                    COUNT(*) as log_count,
                    COUNT(DISTINCT log_execution_id) as execution_count
                FROM batch_log_rec
                WHERE log_timestamp >= :date_from AND log_timestamp < :date_to
                GROUP BY log_level
                ORDER BY log_count DESC
            """), {
                'date_from': date_from_dt,
                'date_to': date_to_dt
            }).fetchall()
            
            level_distribution = {
                row.log_level: {
                    'log_count': row.log_count,
                    'execution_count': row.execution_count
                }
                for row in log_level_stats
            }
            
            # Get most common error messages
            error_patterns = self.db.execute(text("""
                SELECT 
                    log_message,
                    COUNT(*) as occurrence_count,
                    COUNT(DISTINCT log_execution_id) as affected_executions,
                    MAX(log_timestamp) as last_occurrence
                FROM batch_log_rec
                WHERE log_level = 'ERROR'
                AND log_timestamp >= :date_from AND log_timestamp < :date_to
                GROUP BY log_message
                ORDER BY occurrence_count DESC
                LIMIT 10
            """), {
                'date_from': date_from_dt,
                'date_to': date_to_dt
            }).fetchall()
            
            common_errors = [
                {
                    'message': row.log_message,
                    'occurrence_count': row.occurrence_count,
                    'affected_executions': row.affected_executions,
                    'last_occurrence': row.last_occurrence.isoformat() if row.last_occurrence else None
                }
                for row in error_patterns
            ]
            
            # Get hourly log distribution
            hourly_distribution = self.db.execute(text("""
                SELECT 
                    EXTRACT(hour FROM log_timestamp) as log_hour,
                    COUNT(*) as log_count,
                    COUNT(CASE WHEN log_level = 'ERROR' THEN 1 END) as error_count
                FROM batch_log_rec
                WHERE log_timestamp >= :date_from AND log_timestamp < :date_to
                GROUP BY EXTRACT(hour FROM log_timestamp)
                ORDER BY log_hour
            """), {
                'date_from': date_from_dt,
                'date_to': date_to_dt
            }).fetchall()
            
            hourly_stats = {
                int(row.log_hour): {
                    'total_logs': row.log_count,
                    'error_logs': row.error_count
                }
                for row in hourly_distribution
            }
            
            # Get job failure correlation
            job_error_correlation = self.db.execute(text("""
                SELECT 
                    e.exec_job_name,
                    COUNT(DISTINCT e.exec_id) as total_executions,
                    COUNT(CASE WHEN e.exec_status = 'FAILED' THEN 1 END) as failed_executions,
                    COUNT(l.log_id) as error_logs,
                    AVG(
                        CASE 
                            WHEN e.exec_end_date IS NOT NULL AND e.exec_start_date IS NOT NULL
                            THEN (e.exec_end_date - e.exec_start_date) * 1440 + 
                                 (e.exec_end_time - e.exec_start_time) / 100 * 60
                            ELSE NULL
                        END
                    ) as avg_runtime_minutes
                FROM batch_execution_rec e
                LEFT JOIN batch_log_rec l ON e.exec_id = l.log_execution_id AND l.log_level = 'ERROR'
                WHERE e.exec_submitted_date >= :date_from_int AND e.exec_submitted_date <= :date_to_int
                GROUP BY e.exec_job_name
                HAVING COUNT(DISTINCT e.exec_id) > 0
                ORDER BY failed_executions DESC, error_logs DESC
            """), {
                'date_from_int': date_from,
                'date_to_int': date_to
            }).fetchall()
            
            job_reliability = [
                {
                    'job_name': row.exec_job_name,
                    'total_executions': row.total_executions,
                    'failed_executions': row.failed_executions,
                    'failure_rate': (row.failed_executions / row.total_executions * 100) if row.total_executions > 0 else 0,
                    'error_logs': row.error_logs,
                    'avg_runtime_minutes': float(row.avg_runtime_minutes) if row.avg_runtime_minutes else 0,
                    'reliability_score': self._calculate_reliability_score(
                        row.total_executions, row.failed_executions, row.error_logs
                    )
                }
                for row in job_error_correlation
            ]
            
            return {
                'analysis_period': {
                    'date_from': date_from,
                    'date_to': date_to,
                    'days_analyzed': (date_to_dt - date_from_dt).days
                },
                'log_level_distribution': level_distribution,
                'common_errors': common_errors,
                'hourly_distribution': hourly_stats,
                'job_reliability': job_reliability,
                'recommendations': self._generate_log_analysis_recommendations(
                    level_distribution, common_errors, job_reliability
                )
            }
            
        except Exception as e:
            return {"error": str(e)}
            
    def export_logs(self, export_request: Dict) -> Tuple[bool, Optional[str]]:
        """
        Export logs to file format (CSV, JSON, TXT)
        Returns (success, file_path or error_message)
        """
        try:
            execution_id = export_request.get('execution_id')
            job_name = export_request.get('job_name')
            date_from = export_request.get('date_from')
            date_to = export_request.get('date_to')
            format_type = export_request.get('format', 'CSV').upper()
            include_system_logs = export_request.get('include_system', False)
            
            # Get logs based on filters
            log_data = self.get_execution_logs({
                'execution_id': execution_id,
                'job_name': job_name,
                'date_from': date_from,
                'date_to': date_to,
                'page_size': 10000  # Large page size for export
            })
            
            if 'error' in log_data:
                return False, log_data['error']
                
            logs = log_data['logs']
            
            if not logs:
                return False, "No logs found matching the criteria"
                
            # Generate export filename
            timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
            filename_parts = ['batch_logs', timestamp]
            
            if job_name:
                filename_parts.insert(1, job_name.replace(' ', '_'))
            if execution_id:
                filename_parts.insert(1, execution_id)
                
            filename = f"{'_'.join(filename_parts)}.{format_type.lower()}"
            
            # Create exports directory if not exists
            export_dir = Path("exports/batch_logs")
            export_dir.mkdir(parents=True, exist_ok=True)
            file_path = export_dir / filename
            
            # Export based on format
            if format_type == 'CSV':
                success = self._export_to_csv(logs, file_path)
            elif format_type == 'JSON':
                success = self._export_to_json(logs, file_path, export_request)
            elif format_type == 'TXT':
                success = self._export_to_txt(logs, file_path)
            else:
                return False, f"Unsupported format: {format_type}"
                
            if success:
                return True, str(file_path)
            else:
                return False, "Failed to create export file"
                
        except Exception as e:
            return False, str(e)
            
    def cleanup_old_logs(self, cleanup_options: Dict) -> Tuple[bool, Optional[str]]:
        """
        Clean up old batch logs to manage database size
        """
        try:
            retention_days = cleanup_options.get('retention_days', 90)
            log_levels_to_clean = cleanup_options.get('log_levels', ['INFO', 'DEBUG'])
            dry_run = cleanup_options.get('dry_run', False)
            
            cutoff_date = datetime.now() - timedelta(days=retention_days)
            
            # Build cleanup query
            cleanup_query = self.db.query(BatchLogRec).filter(
                and_(
                    BatchLogRec.log_timestamp < cutoff_date,
                    BatchLogRec.log_level.in_(log_levels_to_clean)
                )
            )
            
            # Count logs to be deleted
            logs_to_delete = cleanup_query.count()
            
            if dry_run:
                return True, {
                    'dry_run': True,
                    'logs_to_delete': logs_to_delete,
                    'cutoff_date': cutoff_date.isoformat(),
                    'retention_days': retention_days,
                    'log_levels': log_levels_to_clean
                }
                
            # Perform actual cleanup in batches
            batch_size = 1000
            deleted_count = 0
            
            while True:
                batch_logs = cleanup_query.limit(batch_size).all()
                if not batch_logs:
                    break
                    
                for log in batch_logs:
                    self.db.delete(log)
                    
                self.db.commit()
                deleted_count += len(batch_logs)
                
                # Safety check to prevent runaway deletion
                if deleted_count >= logs_to_delete:
                    break
                    
            return True, {
                'cleanup_completed': True,
                'logs_deleted': deleted_count,
                'cutoff_date': cutoff_date.isoformat(),
                'retention_days': retention_days
            }
            
        except Exception as e:
            self.db.rollback()
            return False, str(e)
            
    def _calculate_runtime_minutes(self, execution: BatchExecutionRec) -> float:
        """Calculate execution runtime in minutes"""
        if not execution.exec_start_date or not execution.exec_start_time:
            return 0.0
            
        try:
            start_dt = datetime.strptime(
                f"{execution.exec_start_date}{execution.exec_start_time:06d}",
                "%Y%m%d%H%M%S"
            )
            
            if execution.exec_end_date and execution.exec_end_time:
                end_dt = datetime.strptime(
                    f"{execution.exec_end_date}{execution.exec_end_time:06d}",
                    "%Y%m%d%H%M%S"
                )
            else:
                end_dt = datetime.now()
                
            return (end_dt - start_dt).total_seconds() / 60
            
        except:
            return 0.0
            
    def _format_runtime(self, minutes: float) -> str:
        """Format runtime in human-readable format"""
        if minutes < 1:
            return f"{int(minutes * 60)}s"
        elif minutes < 60:
            return f"{minutes:.1f}m"
        else:
            hours = int(minutes // 60)
            mins = int(minutes % 60)
            return f"{hours}h {mins}m"
            
    def _get_severity_color(self, level: str) -> str:
        """Get color code for log level"""
        colors = {
            'ERROR': '#dc3545',
            'WARNING': '#ffc107',
            'INFO': '#17a2b8',
            'DEBUG': '#6c757d'
        }
        return colors.get(level, '#6c757d')
        
    def _get_status_color(self, status: str) -> str:
        """Get color code for execution status"""
        colors = {
            'COMPLETED': '#28a745',
            'FAILED': '#dc3545',
            'RUNNING': '#007bff',
            'PENDING': '#ffc107',
            'CANCELLED': '#6c757d'
        }
        return colors.get(status, '#6c757d')
        
    def _calculate_reliability_score(self, total_executions: int, failed_executions: int, error_logs: int) -> float:
        """Calculate job reliability score (0-100)"""
        if total_executions == 0:
            return 0.0
            
        success_rate = (total_executions - failed_executions) / total_executions
        error_penalty = min(error_logs / total_executions * 0.1, 0.2)  # Max 20% penalty
        
        score = (success_rate - error_penalty) * 100
        return max(0.0, min(100.0, score))
        
    def _generate_log_analysis_recommendations(self, level_dist: Dict, common_errors: List, job_reliability: List) -> List[Dict]:
        """Generate recommendations based on log analysis"""
        recommendations = []
        
        # High error rate recommendation
        total_logs = sum(dist['log_count'] for dist in level_dist.values())
        error_logs = level_dist.get('ERROR', {}).get('log_count', 0)
        
        if total_logs > 0 and (error_logs / total_logs) > 0.1:  # More than 10% errors
            recommendations.append({
                'type': 'HIGH_ERROR_RATE',
                'priority': 'HIGH',
                'message': f'High error rate detected: {error_logs/total_logs*100:.1f}% of logs are errors',
                'action': 'Review and resolve common error patterns'
            })
            
        # Frequent error pattern recommendation
        if common_errors and common_errors[0]['occurrence_count'] > 10:
            recommendations.append({
                'type': 'FREQUENT_ERRORS',
                'priority': 'MEDIUM',
                'message': f'Frequent error pattern: "{common_errors[0]["message"][:50]}..."',
                'action': 'Investigate root cause of most common error'
            })
            
        # Unreliable jobs recommendation
        unreliable_jobs = [job for job in job_reliability if job['reliability_score'] < 80]
        if unreliable_jobs:
            recommendations.append({
                'type': 'UNRELIABLE_JOBS',
                'priority': 'MEDIUM',
                'message': f'{len(unreliable_jobs)} jobs have reliability score below 80%',
                'action': 'Review and improve reliability of failing jobs'
            })
            
        return recommendations
        
    def _export_to_csv(self, logs: List[Dict], file_path: Path) -> bool:
        """Export logs to CSV format"""
        try:
            import csv
            
            with open(file_path, 'w', newline='', encoding='utf-8') as csvfile:
                fieldnames = ['execution_id', 'job_name', 'timestamp', 'level', 'message']
                writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
                
                writer.writeheader()
                for log in logs:
                    writer.writerow({
                        'execution_id': log['execution_id'],
                        'job_name': log['job_name'],
                        'timestamp': log['formatted_time'],
                        'level': log['level'],
                        'message': log['message']
                    })
                    
            return True
            
        except Exception:
            return False
            
    def _export_to_json(self, logs: List[Dict], file_path: Path, export_request: Dict) -> bool:
        """Export logs to JSON format"""
        try:
            export_data = {
                'export_info': {
                    'generated_at': datetime.now().isoformat(),
                    'generated_by': self.current_user.username if self.current_user else 'system',
                    'filters': export_request,
                    'total_logs': len(logs)
                },
                'logs': logs
            }
            
            with open(file_path, 'w', encoding='utf-8') as jsonfile:
                json.dump(export_data, jsonfile, indent=2, default=str)
                
            return True
            
        except Exception:
            return False
            
    def _export_to_txt(self, logs: List[Dict], file_path: Path) -> bool:
        """Export logs to text format"""
        try:
            with open(file_path, 'w', encoding='utf-8') as txtfile:
                txtfile.write("ACAS Batch System Log Export\n")
                txtfile.write(f"Generated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\n")
                txtfile.write("=" * 80 + "\n\n")
                
                for log in logs:
                    txtfile.write(f"[{log['formatted_time']}] {log['level']} - {log['job_name']} ({log['execution_id']})\n")
                    txtfile.write(f"  {log['message']}\n\n")
                    
            return True
            
        except Exception:
            return False