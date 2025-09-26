#!/bin/bash

# ACAS Automated Backup Script
# Handles database backups, configuration backups, and cleanup

set -euo pipefail

# Configuration
BACKUP_DIR="/backups"
RETENTION_DAYS="${BACKUP_RETENTION_DAYS:-30}"
TIMESTAMP=$(date '+%Y%m%d_%H%M%S')
LOG_FILE="/backups/backup.log"

# Database configuration
DB_HOST="${PGHOST:-postgres}"
DB_NAME="${PGDATABASE:-acas}"
DB_USER="${PGUSER:-acas_user}"
DB_PASSWORD="${PGPASSWORD:-acas_password}"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# Logging function
log() {
    local level=$1
    shift
    local message="$@"
    local timestamp=$(date '+%Y-%m-%d %H:%M:%S')
    echo -e "${timestamp} [${level}] ${message}" | tee -a "$LOG_FILE"
}

info() { log "INFO" "${BLUE}$@${NC}"; }
warn() { log "WARN" "${YELLOW}$@${NC}"; }
error() { log "ERROR" "${RED}$@${NC}"; }
success() { log "SUCCESS" "${GREEN}$@${NC}"; }

# Check if required commands are available
check_prerequisites() {
    local required_commands=("pg_dump" "gzip" "find")
    for cmd in "${required_commands[@]}"; do
        if ! command -v "$cmd" &> /dev/null; then
            error "Required command '$cmd' not found"
            exit 1
        fi
    done
}

# Create backup directory if it doesn't exist
ensure_backup_directory() {
    if [ ! -d "$BACKUP_DIR" ]; then
        info "Creating backup directory: $BACKUP_DIR"
        mkdir -p "$BACKUP_DIR"
    fi
    
    # Ensure log file exists
    touch "$LOG_FILE"
}

# Test database connectivity
test_database_connection() {
    info "Testing database connectivity..."
    
    if ! pg_isready -h "$DB_HOST" -p 5432 -U "$DB_USER" -d "$DB_NAME" >/dev/null 2>&1; then
        error "Cannot connect to database. Please check database status."
        exit 1
    fi
    
    success "Database connection successful"
}

# Create database backup
backup_database() {
    info "Starting database backup..."
    
    local backup_file="$BACKUP_DIR/database_${TIMESTAMP}.sql"
    local compressed_file="${backup_file}.gz"
    
    # Create database dump
    if pg_dump -h "$DB_HOST" -p 5432 -U "$DB_USER" -d "$DB_NAME" \
        --verbose --clean --no-owner --no-privileges \
        --format=plain > "$backup_file" 2>/dev/null; then
        
        # Compress the backup
        if gzip "$backup_file"; then
            local file_size=$(du -h "$compressed_file" | cut -f1)
            success "Database backup created: $compressed_file (${file_size})"
            
            # Verify backup integrity
            if gunzip -t "$compressed_file" 2>/dev/null; then
                success "Backup integrity verified"
            else
                error "Backup integrity check failed"
                return 1
            fi
        else
            error "Failed to compress backup file"
            return 1
        fi
    else
        error "Database backup failed"
        return 1
    fi
}

# Create schema-only backup
backup_schema() {
    info "Creating schema-only backup..."
    
    local schema_file="$BACKUP_DIR/schema_${TIMESTAMP}.sql"
    local compressed_file="${schema_file}.gz"
    
    if pg_dump -h "$DB_HOST" -p 5432 -U "$DB_USER" -d "$DB_NAME" \
        --schema-only --verbose --clean --no-owner --no-privileges \
        --format=plain > "$schema_file" 2>/dev/null; then
        
        if gzip "$schema_file"; then
            local file_size=$(du -h "$compressed_file" | cut -f1)
            success "Schema backup created: $compressed_file (${file_size})"
        else
            error "Failed to compress schema file"
            return 1
        fi
    else
        error "Schema backup failed"
        return 1
    fi
}

# Create data-only backup
backup_data() {
    info "Creating data-only backup..."
    
    local data_file="$BACKUP_DIR/data_${TIMESTAMP}.sql"
    local compressed_file="${data_file}.gz"
    
    if pg_dump -h "$DB_HOST" -p 5432 -U "$DB_USER" -d "$DB_NAME" \
        --data-only --verbose --no-owner --no-privileges \
        --format=plain > "$data_file" 2>/dev/null; then
        
        if gzip "$data_file"; then
            local file_size=$(du -h "$compressed_file" | cut -f1)
            success "Data backup created: $compressed_file (${file_size})"
        else
            error "Failed to compress data file"
            return 1
        fi
    else
        error "Data backup failed"
        return 1
    fi
}

# Create configuration backup
backup_configuration() {
    info "Creating configuration backup..."
    
    local config_file="$BACKUP_DIR/config_${TIMESTAMP}.tar.gz"
    local temp_dir="/tmp/acas_config_backup"
    
    # Create temporary directory
    mkdir -p "$temp_dir"
    
    # Copy configuration files if they exist
    local config_files=(
        "/opt/acas/.env"
        "/opt/acas/docker-compose.yml"
        "/etc/nginx/sites-available/acas"
        "/etc/systemd/system/acas.service"
    )
    
    local copied_files=0
    for file in "${config_files[@]}"; do
        if [ -f "$file" ]; then
            local dest_dir="$temp_dir$(dirname "$file")"
            mkdir -p "$dest_dir"
            cp "$file" "$dest_dir/"
            copied_files=$((copied_files + 1))
        fi
    done
    
    if [ $copied_files -gt 0 ]; then
        # Create compressed archive
        if tar -czf "$config_file" -C "$temp_dir" .; then
            local file_size=$(du -h "$config_file" | cut -f1)
            success "Configuration backup created: $config_file (${file_size})"
        else
            error "Failed to create configuration backup"
        fi
    else
        warn "No configuration files found to backup"
    fi
    
    # Cleanup temporary directory
    rm -rf "$temp_dir"
}

# Clean up old backups
cleanup_old_backups() {
    info "Cleaning up backups older than $RETENTION_DAYS days..."
    
    local deleted_count=0
    
    # Find and delete old backup files
    while IFS= read -r -d '' file; do
        rm -f "$file"
        deleted_count=$((deleted_count + 1))
    done < <(find "$BACKUP_DIR" -name "*.sql.gz" -o -name "*.tar.gz" -type f -mtime +$RETENTION_DAYS -print0 2>/dev/null)
    
    if [ $deleted_count -gt 0 ]; then
        success "Cleaned up $deleted_count old backup files"
    else
        info "No old backup files to clean up"
    fi
    
    # Clean up old log entries (keep last 1000 lines)
    if [ -f "$LOG_FILE" ]; then
        tail -n 1000 "$LOG_FILE" > "${LOG_FILE}.tmp" && mv "${LOG_FILE}.tmp" "$LOG_FILE"
    fi
}

# Generate backup report
generate_backup_report() {
    info "Generating backup report..."
    
    local report_file="$BACKUP_DIR/backup_report_${TIMESTAMP}.txt"
    
    cat > "$report_file" << EOF
ACAS Backup Report
==================
Date: $(date)
Backup Directory: $BACKUP_DIR
Retention Policy: $RETENTION_DAYS days

Current Backups:
$(ls -lh "$BACKUP_DIR"/*.gz 2>/dev/null | tail -10 || echo "No backup files found")

Disk Usage:
$(df -h "$BACKUP_DIR")

Recent Backup Activity:
$(tail -20 "$LOG_FILE" 2>/dev/null || echo "No log entries found")
EOF
    
    success "Backup report generated: $report_file"
}

# Send backup notification (if configured)
send_notification() {
    local status=$1
    local message=$2
    
    # Email notification (requires mail command)
    if command -v mail &> /dev/null && [ -n "${BACKUP_EMAIL:-}" ]; then
        local subject="ACAS Backup $status - $(date)"
        echo "$message" | mail -s "$subject" "$BACKUP_EMAIL"
        info "Email notification sent to $BACKUP_EMAIL"
    fi
    
    # Slack notification (requires curl and webhook URL)
    if command -v curl &> /dev/null && [ -n "${SLACK_WEBHOOK_URL:-}" ]; then
        local color="good"
        if [ "$status" = "FAILED" ]; then
            color="danger"
        elif [ "$status" = "WARNING" ]; then
            color="warning"
        fi
        
        local payload=$(cat << EOF
{
    "attachments": [
        {
            "color": "$color",
            "title": "ACAS Backup $status",
            "text": "$message",
            "timestamp": $(date +%s)
        }
    ]
}
EOF
)
        
        curl -X POST -H 'Content-type: application/json' \
            --data "$payload" \
            "$SLACK_WEBHOOK_URL" >/dev/null 2>&1
        info "Slack notification sent"
    fi
}

# Main backup function
perform_backup() {
    local start_time=$(date +%s)
    local errors=0
    
    info "Starting ACAS backup process..."
    
    # Check prerequisites
    check_prerequisites
    ensure_backup_directory
    test_database_connection
    
    # Perform backups
    if ! backup_database; then
        errors=$((errors + 1))
    fi
    
    if ! backup_schema; then
        errors=$((errors + 1))
    fi
    
    if ! backup_data; then
        errors=$((errors + 1))
    fi
    
    backup_configuration
    cleanup_old_backups
    generate_backup_report
    
    local end_time=$(date +%s)
    local duration=$((end_time - start_time))
    
    if [ $errors -eq 0 ]; then
        local message="✅ ACAS backup completed successfully in ${duration} seconds"
        success "$message"
        send_notification "SUCCESS" "$message"
    else
        local message="❌ ACAS backup completed with $errors errors in ${duration} seconds"
        error "$message"
        send_notification "FAILED" "$message"
        exit 1
    fi
}

# Script usage
usage() {
    echo "Usage: $0 [OPTIONS]"
    echo ""
    echo "Options:"
    echo "  backup          Perform full backup (default)"
    echo "  cleanup         Clean up old backups only"
    echo "  test            Test database connectivity"
    echo "  report          Generate backup report"
    echo "  --help, -h      Show this help message"
    echo ""
    echo "Environment variables:"
    echo "  BACKUP_RETENTION_DAYS  Number of days to keep backups (default: 30)"
    echo "  BACKUP_EMAIL          Email address for notifications"
    echo "  SLACK_WEBHOOK_URL     Slack webhook URL for notifications"
}

# Main script logic
main() {
    case "${1:-backup}" in
        "backup")
            perform_backup
            ;;
        "cleanup")
            ensure_backup_directory
            cleanup_old_backups
            ;;
        "test")
            test_database_connection
            ;;
        "report")
            ensure_backup_directory
            generate_backup_report
            ;;
        "--help"|"-h"|"help")
            usage
            ;;
        *)
            error "Unknown command: $1"
            usage
            exit 1
            ;;
    esac
}

# Execute main function
main "$@"