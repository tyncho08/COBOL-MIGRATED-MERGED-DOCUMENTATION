#!/bin/bash

# ACAS Production Deployment Script
# This script handles the deployment of ACAS to production environment

set -euo pipefail  # Exit on error, undefined vars, pipe failures

# Script configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
LOG_FILE="/var/log/acas/deploy.log"
BACKUP_DIR="/opt/acas/backups"
CONFIG_DIR="/opt/acas/config"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

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

# Cleanup function for graceful exit
cleanup() {
    if [ $? -ne 0 ]; then
        error "Deployment failed! Check logs at $LOG_FILE"
        if [ "${ROLLBACK_ON_FAILURE:-true}" = "true" ]; then
            warn "Initiating automatic rollback..."
            rollback_deployment
        fi
    fi
}
trap cleanup EXIT

# Check if running as root
check_root() {
    if [ "$EUID" -ne 0 ]; then
        error "This script must be run as root (use sudo)"
        exit 1
    fi
}

# Validate environment
validate_environment() {
    info "Validating deployment environment..."
    
    # Check required commands
    local required_commands=("docker" "docker-compose" "curl" "pg_dump" "systemctl")
    for cmd in "${required_commands[@]}"; do
        if ! command -v "$cmd" &> /dev/null; then
            error "Required command '$cmd' not found"
            exit 1
        fi
    done
    
    # Check required directories
    local required_dirs=("$BACKUP_DIR" "$CONFIG_DIR" "/opt/acas")
    for dir in "${required_dirs[@]}"; do
        if [ ! -d "$dir" ]; then
            warn "Creating missing directory: $dir"
            mkdir -p "$dir"
        fi
    done
    
    # Check environment file
    if [ ! -f "/opt/acas/.env" ]; then
        error "Environment file not found at /opt/acas/.env"
        error "Please copy .env.example to /opt/acas/.env and configure it"
        exit 1
    fi
    
    # Check disk space (minimum 2GB)
    local available_space=$(df /opt/acas | awk 'NR==2 {print $4}')
    if [ "$available_space" -lt 2097152 ]; then  # 2GB in KB
        error "Insufficient disk space. At least 2GB required."
        exit 1
    fi
    
    success "Environment validation completed"
}

# Create pre-deployment backup
create_backup() {
    info "Creating pre-deployment backup..."
    
    local backup_timestamp=$(date '+%Y%m%d_%H%M%S')
    local backup_file="$BACKUP_DIR/pre_deploy_${backup_timestamp}.sql"
    local config_backup="$BACKUP_DIR/config_${backup_timestamp}.tar.gz"
    
    # Backup database
    if docker-compose -f /opt/acas/docker-compose.yml ps postgres | grep -q "Up"; then
        info "Backing up database..."
        docker-compose -f /opt/acas/docker-compose.yml exec -T postgres pg_dump -U acas_user acas > "$backup_file"
        
        if [ -f "$backup_file" ] && [ -s "$backup_file" ]; then
            success "Database backup created: $backup_file"
        else
            error "Database backup failed"
            exit 1
        fi
    else
        warn "Database container not running, skipping database backup"
    fi
    
    # Backup configuration
    info "Backing up configuration..."
    tar -czf "$config_backup" -C /opt/acas .env docker-compose.yml 2>/dev/null || true
    
    # Store backup info for potential rollback
    echo "$backup_file" > "/tmp/acas_last_backup"
    echo "$config_backup" >> "/tmp/acas_last_backup"
    
    success "Backup completed"
}

# Pull latest images
pull_images() {
    info "Pulling latest Docker images..."
    
    cd /opt/acas
    
    # Pull images
    if ! docker-compose pull; then
        error "Failed to pull Docker images"
        exit 1
    fi
    
    success "Docker images updated"
}

# Stop services gracefully
stop_services() {
    info "Stopping ACAS services..."
    
    cd /opt/acas
    
    # Graceful shutdown with timeout
    if ! timeout 60 docker-compose down; then
        warn "Graceful shutdown timed out, forcing stop..."
        docker-compose kill
        docker-compose down
    fi
    
    # Clean up orphaned containers
    docker container prune -f
    
    success "Services stopped"
}

# Start services
start_services() {
    info "Starting ACAS services..."
    
    cd /opt/acas
    
    # Start services
    if ! docker-compose up -d; then
        error "Failed to start services"
        exit 1
    fi
    
    success "Services started"
}

# Wait for services to be healthy
wait_for_health() {
    info "Waiting for services to become healthy..."
    
    local max_attempts=30
    local attempt=1
    
    while [ $attempt -le $max_attempts ]; do
        info "Health check attempt $attempt/$max_attempts..."
        
        # Check if all services are running
        local running_services=$(docker-compose -f /opt/acas/docker-compose.yml ps --services --filter "status=running" | wc -l)
        local total_services=$(docker-compose -f /opt/acas/docker-compose.yml config --services | wc -l)
        
        if [ "$running_services" -eq "$total_services" ]; then
            # Test application endpoints
            if curl -f http://localhost:8000/health >/dev/null 2>&1; then
                success "All services are healthy"
                return 0
            fi
        fi
        
        if [ $attempt -eq $max_attempts ]; then
            error "Services failed to become healthy after $max_attempts attempts"
            return 1
        fi
        
        sleep 10
        attempt=$((attempt + 1))
    done
}

# Run database migrations
run_migrations() {
    info "Running database migrations..."
    
    cd /opt/acas
    
    # Wait for database to be ready
    docker-compose exec -T postgres pg_isready -U acas_user -d acas
    
    # Run migrations
    if docker-compose exec -T backend python -c "
from app.core.database import Base, engine
try:
    Base.metadata.create_all(bind=engine)
    print('✅ Database migrations completed successfully')
except Exception as e:
    print(f'❌ Migration failed: {e}')
    exit(1)
"; then
        success "Database migrations completed"
    else
        error "Database migration failed"
        exit 1
    fi
}

# Run post-deployment tests
run_post_deployment_tests() {
    info "Running post-deployment tests..."
    
    local test_results=()
    
    # Test API endpoints
    info "Testing API endpoints..."
    
    # Health check
    if curl -f http://localhost:8000/health >/dev/null 2>&1; then
        test_results+=("✅ Health endpoint")
    else
        test_results+=("❌ Health endpoint")
    fi
    
    # Info endpoint
    if curl -f http://localhost:8000/info >/dev/null 2>&1; then
        test_results+=("✅ Info endpoint")
    else
        test_results+=("❌ Info endpoint")
    fi
    
    # Database connectivity
    if docker-compose -f /opt/acas/docker-compose.yml exec -T postgres pg_isready -U acas_user -d acas >/dev/null 2>&1; then
        test_results+=("✅ Database connectivity")
    else
        test_results+=("❌ Database connectivity")
    fi
    
    # Redis connectivity
    if docker-compose -f /opt/acas/docker-compose.yml exec -T redis redis-cli ping | grep -q PONG; then
        test_results+=("✅ Redis connectivity")
    else
        test_results+=("❌ Redis connectivity")
    fi
    
    # Frontend accessibility
    if curl -f http://localhost:3000/ >/dev/null 2>&1; then
        test_results+=("✅ Frontend accessibility")
    else
        test_results+=("❌ Frontend accessibility")
    fi
    
    # Print test results
    info "Post-deployment test results:"
    for result in "${test_results[@]}"; do
        echo "  $result"
    done
    
    # Check if any tests failed
    local failed_tests=$(printf '%s\n' "${test_results[@]}" | grep -c "❌" || true)
    if [ "$failed_tests" -gt 0 ]; then
        error "$failed_tests post-deployment tests failed"
        return 1
    else
        success "All post-deployment tests passed"
        return 0
    fi
}

# Rollback deployment
rollback_deployment() {
    warn "Starting deployment rollback..."
    
    if [ ! -f "/tmp/acas_last_backup" ]; then
        error "No backup information found for rollback"
        return 1
    fi
    
    local backup_files=($(cat /tmp/acas_last_backup))
    local db_backup="${backup_files[0]}"
    local config_backup="${backup_files[1]}"
    
    # Stop current services
    cd /opt/acas
    docker-compose down
    
    # Restore configuration
    if [ -f "$config_backup" ]; then
        info "Restoring configuration..."
        tar -xzf "$config_backup" -C /opt/acas
    fi
    
    # Restore database
    if [ -f "$db_backup" ]; then
        info "Restoring database..."
        docker-compose up -d postgres
        sleep 30  # Wait for postgres to be ready
        docker-compose exec -T postgres psql -U acas_user -d acas < "$db_backup"
    fi
    
    # Restart services
    docker-compose up -d
    
    warn "Rollback completed. Please verify system functionality."
}

# Update system configuration
update_system_config() {
    info "Updating system configuration..."
    
    # Update systemd service if it exists
    if [ -f "/etc/systemd/system/acas.service" ]; then
        systemctl daemon-reload
        systemctl enable acas
    fi
    
    # Update logrotate configuration
    cat > /etc/logrotate.d/acas << EOF
/var/log/acas/*.log {
    daily
    missingok
    rotate 30
    compress
    delaycompress
    notifempty
    create 644 root root
    postrotate
        docker-compose -f /opt/acas/docker-compose.yml restart backend frontend
    endscript
}
EOF
    
    success "System configuration updated"
}

# Main deployment function
deploy() {
    local start_time=$(date +%s)
    
    info "Starting ACAS deployment..."
    info "Deployment started at $(date)"
    
    # Deployment steps
    check_root
    validate_environment
    create_backup
    stop_services
    pull_images
    start_services
    wait_for_health
    run_migrations
    run_post_deployment_tests
    update_system_config
    
    local end_time=$(date +%s)
    local duration=$((end_time - start_time))
    
    success "🎉 Deployment completed successfully!"
    success "Total deployment time: ${duration} seconds"
    success "ACAS is now running at:"
    success "  - Frontend: http://localhost:3000"
    success "  - Backend API: http://localhost:8000"
    success "  - Grafana: http://localhost:3001"
    success "  - Prometheus: http://localhost:9090"
    
    # Clean up old backups (keep last 10)
    find "$BACKUP_DIR" -name "pre_deploy_*.sql" -type f | sort | head -n -10 | xargs rm -f 2>/dev/null || true
}

# Script usage
usage() {
    echo "Usage: $0 [OPTIONS]"
    echo ""
    echo "Options:"
    echo "  deploy              Deploy ACAS to production"
    echo "  rollback            Rollback to previous deployment"
    echo "  status              Check deployment status"
    echo "  logs                Show deployment logs"
    echo "  --help, -h          Show this help message"
    echo ""
    echo "Environment variables:"
    echo "  ROLLBACK_ON_FAILURE  Automatically rollback on failure (default: true)"
    echo "  BACKUP_RETENTION     Number of backups to keep (default: 10)"
}

# Main script logic
main() {
    # Ensure log directory exists
    mkdir -p "$(dirname "$LOG_FILE")"
    
    case "${1:-}" in
        "deploy")
            deploy
            ;;
        "rollback")
            rollback_deployment
            ;;
        "status")
            cd /opt/acas
            docker-compose ps
            ;;
        "logs")
            tail -f "$LOG_FILE"
            ;;
        "--help"|"-h"|"help")
            usage
            ;;
        "")
            error "No command specified"
            usage
            exit 1
            ;;
        *)
            error "Unknown command: $1"
            usage
            exit 1
            ;;
    esac
}

# Execute main function with all arguments
main "$@"