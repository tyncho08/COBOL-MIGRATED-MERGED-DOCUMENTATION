#!/bin/bash

# ACAS PostgreSQL Fix Script
# Diagnoses and fixes common PostgreSQL issues on macOS

set -e

# Colors
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
RED='\033[0;31m'
NC='\033[0m'

info() { echo -e "${BLUE}ℹ️  $@${NC}"; }
warn() { echo -e "${YELLOW}⚠️  $@${NC}"; }
error() { echo -e "${RED}❌ $@${NC}"; }
success() { echo -e "${GREEN}✅ $@${NC}"; }

# Detect OS
OS="$(uname -s)"
case "${OS}" in
    Darwin*)    MACHINE=Mac;;
    Linux*)     MACHINE=Linux;;
    *)          MACHINE="UNKNOWN:${OS}"
esac

info "Detected OS: $MACHINE"

# Check if PostgreSQL is installed
check_postgres_installation() {
    info "Checking PostgreSQL installation..."
    
    if command -v psql >/dev/null 2>&1; then
        success "PostgreSQL client found: $(psql --version)"
    else
        error "PostgreSQL client not found"
        if [[ "$MACHINE" == "Mac" ]]; then
            warn "Install with: brew install postgresql@15"
        fi
        exit 1
    fi
}

# Check if PostgreSQL server is running
check_postgres_server() {
    info "Checking PostgreSQL server status..."
    
    if pg_isready -h localhost -p 5432 >/dev/null 2>&1; then
        success "PostgreSQL server is running"
    else
        warn "PostgreSQL server is not running"
        
        if [[ "$MACHINE" == "Mac" ]]; then
            info "Attempting to start PostgreSQL..."
            
            # Try different start methods
            if command -v brew >/dev/null 2>&1; then
                brew services start postgresql@15 || brew services start postgresql
            fi
            
            sleep 3
            
            if pg_isready -h localhost -p 5432 >/dev/null 2>&1; then
                success "PostgreSQL server started successfully"
            else
                error "Failed to start PostgreSQL server"
                info "Manual start options:"
                info "1. brew services start postgresql@15"
                info "2. pg_ctl -D /opt/homebrew/var/postgres start"
                info "3. pg_ctl -D /usr/local/var/postgres start"
                exit 1
            fi
        fi
    fi
}

# Test PostgreSQL connection and find correct user
test_postgres_connection() {
    info "Testing PostgreSQL connection..."
    
    local current_user=$(whoami)
    local test_users=("$current_user" "postgres" "")
    
    for user in "${test_users[@]}"; do
        local connection_cmd="psql -h localhost -p 5432"
        if [ -n "$user" ]; then
            connection_cmd="$connection_cmd -U $user"
            info "Trying user: $user"
        else
            info "Trying default connection (no user specified)"
        fi
        
        if $connection_cmd -d postgres -c "SELECT 1;" >/dev/null 2>&1; then
            success "Connection successful with user: ${user:-default}"
            export PGUSER=${user:-$current_user}
            return 0
        fi
    done
    
    warn "No working PostgreSQL user found"
    return 1
}

# Create PostgreSQL user if needed
create_postgres_user() {
    local current_user=$(whoami)
    
    info "Attempting to create PostgreSQL user: $current_user"
    
    # Try to create user with different methods
    if createuser -s "$current_user" 2>/dev/null; then
        success "Created PostgreSQL superuser: $current_user"
    elif psql -d postgres -c "CREATE USER $current_user SUPERUSER;" 2>/dev/null; then
        success "Created PostgreSQL superuser: $current_user"
    else
        warn "Could not create PostgreSQL user automatically"
        info "You may need to run manually:"
        info "  createuser -s $current_user"
        info "  or"
        info "  psql -d postgres -c \"CREATE USER $current_user SUPERUSER;\""
    fi
}

# List PostgreSQL users and databases
list_postgres_info() {
    info "PostgreSQL system information:"
    echo
    
    if psql -d postgres -c "\\du" 2>/dev/null; then
        echo
    else
        warn "Could not list PostgreSQL users"
    fi
    
    if psql -d postgres -c "\\l" 2>/dev/null; then
        echo
    else
        warn "Could not list PostgreSQL databases"
    fi
}

# Test database creation
test_database_creation() {
    info "Testing database creation..."
    
    local test_db="test_acas_$(date +%s)"
    
    if createdb "$test_db" 2>/dev/null; then
        success "Database creation test successful"
        dropdb "$test_db" 2>/dev/null
    else
        error "Database creation test failed"
        return 1
    fi
}

# Main fix process
main() {
    info "🔧 ACAS PostgreSQL Diagnostic and Fix Tool"
    echo "=================================================="
    
    check_postgres_installation
    check_postgres_server
    
    if ! test_postgres_connection; then
        warn "PostgreSQL connection failed, attempting to fix..."
        create_postgres_user
        
        if ! test_postgres_connection; then
            error "Could not establish PostgreSQL connection"
            list_postgres_info
            echo
            error "Manual intervention required:"
            error "1. Ensure PostgreSQL is properly installed and running"
            error "2. Create a PostgreSQL user with: createuser -s $(whoami)"
            error "3. Set PGUSER environment variable if needed"
            exit 1
        fi
    fi
    
    list_postgres_info
    test_database_creation
    
    echo
    success "🎉 PostgreSQL is properly configured!"
    success "User: ${PGUSER:-$(whoami)}"
    success "Server: localhost:5432"
    echo
    info "You can now run: ./run_app.sh"
}

# Execute main function
main "$@"