#!/bin/bash

# ACAS Log Management Utility
# Helper script for viewing and managing application logs

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
LOGS_DIR="$PROJECT_ROOT/logs"

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

# Check if logs directory exists
check_logs_dir() {
    if [ ! -d "$LOGS_DIR" ]; then
        error "Logs directory not found: $LOGS_DIR"
        error "Make sure to run ./run_app.sh first to create logs"
        exit 1
    fi
}

# List available log files
list_logs() {
    info "Available log files:"
    echo
    
    cd "$LOGS_DIR"
    for log_file in *.log; do
        if [ -f "$log_file" ]; then
            size=$(du -h "$log_file" | cut -f1)
            lines=$(wc -l < "$log_file" 2>/dev/null || echo "0")
            modified=$(stat -f "%Sm" -t "%Y-%m-%d %H:%M" "$log_file" 2>/dev/null || date)
            
            echo "📄 $log_file ($size, $lines lines, modified: $modified)"
        fi
    done
    
    echo
}

# View log file
view_log() {
    local log_file="$1"
    local lines="${2:-50}"
    
    if [ -z "$log_file" ]; then
        error "Please specify a log file to view"
        echo "Available logs:"
        ls "$LOGS_DIR"/*.log 2>/dev/null | xargs -n 1 basename
        return 1
    fi
    
    local full_path="$LOGS_DIR/$log_file"
    if [ ! -f "$full_path" ]; then
        # Try with .log extension
        full_path="$LOGS_DIR/${log_file}.log"
        if [ ! -f "$full_path" ]; then
            error "Log file not found: $log_file"
            return 1
        fi
    fi
    
    info "Viewing last $lines lines of $log_file"
    echo "=================================================="
    tail -n "$lines" "$full_path"
}

# Follow log file in real time
follow_log() {
    local log_file="$1"
    
    if [ -z "$log_file" ]; then
        error "Please specify a log file to follow"
        return 1
    fi
    
    local full_path="$LOGS_DIR/$log_file"
    if [ ! -f "$full_path" ]; then
        full_path="$LOGS_DIR/${log_file}.log"
        if [ ! -f "$full_path" ]; then
            error "Log file not found: $log_file"
            return 1
        fi
    fi
    
    info "Following $log_file (press Ctrl+C to stop)"
    echo "=================================================="
    tail -f "$full_path"
}

# Search in logs
search_logs() {
    local pattern="$1"
    local log_file="$2"
    
    if [ -z "$pattern" ]; then
        error "Please specify a search pattern"
        return 1
    fi
    
    info "Searching for '$pattern' in logs"
    echo "=================================================="
    
    if [ -n "$log_file" ]; then
        local full_path="$LOGS_DIR/$log_file"
        if [ ! -f "$full_path" ]; then
            full_path="$LOGS_DIR/${log_file}.log"
        fi
        
        if [ -f "$full_path" ]; then
            grep -n --color=always "$pattern" "$full_path"
        else
            error "Log file not found: $log_file"
        fi
    else
        grep -r -n --color=always "$pattern" "$LOGS_DIR"/*.log 2>/dev/null
    fi
}

# Show error summary
show_errors() {
    local hours="${1:-24}"
    
    info "Error summary for last $hours hours"
    echo "=================================================="
    
    local since_time=$(date -v-${hours}H '+%Y-%m-%d %H:%M:%S' 2>/dev/null || date -d "$hours hours ago" '+%Y-%m-%d %H:%M:%S' 2>/dev/null || echo "")
    
    cd "$LOGS_DIR"
    for log_file in *.log; do
        if [ -f "$log_file" ]; then
            local error_count=0
            if [ -n "$since_time" ]; then
                error_count=$(awk -v since="$since_time" '$0 >= since && /ERROR|CRITICAL/ {count++} END {print count+0}' "$log_file")
            else
                error_count=$(grep -c "ERROR\|CRITICAL" "$log_file" 2>/dev/null || echo "0")
            fi
            
            if [ "$error_count" -gt 0 ]; then
                echo "🔴 $log_file: $error_count errors"
            fi
        fi
    done
}

# Clean old logs
clean_logs() {
    local days="${1:-30}"
    
    warn "This will delete log files older than $days days"
    read -p "Are you sure? (y/N): " -n 1 -r
    echo
    
    if [[ ! $REPLY =~ ^[Yy]$ ]]; then
        info "Cancelled"
        return 0
    fi
    
    info "Cleaning logs older than $days days"
    
    local deleted=0
    find "$LOGS_DIR" -name "*.log.*" -mtime +$days -type f | while read file; do
        echo "Deleting: $(basename "$file")"
        rm -f "$file"
        deleted=$((deleted + 1))
    done
    
    success "Cleanup completed"
}

# Archive logs
archive_logs() {
    local archive_name="logs_$(date +%Y%m%d_%H%M%S).tar.gz"
    
    info "Creating log archive: $archive_name"
    
    cd "$PROJECT_ROOT"
    tar -czf "$archive_name" logs/*.log 2>/dev/null
    
    if [ $? -eq 0 ]; then
        success "Archive created: $archive_name"
        echo "Size: $(du -h "$archive_name" | cut -f1)"
    else
        error "Failed to create archive"
    fi
}

# Show log statistics
show_stats() {
    info "Log Statistics"
    echo "=================================================="
    
    local total_size=0
    local total_lines=0
    local file_count=0
    
    cd "$LOGS_DIR"
    for log_file in *.log; do
        if [ -f "$log_file" ]; then
            local size_bytes=$(stat -f%z "$log_file" 2>/dev/null || stat -c%s "$log_file" 2>/dev/null || echo "0")
            local lines=$(wc -l < "$log_file" 2>/dev/null || echo "0")
            
            total_size=$((total_size + size_bytes))
            total_lines=$((total_lines + lines))
            file_count=$((file_count + 1))
        fi
    done
    
    echo "📁 Total log files: $file_count"
    echo "📊 Total lines: $total_lines"
    echo "💾 Total size: $(echo "$total_size" | awk '{printf "%.2f MB", $1/1024/1024}')"
    echo "📈 Average file size: $(echo "$total_size $file_count" | awk '{printf "%.2f KB", $1/1024/$2}')"
    
    echo
    echo "Recent activity:"
    tail -5 "$LOGS_DIR"/app.log 2>/dev/null | sed 's/^/  /'
}

# Usage information
usage() {
    echo "ACAS Log Management Utility"
    echo "================================"
    echo
    echo "Usage: $0 [command] [options]"
    echo
    echo "Commands:"
    echo "  list                    List all available log files"
    echo "  view <log> [lines]      View last N lines of log file (default: 50)"
    echo "  follow <log>            Follow log file in real time"
    echo "  search <pattern> [log]  Search for pattern in logs"
    echo "  errors [hours]          Show error summary (default: 24 hours)"
    echo "  clean [days]            Clean logs older than N days (default: 30)"
    echo "  archive                 Create compressed archive of current logs"
    echo "  stats                   Show log statistics"
    echo
    echo "Examples:"
    echo "  $0 list                 # List all log files"
    echo "  $0 view app             # View app.log"
    echo "  $0 view backend 100     # View last 100 lines of backend.log"
    echo "  $0 follow app           # Follow app.log in real time"
    echo "  $0 search 'ERROR'       # Search for ERROR in all logs"
    echo "  $0 search 'login' security  # Search for 'login' in security.log"
    echo "  $0 errors 12            # Show errors from last 12 hours"
    echo "  $0 clean 7              # Delete logs older than 7 days"
}

# Main script logic
main() {
    check_logs_dir
    
    case "${1:-}" in
        "list")
            list_logs
            ;;
        "view")
            view_log "$2" "$3"
            ;;
        "follow")
            follow_log "$2"
            ;;
        "search")
            search_logs "$2" "$3"
            ;;
        "errors")
            show_errors "$2"
            ;;
        "clean")
            clean_logs "$2"
            ;;
        "archive")
            archive_logs
            ;;
        "stats")
            show_stats
            ;;
        "help"|"--help"|"-h"|"")
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