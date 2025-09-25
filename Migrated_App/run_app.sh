#!/bin/bash
# ACAS Migration - Complete Application Deployment
# This script deploys the entire ACAS migration system in one command

set -e  # Exit on any error

echo "🚀 Starting ACAS Migration App Deployment..."
echo "=================================================="

# Store current directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

# Color codes for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Function to print colored output
print_status() {
    echo -e "${BLUE}ℹ️  $1${NC}"
}

print_success() {
    echo -e "${GREEN}✅ $1${NC}"
}

print_warning() {
    echo -e "${YELLOW}⚠️  $1${NC}"
}

print_error() {
    echo -e "${RED}❌ $1${NC}"
}

# Check if running on macOS or Linux
OS="$(uname -s)"
case "${OS}" in
    Linux*)     MACHINE=Linux;;
    Darwin*)    MACHINE=Mac;;
    *)          MACHINE="UNKNOWN:${OS}"
esac

print_status "Detected OS: $MACHINE"

# Function to check if command exists
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# Function to kill processes on ports
kill_port() {
    local port=$1
    print_status "Checking port $port..."
    
    if command_exists lsof; then
        local pid=$(lsof -ti:$port 2>/dev/null)
        if [ ! -z "$pid" ]; then
            print_warning "Killing existing process on port $port (PID: $pid)"
            kill -9 $pid 2>/dev/null || true
            sleep 2
        fi
    else
        print_warning "lsof not found, cannot check port $port"
    fi
}

# Clean up existing processes
print_status "Cleaning up existing processes..."
pkill -f "uvicorn" 2>/dev/null || true
pkill -f "next dev" 2>/dev/null || true
pkill -f "next start" 2>/dev/null || true
kill_port 8000
kill_port 3000
sleep 2

print_success "Process cleanup completed"

# Check required dependencies
print_status "Checking system dependencies..."

# Check Python 3
if ! command_exists python3; then
    print_error "Python 3 is required but not installed"
    exit 1
fi

PYTHON_VERSION=$(python3 --version 2>&1 | awk '{print $2}')
print_success "Python 3 found: $PYTHON_VERSION"

# Check Node.js and npm
if ! command_exists node; then
    print_error "Node.js is required but not installed"
    exit 1
fi

NODE_VERSION=$(node --version)
print_success "Node.js found: $NODE_VERSION"

if ! command_exists npm; then
    print_error "npm is required but not installed"
    exit 1
fi

NPM_VERSION=$(npm --version)
print_success "npm found: $NPM_VERSION"

# Check PostgreSQL
if ! command_exists psql; then
    print_error "PostgreSQL client (psql) is required but not found"
    print_error "Please install PostgreSQL and ensure psql is in your PATH"
    exit 1
fi

print_success "PostgreSQL client found"

# Check if PostgreSQL server is running
if ! pg_isready -h localhost -p 5432 >/dev/null 2>&1; then
    print_error "PostgreSQL server is not running on localhost:5432"
    print_error "Please start PostgreSQL server and ensure it's accessible"
    
    if [[ "$MACHINE" == "Mac" ]]; then
        print_status "On macOS, you can start PostgreSQL with:"
        print_status "brew services start postgresql@15"
        print_status "or"
        print_status "pg_ctl -D /usr/local/var/postgres start"
    elif [[ "$MACHINE" == "Linux" ]]; then
        print_status "On Linux, you can start PostgreSQL with:"
        print_status "sudo systemctl start postgresql"
        print_status "or"
        print_status "sudo service postgresql start"
    fi
    
    exit 1
fi

print_success "PostgreSQL server is running"

# Database setup
print_status "Setting up PostgreSQL database..."

# Set default PostgreSQL connection parameters
export PGHOST=localhost
export PGPORT=5432
export PGUSER=${PGUSER:-postgres}
export PGDATABASE=postgres

# Drop and recreate database
print_status "Dropping existing database (if exists)..."
dropdb acas_db 2>/dev/null || true

print_status "Creating ACAS database..."
createdb acas_db --encoding=UTF-8 --template=template0

print_success "Database created successfully"

# Apply database schema
if [ -f "database/complete_schema.sql" ]; then
    print_status "Applying complete database schema (43 tables)..."
    psql -d acas_db -f database/complete_schema.sql
    if [ $? -eq 0 ]; then
        print_success "Complete database schema applied (43 tables)"
    else
        print_error "Failed to apply database schema"
        exit 1
    fi
elif [ -f "database/schema.sql" ]; then
    print_status "Applying database schema..."
    psql -d acas_db -f database/schema.sql > /dev/null
    print_success "Database schema applied"
else
    print_warning "Schema file not found, will be created by application"
fi

# Backend setup
print_status "Setting up backend (FastAPI)..."
cd backend

# Create virtual environment if it doesn't exist
if [ ! -d "venv" ]; then
    print_status "Creating Python virtual environment..."
    python3 -m venv venv
fi

# Activate virtual environment
print_status "Activating virtual environment..."
source venv/bin/activate

# Install Python dependencies
print_status "Installing Python dependencies..."
pip install --upgrade pip > /dev/null 2>&1
pip install -r requirements.txt > /dev/null 2>&1

print_success "Backend dependencies installed"

# Initialize database with models
print_status "Initializing database schema..."
python scripts/init_db.py

print_success "Database schema initialized"

# Populate demo data
print_status "Populating demo data..."
python scripts/populate_demo.py

print_success "Demo data populated"

# Start backend server
print_status "Starting backend server on port 8000..."
nohup uvicorn app.main:app --host 0.0.0.0 --port 8000 --reload > ../logs/backend.log 2>&1 &
BACKEND_PID=$!

# Wait for backend to start
print_status "Waiting for backend to start..."
for i in {1..30}; do
    if curl -s http://localhost:8000/health > /dev/null 2>&1; then
        print_success "Backend server started successfully"
        break
    fi
    if [ $i -eq 30 ]; then
        print_error "Backend server failed to start within 30 seconds"
        kill $BACKEND_PID 2>/dev/null || true
        exit 1
    fi
    sleep 1
    echo -n "."
done

# Frontend setup  
print_status "Setting up frontend (Next.js)..."
cd ../frontend

# Install Node.js dependencies
print_status "Installing Node.js dependencies..."
npm install > /dev/null 2>&1

print_success "Frontend dependencies installed"

# Build the frontend for production
print_status "Building frontend for production..."
npm run build > ../logs/frontend-build.log 2>&1

print_success "Frontend built successfully"

# Start frontend server
print_status "Starting frontend server on port 3000..."
nohup npm start > ../logs/frontend.log 2>&1 &
FRONTEND_PID=$!

# Wait for frontend to start
print_status "Waiting for frontend to start..."
for i in {1..60}; do
    if curl -s http://localhost:3000 > /dev/null 2>&1; then
        print_success "Frontend server started successfully"
        break
    fi
    if [ $i -eq 60 ]; then
        print_error "Frontend server failed to start within 60 seconds"
        kill $FRONTEND_PID 2>/dev/null || true
        kill $BACKEND_PID 2>/dev/null || true
        exit 1
    fi
    sleep 1
    echo -n "."
done

# Create logs directory
cd ..
mkdir -p logs

# Test API endpoints
print_status "Testing API endpoints..."

# Test health endpoint
if curl -s http://localhost:8000/health | grep -q "healthy"; then
    print_success "API health check passed"
else
    print_warning "API health check failed"
fi

# Test system info endpoint
if curl -s http://localhost:8000/info > /dev/null; then
    print_success "API system info endpoint working"
else
    print_warning "API system info endpoint not responding"
fi

# Final success message
echo ""
echo "=================================================="
print_success "🎉 ACAS Migration App is now running!"
echo "=================================================="
echo ""
print_success "✅ Frontend: http://localhost:3000"
print_success "✅ Backend API: http://localhost:8000"
print_success "✅ API Documentation: http://localhost:8000/docs"
print_success "✅ Database: PostgreSQL (acas_db)"
echo ""
print_status "📊 System Status:"
print_status "   • Backend PID: $BACKEND_PID"
print_status "   • Frontend PID: $FRONTEND_PID"
print_status "   • Database: Connected"
print_status "   • Demo Data: Populated"
echo ""
print_status "📁 Log Files:"
print_status "   • Backend: logs/backend.log"
print_status "   • Frontend: logs/frontend.log"
print_status "   • Frontend Build: logs/frontend-build.log"
echo ""
print_status "🔧 To stop all services:"
print_status "   kill $BACKEND_PID $FRONTEND_PID"
echo ""

# Function to cleanup on script exit
cleanup() {
    print_status "Shutting down services..."
    kill $BACKEND_PID 2>/dev/null || true
    kill $FRONTEND_PID 2>/dev/null || true
    print_success "Services stopped"
}

# Set up trap for cleanup on script termination
trap cleanup EXIT INT TERM

print_status "🔄 Application is ready! Press Ctrl+C to stop all services."

# Keep script running and monitor services
while true; do
    # Check if backend is still running
    if ! kill -0 $BACKEND_PID 2>/dev/null; then
        print_error "Backend process died unexpectedly"
        break
    fi
    
    # Check if frontend is still running
    if ! kill -0 $FRONTEND_PID 2>/dev/null; then
        print_error "Frontend process died unexpectedly"
        break
    fi
    
    sleep 10
done