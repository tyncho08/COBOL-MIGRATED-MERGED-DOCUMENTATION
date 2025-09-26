#!/bin/bash

# ACAS Local Development Startup Script
# Starts the application in local development mode

set -e

# Script configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

# Colors for output
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
RED='\033[0;31m'
NC='\033[0m' # No Color

info() { echo -e "${BLUE}ℹ️  $@${NC}"; }
warn() { echo -e "${YELLOW}⚠️  $@${NC}"; }
error() { echo -e "${RED}❌ $@${NC}"; }
success() { echo -e "${GREEN}✅ $@${NC}"; }

# Check if Docker is running
check_docker() {
    info "Checking Docker..."
    if ! docker info >/dev/null 2>&1; then
        error "Docker is not running. Please start Docker Desktop."
        exit 1
    fi
    success "Docker is running"
}

# Check if docker-compose is available
check_docker_compose() {
    info "Checking Docker Compose..."
    if ! command -v docker-compose &> /dev/null; then
        error "docker-compose command not found. Please install Docker Compose."
        exit 1
    fi
    success "Docker Compose is available"
}

# Create local environment file if it doesn't exist
setup_environment() {
    info "Setting up local environment..."
    
    cd "$PROJECT_ROOT"
    
    if [ ! -f ".env" ]; then
        info "Creating local .env file..."
        cat > .env << EOF
# ACAS Local Development Environment
ENVIRONMENT=development
DEBUG=true

# Database
POSTGRES_DB=acas
POSTGRES_USER=acas_user
POSTGRES_PASSWORD=acas_password
POSTGRES_PORT=5432

# Redis
REDIS_PORT=6379

# Application
BACKEND_PORT=8000
FRONTEND_PORT=3000
SECRET_KEY=local-development-secret-key-not-for-production
ALGORITHM=HS256
ACCESS_TOKEN_EXPIRE_MINUTES=30

# CORS
CORS_ORIGINS=http://localhost:3000,http://localhost:8000

# Logging
LOG_LEVEL=INFO

# Frontend
NEXT_PUBLIC_API_URL=http://localhost:8000
NODE_ENV=development
EOF
        success "Local .env file created"
    else
        info ".env file already exists"
    fi
}

# Start services
start_services() {
    info "Starting ACAS services locally..."
    
    cd "$PROJECT_ROOT"
    
    # Start only core services for local development
    info "Starting database and Redis..."
    docker-compose up -d postgres redis
    
    # Wait for database to be ready
    info "Waiting for database to be ready..."
    until docker-compose exec postgres pg_isready -U acas_user -d acas >/dev/null 2>&1; do
        echo -n "."
        sleep 2
    done
    echo
    success "Database is ready"
    
    # Check if backend needs to be built
    info "Starting backend..."
    if [ ! -f "backend/Dockerfile" ]; then
        # Create a simple Dockerfile for local development
        cat > backend/Dockerfile << EOF
FROM python:3.11-slim

WORKDIR /app

# Install system dependencies
RUN apt-get update && apt-get install -y \\
    gcc \\
    && rm -rf /var/lib/apt/lists/*

# Install Python dependencies
COPY requirements.txt .
RUN pip install --no-cache-dir -r requirements.txt

# Copy application code
COPY . .

# Create logs directory
RUN mkdir -p logs

# Expose port
EXPOSE 8000

# Start command
CMD ["uvicorn", "app.main:app", "--host", "0.0.0.0", "--port", "8000", "--reload"]
EOF
        info "Created backend Dockerfile"
    fi
    
    docker-compose up -d backend
    
    # Wait for backend to be ready
    info "Waiting for backend to be ready..."
    local max_attempts=30
    local attempt=1
    
    while [ $attempt -le $max_attempts ]; do
        if curl -f http://localhost:8000/health >/dev/null 2>&1; then
            success "Backend is ready"
            break
        fi
        
        if [ $attempt -eq $max_attempts ]; then
            error "Backend failed to start after $max_attempts attempts"
            show_logs
            exit 1
        fi
        
        echo -n "."
        sleep 2
        attempt=$((attempt + 1))
    done
    echo
    
    # Check if frontend needs to be built
    if [ -d "frontend" ]; then
        info "Starting frontend..."
        if [ ! -f "frontend/Dockerfile" ]; then
            # Create a simple Dockerfile for local development
            cat > frontend/Dockerfile << EOF
FROM node:18-alpine

WORKDIR /app

# Copy package files
COPY package*.json ./

# Install dependencies
RUN npm ci

# Copy source code
COPY . .

# Build the application
RUN npm run build

# Expose port
EXPOSE 3000

# Start command
CMD ["npm", "start"]
EOF
            info "Created frontend Dockerfile"
        fi
        
        docker-compose up -d frontend
        
        # Wait for frontend to be ready
        info "Waiting for frontend to be ready..."
        local max_attempts=30
        local attempt=1
        
        while [ $attempt -le $max_attempts ]; do
            if curl -f http://localhost:3000/ >/dev/null 2>&1; then
                success "Frontend is ready"
                break
            fi
            
            if [ $attempt -eq $max_attempts ]; then
                warn "Frontend may still be starting (this can take a few minutes)"
                break
            fi
            
            echo -n "."
            sleep 3
            attempt=$((attempt + 1))
        done
        echo
    else
        warn "Frontend directory not found, skipping frontend startup"
    fi
}

# Show service status
show_status() {
    info "Service Status:"
    cd "$PROJECT_ROOT"
    docker-compose ps
    
    echo
    info "Available services:"
    echo "🗄️  Database (PostgreSQL): localhost:5432"
    echo "🔴 Redis: localhost:6379"
    echo "🖥️  Backend API: http://localhost:8000"
    echo "📊 API Documentation: http://localhost:8000/docs"
    echo "🌐 Frontend: http://localhost:3000"
    echo "💊 Health Check: http://localhost:8000/health"
    echo "ℹ️  System Info: http://localhost:8000/info"
}

# Show logs
show_logs() {
    info "Recent logs:"
    cd "$PROJECT_ROOT"
    docker-compose logs --tail=20
}

# Run database migrations
run_migrations() {
    info "Running database migrations..."
    cd "$PROJECT_ROOT"
    
    if docker-compose exec backend python -c "
from app.core.database import Base, engine
try:
    Base.metadata.create_all(bind=engine)
    print('✅ Database tables created successfully')
except Exception as e:
    print(f'❌ Migration failed: {e}')
    exit(1)
" 2>/dev/null; then
        success "Database migrations completed"
    else
        error "Database migrations failed"
        exit 1
    fi
}

# Main function
main() {
    echo
    info "🚀 Starting ACAS locally..."
    echo
    
    check_docker
    check_docker_compose
    setup_environment
    start_services
    
    # Give services a moment to fully start
    sleep 5
    
    run_migrations
    show_status
    
    echo
    success "🎉 ACAS is now running locally!"
    echo
    info "To view logs: docker-compose logs -f"
    info "To stop: docker-compose down"
    info "To restart: $0"
    echo
    warn "Press Ctrl+C to stop all services"
    
    # Keep script running and show logs
    trap 'echo; info "Stopping services..."; cd "$PROJECT_ROOT"; docker-compose down; success "Services stopped"; exit 0' INT
    
    cd "$PROJECT_ROOT"
    docker-compose logs -f
}

# Run main function
main "$@"