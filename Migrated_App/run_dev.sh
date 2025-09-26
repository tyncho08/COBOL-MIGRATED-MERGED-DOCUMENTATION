#!/bin/bash
# Simple development script for ACAS
set -e

echo "🚀 Starting ACAS in Development Mode"
echo "===================================="

# Kill existing processes
pkill -f "uvicorn" 2>/dev/null || true
pkill -f "next" 2>/dev/null || true
sleep 2

# Start backend
echo "Starting backend..."
cd backend
source venv/bin/activate
uvicorn app.main:app --host 0.0.0.0 --port 8000 --reload > ../logs/backend.log 2>&1 &
BACKEND_PID=$!

# Start frontend in development mode
echo "Starting frontend in DEV mode..."
cd ../frontend
npm run dev > ../logs/frontend.log 2>&1 &
FRONTEND_PID=$!

# Wait for services
sleep 5

echo ""
echo "✅ Services Started:"
echo "   Backend: http://localhost:8000 (PID: $BACKEND_PID)"
echo "   Frontend: http://localhost:3000 (PID: $FRONTEND_PID)"
echo ""
echo "🛑 Press Ctrl+C to stop all services"
echo ""

# Wait forever
while true; do
    sleep 10
done