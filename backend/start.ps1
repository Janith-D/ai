# Backend Quick Start Script
Write-Host "🏋️ Fitness AI Coach - Backend Setup" -ForegroundColor Cyan
Write-Host "=" * 50

# Check Python
Write-Host "`n1. Checking Python..." -ForegroundColor Yellow
python --version

# Create virtual environment
Write-Host "`n2. Creating virtual environment..." -ForegroundColor Yellow
python -m venv venv

# Activate virtual environment
Write-Host "`n3. Activating virtual environment..." -ForegroundColor Yellow
.\venv\Scripts\Activate.ps1

# Install dependencies
Write-Host "`n4. Installing dependencies..." -ForegroundColor Yellow
pip install Flask==3.0.0 Flask-CORS==4.0.0 Flask-SQLAlchemy==3.1.1 Flask-Migrate==4.0.5 Flask-JWT-Extended==4.6.0 python-dotenv==1.0.0 sqlalchemy==2.0.25 werkzeug==3.0.1 marshmallow==3.20.1 flask-marshmallow==0.15.0 marshmallow-sqlalchemy==0.29.0 bcrypt==4.1.2 python-dateutil==2.8.2

# Start server
Write-Host "`n5. Starting server..." -ForegroundColor Yellow
Write-Host "Backend will start on http://localhost:5000" -ForegroundColor Green
Write-Host "Press Ctrl+C to stop`n" -ForegroundColor Yellow
python run.py
