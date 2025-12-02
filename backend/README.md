# 🏋️ Fitness AI Coach - Backend Setup Guide

## Prerequisites
- Python 3.10+
- pip

## Quick Start

### 1. Install Dependencies
```bash
cd backend
pip install -r requirements.txt
```

### 2. Configure Environment
```bash
# Copy .env.example to .env (already done)
# Update GEMINI_API_KEY if needed
```

### 3. Initialize Database
```bash
# Database will be created automatically on first run
# Using SQLite by default
```

### 4. Run Server
```bash
python run.py
```

Server will start on `http://localhost:5000`

## API Endpoints

### Authentication
- `POST /api/auth/register` - Register new user
- `POST /api/auth/login` - Login
- `GET /api/auth/me` - Get current user

### Chat (AI Coach)
- `POST /api/chat/message` - Send message to AI Coach
- `GET /api/chat/history` - Get chat history
- `GET /api/chat/stats` - Get AI statistics

### Workouts
- `GET /api/workouts` - Get workouts
- `POST /api/workouts` - Create workout
- `PUT /api/workouts/:id` - Update workout
- `DELETE /api/workouts/:id` - Delete workout
- `GET /api/workouts/stats` - Get workout statistics

### Profile
- `GET /api/profile` - Get profile
- `PUT /api/profile` - Update profile
- `GET /api/profile/progress` - Get progress logs
- `POST /api/profile/progress` - Log progress

## Example API Calls

### Register
```bash
curl -X POST http://localhost:5000/api/auth/register \
  -H "Content-Type: application/json" \
  -d '{
    "email": "user@example.com",
    "username": "testuser",
    "password": "password123",
    "age": 25,
    "weight": 70,
    "fitness_level": "intermediate"
  }'
```

### Login
```bash
curl -X POST http://localhost:5000/api/auth/login \
  -H "Content-Type: application/json" \
  -d '{
    "email": "user@example.com",
    "password": "password123"
  }'
```

### Chat with AI Coach
```bash
curl -X POST http://localhost:5000/api/chat/message \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer YOUR_TOKEN_HERE" \
  -d '{
    "message": "I want to train chest today!"
  }'
```

## Database

By default uses SQLite (`fitness_app.db`).

To use PostgreSQL:
```bash
# Update .env
DATABASE_URL=postgresql://username:password@localhost:5432/fitness_db
```

## Project Structure
```
backend/
├── app/
│   ├── __init__.py          # Flask app factory
│   ├── models/              # Database models
│   │   └── __init__.py
│   ├── routes/              # API routes
│   │   ├── auth.py          # Authentication
│   │   ├── chat.py          # AI Chat
│   │   ├── workouts.py      # Workouts
│   │   └── profile.py       # Profile
│   └── services/            # Business logic
│       └── ai_service.py    # AI Coach service
├── run.py                   # Application entry point
├── requirements.txt         # Dependencies
└── .env                     # Environment config
```

## Environment Variables

```
FLASK_APP=run.py
FLASK_ENV=development
SECRET_KEY=your-secret-key
JWT_SECRET_KEY=your-jwt-secret
DATABASE_URL=sqlite:///fitness_app.db
AI_PATH=../ai
GEMINI_API_KEY=your-api-key
CORS_ORIGINS=http://localhost:3000
HOST=0.0.0.0
PORT=5000
```

## Development

### Run in development mode
```bash
export FLASK_ENV=development  # Linux/Mac
$env:FLASK_ENV="development"  # Windows PowerShell
python run.py
```

### Database migrations
```bash
flask db init
flask db migrate -m "Initial migration"
flask db upgrade
```

## Troubleshooting

### AI brains not loading
- Check AI_PATH in .env points to correct directory
- Ensure all AI dependencies are installed
- Check GEMINI_API_KEY is set

### Database errors
- Delete fitness_app.db and restart to reset
- Check DATABASE_URL format

### CORS errors
- Update CORS_ORIGINS in .env with your frontend URL

## Next Steps

1. ✅ Backend is ready!
2. 🔜 Create frontend (React app)
3. 🔜 Connect frontend to backend
4. 🔜 Deploy to cloud

---

**Backend Status:** ✅ Ready to run!
