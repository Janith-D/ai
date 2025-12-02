"""
Flask Application Entry Point
"""
import os
from app import create_app

# Create Flask app
app = create_app()

if __name__ == '__main__':
    host = os.getenv('HOST', '0.0.0.0')
    port = int(os.getenv('PORT', 5000))
    debug = os.getenv('FLASK_ENV') == 'development'
    
    print(f"""
╔═══════════════════════════════════════════╗
║   🏋️ FITNESS AI COACH - Backend API     ║
╚═══════════════════════════════════════════╝

🚀 Server starting on http://{host}:{port}
🔧 Environment: {os.getenv('FLASK_ENV', 'production')}
🗄️  Database: {os.getenv('DATABASE_URL', 'sqlite:///fitness_app.db')}
🧠 AI Path: {os.path.abspath(os.path.join(os.path.dirname(__file__), '..', 'ai'))}

📋 Available endpoints:
   - GET  /health              Health check
   - POST /api/auth/register   Register user
   - POST /api/auth/login      Login user
   - GET  /api/auth/me         Get current user
   - POST /api/chat/message    Chat with AI Coach
   - GET  /api/chat/history    Get chat history
   - GET  /api/workouts        Get workouts
   - POST /api/workouts        Create workout
   - GET  /api/profile         Get profile
   - PUT  /api/profile         Update profile

Press Ctrl+C to stop the server
    """)
    
    app.run(host=host, port=port, debug=debug)
