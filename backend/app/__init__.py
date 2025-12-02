"""
Flask Application Factory
"""
import os
import sys
from flask import Flask
from flask_sqlalchemy import SQLAlchemy
from flask_migrate import Migrate
from flask_jwt_extended import JWTManager
from flask_cors import CORS
from dotenv import load_dotenv

# Load environment variables
load_dotenv()

# Initialize extensions
db = SQLAlchemy()
migrate = Migrate()
jwt = JWTManager()

def create_app(config=None):
    """Create and configure the Flask application"""
    app = Flask(__name__)
    
    # Configuration
    app.config['SECRET_KEY'] = os.getenv('SECRET_KEY', 'dev-secret-key')
    app.config['JWT_SECRET_KEY'] = os.getenv('JWT_SECRET_KEY', 'dev-jwt-secret')
    app.config['SQLALCHEMY_DATABASE_URI'] = os.getenv('DATABASE_URL', 'sqlite:///fitness_app.db')
    app.config['SQLALCHEMY_TRACK_MODIFICATIONS'] = False
    app.config['JWT_ACCESS_TOKEN_EXPIRES'] = 86400  # 24 hours
    
    # Custom config
    if config:
        app.config.update(config)
    
    # Initialize extensions
    db.init_app(app)
    migrate.init_app(app, db)
    jwt.init_app(app)
    
    # CORS
    CORS(app, resources={
        r"/*": {
            "origins": os.getenv('CORS_ORIGINS', 'http://localhost:3000').split(','),
            "methods": ["GET", "POST", "PUT", "DELETE", "OPTIONS"],
            "allow_headers": ["Content-Type", "Authorization"]
        }
    })
    
    # Add AI path to sys.path
    ai_path = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..', 'ai'))
    if ai_path not in sys.path:
        sys.path.insert(0, ai_path)
    
    # Register blueprints
    with app.app_context():
        from app.routes import auth, chat, workouts, profile
        
        app.register_blueprint(auth.bp, url_prefix='/api/auth')
        app.register_blueprint(chat.bp, url_prefix='/api/chat')
        app.register_blueprint(workouts.bp, url_prefix='/api/workouts')
        app.register_blueprint(profile.bp, url_prefix='/api/profile')
        
        # Create tables
        db.create_all()
    
    # Health check
    @app.route('/health')
    def health():
        return {'status': 'healthy', 'service': 'fitness-api'}, 200
    
    return app
