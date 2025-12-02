"""
Database Models for Fitness App
"""
from datetime import datetime
from werkzeug.security import generate_password_hash, check_password_hash
from app import db


class User(db.Model):
    """User model"""
    __tablename__ = 'users'
    
    id = db.Column(db.Integer, primary_key=True)
    email = db.Column(db.String(120), unique=True, nullable=False, index=True)
    username = db.Column(db.String(80), unique=True, nullable=False)
    password_hash = db.Column(db.String(255), nullable=False)
    
    # Profile
    full_name = db.Column(db.String(120))
    age = db.Column(db.Integer)
    weight = db.Column(db.Float)
    height = db.Column(db.Float)
    gender = db.Column(db.String(10))
    fitness_level = db.Column(db.String(20))  # beginner, intermediate, advanced
    fitness_goal = db.Column(db.String(50))  # weight_loss, muscle_gain, general_fitness
    
    # Timestamps
    created_at = db.Column(db.DateTime, default=datetime.utcnow)
    updated_at = db.Column(db.DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)
    last_active = db.Column(db.DateTime, default=datetime.utcnow)
    
    # Relationships
    workouts = db.relationship('Workout', backref='user', lazy=True, cascade='all, delete-orphan')
    chat_messages = db.relationship('ChatMessage', backref='user', lazy=True, cascade='all, delete-orphan')
    
    def set_password(self, password):
        """Hash and set password"""
        self.password_hash = generate_password_hash(password)
    
    def check_password(self, password):
        """Check password against hash"""
        return check_password_hash(self.password_hash, password)
    
    def to_dict(self):
        """Convert to dictionary"""
        return {
            'id': self.id,
            'email': self.email,
            'username': self.username,
            'full_name': self.full_name,
            'age': self.age,
            'weight': self.weight,
            'height': self.height,
            'gender': self.gender,
            'fitness_level': self.fitness_level,
            'fitness_goal': self.fitness_goal,
            'created_at': self.created_at.isoformat() if self.created_at else None,
            'last_active': self.last_active.isoformat() if self.last_active else None
        }


class Workout(db.Model):
    """Workout session model"""
    __tablename__ = 'workouts'
    
    id = db.Column(db.Integer, primary_key=True)
    user_id = db.Column(db.Integer, db.ForeignKey('users.id'), nullable=False)
    
    # Workout details
    workout_type = db.Column(db.String(50), nullable=False)  # chest, legs, cardio, etc.
    duration_minutes = db.Column(db.Integer)
    calories_burned = db.Column(db.Integer)
    intensity = db.Column(db.String(20))  # low, medium, high
    
    # AI recommendations
    recommended_by_ai = db.Column(db.Boolean, default=False)
    ai_confidence = db.Column(db.Float)
    
    # Status
    status = db.Column(db.String(20), default='planned')  # planned, completed, skipped
    notes = db.Column(db.Text)
    
    # Timestamps
    scheduled_for = db.Column(db.DateTime)
    completed_at = db.Column(db.DateTime)
    created_at = db.Column(db.DateTime, default=datetime.utcnow)
    
    def to_dict(self):
        """Convert to dictionary"""
        return {
            'id': self.id,
            'user_id': self.user_id,
            'workout_type': self.workout_type,
            'duration_minutes': self.duration_minutes,
            'calories_burned': self.calories_burned,
            'intensity': self.intensity,
            'recommended_by_ai': self.recommended_by_ai,
            'ai_confidence': self.ai_confidence,
            'status': self.status,
            'notes': self.notes,
            'scheduled_for': self.scheduled_for.isoformat() if self.scheduled_for else None,
            'completed_at': self.completed_at.isoformat() if self.completed_at else None,
            'created_at': self.created_at.isoformat() if self.created_at else None
        }


class ChatMessage(db.Model):
    """Chat message model"""
    __tablename__ = 'chat_messages'
    
    id = db.Column(db.Integer, primary_key=True)
    user_id = db.Column(db.Integer, db.ForeignKey('users.id'), nullable=False)
    
    # Message content
    message = db.Column(db.Text, nullable=False)
    response = db.Column(db.Text, nullable=False)
    
    # AI metadata
    emotion_detected = db.Column(db.String(50))
    intent_detected = db.Column(db.String(50))
    energy_level = db.Column(db.Integer)
    confidence_score = db.Column(db.Float)
    
    # Processing info
    brains_used = db.Column(db.String(100))  # JSON string: ["NLP", "ML", "Logic", "Personality"]
    processing_time_ms = db.Column(db.Float)
    
    # Timestamps
    created_at = db.Column(db.DateTime, default=datetime.utcnow)
    
    def to_dict(self):
        """Convert to dictionary"""
        return {
            'id': self.id,
            'user_id': self.user_id,
            'message': self.message,
            'response': self.response,
            'emotion_detected': self.emotion_detected,
            'intent_detected': self.intent_detected,
            'energy_level': self.energy_level,
            'confidence_score': self.confidence_score,
            'brains_used': self.brains_used,
            'processing_time_ms': self.processing_time_ms,
            'created_at': self.created_at.isoformat() if self.created_at else None
        }


class ProgressLog(db.Model):
    """User progress tracking"""
    __tablename__ = 'progress_logs'
    
    id = db.Column(db.Integer, primary_key=True)
    user_id = db.Column(db.Integer, db.ForeignKey('users.id'), nullable=False)
    
    # Measurements
    weight = db.Column(db.Float)
    body_fat_percentage = db.Column(db.Float)
    muscle_mass = db.Column(db.Float)
    
    # Notes
    notes = db.Column(db.Text)
    mood = db.Column(db.String(50))
    energy_level = db.Column(db.Integer)  # 1-100
    
    # Timestamp
    logged_at = db.Column(db.DateTime, default=datetime.utcnow)
    
    def to_dict(self):
        """Convert to dictionary"""
        return {
            'id': self.id,
            'user_id': self.user_id,
            'weight': self.weight,
            'body_fat_percentage': self.body_fat_percentage,
            'muscle_mass': self.muscle_mass,
            'notes': self.notes,
            'mood': self.mood,
            'energy_level': self.energy_level,
            'logged_at': self.logged_at.isoformat() if self.logged_at else None
        }
