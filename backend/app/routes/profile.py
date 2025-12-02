"""
Profile Routes
"""
from flask import Blueprint, request, jsonify
from flask_jwt_extended import jwt_required, get_jwt_identity
from datetime import datetime
from app import db
from app.models import User, ProgressLog

bp = Blueprint('profile', __name__)


@bp.route('/', methods=['GET'])
@jwt_required()
def get_profile():
    """Get user profile"""
    try:
        user_id = get_jwt_identity()
        user = User.query.get(user_id)
        
        if not user:
            return jsonify({'error': 'User not found'}), 404
        
        return jsonify({'user': user.to_dict()}), 200
        
    except Exception as e:
        return jsonify({'error': str(e)}), 500


@bp.route('/', methods=['PUT'])
@jwt_required()
def update_profile():
    """Update user profile"""
    try:
        user_id = get_jwt_identity()
        user = User.query.get(user_id)
        
        if not user:
            return jsonify({'error': 'User not found'}), 404
        
        data = request.get_json()
        
        # Update fields
        if 'full_name' in data:
            user.full_name = data['full_name']
        if 'age' in data:
            user.age = data['age']
        if 'weight' in data:
            user.weight = data['weight']
        if 'height' in data:
            user.height = data['height']
        if 'gender' in data:
            user.gender = data['gender']
        if 'fitness_level' in data:
            user.fitness_level = data['fitness_level']
        if 'fitness_goal' in data:
            user.fitness_goal = data['fitness_goal']
        
        user.updated_at = datetime.utcnow()
        db.session.commit()
        
        return jsonify({
            'message': 'Profile updated',
            'user': user.to_dict()
        }), 200
        
    except Exception as e:
        db.session.rollback()
        return jsonify({'error': str(e)}), 500


@bp.route('/progress', methods=['GET'])
@jwt_required()
def get_progress_logs():
    """Get progress logs"""
    try:
        user_id = get_jwt_identity()
        limit = request.args.get('limit', 30, type=int)
        
        logs = ProgressLog.query.filter_by(user_id=user_id)\
            .order_by(ProgressLog.logged_at.desc())\
            .limit(limit)\
            .all()
        
        return jsonify({
            'progress_logs': [log.to_dict() for log in logs],
            'total': ProgressLog.query.filter_by(user_id=user_id).count()
        }), 200
        
    except Exception as e:
        return jsonify({'error': str(e)}), 500


@bp.route('/progress', methods=['POST'])
@jwt_required()
def log_progress():
    """Log progress"""
    try:
        user_id = get_jwt_identity()
        data = request.get_json()
        
        log = ProgressLog(
            user_id=user_id,
            weight=data.get('weight'),
            body_fat_percentage=data.get('body_fat_percentage'),
            muscle_mass=data.get('muscle_mass'),
            notes=data.get('notes'),
            mood=data.get('mood'),
            energy_level=data.get('energy_level')
        )
        
        db.session.add(log)
        db.session.commit()
        
        return jsonify({
            'message': 'Progress logged',
            'log': log.to_dict()
        }), 201
        
    except Exception as e:
        db.session.rollback()
        return jsonify({'error': str(e)}), 500
