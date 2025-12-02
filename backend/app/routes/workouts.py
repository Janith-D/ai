"""
Workout Routes
"""
from flask import Blueprint, request, jsonify
from flask_jwt_extended import jwt_required, get_jwt_identity
from datetime import datetime
from app import db
from app.models import User, Workout

bp = Blueprint('workouts', __name__)


@bp.route('/', methods=['GET'])
@jwt_required()
def get_workouts():
    """Get user's workouts"""
    try:
        user_id = get_jwt_identity()
        status = request.args.get('status')  # planned, completed, skipped
        limit = request.args.get('limit', 50, type=int)
        
        query = Workout.query.filter_by(user_id=user_id)
        
        if status:
            query = query.filter_by(status=status)
        
        workouts = query.order_by(Workout.created_at.desc()).limit(limit).all()
        
        return jsonify({
            'workouts': [w.to_dict() for w in workouts],
            'total': query.count()
        }), 200
        
    except Exception as e:
        return jsonify({'error': str(e)}), 500


@bp.route('/', methods=['POST'])
@jwt_required()
def create_workout():
    """Create a new workout"""
    try:
        user_id = get_jwt_identity()
        data = request.get_json()
        
        workout = Workout(
            user_id=user_id,
            workout_type=data.get('workout_type', 'general'),
            duration_minutes=data.get('duration_minutes'),
            calories_burned=data.get('calories_burned'),
            intensity=data.get('intensity', 'medium'),
            recommended_by_ai=data.get('recommended_by_ai', False),
            ai_confidence=data.get('ai_confidence'),
            status=data.get('status', 'planned'),
            notes=data.get('notes'),
            scheduled_for=datetime.fromisoformat(data['scheduled_for']) if data.get('scheduled_for') else None
        )
        
        db.session.add(workout)
        db.session.commit()
        
        return jsonify({
            'message': 'Workout created',
            'workout': workout.to_dict()
        }), 201
        
    except Exception as e:
        db.session.rollback()
        return jsonify({'error': str(e)}), 500


@bp.route('/<int:workout_id>', methods=['PUT'])
@jwt_required()
def update_workout(workout_id):
    """Update a workout"""
    try:
        user_id = get_jwt_identity()
        workout = Workout.query.filter_by(id=workout_id, user_id=user_id).first()
        
        if not workout:
            return jsonify({'error': 'Workout not found'}), 404
        
        data = request.get_json()
        
        # Update fields
        if 'status' in data:
            workout.status = data['status']
            if data['status'] == 'completed' and not workout.completed_at:
                workout.completed_at = datetime.utcnow()
        
        if 'duration_minutes' in data:
            workout.duration_minutes = data['duration_minutes']
        if 'calories_burned' in data:
            workout.calories_burned = data['calories_burned']
        if 'notes' in data:
            workout.notes = data['notes']
        if 'intensity' in data:
            workout.intensity = data['intensity']
        
        db.session.commit()
        
        return jsonify({
            'message': 'Workout updated',
            'workout': workout.to_dict()
        }), 200
        
    except Exception as e:
        db.session.rollback()
        return jsonify({'error': str(e)}), 500


@bp.route('/<int:workout_id>', methods=['DELETE'])
@jwt_required()
def delete_workout(workout_id):
    """Delete a workout"""
    try:
        user_id = get_jwt_identity()
        workout = Workout.query.filter_by(id=workout_id, user_id=user_id).first()
        
        if not workout:
            return jsonify({'error': 'Workout not found'}), 404
        
        db.session.delete(workout)
        db.session.commit()
        
        return jsonify({'message': 'Workout deleted'}), 200
        
    except Exception as e:
        db.session.rollback()
        return jsonify({'error': str(e)}), 500


@bp.route('/stats', methods=['GET'])
@jwt_required()
def get_workout_stats():
    """Get workout statistics"""
    try:
        user_id = get_jwt_identity()
        
        total = Workout.query.filter_by(user_id=user_id).count()
        completed = Workout.query.filter_by(user_id=user_id, status='completed').count()
        planned = Workout.query.filter_by(user_id=user_id, status='planned').count()
        
        # Total calories
        result = db.session.query(db.func.sum(Workout.calories_burned))\
            .filter_by(user_id=user_id, status='completed').scalar()
        total_calories = int(result) if result else 0
        
        # Total minutes
        result = db.session.query(db.func.sum(Workout.duration_minutes))\
            .filter_by(user_id=user_id, status='completed').scalar()
        total_minutes = int(result) if result else 0
        
        return jsonify({
            'total_workouts': total,
            'completed': completed,
            'planned': planned,
            'total_calories_burned': total_calories,
            'total_minutes_exercised': total_minutes
        }), 200
        
    except Exception as e:
        return jsonify({'error': str(e)}), 500
