"""
Chat Routes - AI Coach Interaction
"""
from flask import Blueprint, request, jsonify
from flask_jwt_extended import jwt_required, get_jwt_identity
from datetime import datetime
import json
from app import db
from app.models import User, ChatMessage
from app.services.ai_service import ai_coach

bp = Blueprint('chat', __name__)


@bp.route('/message', methods=['POST'])
@jwt_required()
def send_message():
    """Send message to AI Coach"""
    try:
        user_id = get_jwt_identity()
        user = User.query.get(user_id)
        
        if not user:
            return jsonify({'error': 'User not found'}), 404
        
        data = request.get_json()
        message = data.get('message', '').strip()
        
        if not message:
            return jsonify({'error': 'Message required'}), 400
        
        # Prepare user data for AI
        user_data = {
            'user_id': user.id,
            'age': user.age,
            'weight': user.weight,
            'height': user.height,
            'gender': user.gender,
            'fitness_level': user.fitness_level,
            'fitness_goal': user.fitness_goal
        }
        
        # Get AI response
        ai_response = ai_coach.chat(message, user_data)
        
        # Save chat message
        chat_msg = ChatMessage(
            user_id=user.id,
            message=message,
            response=ai_response.get('response', ''),
            emotion_detected=ai_response.get('emotion_detected'),
            intent_detected=ai_response.get('intent_detected'),
            energy_level=ai_response.get('energy_level'),
            confidence_score=ai_response.get('confidence_score'),
            brains_used=json.dumps(ai_response.get('brains_used', [])),
            processing_time_ms=ai_response.get('processing_time_ms')
        )
        db.session.add(chat_msg)
        
        # Update user last active
        user.last_active = datetime.utcnow()
        db.session.commit()
        
        return jsonify({
            'message_id': chat_msg.id,
            'response': ai_response.get('response'),
            'workout_recommendation': ai_response.get('workout_recommendation'),
            'safety_status': ai_response.get('safety_status'),
            'metadata': {
                'emotion': ai_response.get('emotion_detected'),
                'intent': ai_response.get('intent_detected'),
                'energy_level': ai_response.get('energy_level'),
                'confidence': ai_response.get('confidence_score'),
                'brains_used': ai_response.get('brains_used', []),
                'processing_time_ms': ai_response.get('processing_time_ms')
            }
        }), 200
        
    except Exception as e:
        db.session.rollback()
        return jsonify({'error': str(e)}), 500


@bp.route('/history', methods=['GET'])
@jwt_required()
def get_chat_history():
    """Get user's chat history"""
    try:
        user_id = get_jwt_identity()
        limit = request.args.get('limit', 50, type=int)
        offset = request.args.get('offset', 0, type=int)
        
        messages = ChatMessage.query.filter_by(user_id=user_id)\
            .order_by(ChatMessage.created_at.desc())\
            .limit(limit)\
            .offset(offset)\
            .all()
        
        return jsonify({
            'messages': [msg.to_dict() for msg in messages],
            'total': ChatMessage.query.filter_by(user_id=user_id).count()
        }), 200
        
    except Exception as e:
        return jsonify({'error': str(e)}), 500


@bp.route('/stats', methods=['GET'])
@jwt_required()
def get_ai_stats():
    """Get AI Coach statistics"""
    try:
        stats = ai_coach.get_stats()
        return jsonify(stats), 200
        
    except Exception as e:
        return jsonify({'error': str(e)}), 500
