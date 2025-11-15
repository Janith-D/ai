"""
Response Templates
Structured templates for consistent, high-quality responses
"""

from typing import Dict, List, Optional
from dataclasses import dataclass


@dataclass
class ResponseTemplate:
    """Template for structured responses"""
    name: str
    category: str
    structure: List[str]
    example: str
    required_fields: List[str]


class ResponseTemplates:
    """
    Manages response templates for different scenarios
    
    Template structure: [Emotion Opener + Reasoning + Recommendation + Motivation + Action]
    """
    
    def __init__(self):
        """Initialize response templates"""
        self.templates = self._initialize_templates()
        
    def _initialize_templates(self) -> Dict[str, ResponseTemplate]:
        """
        Create response templates for different scenarios
        
        Returns:
            Dictionary of response templates
        """
        return {
            'workout_plan': ResponseTemplate(
                name='workout_plan',
                category='planning',
                structure=[
                    'emotion_acknowledgment',
                    'current_state_assessment',
                    'recommendation',
                    'reasoning',
                    'motivation',
                    'next_action'
                ],
                example="""
{emotion_acknowledgment}
{current_state_assessment}

Here's what makes sense: {recommendation}

{reasoning}

{motivation}

{next_action}
                """.strip(),
                required_fields=['recommendation', 'reasoning']
            ),
            
            'rest_day': ResponseTemplate(
                name='rest_day',
                category='recovery',
                structure=[
                    'validation',
                    'education',
                    'recommendation',
                    'reframe',
                    'next_action'
                ],
                example="""
{validation}

{education}

Here's the plan: {recommendation}

{reframe}

{next_action}
                """.strip(),
                required_fields=['recommendation', 'education']
            ),
            
            'injury_safety': ResponseTemplate(
                name='injury_safety',
                category='safety',
                structure=[
                    'urgency_acknowledgment',
                    'safety_priority',
                    'recommendation',
                    'explanation',
                    'next_steps',
                    'reassurance'
                ],
                example="""
{urgency_acknowledgment}

{safety_priority}

Here's what we need to do: {recommendation}

{explanation}

{next_steps}

{reassurance}
                """.strip(),
                required_fields=['recommendation', 'safety_priority', 'next_steps']
            ),
            
            'motivation_boost': ResponseTemplate(
                name='motivation_boost',
                category='psychological',
                structure=[
                    'validation',
                    'perspective_shift',
                    'win_reminder',
                    'small_step',
                    'encouragement'
                ],
                example="""
{validation}

{perspective_shift}

{win_reminder}

Here's what I want you to do: {small_step}

{encouragement}
                """.strip(),
                required_fields=['validation', 'small_step']
            ),
            
            'progress_check': ResponseTemplate(
                name='progress_check',
                category='tracking',
                structure=[
                    'celebration',
                    'data_summary',
                    'insight',
                    'next_goal',
                    'motivation'
                ],
                example="""
{celebration}

{data_summary}

{insight}

{next_goal}

{motivation}
                """.strip(),
                required_fields=['data_summary', 'insight']
            ),
            
            'adjustment_needed': ResponseTemplate(
                name='adjustment_needed',
                category='adaptation',
                structure=[
                    'acknowledgment',
                    'analysis',
                    'adjustment',
                    'reasoning',
                    'positive_frame'
                ],
                example="""
{acknowledgment}

{analysis}

Let's adjust: {adjustment}

{reasoning}

{positive_frame}
                """.strip(),
                required_fields=['analysis', 'adjustment', 'reasoning']
            ),
            
            'goal_setting': ResponseTemplate(
                name='goal_setting',
                category='planning',
                structure=[
                    'goal_validation',
                    'reality_check',
                    'plan',
                    'milestones',
                    'commitment'
                ],
                example="""
{goal_validation}

{reality_check}

Here's the plan: {plan}

{milestones}

{commitment}
                """.strip(),
                required_fields=['plan', 'milestones']
            ),
            
            'obstacle_handling': ResponseTemplate(
                name='obstacle_handling',
                category='problem_solving',
                structure=[
                    'validation',
                    'normalize',
                    'solution',
                    'backup_plan',
                    'encouragement'
                ],
                example="""
{validation}

{normalize}

Solution: {solution}

{backup_plan}

{encouragement}
                """.strip(),
                required_fields=['solution']
            )
        }
    
    def get_template(self, template_name: str) -> ResponseTemplate:
        """
        Get template by name
        
        Args:
            template_name: Name of template
            
        Returns:
            ResponseTemplate object
        """
        return self.templates.get(template_name, self.templates['workout_plan'])
    
    def select_template(
        self,
        intent: str,
        has_injury: bool = False,
        needs_motivation: bool = False
    ) -> str:
        """
        Select appropriate template based on intent and context
        
        Args:
            intent: User's intent
            has_injury: Whether user has injury
            needs_motivation: Whether user needs motivation
            
        Returns:
            Template name
        """
        # Safety first
        if has_injury:
            return 'injury_safety'
        
        # Intent-based selection
        intent_map = {
            'plan_workout': 'workout_plan',
            'get_recommendation': 'workout_plan',
            'ask_rest': 'rest_day',
            'report_injury': 'injury_safety',
            'check_progress': 'progress_check',
            'set_goals': 'goal_setting',
            'ask_motivation': 'motivation_boost',
            'report_obstacle': 'obstacle_handling',
            'request_adjustment': 'adjustment_needed'
        }
        
        template = intent_map.get(intent, 'workout_plan')
        
        # Override with motivation template if needed
        if needs_motivation and template == 'workout_plan':
            template = 'motivation_boost'
        
        return template
    
    def fill_template(
        self,
        template_name: str,
        fields: Dict[str, str]
    ) -> str:
        """
        Fill template with provided fields
        
        Args:
            template_name: Name of template
            fields: Dictionary of field values
            
        Returns:
            Filled template string
        """
        template = self.get_template(template_name)
        
        # Check required fields
        missing_fields = [f for f in template.required_fields if f not in fields]
        if missing_fields:
            raise ValueError(f"Missing required fields: {missing_fields}")
        
        # Fill template
        try:
            result = template.example.format(**fields)
            return result
        except KeyError as e:
            raise ValueError(f"Missing template field: {e}")
    
    def get_structure(self, template_name: str) -> List[str]:
        """
        Get template structure (list of sections)
        
        Args:
            template_name: Name of template
            
        Returns:
            List of section names
        """
        template = self.get_template(template_name)
        return template.structure
    
    def get_example_fields(self, template_name: str) -> Dict[str, str]:
        """
        Get example fields for a template
        
        Args:
            template_name: Name of template
            
        Returns:
            Dictionary of example field values
        """
        examples = {
            'workout_plan': {
                'emotion_acknowledgment': "I hear you — you're tired, but you still want to keep going.",
                'current_state_assessment': "Your energy is at 30%, but your motivation is strong.",
                'recommendation': "light cardio for 20 minutes",
                'reasoning': "This keeps your momentum without draining you further. Recovery is part of training, not the opposite of it.",
                'motivation': "That dedication is what builds real progress.",
                'next_action': "Let's do 20 minutes at a conversational pace. You in?"
            },
            'rest_day': {
                'validation': "You're feeling worn out, and that's your body talking.",
                'education': "Rest days aren't lazy days — they're when your muscles actually grow and repair.",
                'recommendation': "full rest day today",
                'reframe': "This isn't stopping, it's strategic recovery. You'll come back stronger.",
                'next_action': "Focus on sleep, hydration, and maybe some light stretching. Tomorrow's workout will benefit from this."
            },
            'injury_safety': {
                'urgency_acknowledgment': "Hold up — shoulder pain is serious.",
                'safety_priority': "I'm not taking chances with your health.",
                'recommendation': "rest day, ice the area, and monitor the pain",
                'explanation': "No workout is worth making an injury worse. Sharp or persistent pain needs professional evaluation.",
                'next_steps': "If the pain continues tomorrow or gets worse, see a doctor or physical therapist.",
                'reassurance': "Your long-term strength matters way more than today's session. We'll come back stronger when you're healed."
            },
            'motivation_boost': {
                'validation': "I get it — motivation isn't always there, and that's completely normal.",
                'perspective_shift': "But here's the thing: you don't need motivation. You just need to show up.",
                'win_reminder': "Remember last week? You didn't feel like it then either, but you did it anyway. And you felt amazing after.",
                'small_step': "Just commit to 10 minutes. Start with a warm-up, and if you still want to stop after, that's fine.",
                'encouragement': "But I bet once you start, you'll want to keep going. You've got this."
            },
            'progress_check': {
                'celebration': "🎉 Let's talk about what you've accomplished!",
                'data_summary': "Over the past 4 weeks: 12 workouts completed, chest strength up 15%, consistency at 80%.",
                'insight': "You're building real momentum. That consistency is the key — most people quit before they see this kind of progress.",
                'next_goal': "Next milestone: hit 85% consistency and add 10 lbs to your bench press.",
                'motivation': "You're on a solid path. Keep this up and you'll be amazed where you are in 3 months."
            },
            'adjustment_needed': {
                'acknowledgment': "I see what's happening here.",
                'analysis': "You've been hitting chest 3x per week but your recovery isn't keeping up. Overtraining can stall progress.",
                'adjustment': "Let's bring it down to 2x per week for chest, and add an extra rest day",
                'reasoning': "This gives your muscles more time to repair and grow. Quality over quantity.",
                'positive_frame': "This isn't a step back — it's smart training. You'll actually see better gains with proper recovery."
            },
            'goal_setting': {
                'goal_validation': "Building muscle and losing fat — that's a solid goal.",
                'reality_check': "Real talk: it's tough to do both at once, but not impossible. It's about timing and strategy.",
                'plan': "Focus on muscle building first (12 weeks), then shift to fat loss (8 weeks). That's how you get the best results.",
                'milestones': "Month 1: Establish routine, Month 2-3: Progressive overload, Month 4: Assess and adjust.",
                'commitment': "This is a 5-month journey. You ready to commit? Because I'm here with you the whole way."
            },
            'obstacle_handling': {
                'validation': "Travel makes it tough — no gym, busy schedule, different routine.",
                'normalize': "This happens to everyone. The difference is how you adapt.",
                'solution': "Bodyweight workout in your hotel room. 20 minutes, no equipment needed: push-ups, squats, lunges, planks.",
                'backup_plan': "If that doesn't work, even a 15-minute walk keeps momentum alive.",
                'encouragement': "You're not starting over — you're maintaining. That's huge."
            }
        }
        
        return examples.get(template_name, {})


# Test function
if __name__ == "__main__":
    templates = ResponseTemplates()
    
    print("=" * 80)
    print("📝 RESPONSE TEMPLATES")
    print("=" * 80)
    print()
    
    # Show all templates
    print("Available Templates:")
    for name, template in templates.templates.items():
        print(f"  • {name} ({template.category})")
        print(f"    Structure: {' → '.join(template.structure)}")
        print(f"    Required: {', '.join(template.required_fields)}")
        print()
    
    # Test template filling
    print("=" * 80)
    print("📋 FILLED TEMPLATE EXAMPLES")
    print("=" * 80)
    print()
    
    # Workout plan example
    print("🏋️ Workout Plan (Tired User):")
    print("-" * 80)
    fields = templates.get_example_fields('workout_plan')
    filled = templates.fill_template('workout_plan', fields)
    print(filled)
    print()
    
    # Injury safety example
    print("⚠️ Injury Safety (Shoulder Pain):")
    print("-" * 80)
    fields = templates.get_example_fields('injury_safety')
    filled = templates.fill_template('injury_safety', fields)
    print(filled)
    print()
    
    # Motivation boost example
    print("🔥 Motivation Boost (Unmotivated User):")
    print("-" * 80)
    fields = templates.get_example_fields('motivation_boost')
    filled = templates.fill_template('motivation_boost', fields)
    print(filled)
    print()
    
    # Template selection test
    print("=" * 80)
    print("🎯 TEMPLATE SELECTION")
    print("=" * 80)
    print()
    
    scenarios = [
        ('plan_workout', False, False, 'Normal workout planning'),
        ('plan_workout', True, False, 'Workout planning with injury'),
        ('ask_rest', False, False, 'Rest day question'),
        ('plan_workout', False, True, 'Needs motivation'),
    ]
    
    for intent, has_injury, needs_motivation, description in scenarios:
        selected = templates.select_template(intent, has_injury, needs_motivation)
        print(f"  {description}")
        print(f"    Intent: {intent}, Injury: {has_injury}, Needs Motivation: {needs_motivation}")
        print(f"    → Selected: {selected}")
        print()
