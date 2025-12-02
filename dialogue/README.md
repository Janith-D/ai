# Personality & Dialogue Unit - "The Expressive Brain"

## 🎭 Overview

The **Personality & Dialogue Unit** is the final component of the AI Fitness Coach - the "mouth" that transforms all brain outputs into natural, motivating, emotion-aware conversation. This system creates **Coach Atlas**, a wise, supportive fitness mentor with consistent personality and adaptive communication.

## 🧠 System Architecture

```
User Message
     ↓
NLP Analysis (emotion, intent, context)
     ↓
ML Prediction (workout recommendation)
     ↓
Prolog Validation (safety rules)
     ↓
DIALOGUE BRAIN → Coach Atlas Response
     ↓
Natural Response to User
```

## ⚡ Core Components

### 1. **CoachPersonality** (`personality.py`)
Defines Coach Atlas character and communication guidelines.

**Features:**
- Personality traits (wise, supportive, disciplined)
- Core values (consistency, safety, sustainability)
- System prompt for LLM
- Tone recommendations by emotion
- Greeting generation
- Personality guidelines (DO/DON'T lists)

**Usage:**
```python
from dialogue import CoachPersonality

personality = CoachPersonality()
system_prompt = personality.get_system_prompt()  # For Gemini API
tone = personality.get_tone_for_emotion('tired')  # → 'supportive'
greeting = personality.get_greeting("Alex", "morning")  # → "Good morning, Alex! Ready to start the day strong?"
```

### 2. **ToneAdapter** (`tone_adapter.py`)
Adjusts communication tone based on user's emotional state.

**7 Tone Profiles:**
- **Gentle**: Tired users → soft, non-demanding
- **Encouraging**: Unmotivated users → positive reinforcement
- **Energetic**: Excited users → high energy, enthusiastic
- **Calm**: Frustrated users → grounding, rational
- **Empathetic**: Anxious/sad users → validating, supportive
- **Balanced**: Neutral situations → clear, professional
- **Serious**: Injury/safety → firm, protective

**Features:**
- Emotion-to-tone mapping
- Energy level adjustment
- Context-aware selection
- Tone blending for mixed emotions
- Supportive phrase generation

**Usage:**
```python
from dialogue import ToneAdapter

adapter = ToneAdapter()

# Select tone
tone = adapter.select_tone(
    emotion='tired',
    energy_level=20,
    context='wants to workout',
    injury_present=False
)  # → 'gentle'

# Get complete guidance
guidance = adapter.get_tone_guidance('tired', energy_level=30)
# {
#   'tone': 'gentle',
#   'description': 'Soft, understanding, non-demanding',
#   'key_phrases': ['I hear you', 'That's totally okay', 'No pressure'],
#   'emphasis_level': 'low',
#   'emoji_intensity': 'minimal'
# }
```

### 3. **ResponseTemplates** (`response_templates.py`)
Structured templates for consistent, high-quality responses.

**8 Template Types:**
- **workout_plan**: Planning workouts
- **rest_day**: Recovery guidance
- **injury_safety**: Injury handling
- **motivation_boost**: Motivation support
- **progress_check**: Progress tracking
- **adjustment_needed**: Plan adjustments
- **goal_setting**: Goal planning
- **obstacle_handling**: Problem solving

**Template Structure:**
```
[Emotion Opener + Reasoning + Recommendation + Motivation + Action]
```

**Usage:**
```python
from dialogue import ResponseTemplates

templates = ResponseTemplates()

# Select template
template_name = templates.select_template(
    intent='plan_workout',
    has_injury=False,
    needs_motivation=True
)  # → 'motivation_boost'

# Fill template
fields = {
    'validation': "I get it — motivation isn't always there",
    'small_step': "Just commit to 10 minutes"
}
response = templates.fill_template('motivation_boost', fields)
```

### 4. **DialogueGenerator** (`dialogue_generator.py`)
Integrates Gemini API to generate Coach Atlas responses.

**Features:**
- Gemini Pro API integration
- System prompt with Coach Atlas personality
- Context formatting (NLP + ML + Prolog)
- Template-based fallback
- Response generation with tone adaptation

**Usage:**
```python
from dialogue import DialogueGenerator, DialogueContext

generator = DialogueGenerator(
    api_key='your_gemini_api_key',  # or set GEMINI_API_KEY env var
    use_fallback=True
)

context = DialogueContext(
    user_message="I'm tired but want to train",
    nlp_analysis={'emotion': 'tired', 'intent': 'plan_workout', 'energy_level': 30},
    ml_prediction={'workout_type': 'CARDIO', 'confidence': 0.7},
    prolog_reasoning={'validation': 'safe'},
    memory={'recent_patterns': 'consistent engagement'}
)

response = generator.generate_response(context)
```

### 5. **ConversationMemory** (`memory_system.py`)
Tracks conversation history and user patterns.

**Features:**
- Conversation turn tracking
- Pattern detection (recurring injuries, motivation dips)
- User profile building
- Continuity context generation
- JSON persistence

**Detected Patterns:**
- Recurring injuries
- Low energy trends
- Motivation struggles
- Consistent engagement

**Usage:**
```python
from dialogue import ConversationMemory

memory = ConversationMemory(max_history=50)

# Add conversation turn
memory.add_turn(
    user_message="I want to train chest",
    bot_response="Let's do chest workout!",
    emotion="motivated",
    intent="plan_workout",
    workout_recommended="CHEST",
    energy_level=80,
    has_injury=False
)

# Get user summary
summary = memory.get_user_summary()
# {
#   'total_interactions': 10,
#   'average_energy': 65.3,
#   'most_common_emotion': 'motivated',
#   'injury_count': 2,
#   'detected_patterns': 1
# }

# Get continuity context
context = memory.get_continuity_context()
# "Last workout was CHEST 2 day(s) ago | You've been really motivated lately"

# Save/load memory
memory.save_to_file('user_memory.json')
memory.load_from_file('user_memory.json')
```

### 6. **CoachPipeline** (`coach_pipeline.py`)
**THE UNIFIED BRAIN** - Connects all AI systems.

**Pipeline Flow:**
1. NLP Analysis (emotion, intent, context)
2. ML Prediction (workout recommendation)
3. Prolog Validation (safety rules)
4. Dialogue Generation (Coach Atlas response)
5. Memory Update (tracking patterns)

**Usage:**
```python
from dialogue import CoachPipeline

pipeline = CoachPipeline(
    nlp_pipeline=your_nlp_pipeline,
    ml_integration=your_ml_integration,
    prolog_integration=your_prolog_integration,
    gemini_api_key='your_api_key',
    enable_memory=True
)

# Simple chat interface
response = pipeline.chat("I'm tired but want to train legs")
# → "I hear you — you're feeling tired. Your energy is at 30%, but your motivation is strong.
#     Here's what makes sense: light cardio for 20 minutes..."

# Full pipeline with metadata
result = pipeline.process_message("I want to train chest", user_data={'age': 25, 'weight': 70})
# {
#   'user_message': '...',
#   'nlp_analysis': {...},
#   'ml_prediction': {...},
#   'prolog_reasoning': {...},
#   'response': '...',
#   'metadata': {...}
# }

# Get stats
stats = pipeline.get_stats()
# {
#   'total_requests': 42,
#   'nlp_analyses': 42,
#   'ml_predictions': 38,
#   'prolog_validations': 40,
#   'responses_generated': 42,
#   'safety_interventions': 3,
#   'conversation_turns': 42
# }
```

## 🎯 Coach Atlas Personality

### Core Identity
- **Name**: Coach Atlas
- **Archetype**: Wise Mentor
- **Description**: A wise, supportive, and disciplined fitness mentor

### Core Values
1. Consistency over intensity
2. Long-term progress over quick wins
3. Safety first
4. Physical + mental health
5. Sustainable habits

### Communication Style
- **Empathy Level**: High
- **Strictness**: Medium (firm but kind)
- **Motivation Style**: Encouraging (not pushy)
- **Vocabulary**: Accessible (not overly technical)

### Response Guidelines

**✓ DO:**
- Acknowledge and validate feelings
- Explain reasoning behind recommendations
- Use "we" language (partnership)
- Celebrate small wins
- Match tone to emotion
- Make science accessible
- End with clear action

**✗ DON'T:**
- Shame or guilt users
- Make unrealistic promises
- Ignore safety concerns
- Use technical jargon
- Be preachy or condescending
- Break character

## 🔄 Complete Example Flow

```python
from dialogue import CoachPipeline
from nlp import NLPPipeline
from nlp.integration import NLPML_Integration, NLPPrologIntegration

# Initialize all brains
nlp = NLPPipeline()
ml_integration = NLPML_Integration()
prolog_integration = NLPPrologIntegration()

# Create unified coach
coach = CoachPipeline(
    nlp_pipeline=nlp,
    ml_integration=ml_integration,
    prolog_integration=prolog_integration,
    gemini_api_key='your_api_key',
    enable_memory=True
)

# User interaction
user_message = "I'm exhausted but I still want to train my legs"

result = coach.process_message(user_message)

print(f"User: {user_message}")
print(f"Coach Atlas: {result['response']}")
print(f"\nEmotion: {result['nlp_analysis']['emotion']}")
print(f"Recommended: {result['ml_prediction']['workout_type']}")
print(f"Safety Check: {result['prolog_reasoning']['validation']}")
```

**Expected Output:**
```
User: I'm exhausted but I still want to train my legs
Coach Atlas: 💪 I hear you — you're feeling exhausted, but your dedication to show up is what builds real progress. Here's what makes sense:

Light CARDIO for 20 minutes.

Your energy is at 20%, so pushing for a legs workout could drain you further and risk injury. This lighter option keeps your momentum without compromising recovery.

That commitment to keep going? That's the mindset of someone who wins long-term. Let's do this smart. You in?

Emotion: tired
Recommended: CARDIO (adjusted from LEGS)
Safety Check: caution
```

## 📊 Testing

Run comprehensive tests:
```bash
python tests/test_dialogue.py
```

**Test Coverage:**
- ✅ Personality system (traits, prompts, guidelines)
- ✅ Tone adapter (selection, blending, guidance)
- ✅ Response templates (selection, filling, structure)
- ✅ Conversation memory (tracking, patterns, summaries)
- ✅ Dialogue generator (response generation)
- ✅ Coach pipeline (end-to-end integration)
- ✅ Complete scenarios (tired user, injured user, motivated user)

## 🚀 Quick Start

### 1. Install Dependencies
```bash
pip install google-generativeai
```

### 2. Set API Key
```bash
# Windows PowerShell
$env:GEMINI_API_KEY="your_api_key_here"

# Linux/Mac
export GEMINI_API_KEY="your_api_key_here"
```

### 3. Basic Usage
```python
from dialogue import CoachPipeline

# Create coach (works without NLP/ML/Prolog for testing)
coach = CoachPipeline(enable_memory=True)

# Chat!
response = coach.chat("I want to train chest today")
print(response)

response = coach.chat("I'm feeling tired though")
print(response)

response = coach.chat("My shoulder hurts")
print(response)
```

### 4. Full Integration
```python
from nlp import NLPPipeline
from nlp.integration import NLPML_Integration, NLPPrologIntegration
from dialogue import CoachPipeline

# Initialize all brains
nlp = NLPPipeline()
ml = NLPML_Integration()
prolog = NLPPrologIntegration()

# Create unified coach
coach = CoachPipeline(
    nlp_pipeline=nlp,
    ml_integration=ml,
    prolog_integration=prolog,
    gemini_api_key='your_api_key'
)

# Use it!
response = coach.chat("I'm tired but want to train")
```

## 📈 Performance

- **Response Generation**: < 2s (with Gemini API)
- **Fallback Generation**: < 100ms (template-based)
- **Memory Operations**: < 10ms
- **Tone Selection**: < 5ms
- **Template Filling**: < 10ms

## 🔧 Configuration

### Personality Customization
```python
from dialogue import CoachPersonality, PersonalityTraits

custom_traits = PersonalityTraits(
    name="Coach Max",
    archetype="energetic_motivator",
    empathy_level="high",
    strictness_level="high",
    values=["intensity", "discipline", "results"]
)

personality = CoachPersonality(traits=custom_traits)
```

### Memory Customization
```python
memory = ConversationMemory(max_history=100)  # Keep 100 turns
```

### Tone Customization
```python
adapter = ToneAdapter()
# Modify tone profiles in tone_adapter.py
```

## 🎓 Example Responses

### Scenario 1: Tired User
**Input**: "I'm exhausted but want to train legs"
**Tone**: Gentle
**Response**: 
> "I hear you — you're feeling exhausted. That dedication to show up is what builds real progress. Here's what makes sense: light CARDIO for 20 minutes. Your energy is at 20%, so pushing for legs could drain you further. Recovery is part of training. Let's do this smart. You in?"

### Scenario 2: Injured User
**Input**: "My shoulder is hurting"
**Tone**: Serious
**Response**: 
> "⚠️ Hold up — shoulder pain is serious. I'm not taking chances with your health. Here's what we need to do: REST day, ice the area, and monitor the pain. No workout is worth making an injury worse. If it continues, see a professional. Your long-term strength matters way more than today's session."

### Scenario 3: Motivated User
**Input**: "Let's do a killer chest workout!"
**Tone**: Energetic
**Response**: 
> "🔥 THAT'S the energy I love to see! Let's channel this into an amazing chest workout. You're primed for this — high energy means we can push intensity today. I'm thinking 4 sets of compound movements, then we finish strong. You're going to feel amazing after this. Let's make today count!"

## 📝 API Reference

See individual module docstrings for detailed API documentation:
- `dialogue/personality.py` - CoachPersonality API
- `dialogue/tone_adapter.py` - ToneAdapter API
- `dialogue/response_templates.py` - ResponseTemplates API
- `dialogue/dialogue_generator.py` - DialogueGenerator API
- `dialogue/memory_system.py` - ConversationMemory API
- `dialogue/coach_pipeline.py` - CoachPipeline API

## 🔮 Future Enhancements

- [ ] Multi-language support
- [ ] Voice tone parameters for TTS
- [ ] Advanced pattern detection (ML-based)
- [ ] Personalization learning (adapt to individual users)
- [ ] Emotion tracking graphs
- [ ] Integration with real workout tracking apps
- [ ] Coach personality variations (strict, casual, technical)

## 📄 License

Part of AI Fitness Coach system. See main project README.

---

**Coach Atlas**: *"You're not just building muscle — you're building a better version of yourself. Let's do this together."* 💪
