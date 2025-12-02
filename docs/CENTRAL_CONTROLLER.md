# Central Controller (Brainstem) 🧠

## Overview

The **Central Controller** is the coordinator that combines outputs from all AI units into one final decision. It acts as the "brainstem" of the AI Fitness Coach system, orchestrating four specialized brains:

```
User Input
    ↓
Central Controller
    ├── NLP Brain (Emotion & Intent)
    ├── Logic Brain (Safety Rules - Prolog)
    ├── ML Brain (Workout Prediction - XGBoost)
    └── Personality Brain (Coach Atlas - Gemini)
    ↓
Final Coach Response
```

## Architecture

### Data Flow

```
1. USER INPUT
   "I'm exhausted but want to train legs"
   
2. NLP BRAIN
   → Emotion: tired (confidence: 0.85)
   → Intent: plan_workout
   → Energy Level: 20%
   → Injury Detection: none
   
3. LOGIC BRAIN (Prolog)
   → Safety Check: CAUTION (low energy)
   → Rules Applied: [low_energy_adjustment]
   → Recommendation: Reduce intensity
   
4. ML BRAIN (XGBoost)
   → Input Features: emotion, energy, context
   → Prediction: CARDIO (76.53% model accuracy)
   → Reasoning: Low energy → gentle cardio safer than legs
   
5. PERSONALITY BRAIN (Coach Atlas)
   → Tone: encouraging_gentle
   → Template: motivational_with_caution
   → Response: "I hear you're tired. Let's do light cardio instead..."
   
6. FINAL DECISION
   → Workout: CARDIO (overridden from LEGS by safety)
   → Safety: caution
   → Response: Coach Atlas reply
   → Confidence: 0.78
```

## Classes

### BrainOutput

Represents output from a single brain.

```python
@dataclass
class BrainOutput:
    brain_name: str           # "NLP", "Logic", "ML", "Personality"
    status: str               # "success", "error", "skipped"
    data: dict                # Brain-specific output data
    execution_time_ms: float  # Processing time
    error_message: str = ""   # Error details if status="error"
```

### CoachDecision

Represents the final coordinated decision.

```python
@dataclass
class CoachDecision:
    nlp_output: BrainOutput
    logic_output: BrainOutput
    ml_output: BrainOutput
    personality_output: BrainOutput
    
    final_response: str            # The coach's reply to user
    workout_recommendation: str    # "CHEST", "LEGS", "REST", etc.
    safety_status: str             # "safe", "caution", "danger"
    confidence_score: float        # 0.0 - 1.0
    decision_path: list           # ["NLP", "Logic", "ML", "Personality"]
    total_execution_time_ms: float
    timestamp: datetime
```

### CentralController

The main orchestrator class.

```python
class CentralController:
    def __init__(self, nlp_brain=None, logic_brain=None, 
                 ml_brain=None, personality_brain=None):
        """
        Initialize with brain instances.
        If brains are None, uses fallback logic.
        """
    
    def process(self, user_message: str, user_data: dict = None, 
                context: dict = None) -> CoachDecision:
        """
        Main processing pipeline.
        Coordinates all brains and returns final decision.
        """
    
    def chat(self, message: str) -> str:
        """Simple chat interface - returns just the response text."""
    
    def get_stats(self) -> dict:
        """Get processing statistics."""
    
    def get_decision_history(self, limit: int = 10) -> list:
        """Get recent decisions."""
```

## Usage

### Basic Usage

```python
from central_controller import CentralController

# Create controller (with fallback logic)
controller = CentralController()

# Process user input
decision = controller.process("I want to train chest today!")

print(f"Workout: {decision.workout_recommendation}")
print(f"Safety: {decision.safety_status}")
print(f"Response: {decision.final_response}")
print(f"Time: {decision.total_execution_time_ms}ms")
```

### Full Integration (All Brains)

```python
from nlp.nlp_pipeline import NLPPipeline
from nlp.ml_integration import NLPMLIntegration
from nlp.prolog_integration import NLPPrologIntegration
from dialogue.coach_pipeline import CoachPipeline
from central_controller import CentralController

# Initialize all brains
nlp_brain = NLPPipeline()
ml_brain = NLPMLIntegration(nlp_brain)
logic_brain = NLPPrologIntegration(nlp_brain)
personality_brain = CoachPipeline()

# Create controller with all brains
controller = CentralController(
    nlp_brain=nlp_brain,
    logic_brain=logic_brain,
    ml_brain=ml_brain,
    personality_brain=personality_brain
)

# Process with full AI
decision = controller.process(
    user_message="I'm exhausted but want to workout",
    user_data={"age": 25, "fitness_level": "intermediate"},
    context={"last_workout": "LEGS", "days_since_last": 1}
)

# All 4 brains contributed!
print(f"Decision Path: {' → '.join(decision.decision_path)}")
# Output: "Decision Path: NLP → Logic → ML → Personality"
```

### Chat Interface

```python
controller = CentralController()

# Simple back-and-forth
response1 = controller.chat("What workout should I do?")
response2 = controller.chat("I'm feeling tired today")
response3 = controller.chat("My shoulder hurts")

print(response1)  # Just the text response
```

### Statistics Tracking

```python
# After processing multiple decisions
stats = controller.get_stats()

print(f"Total Decisions: {stats['total_decisions']}")
print(f"Average Time: {stats['average_response_time_ms']}ms")
print(f"NLP Calls: {stats['nlp_calls']}")
print(f"Logic Calls: {stats['logic_calls']}")
print(f"ML Calls: {stats['ml_calls']}")
print(f"Personality Calls: {stats['personality_calls']}")
print(f"Safety Interventions: {stats['safety_interventions']}")
```

### Decision History

```python
# Get last 5 decisions
history = controller.get_decision_history(limit=5)

for decision in history:
    print(f"{decision.timestamp}: {decision.workout_recommendation}")
    print(f"  Brains: {decision.decision_path}")
    print(f"  Safety: {decision.safety_status}")
```

## Brain-Specific Details

### 1. NLP Brain

**Input:** User message (string)

**Output:**
```python
{
    "emotion": "tired",
    "emotion_confidence": 0.85,
    "intent": "plan_workout",
    "intent_confidence": 0.92,
    "energy_level": 20,  # 0-100
    "injury_keywords": ["shoulder", "pain"],
    "context_factors": ["exhausted", "workout"]
}
```

**Execution Time:** ~50ms (DistilRoBERTa inference)

### 2. Logic Brain (Prolog)

**Input:** NLP output + user data

**Output:**
```python
{
    "safety_status": "caution",
    "rules_triggered": ["low_energy_adjustment"],
    "recommendation": "Reduce intensity or rest",
    "prolog_facts": ["energy(20)", "emotion(tired)", ...],
    "logic_reasoning": "Energy below 30% threshold"
}
```

**Execution Time:** ~20ms (Prolog query execution)

### 3. ML Brain (XGBoost)

**Input:** NLP output + user data + logic output

**Output:**
```python
{
    "predicted_workout": "CARDIO",
    "confidence": 0.76,
    "features_used": 34,
    "model_accuracy": 76.53,
    "alternative_workouts": ["REST", "YOGA"],
    "reasoning": "Low energy detected - gentle cardio recommended"
}
```

**Execution Time:** ~30ms (XGBoost prediction)

### 4. Personality Brain (Coach Atlas)

**Input:** All previous outputs + conversation history

**Output:**
```python
{
    "response": "I hear you're tired, but great job...",
    "tone": "encouraging_gentle",
    "template_used": "motivational_with_caution",
    "gemini_used": True,
    "conversation_turn": 3
}
```

**Execution Time:** ~150ms (Gemini API call) or ~5ms (template fallback)

## Performance

**Target:** <150ms total response time

**Actual (with all brains):**
- NLP Brain: ~50ms
- Logic Brain: ~20ms
- ML Brain: ~30ms
- Personality Brain: ~150ms (Gemini) or ~5ms (template)
- Total: ~250ms (Gemini) or ~105ms (template)

**Optimization Opportunities:**
1. Async brain execution (NLP, Logic, ML can run in parallel)
2. Cache Gemini responses for similar queries
3. Batch processing for multiple users

## Error Handling

The Central Controller has robust error handling:

### Brain Failures
```python
# If NLP brain fails
decision = controller.process("Hello")
print(decision.nlp_output.status)  # "error"
print(decision.nlp_output.error_message)  # "NLP model not loaded"

# Other brains continue with fallback
print(decision.final_response)  # Still gets a response!
```

### Graceful Degradation
```python
# Works with 0, 1, 2, 3, or 4 brains initialized
controller1 = CentralController()  # No brains - pure fallback
controller2 = CentralController(nlp_brain=nlp)  # Just NLP
controller3 = CentralController(nlp_brain=nlp, logic_brain=logic)  # NLP + Logic
# ... etc
```

### Safety Overrides
```python
# Logic brain can override ML predictions
user_msg = "I have a broken arm but want to train"

decision = controller.process(user_msg)

# ML might predict: ARMS
# Logic overrides to: REST (due to injury)
print(decision.workout_recommendation)  # "REST"
print(decision.safety_status)  # "danger"
```

## Decision Paths

Different scenarios create different decision paths:

### Path 1: All Brains (Normal)
```
User: "I want to train chest today!"
Path: NLP → Logic → ML → Personality
Result: Safe workout with personalized response
```

### Path 2: Safety Override
```
User: "My back is injured but I'll push through"
Path: NLP → Logic (STOP) → Personality
Result: ML skipped, safety enforced
```

### Path 3: Low Confidence
```
User: "Maybe workout? Or not? Idk..."
Path: NLP (unclear) → Personality
Result: Logic & ML skipped, asks for clarification
```

### Path 4: Fallback Mode
```
User: "Hello"
Path: (no brains initialized) → Fallback
Result: Generic response, no AI processing
```

## Integration with Frontend

### REST API Example

```python
from flask import Flask, request, jsonify
from central_controller import CentralController

app = Flask(__name__)
controller = CentralController()  # Initialize once

@app.route('/chat', methods=['POST'])
def chat():
    data = request.json
    message = data.get('message')
    user_data = data.get('user_data', {})
    
    decision = controller.process(message, user_data)
    
    return jsonify({
        'response': decision.final_response,
        'workout': decision.workout_recommendation,
        'safety': decision.safety_status,
        'confidence': decision.confidence_score,
        'time_ms': decision.total_execution_time_ms
    })

if __name__ == '__main__':
    app.run(port=5000)
```

### WebSocket Example

```python
import asyncio
from websockets import serve
from central_controller import CentralController

controller = CentralController()

async def handle_client(websocket, path):
    async for message in websocket:
        decision = controller.process(message)
        await websocket.send(decision.final_response)

async def main():
    async with serve(handle_client, "localhost", 8765):
        await asyncio.Future()  # run forever

asyncio.run(main())
```

## Testing

### Unit Tests

```python
# tests/test_central_controller.py
import pytest
from central_controller import CentralController

def test_basic_processing():
    controller = CentralController()
    decision = controller.process("Hello")
    
    assert decision.final_response is not None
    assert decision.workout_recommendation is not None
    assert decision.safety_status in ["safe", "caution", "danger"]

def test_statistics():
    controller = CentralController()
    controller.process("Test 1")
    controller.process("Test 2")
    
    stats = controller.get_stats()
    assert stats['total_decisions'] == 2
```

### Integration Tests

See `test_full_integration.py` for comprehensive integration tests with all brains.

## Configuration

### Environment Variables

```bash
# .env file
GEMINI_API_KEY=your_api_key_here
MODEL_PATH=models/xgboost_model.json
PROLOG_KB_PATH=prolog_kb/fitness_coach.pl
```

### Custom Configuration

```python
controller = CentralController(
    nlp_brain=nlp,
    logic_brain=logic,
    ml_brain=ml,
    personality_brain=personality
)

# Configure individual brains
controller.personality_brain.set_tone("motivational")
controller.logic_brain.set_safety_threshold(0.8)
```

## Troubleshooting

### Issue: "NLP analysis error: unhashable type: 'dict'"
**Solution:** Updated in v1.1 - now handles nested dict structures from NLP pipeline

### Issue: Unicode encoding errors on Windows
**Solution:** Add UTF-8 encoding wrapper:
```python
import sys, io
sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8')
```

### Issue: ML brain not contributing
**Check:** Ensure NLP output format matches ML brain expectations
```python
# NLP should return this format:
{
    "emotion_analysis": {...},
    "intent_classification": {...},
    "context_extraction": {...}
}
```

### Issue: Slow response times
**Solutions:**
1. Use template fallback for Personality brain (5ms vs 150ms)
2. Cache frequent queries
3. Run brains in parallel (async)

## Future Enhancements

1. **Async Processing** - Run independent brains in parallel
2. **Caching Layer** - Cache Gemini responses and ML predictions
3. **A/B Testing** - Compare different decision strategies
4. **Explainability** - Detailed reasoning for each decision
5. **User Feedback Loop** - Learn from user responses
6. **Multi-Language Support** - NLP in multiple languages
7. **Voice Integration** - Audio input/output
8. **Workout History Analysis** - Learn user preferences over time

## Version History

- **v1.0** (Current) - Initial Central Controller with 4-brain architecture
- **v1.1** - Enhanced NLP format handling for nested dicts
- **v1.2** (Planned) - Async brain execution

## Credits

- **NLP Brain**: DistilRoBERTa emotion detection (90%+ accuracy)
- **Logic Brain**: SWI-Prolog safety rules
- **ML Brain**: XGBoost 3.1.1 (76.53% accuracy, 34 features)
- **Personality Brain**: Google Gemini 2.5 Flash + Coach Atlas templates

## License

MIT License - See LICENSE file for details
