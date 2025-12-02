# 🚀 NLP Quick Start Guide

## 30-Second Setup

```bash
# 1. Install dependencies
pip install transformers torch spacy nltk scikit-learn

# 2. Download spaCy model
python -m spacy download en_core_web_sm

# 3. Test the system
python -m nlp.nlp_pipeline
```

---

## 5-Minute Usage

### Basic Analysis

```python
from nlp.nlp_pipeline import NLPPipeline

# Initialize
pipeline = NLPPipeline()

# Analyze message
result = pipeline.analyze("I'm tired but want to train legs")

# Output:
# {
#   'emotion': {'fitness_emotion': 'demotivated', 'energy_level': 'low'},
#   'intent': {'intent': 'plan_workout'},
#   'context': {'muscle_groups': ['legs'], 'physical_state': ['tired']}
# }
```

### Get ML Features

```python
features = pipeline.to_ml_features("I'm pumped for chest day!")
# {'emotion_motivated': 1, 'energy_level': 2, 'target_chest': 1, ...}
```

### Get Prolog Facts

```python
result = pipeline.analyze(
    "I'm exhausted, need rest",
    user_id='user123',
    include_prolog=True
)

# result['prolog_facts']:
# ["emotion('user123', demotivated, '2025-11-12', 0.96).",
#  "intent('user123', rest_request, '2025-11-12', '10:30:00')."]
```

---

## Integration Examples

### With ML Model

```python
from nlp.integration import NLPMLIntegration

integration = NLPMLIntegration(
    ml_model_path='ml/models/saved_models/advanced_xgb_latest'
)

result = integration.chat_based_prediction(
    user_message="My shoulder hurts",
    goal='muscle_gain'
)

# result['adjusted_prediction'] = 'rest'
# result['adjustment_reason'] = 'User reported injury - safety first'
# result['recommendation'] = '⚠️ Safety first! ...'
```

### With Prolog

```python
from nlp.integration import NLPPrologIntegration

integration = NLPPrologIntegration(
    prolog_facts_dir='prolog/facts/nlp'
)

# Save facts to file
integration.save_facts(
    user_message="I want intense chest workout",
    user_id='user123'
)

# Create reasoning rules
integration.create_reasoning_rules()
```

---

## Common Use Cases

### 1. Safety Check

```python
message = "My knee hurts when I squat"
result = pipeline.analyze(message)

if 'injured' in result['context'].get('physical_state', []):
    print("⚠️ Recommend rest and medical consultation")
```

### 2. Motivation Detection

```python
message = "I don't feel like working out"
result = pipeline.analyze(message)

if result['intent']['intent'] == 'motivation_request':
    print("💪 User needs encouragement!")
```

### 3. Context-Aware Recommendation

```python
message = "I'm pumped! Let's do heavy chest!"
result = pipeline.analyze(message)

emotion = result['emotion']['fitness_emotion']  # 'motivated'
muscles = result['context']['muscle_groups']     # ['chest']
intensity = result['context']['intensity']       # 'high'

# Use this to adjust workout recommendation
```

---

## Testing

```bash
# Run all tests
python tests/test_nlp.py

# Test individual modules
python -m nlp.emotion_detector
python -m nlp.intent_classifier
python -m nlp.context_extractor

# Interactive demo
python demo_nlp.py
```

---

## Emotion → Fitness Mapping

| Base Emotion | Fitness Emotion | Energy Level |
|-------------|----------------|--------------|
| joy         | motivated      | high         |
| sadness     | demotivated    | low          |
| anger       | frustrated     | medium       |
| fear        | anxious        | low          |
| neutral     | neutral        | medium       |

---

## Intent Categories

| Intent | Description | Action |
|--------|-------------|--------|
| `plan_workout` | Wants to exercise | Recommend workout |
| `rest_request` | Needs recovery | Suggest rest day |
| `diet_question` | Nutrition query | Provide diet info |
| `motivation_request` | Needs boost | Give encouragement |
| `injury_report` | Pain/injury | Safety first, rest |
| `progress_check` | Check stats | Show progress |
| `update_goal` | Change goal | Update plan |

---

## Context Extraction

### Muscle Groups
- chest, legs, back, arms, shoulders, abs, cardio

### Goals
- muscle_gain, fat_loss, endurance, strength, general_fitness

### Physical State
- sore, tired, energized, injured

### Intensity
- low, medium, high

---

## Performance Tips

1. **First run**: Downloads 329MB emotion model (one-time)
2. **Reuse pipeline**: Create once, use many times
3. **Batch processing**: Use `analyze_batch()` for multiple messages
4. **Skip emotion model**: Set `load_emotion_model=False` for faster initialization (uses rule-based fallback)

---

## Troubleshooting

### Issue: "spaCy model not found"
```bash
python -m spacy download en_core_web_sm
```

### Issue: "Slow first prediction"
First prediction loads the emotion model (~2-3 seconds). Subsequent predictions are fast (~100ms).

### Issue: "Out of memory"
The emotion model requires ~500MB RAM. For resource-constrained environments:
```python
pipeline = NLPPipeline(load_emotion_model=False)
```

---

## API Summary

### NLPPipeline

| Method | Purpose | Returns |
|--------|---------|---------|
| `analyze(text)` | Complete analysis | Dict with emotion, intent, context |
| `analyze_batch(texts)` | Multiple messages | List of dicts |
| `get_summary(text)` | Human-readable | Summary string |
| `to_ml_features(text)` | ML integration | Feature dict |
| `export_json(text, file)` | Save to file | None |

### EmotionDetector

| Method | Purpose | Returns |
|--------|---------|---------|
| `detect(text)` | Detect emotion | Dict with emotion, confidence, energy |
| `is_positive_emotion(text)` | Check positivity | Boolean |
| `get_energy_level(text)` | Get energy | 'low'/'medium'/'high' |

### IntentClassifier

| Method | Purpose | Returns |
|--------|---------|---------|
| `classify(text)` | Classify intent | Dict with intent, confidence |
| `is_action_intent(text)` | Needs action? | Boolean |
| `get_intent_category(text)` | Broad category | 'training'/'nutrition'/'support'/'information' |

### ContextExtractor

| Method | Purpose | Returns |
|--------|---------|---------|
| `extract_all(text)` | All context | Dict with all extracted info |
| `extract_muscle_groups(text)` | Muscles | List of muscles |
| `extract_goal(text)` | Fitness goal | Goal string or None |
| `to_prolog_facts(user, text)` | Prolog facts | List of fact strings |

---

## Resources

- **Documentation**: `nlp/README.md`
- **Tests**: `tests/test_nlp.py`
- **Demo**: `demo_nlp.py`
- **Examples**: See README.md "Examples & Use Cases"

---

## Next Steps

1. ✅ **You are here**: NLP system working
2. 🔄 **Integrate**: Connect with existing ML & Prolog
3. 🎨 **Response Generation**: Build reply generation unit
4. 🚀 **Deploy**: Create API endpoint for chatbot
5. 📊 **Monitor**: Track emotional trends over time

---

**Need help?** Check `nlp/README.md` for detailed documentation!
