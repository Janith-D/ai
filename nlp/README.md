# 🧠 Language & Emotion Understanding Unit

## Documentation for AI Fitness Coach NLP Module

---

## 📋 Table of Contents

1. [Overview](#overview)
2. [Architecture](#architecture)
3. [Components](#components)
4. [Installation](#installation)
5. [Usage Examples](#usage-examples)
6. [Integration with ML & Prolog](#integration)
7. [API Reference](#api-reference)
8. [Performance](#performance)
9. [Future Enhancements](#future-enhancements)

---

## Overview

The **Language & Emotion Understanding Unit** is the emotional brain of the AI Fitness Coach. It makes the AI feel human-like by understanding:

- ❤️ **What users feel** (tired, motivated, frustrated)
- 🎯 **What users want** (workout, rest, diet advice)
- 🔍 **Why users say things** (context: muscle groups, goals, intensity)

This enables empathetic, context-aware responses instead of robotic replies.

### Key Features

- **Emotion Detection**: 7 emotions mapped to fitness context (motivated, demotivated, frustrated, etc.)
- **Intent Recognition**: 9 intents (plan_workout, rest_request, diet_question, etc.)
- **Context Extraction**: Muscle groups, goals, intensity, physical state
- **ML Integration**: Convert NLP insights to ML features
- **Prolog Integration**: Generate logical facts for reasoning
- **Pretrained Models**: Uses DistilRoBERTa for emotion detection

---

## Architecture

```
User Message
     ↓
┌────────────────────────┐
│  Text Preprocessing    │
│  (spaCy)               │
└────────────────────────┘
     ↓
┌────────────────────────┐
│  Emotion Detection     │
│  (DistilRoBERTa)       │
└────────────────────────┘
     ↓
┌────────────────────────┐
│  Intent Classification │
│  (Rule-based)          │
└────────────────────────┘
     ↓
┌────────────────────────┐
│  Context Extraction    │
│  (NER + Keywords)      │
└────────────────────────┘
     ↓
┌────────────────────────┐
│  Unified Output        │
│  (JSON)                │
└────────────────────────┘
     ↓
┌─────────────┬──────────────┐
│  ML Model   │  Prolog KB   │
└─────────────┴──────────────┘
```

---

## Components

### 1. **TextPreprocessor** (`nlp/preprocessor.py`)

Cleans and tokenizes text using spaCy.

**Features:**
- Tokenization & lemmatization
- Stop word removal (optional)
- Special character handling
- Named entity recognition

**Example:**
```python
from nlp.preprocessor import get_preprocessor

preprocessor = get_preprocessor()
cleaned = preprocessor.preprocess("I'm exhausted but still want to train!")
# Output: "I be exhausted but still want to train"
```

---

### 2. **EmotionDetector** (`nlp/emotion_detector.py`)

Detects emotional state using pretrained DistilRoBERTa model.

**Detected Emotions:**
- `joy` → `motivated` (high energy)
- `sadness` → `demotivated` (low energy)
- `anger` → `frustrated` (medium energy)
- `fear` → `anxious` (low energy)
- `neutral` → `neutral` (medium energy)

**Example:**
```python
from nlp.emotion_detector import EmotionDetector

detector = EmotionDetector()
result = detector.detect("I'm so pumped for chest day!")

# Output:
{
    'emotion': 'joy',
    'fitness_emotion': 'motivated',
    'confidence': 0.96,
    'energy_level': 'high'
}
```

---

### 3. **IntentClassifier** (`nlp/intent_classifier.py`)

Recognizes user intent using rule-based pattern matching.

**Supported Intents:**
- `plan_workout` - User wants to exercise
- `rest_request` - User needs rest/recovery
- `diet_question` - Nutrition questions
- `progress_check` - Check results/stats
- `motivation_request` - Needs encouragement
- `update_goal` - Change fitness goals
- `injury_report` - Report pain/injury
- `schedule_query` - Questions about timing
- `general_chat` - Casual conversation

**Example:**
```python
from nlp.intent_classifier import IntentClassifier

classifier = IntentClassifier()
result = classifier.classify("I want to train my chest today")

# Output:
{
    'intent': 'plan_workout',
    'confidence': 1.0
}
```

---

### 4. **ContextExtractor** (`nlp/context_extractor.py`)

Extracts fitness-specific information from text.

**Extracted Information:**
- **Muscle groups**: chest, legs, back, arms, shoulders, abs, cardio
- **Goals**: muscle_gain, fat_loss, endurance, strength
- **Time**: today, tomorrow, morning, evening
- **Intensity**: high, medium, low
- **Physical state**: sore, tired, energized, injured
- **Frequency**: "3 times a week"
- **Duration**: "60 minutes"

**Example:**
```python
from nlp.context_extractor import ContextExtractor

extractor = ContextExtractor()
context = extractor.extract_all("I want intense chest workout 4 times a week")

# Output:
{
    'muscle_groups': ['chest'],
    'intensity': 'high',
    'frequency': {'count': 4, 'period': 'week'}
}
```

---

### 5. **NLPPipeline** (`nlp/nlp_pipeline.py`)

Unified pipeline combining all components.

**Example:**
```python
from nlp.nlp_pipeline import NLPPipeline

pipeline = NLPPipeline()
result = pipeline.analyze(
    "I'm exhausted but still want to train my legs",
    user_id='user123',
    include_prolog=True
)

# Output:
{
    'text': 'I\'m exhausted but still want to train my legs',
    'preprocessed': 'I be exhausted but still want to train my leg',
    'emotion': {
        'emotion': 'sadness',
        'fitness_emotion': 'demotivated',
        'confidence': 0.961,
        'energy_level': 'low'
    },
    'intent': {
        'intent': 'plan_workout',
        'confidence': 1.0
    },
    'context': {
        'muscle_groups': ['legs'],
        'physical_state': ['tired']
    },
    'prolog_facts': [
        "emotion('user123', demotivated, '2025-11-12', 0.961).",
        "energy_level('user123', low, '2025-11-12').",
        ...
    ],
    'timestamp': '2025-11-12T10:30:00'
}
```

---

## Installation

### 1. Install Dependencies

```bash
pip install transformers torch spacy nltk scikit-learn
```

### 2. Download spaCy Model

```bash
python -m spacy download en_core_web_sm
```

### 3. First Run (Download Emotion Model)

On first run, the system will automatically download the DistilRoBERTa emotion model (~329MB).

---

## Usage Examples

### Basic Usage

```python
from nlp.nlp_pipeline import NLPPipeline

# Initialize pipeline
pipeline = NLPPipeline()

# Analyze message
result = pipeline.analyze("I'm too tired for legs today, need rest")

# Get summary
summary = pipeline.get_summary("I'm too tired for legs today, need rest")
print(summary)
# Output: "Emotion: demotivated (96%) | Intent: rest_request (80%) | Context: Muscles: legs"
```

### Convert to ML Features

```python
# Get features for ML model
ml_features = pipeline.to_ml_features("I'm pumped for chest day!")

# Output:
{
    'emotion_motivated': 1,
    'emotion_tired': 0,
    'energy_level': 2,  # 0=low, 1=medium, 2=high
    'intent_workout': 1,
    'intent_rest': 0,
    'target_chest': 1,
    'target_legs': 0,
    'preferred_intensity': 2
}
```

### Export to JSON

```python
pipeline.export_json(
    "I want to train chest today",
    filepath='analysis_output.json'
)
```

---

## Integration

### ML Integration

The **NLPMLIntegration** class enriches ML predictions with emotional context.

```python
from nlp.integration import NLPMLIntegration

integration = NLPMLIntegration(
    ml_model_path='ml/models/saved_models/advanced_xgb_latest'
)

result = integration.chat_based_prediction(
    user_message="I'm exhausted but want to train",
    goal='muscle_gain',
    recent_workouts=['chest', 'back', 'rest']
)

# Output includes:
# - NLP analysis
# - Base ML prediction
# - Adjusted prediction based on emotion/intent
# - Explanation & recommendation
```

**Adjustment Rules:**

1. **Explicit rest request** → Force rest prediction
2. **Injury reported** → Force rest for safety
3. **Low energy + no workout intent** → Suggest rest
4. **Sore + wants to train** → Suggest light cardio
5. **User targets specific muscle** → Adjust to preference
6. **High motivation** → Boost confidence

---

### Prolog Integration

The **NLPPrologIntegration** class generates Prolog facts for logical reasoning.

```python
from nlp.integration import NLPPrologIntegration

integration = NLPPrologIntegration(
    prolog_facts_dir='prolog/facts/nlp'
)

# Generate facts
facts = integration.message_to_prolog(
    user_message="My shoulder hurts, need rest",
    user_id='user123'
)

# Save to file
integration.save_facts(
    user_message="My shoulder hurts, need rest",
    user_id='user123',
    filename='user123_nlp.pl'
)

# Create reasoning rules
integration.create_reasoning_rules()
```

**Generated Prolog Facts:**

```prolog
emotion('user123', demotivated, '2025-11-12', 0.85).
energy_level('user123', low, '2025-11-12').
intent('user123', rest_request, '2025-11-12', '14:30:00').
physical_state('user123', injured, '2025-11-12').
target_muscle('user123', shoulders, '2025-11-12').
```

**Reasoning Rules:**

```prolog
% Check if user needs rest
needs_rest(UserID, Date) :-
    physical_state(UserID, injured, Date).

% Check if safe to train
safe_to_train(UserID, Date) :-
    \+ physical_state(UserID, injured, Date),
    energy_level(UserID, Level, Date),
    Level \= low.
```

---

## API Reference

### NLPPipeline Methods

#### `analyze(text, user_id=None, include_prolog=False, timestamp=None)`
Analyze user message through complete pipeline.

**Parameters:**
- `text` (str): User message
- `user_id` (str, optional): User identifier
- `include_prolog` (bool): Generate Prolog facts
- `timestamp` (datetime, optional): Timestamp

**Returns:** Dictionary with complete analysis

---

#### `analyze_batch(texts, user_id=None, include_prolog=False)`
Analyze multiple messages.

**Parameters:**
- `texts` (List[str]): List of messages
- `user_id` (str, optional): User identifier
- `include_prolog` (bool): Generate Prolog facts

**Returns:** List of analysis results

---

#### `get_summary(text)`
Get human-readable summary.

**Returns:** Summary string

---

#### `to_ml_features(text)`
Convert analysis to ML features.

**Returns:** Dictionary of features

---

#### `export_json(text, filepath)`
Export analysis to JSON file.

---

## Performance

### Emotion Detection Accuracy

- **Model**: DistilRoBERTa (j-hartmann/emotion-english-distilroberta-base)
- **Base Emotions**: 7 classes (joy, sadness, anger, fear, neutral, surprise, disgust)
- **Fitness Mapping**: Custom mapping to fitness-specific emotions
- **Confidence**: Typically 70-98%

### Intent Classification Accuracy

- **Method**: Rule-based pattern matching with negative indicators
- **Accuracy**: 100% on test cases (improved from 62.5%)
- **Intents**: 9 classes
- **Edge Cases**: Fixed (motivation requests, injury reports, schedule queries)
- **Fallback**: General chat for uncertain cases

### Processing Speed

- **First message**: ~2-3 seconds (model loading)
- **Subsequent messages**: ~100-200ms per message
- **Batch processing**: ~50ms per message

### Resource Usage

- **Model size**: 329MB (emotion model)
- **Memory**: ~500MB during processing
- **CPU**: Single-threaded, no GPU required

---

## Future Enhancements

### Planned Improvements

1. **Fine-tune Emotion Model**
   - Train on fitness-specific conversations
   - Expected accuracy boost: +5-10%

2. **Upgrade Intent Classifier**
   - Train ML model (BERT-tiny) on labeled dataset
   - Better handling of ambiguous cases

3. **Advanced Context Extraction**
   - Custom NER model for fitness entities
   - Extract sets, reps, weights from text

4. **Multi-turn Conversation**
   - Track conversation history
   - Context-aware follow-up understanding

5. **Sentiment Tracking**
   - Track emotional trends over time
   - Detect motivation patterns

6. **Multilingual Support**
   - Translate to English → Process → Respond in original language
   - Support Spanish, French, German, etc.

---

## Examples & Use Cases

### Example 1: Motivation Boost

**Input:** "I don't feel like working out today"

**NLP Output:**
- Emotion: `demotivated` (96% confidence)
- Intent: `motivation_request`
- Energy: `low`

**AI Response:**
> "💪 I understand you're feeling low. Let's start small - even a 15-minute light workout is better than nothing. You've got this! Remember your goal: muscle gain. Every small step counts."

---

### Example 2: Safety Check

**Input:** "My shoulder hurts but I want to train chest"

**NLP Output:**
- Emotion: `neutral`
- Intent: `plan_workout`
- Context: `injured`, `shoulders`, `chest`

**AI Response:**
> "⚠️ Safety first! I noticed you mentioned shoulder pain. Chest exercises often involve shoulders. I strongly recommend a rest day or consulting a professional. Your long-term progress is more important than one workout."

---

### Example 3: Context-Aware Adjustment

**Input:** "I'm super pumped! Let's do heavy legs!"

**NLP Output:**
- Emotion: `motivated` (97% confidence)
- Intent: `plan_workout`
- Context: `legs`, `high intensity`
- Energy: `high`

**AI Response:**
> "🔥 Love the energy! Great day for legs. I recommend a heavy intensity leg workout with squats, deadlifts, and lunges. Your motivation is at 97% - let's channel that into crushing your sets!"

---

## Testing

Run comprehensive tests:

```bash
python tests/test_nlp.py
```

Test individual components:

```bash
python -m nlp.emotion_detector
python -m nlp.intent_classifier
python -m nlp.context_extractor
python -m nlp.nlp_pipeline
```

---

## Architecture Diagram

```
┌─────────────────────────────────────────────────────────────┐
│                    AI FITNESS COACH                         │
│                                                             │
│  ┌───────────────────────────────────────────────────────┐ │
│  │          LANGUAGE & EMOTION UNDERSTANDING             │ │
│  │                                                       │ │
│  │  ┌──────────────┐  ┌──────────────┐  ┌────────────┐ │ │
│  │  │   Emotion    │  │    Intent    │  │  Context   │ │ │
│  │  │  Detection   │  │Classification│  │ Extraction │ │ │
│  │  └──────────────┘  └──────────────┘  └────────────┘ │ │
│  │         │                  │                 │       │ │
│  │         └──────────────────┴─────────────────┘       │ │
│  │                           │                          │ │
│  │                   ┌───────▼────────┐                 │ │
│  │                   │  NLP Pipeline  │                 │ │
│  │                   └───────┬────────┘                 │ │
│  └───────────────────────────┼──────────────────────────┘ │
│                              │                            │
│         ┌────────────────────┼────────────────────┐       │
│         │                    │                    │       │
│    ┌────▼─────┐       ┌──────▼──────┐      ┌─────▼────┐  │
│    │ ML Model │       │   Prolog    │      │ Response │  │
│    │ (76.53%) │       │  Knowledge  │      │Generator │  │
│    │ XGBoost  │       │    Base     │      │          │  │
│    └──────────┘       └─────────────┘      └──────────┘  │
│                                                           │
└───────────────────────────────────────────────────────────┘
```

---

## File Structure

```
nlp/
├── __init__.py
├── preprocessor.py           # Text cleaning & tokenization
├── emotion_detector.py       # Emotion detection (DistilRoBERTa)
├── intent_classifier.py      # Intent recognition (rule-based)
├── context_extractor.py      # Context extraction (NER + keywords)
├── nlp_pipeline.py          # Unified pipeline
└── integration/
    ├── __init__.py
    ├── nlp_ml_integration.py     # ML integration
    └── nlp_prolog_integration.py # Prolog integration

tests/
└── test_nlp.py              # Comprehensive test suite

prolog/
└── facts/
    └── nlp/                  # Generated Prolog facts
        ├── user123_nlp.pl
        └── nlp_reasoning_rules.pl
```

---

## Credits

- **Emotion Model**: [j-hartmann/emotion-english-distilroberta-base](https://huggingface.co/j-hartmann/emotion-english-distilroberta-base)
- **NLP Library**: spaCy
- **ML Integration**: XGBoost (76.53% accuracy)
- **Logic Reasoning**: SWI-Prolog

---

## License

MIT License - See LICENSE file for details

---

## Contact & Support

For questions, issues, or contributions:
- GitHub Issues: [fitnessApp/issues]
- Email: [your-email@example.com]

---

**Built with ❤️ for empathetic AI fitness coaching**
