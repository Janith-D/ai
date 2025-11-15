"""
Quick test of Coach Atlas with Gemini API
"""

from dialogue import CoachPipeline
import os

# Set API key
os.environ['GEMINI_API_KEY'] = 'AIzaSyDY5iz2caJBLt0qux4SqeGk7aFTFccD42E'

# Create coach
coach = CoachPipeline(enable_memory=True)

print("\n" + "=" * 80)
print("🎭 COACH ATLAS - QUICK TEST")
print("=" * 80 + "\n")

# Test messages
messages = [
    "I want to train chest",
    "I'm feeling tired",
    "My shoulder hurts"
]

for msg in messages:
    print(f"👤 User: {msg}")
    response = coach.chat(msg)
    print(f"🤖 Coach: {response[:200]}...")
    print("-" * 80 + "\n")

print("✅ All tests completed successfully!")
print("=" * 80)
