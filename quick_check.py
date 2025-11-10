import re
from pathlib import Path

kb = Path('kb_generated.pl')
if not kb.exists():
    print('kb_generated.pl not found in current directory')
    raise SystemExit(1)

entity_re = re.compile(r"^\s*entity\(\s*(e_ex_[0-9]{3})\s*,\s*exercise", re.I)
sub_re = re.compile(r"substitution_for")
contra_re = re.compile(r"contraindicated_for\s*,\s*cond_knee|contraindicated_for\s*,\s*cond_knee\)|contraindicated_for\s*,\s*cond_knee\.", re.I)
# simpler: look for 'contraindicated_for' and 'cond_knee' on same line

cnt_entities = 0
cnt_sub = 0
cnt_knee = 0

with kb.open('r', encoding='utf-8') as f:
    for line in f:
        if entity_re.search(line):
            cnt_entities += 1
        if sub_re.search(line):
            cnt_sub += 1
        if 'contraindicated_for' in line and 'cond_knee' in line:
            cnt_knee += 1

print('exercise entity count:', cnt_entities)
print('substitution_for count (lines):', cnt_sub)
print('knee contraindication lines:', cnt_knee)

# Print a few example lines for manual inspection
print('\nSample substitution lines:')
with kb.open('r', encoding='utf-8') as f:
    for i, l in enumerate(f):
        if 'substitution_for' in l:
            print(l.strip())
            if i>20:
                break

print('\nSample contraindication lines (knee):')
with kb.open('r', encoding='utf-8') as f:
    for i, l in enumerate(f):
        if 'contraindicated_for' in l and 'cond_knee' in l:
            print(l.strip())
            if i>20:
                break
