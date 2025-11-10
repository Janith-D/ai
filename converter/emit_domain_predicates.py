"""
Small utility to read a Prolog .pl file that contains generic `fact/3`, `attr/3`, and `entity/3`
lines and emit a new .pl file with explicit domain predicates.

Usage:
  python emit_domain_predicates.py --in kb_generated.pl --out kb_generated_domain.pl

It preserves `entity/3` lines and converts:
  fact(ID, uses_muscle, m_quadriceps). -> uses_muscle(ID, m_quadriceps).
  fact(ID, requires_equipment, eq_barbell). -> requires_equipment(ID, eq_barbell).
  fact(ID, contraindicated_for, cond_knee). -> contraindicated_for(ID, cond_knee).
  fact(ID, substitution_for, Other). -> substitution_for(ID, Other).
  attr(ID, difficulty, easy). -> difficulty(ID, easy).

The script is intentionally simple (line-based parsing) and robust to spacing.
"""
import re
import argparse
from pathlib import Path

FACT_RE = re.compile(r"^\s*fact\s*\(\s*([^,]+)\s*,\s*([^,]+)\s*,\s*([^\)]+)\s*\)\s*\.?\s*$")
ATTR_RE = re.compile(r"^\s*attr\s*\(\s*([^,]+)\s*,\s*([^,]+)\s*,\s*([^\)]+)\s*\)\s*\.?\s*$")
ENTITY_RE = re.compile(r"^\s*entity\s*\(.*")

MAPPING = {
    'uses_muscle': 'uses_muscle',
    'requires_equipment': 'requires_equipment',
    'contraindicated_for': 'contraindicated_for',
    'substitution_for': 'substitution_for',
}


def normalize_atom(atom_text: str) -> str:
    """Strip whitespace and trailing dots from an atom token."""
    return atom_text.strip().rstrip('.')


def convert_line(line: str) -> str:
    m = FACT_RE.match(line)
    if m:
        id_tok = normalize_atom(m.group(1))
        rel_tok = normalize_atom(m.group(2))
        obj_tok = normalize_atom(m.group(3))
        # remove surrounding quotes for label-like objects
        if rel_tok in MAPPING:
            pred = MAPPING[rel_tok]
            return f"{pred}({id_tok},{obj_tok}).\n"
        else:
            # keep generic fact as-is
            return line
    m2 = ATTR_RE.match(line)
    if m2:
        id_tok = normalize_atom(m2.group(1))
        key_tok = normalize_atom(m2.group(2))
        val_tok = normalize_atom(m2.group(3))
        # emit key(ID, Value).
        return f"{key_tok}({id_tok},{val_tok}).\n"
    # preserve entity and other lines unchanged
    if ENTITY_RE.match(line) or line.strip().startswith('%') or line.strip()=='' or line.strip().startswith(':-'):
        return line
    # fallback: return line unchanged
    return line


def process(in_path: Path, out_path: Path):
    # Write safe discontiguous directives at the top of the emitted file so
    # predicate clauses that are emitted in interleaved order don't trigger
    # Prolog warnings when the file is consulted.
    directives = [
        ':- discontiguous fact/3.\n',
        ':- discontiguous attr/3.\n',
        ':- discontiguous entity/3.\n',
        ':- discontiguous uses_muscle/2.\n',
        ':- discontiguous requires_equipment/2.\n',
        ':- discontiguous difficulty/2.\n',
        ':- discontiguous contraindicated_for/2.\n',
        ':- discontiguous substitution_for/2.\n',
        '\n'
    ]
    with in_path.open('r', encoding='utf-8') as fin, out_path.open('w', encoding='utf-8') as fout:
        # Always emit the directives first (idempotent and safe for facts-only KBs)
        for d in directives:
            fout.write(d)
        for line in fin:
            try:
                out_line = convert_line(line)
            except Exception:
                out_line = line
            fout.write(out_line)


def main():
    parser = argparse.ArgumentParser(description='Emit domain predicates from generic fact/attr style .pl')
    parser.add_argument('--in', dest='infile', required=True, help='Input .pl file')
    parser.add_argument('--out', dest='outfile', required=True, help='Output .pl file')
    args = parser.parse_args()
    in_path = Path(args.infile)
    out_path = Path(args.outfile)
    if not in_path.exists():
        print(f"Input not found: {in_path}")
        raise SystemExit(1)
    process(in_path, out_path)
    print(f"Wrote {out_path}")


if __name__ == '__main__':
    main()
