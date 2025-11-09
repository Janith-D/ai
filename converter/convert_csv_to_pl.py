#!/usr/bin/env python3
"""
convert_csv_to_pl.py - enhanced converter to emit Prolog facts (.pl) from local data dumps.

Capabilities:
- Convert CSV exercise tables to Prolog facts (keeps original simple behaviour).
- Parse ConceptNet JSON-lines dump (local file) and emit normalized facts with provenance.
- Parse Wikidata JSON dump (local file with one JSON object per line) and emit normalized facts.

Usage examples:
  python convert_csv_to_pl.py --csv-dir converter/data --out ai/kb_generated.pl
  python convert_csv_to_pl.py --conceptnet data/conceptnet-assertions-5.7.0.jsonl --out ai/kb_conceptnet.pl
  python convert_csv_to_pl.py --wikidata data/wikidata-2025-xx.json --out ai/kb_wikidata.pl

Notes:
- This script does NOT download any external dumps; place the raw files locally and pass their paths.
- For large dumps, the script streams and writes incrementally; it may take time and disk space.
"""
import argparse
import csv
import json
import re
from pathlib import Path
from typing import Optional


def sanitize_atom(s: str) -> str:
    """Return a Prolog-friendly atom identifier (lowercase, alnum and underscores).

    Examples: 'Barbell Back Squat' -> barbell_back_squat
              'Q12345' -> q12345
    """
    if s is None:
        return 'unknown'
    s2 = s.strip()
    # replace non-alphanumeric with underscore
    s2 = re.sub(r"[^0-9A-Za-z]+", '_', s2)
    s2 = re.sub(r"_+", '_', s2)
    s2 = s2.strip('_')
    if s2 == '':
        return 'unknown'
    return s2.lower()


def prolog_quote(s: str) -> str:
    if s is None:
        return "''"
    return "'" + s.replace("'", "\\'") + "'"


def emit_entity(f, id_, type_, label):
    f.write(f"entity({id_}, {type_}, {prolog_quote(label)}).\n")


def emit_fact(f, subj, pred, obj, provenance: Optional[str] = None, confidence: Optional[float] = None):
    f.write(f"fact({subj}, {pred}, {obj}).\n")
    if provenance:
        f.write(f"confidence(fact({subj}, {pred}, {obj}), {prolog_quote(provenance)}, {confidence or 0.0}).\n")


def process_csvs(csv_dir: Path, out_f):
    # exercises.csv
    ex_file = csv_dir / 'exercises.csv'
    if ex_file.exists():
        with open(ex_file, newline='', encoding='utf-8') as csvfile:
            r = csv.DictReader(csvfile)
            for row in r:
                raw_id = row['id']
                id_ = sanitize_atom(raw_id)
                name = row.get('name', '')
                req = row.get('requires_equipment', 'eq_none') or 'eq_none'
                diff = row.get('difficulty', 'unknown')
                emit_entity(out_f, id_, 'exercise', name)
                emit_fact(out_f, id_, 'requires_equipment', sanitize_atom(req))
                out_f.write(f"attr({id_}, difficulty, {sanitize_atom(diff)}).\n")

    uses_file = csv_dir / 'uses_muscle.csv'
    if uses_file.exists():
        with open(uses_file, newline='', encoding='utf-8') as csvfile:
            r = csv.DictReader(csvfile)
            for row in r:
                emit_fact(out_f, sanitize_atom(row['exercise_id']), 'uses_muscle', sanitize_atom(row['muscle_id']))

    ci_file = csv_dir / 'contraindications.csv'
    if ci_file.exists():
        with open(ci_file, newline='', encoding='utf-8') as csvfile:
            r = csv.DictReader(csvfile)
            for row in r:
                emit_fact(out_f, sanitize_atom(row['exercise_id']), 'contraindicated_for', sanitize_atom(row['condition_id']))


def process_conceptnet(conceptnet_path: Path, out_f, lang: str = 'en'):
    """Process ConceptNet JSON-lines (assertions) file.

    Each line in ConceptNet's raw dump is a JSON object with fields like 'start', 'end', 'rel', 'surfaceText'.
    We create entities for start/end (preferring labels if available) and facts like fact(start_id, rel, end_id).
    """
    if not conceptnet_path.exists():
        print('ConceptNet file not found:', conceptnet_path)
        return
    with open(conceptnet_path, 'r', encoding='utf-8') as fh:
        for i, line in enumerate(fh):
            line = line.strip()
            if not line:
                continue
            try:
                obj = json.loads(line)
            except Exception:
                continue
            # start/end may be dicts or strings
            start = obj.get('start') or obj.get('start')
            end = obj.get('end') or obj.get('end')
            rel = obj.get('rel') or obj.get('rel')

            def get_label(x):
                if isinstance(x, dict):
                    # Common fields: '@id', 'label', 'language'
                    lab = x.get('label') or x.get('@id')
                    return lab
                return x

            start_label = get_label(start)
            end_label = get_label(end)
            rel_label = get_label(rel)

            if isinstance(start_label, dict):
                start_label = start_label.get('label') or start_label.get('@id')
            if isinstance(end_label, dict):
                end_label = end_label.get('label') or end_label.get('@id')
            if isinstance(rel_label, dict):
                rel_label = rel_label.get('label') or rel_label.get('@id')

            if not start_label or not end_label or not rel_label:
                continue

            sid = 'concept_' + sanitize_atom(str(start_label))
            eid = 'concept_' + sanitize_atom(str(end_label))
            rpred = 'rel_' + sanitize_atom(str(rel_label))

            emit_entity(out_f, sid, 'concept', str(start_label))
            emit_entity(out_f, eid, 'concept', str(end_label))
            emit_fact(out_f, sid, rpred, eid, provenance='conceptnet')


def process_wikidata(wikidata_path: Path, out_f):
    """Process a Wikidata JSON dump (one JSON object per line).

    This function extracts the entity id, English label (if any), and primary claims. It writes entity/3
    and fact(entity, prop_Pxxx, value) for simple statements. For large dumps, stream line-by-line.
    """
    if not wikidata_path.exists():
        print('Wikidata file not found:', wikidata_path)
        return
    with open(wikidata_path, 'r', encoding='utf-8') as fh:
        for i, line in enumerate(fh):
            line = line.strip()
            if not line:
                continue
            try:
                obj = json.loads(line)
            except Exception:
                continue
            qid = obj.get('id')
            if not qid:
                continue
            labels = obj.get('labels', {})
            en_label = labels.get('en', {}).get('value') if isinstance(labels, dict) else None
            ent_id = 'q' + sanitize_atom(qid)
            emit_entity(out_f, ent_id, 'wikientity', en_label or qid)

            claims = obj.get('claims', {})
            for prop, statements in claims.items():
                for st in statements:
                    mainsnak = st.get('mainsnak', {})
                    datavalue = mainsnak.get('datavalue')
                    if not datavalue:
                        continue
                    dv = datavalue.get('value')
                    if isinstance(dv, dict) and 'id' in dv:
                        val = 'q' + sanitize_atom(dv['id'])
                        emit_fact(out_f, ent_id, sanitize_atom(prop), val, provenance='wikidata')
                    elif isinstance(dv, str):
                        emit_fact(out_f, ent_id, sanitize_atom(prop), prolog_quote(dv), provenance='wikidata')
                    elif isinstance(dv, dict) and 'time' in dv:
                        emit_fact(out_f, ent_id, sanitize_atom(prop), prolog_quote(dv['time']), provenance='wikidata')


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--csv-dir', type=str, help='Directory with CSVs (exercises.csv, uses_muscle.csv, contraindications.csv)')
    parser.add_argument('--conceptnet', type=str, help='Path to ConceptNet assertions JSONL file (local)')
    parser.add_argument('--wikidata', type=str, help='Path to Wikidata JSON dump (local, one JSON per line)')
    parser.add_argument('--out', type=str, default='kb_generated.pl', help='Output .pl file')
    args = parser.parse_args()

    out_path = Path(args.out)
    out_path.parent.mkdir(parents=True, exist_ok=True)

    with open(out_path, 'w', encoding='utf-8') as out_f:
        out_f.write('% Auto-generated KB\n')
        if args.csv_dir:
            process_csvs(Path(args.csv_dir), out_f)
        if args.conceptnet:
            process_conceptnet(Path(args.conceptnet), out_f)
        if args.wikidata:
            process_wikidata(Path(args.wikidata), out_f)

    print(f'Wrote {out_path}')


if __name__ == '__main__':
    main()
