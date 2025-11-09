# Fitness AI - Prolog starter

This folder contains a small Prolog knowledge base (`kb.pl`) and example queries/tests (`queries.pl`) for a fitness-coach AI prototype.

Files added:
- `kb.pl` - sample entities, facts and rules (fitness domain)
- `queries.pl` - example queries and `plunit` tests
- `converter/convert_csv_to_pl.py` - Python converter skeleton to emit `.pl` facts from CSVs
- `requirements.txt` - minimal Python deps for the converter

Quick start (Windows PowerShell):

1) Install SWI-Prolog (https://www.swi-prolog.org/) and ensure `swipl` is on PATH.

2) Open PowerShell and start SWI-Prolog:

```powershell
swipl
```

3) Load the KB and run example queries or tests:

```powershell
?- ["d:/MY IDEA/fitnessApp/ai/kb.pl"].
?- ["d:/MY IDEA/fitnessApp/ai/queries.pl"].
?- example_queries.
?- run_tests.
```

Notes:
- The converter is a skeleton; place CSV files under `converter/data/` and run the script to generate `kb_generated.pl`.
- Expand `kb.pl` with more facts (exercises, muscles, contraindications) and keep general-purpose rules in the same file or separate `rules.pl` as the KB grows.

Converter usage for real dumps
--------------------------------
- ConceptNet: download the assertions file (JSON-lines). Place it locally, then run:

```powershell
python .\converter\convert_csv_to_pl.py --conceptnet D:\path\to\conceptnet-assertions-*.jsonl --out d:\MY IDEA\fitnessApp\ai\kb_conceptnet.pl
```

- Wikidata: download the JSON dump (one entity per line) or an extracted subset. Then run:

```powershell
python .\converter\convert_csv_to_pl.py --wikidata D:\path\to\wikidata_subset.json --out d:\MY IDEA\fitnessApp\ai\kb_wikidata.pl
```

Notes and limitations
- This script expects local files; it does not call external APIs. For full Wikidata or ConceptNet downloads, get files from the project websites (ConceptNet: https://conceptnet.io/; Wikidata dumps: https://dumps.wikimedia.org/wikidatawiki/). Large files require sufficient disk space and time to process.
- The converter normalizes labels into Prolog-friendly atom IDs and emits `entity/3`, `fact/3` and simple `confidence/3` provenance entries. Review generated facts and add domain-specific mappings (e.g., mapping ConceptNet relations to your KB predicates).
- If you want, I can add a mapping config file (YAML) so you can control how properties map to `fact(..., predicate, ...)` and what predicates to emit for specific relations.

If you want, I can populate `kb.pl` with more exercises (50+) and show an automated way to generate many facts from CSV or ConceptNet dumps.
