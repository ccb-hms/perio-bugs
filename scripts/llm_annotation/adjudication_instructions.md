# Deciding the taxa-review items left for review

Instructions for an LLM adjudicator. The first taxa review (`taxa_review_instructions.md`) left some
proposed changes open: the reviewer could not settle them from the paper (`check`), or made them depend on
a curator's choice. You decide each one the way the PerioSigDB curators would have. Your decisions are
applied in the corrected build. Curators approve or reject each one later, so every decision needs a short
reason they can check against the paper.

## Input, for each study you are given

- `output/llm_annotation/taxa_review/adjudication/input/Study <ID>_items.csv`: the open items. Columns:
  `item_id`, `action` (what the reviewer proposed: check, add, include, remove, fix_ncbi), `direction`
  (increased/decreased in Group 1, periodontitis), `recorded_taxon` (the curated taxon's NCBI name, or the
  paper's name for an add), `paper_name`, `from_taxid`/`from_name` (the recorded taxon), `to_taxid`/
  `to_name`/`to_rank` (the reviewer's proposed taxon), `reason` (why it was left open), `evidence`, `note`.
- The first review of the whole study, for context: `output/llm_annotation/taxa_review/Study <ID>_review.csv`
  (or `output/llm_annotation/added/taxa_review/` for studies 403, 1032, 1531, 6, 74, 105, 109, 167 and
  added-30710838), and the recorded taxa in the `input/` folder next to it.
- `output/llm_annotation/taxa_review/manifest.csv`: groups chosen by the curators (`group_0`, `group_1`,
  names and sizes), method, `full_text_file`, `supplements_dir` and the curators' source for each
  signature (`source_increased`, `source_decreased`). Studies in the added set are listed in
  `output/llm_annotation/full_text_files.csv`.
- `output/llm_annotation/taxa_review/study_issues.csv`: problems the first review found with a whole study.
- The paper (PDF) and its supplements. Read the parts you need; re-read the evidence yourself rather than
  trusting the first review's quote.

Do not open `output/perio_bugs*`, `output/overview*`, `corrections.csv` or other studies' files.

## How the curators worked

1. One health vs periodontitis comparison per study, chosen by the curators. Never switch to another one.
2. One analysis per study: the one the manifest names as the source. A taxon significant only in another
   analysis of the same groups (prevalence when the curators used abundance, an indicator-species test, a
   subset of the samples sequenced on another platform) is not added. A recorded taxon that comes from such
   an analysis is kept if it is significant there for the curated groups, because the curators chose it.
3. Ranks: phyla, genera and species. A family, order or class only for an OTU the paper names only to that
   rank ("OTU0016 family of Propionibacteriaceae"), not for a test run at that rank.
4. When the paper never tests the curated groups as such (e.g. curated "periodontitis" pools chronic and
   aggressive, but the paper tests each against health), look at what the curators recorded. If it is the
   union of the subgroup-vs-health tests, keep a recorded taxon that is significant, in the recorded
   direction, in any of those tests, and add one they missed from those same tests. If they used only
   one subgroup test, follow that one. Do not use tests between two disease groups, correlations, or a
   multi-group test that gives no direction for health vs disease.
5. A recorded taxon that is not significant for the curated comparison in any analysis the curators could
   have used: remove. If it is significant in the other direction: fix_direction.
6. If two measures of the same test disagree on direction and the curators recorded both, keep both and say
   so. If they recorded one, keep it.
7. NCBI IDs: the organism the paper names, at the paper's rank, with a current ID. If NCBI lacks it (e.g. a
   HOMD oral taxon), use the nearest ancestor at a standard rank (usually the genus). Never use a catch-all
   such as "Streptococcus sp." or "uncultured bacterium". NCBI renames are fine (Hoylesella for Prevotella
   loescheii, Centipeda noxia for Selenomonas noxia). Confirm every new ID is current with
   `curl "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi?db=taxonomy&retmode=json&id=<taxid>"`,
   pausing 0.4 s between calls.
8. If the paper still does not settle it after you have looked (table unreadable, supplement missing), make
   no change: `keep` a recorded taxon, `decline` an add. Say what is missing.
9. If the recorded taxa are not in the paper at all (study_issues.csv says so), search for the source (e.g.
   the companion paper the study cites, via PubMed). If that paper reports them for the same groups, keep
   them and give its PMID. Otherwise remove them.

## Output

Write `output/llm_annotation/taxa_review/decisions/Study <ID>.csv` with exactly one row per item in the
input file and these columns:

- `item_id`: from the input
- `decision`:
  - for add, include, remove and fix_ncbi items: `apply` (make the proposed change) or `decline` (change
    nothing). You can also give a different change instead (e.g. `fix_ncbi` with another ID).
  - for check items: `keep`, `remove`, `fix_direction`, `fix_ncbi`, or `add` (when the item is an unrecorded
    taxon; give `to_taxid`), or `decline` (an unrecorded taxon that should stay out)
- `to_taxid`: only for fix_ncbi or add, or to change the reviewer's ID
- `to_direction`: only if it differs from `direction` (an add in the other direction)
- `rationale`: one or two plain sentences a curator can check, citing page and table or figure. No em
  dashes.
- `decided_by`: `adjudicator`

If you find a further error in the study that no item covers, do not decide it. List it in
`output/llm_annotation/taxa_review/adjudication/Study <ID>_extra.csv` (columns: direction, taxon,
paper_name, problem, evidence).

When done, report per study: counts by decision, and any item you were unsure about.
