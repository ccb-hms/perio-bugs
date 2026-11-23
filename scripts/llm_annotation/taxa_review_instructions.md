# Review of each signature's recorded taxa against the paper

Instructions for an LLM reviewer that checks, for one PerioSigDB study at a time, every taxon recorded in
the study's signatures against the paper. The curated data are not changed: the review writes one CSV per
study, and the results are left for curators.

PerioSigDB keeps one health vs periodontitis comparison per study, as up to two signatures: the taxa
increased and the taxa decreased in periodontitis (Group 1) vs the comparison group (Group 0, usually
periodontal health), in subgingival plaque.

## Input

- `output/llm_annotation/taxa_review/manifest.csv`: one row per study with its PMID, title, the curated
  groups (`group_0`, `group_1`, names and sizes), the detection method, the full text (`full_text_file`),
  the supplements folder (`supplements_dir`, may be empty) and where the curators took each signature from
  (`source_increased`, `source_decreased`, may be empty).
- `output/llm_annotation/taxa_review/input/Study <ID>_recorded_taxa.csv`: the recorded taxa, one row per
  taxon and direction: `direction` (increased/decreased in Group 1), `recorded_taxon` (the NCBI name the
  curated taxon was mapped to), `ncbi_taxid`, `metaphlan_name` (full lineage).

The curators chose the comparison; use the curated groups to find it in the paper and do not propose a
different one. The recorded names are current NCBI names, so they can differ from the paper's names
(e.g. *Tannerella forsythia* for *Bacteroides forsythus*, *Schaalia* for some *Actinomyces*, *Segatella*
for some *Prevotella*, *Metamycoplasma* for some *Mycoplasma*): match them through synonyms.

Do NOT open `output/perio_bugs*`, `output/overview*` or other review outputs; the inputs above are all you
need besides the paper.

## What belongs in a signature (BugSigDB practice)

Taxa the paper reports as significantly different between the curated groups, at the paper's own
threshold, in a differential abundance or prevalence test (including detection frequency for
checkerboard, PCR and culture studies), at any rank the paper tests. Not: taxa only described without a
test, correlations with clinical variables, or results for other group pairs.

## Checks, for every recorded row

1. `in_paper`: is the taxon reported as significantly different for the curated comparison?
   yes / no / unclear (unclear: e.g. the paper gives no test, or the comparison is not reported as such).
2. `direction_ok`: is the direction right (increased = higher in Group 1)? yes / no / NA.
3. `ncbi_ok`: is the NCBI taxon the organism the paper names, at the paper's rank? yes / no / parent
   (parent = mapped to a higher rank, e.g. an oral taxon NCBI lacks mapped to its genus). A different
   organism, or a higher rank where NCBI has the paper's taxon, is `no`; give the right ID. Check with
   `curl "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi?db=taxonomy&retmode=json&id=<taxid>"`
   or esearch, pausing 0.4 s between calls.

Then add a row for each taxon the paper reports as significant for this comparison that is not recorded
(`action` = add).

## Output

`output/llm_annotation/taxa_review/Study <ID>_review.csv` with columns: `row` (from the input; empty for
added taxa), `direction`, `recorded_taxon` (or the paper's name for added taxa), `paper_name` (the name as
the paper gives it), `in_paper`, `direction_ok`, `ncbi_ok`, `suggested_ncbi_taxid`, `suggested_ncbi_name`,
`action` (keep / remove / fix_direction / fix_ncbi / add / check), `evidence` (verbatim quote, table cell or
figure label, with page and table/figure), `note`.

`check` is for rows the reviewer cannot settle (e.g. the paper's table is unreadable, or the curated
comparison is not reported as such); say why in `note`.

If the paper cannot be read, or the curated comparison cannot be found at all, still write the file, with
one `check` row per recorded taxon and the reason in `note`.
