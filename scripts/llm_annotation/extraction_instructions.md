# PerioSigDB annotation extraction from full text

Instructions for an LLM extractor that reads one study's full text and records PerioSigDB/BugSigDB
annotation fields, independently of the manual curation. The output is used to (1) fill fields the
manual curation does not have (statistical analysis, diversity, signature source), (2) fill missing
values in existing fields, and (3) flag disagreements with existing values for curator review. The
manual annotation is never overwritten: results live in `output/llm_annotation/` and are compared in
a separate step.

## Input

- `study`, `pmid`, `full_text` (a PDF, EPUB or PMC XML; read every page, including tables, figure
  legends and supplementary material if bundled), and the curated comparison: `group_0_name` (the
  comparison group, usually health) and `group_1_name` (periodontitis). Use these names only to
  identify which comparison the signature belongs to; do not assume any other curated value.
  PerioSigDB keeps one health vs periodontitis comparison per study. If the paper has several
  periodontitis groups (e.g. chronic and aggressive) or several comparison groups, use the
  comparison its differential abundance results are reported for; if results are given for more
  than one, use all periodontitis participants vs healthy if reported, otherwise the first
  periodontitis group listed. When groups also differ by a systemic condition (HIV, diabetes,
  COPD, pregnancy...) or exposure (smoking), use the systemically healthy (or non-exposed) pair.
  Describe every candidate comparison in `comparison_note`.
- Supplementary files, when retrieved, are in `data/articles_supplements/<study number>/` (PDF,
  DOCX, XLSX, CSV...). Read them too: participant characteristics and differential abundance
  results are often only there. Cite them in `location` (e.g. "Supplementary Table S1").
- Do NOT open `output/perio_bugs*`, `output/overview*` or any other curated annotation file.

## Output

One JSON file per study, `output/llm_annotation/<run>/<Study>.json` (e.g. `Study 484.json`):

```json
{
  "study": "Study 484",
  "pmid": "28871594",
  "full_text_file": "data/articles_included/484. Perez Chaparro 2017.pdf",
  "comparison_found": true,
  "comparison_note": "Group 0 = 'periodontally healthy' (n=7), Group 1 = 'chronic periodontitis' (n=9)",
  "fields": {
    "<field name>": {"value": ..., "quote": "...", "location": "p. 3, Table 1", "confidence": "high", "note": ""}
  }
}
```

- Every field below appears in `fields`. When the paper does not report it, use `"value": null`
  with `"note": "not reported"`; when it is reported but ambiguous, give the best value with
  `"confidence": "low"` and say why in `note`.
- `quote`: the shortest verbatim text (or table cell with its row/column labels) supporting the
  value, so a curator can find it in seconds. `location`: page and section/table/figure.
- `confidence`: `high` (stated explicitly), `medium` (derived, e.g. computed from counts or from
  subgroups), `low` (inferred or ambiguous).
- Numbers: plain numbers, no units. Means/SDs as reported for the curated Group 0/Group 1. If the
  paper gives SEM, median/IQR or only subgroup values, give the value it reports and explain in
  `note` (e.g. "median (IQR) 45 (38-52); no mean reported"); do not convert.
- Lists (statistical tests, matched on, confounders): JSON arrays.

## Fields

### New fields (not in the manual curation; BugSigDB experiment/signature fields)

| field | value |
|---|---|
| `statistical_test` | array; test(s) used to call the differentially abundant taxa in the signature (not tests of clinical variables or diversity). Use the names in `statistical_test` in `scripts/controlled_vocab.R` where possible; otherwise free text |
| `significance_threshold` | number, e.g. 0.05 |
| `mht_correction` | `true`/`false`: were the taxon p-values corrected for multiple testing (FDR, Bonferroni, q-values...)? Give the method in `note` |
| `lda_score_above` | number, for LEfSe only (e.g. 2) |
| `data_transformation` | one of `data_transformation` in `scripts/controlled_vocab.R`, or free text |
| `matched_on` | array; variables the groups were matched on by design (e.g. age, sex) |
| `confounders_controlled_for` | array; variables adjusted for in the differential abundance model |
| `pielou`, `shannon`, `chao1`, `simpson`, `inverse_simpson`, `richness` | `increased` / `decreased` / `unchanged` in Group 1 vs Group 0, as reported by the paper's own test; null if not reported. `richness` = observed taxa/OTUs/ASVs/species |
| `source_increased`, `source_decreased` | where the taxa increased / decreased in Group 1 are reported (e.g. "Table 2", "Figure 3b", "text, p. 5") |
| `description_increased`, `description_decreased` | one sentence describing the signature, BugSigDB style (e.g. "Differentially abundant taxa between chronic periodontitis and healthy subjects, LEfSe") |

### Existing fields (re-extracted blind, for verification and filling gaps)

Study level: `study_design` (vocab `study_design`), `location_of_subjects` (country; vocab
`location`), `body_site` (sample type, e.g. "subgingival dental plaque", "saliva"),
`periodontitis_classification` (e.g. "chronic periodontitis", "aggressive periodontitis", "stage
III grade B (2017 AAP/EFP)"), `systemic_condition` (e.g. "systemically healthy", "type 2 diabetes"),
`sequencing_type` (vocab `sequencing_type`), `variable_region_16s` (e.g. "4", "3-4"),
`sequencing_platform` (vocab `sequencing_platform`). Follow BugSigDB: `sequencing_type` is one of
16S, 18S, ITS / ITS2, WMS (shotgun metagenomes or metatranscriptomes) or PCR (targeted PCR/qPCR),
and 16S microarrays (HOMIM, PhyloChip...) count as 16S. Checkerboard DNA-DNA hybridization, culture
and MALDI-TOF identification are not sequencing: `sequencing_type` is null and the platform is
"DNA-DNA Hybridization", "Mass spectrometry" or null (culture).
`diagnostic_method`: how the microbes were detected, in a few words, for every study (e.g.
"Checkerboard DNA-DNA hybridization", "Culture", "Culture and PCR", "qPCR", "16S rRNA gene
sequencing (V4)", "Shotgun metagenomics", "16S microarray (HOMIM)").

Per group (prefix `group_0_` and `group_1_`): `name`, `definition` (the case definition used, quoted),
`sample_size`, `age_mean`, `age_sd`, `num_males`, `percent_males`, `num_smokers`, `percent_smokers`,
`bleeding_on_probing_percent`, `bleeding_on_probing_sd`, `suppuration_percent`, `suppuration_sd`,
`pocket_depth_mean`, `pocket_depth_sd`, `clinical_attachment_loss_mean`, `clinical_attachment_loss_sd`,
`plaque`, `plaque_sd`.

Also `plaque_measure` (e.g. "% sites", "Plaque Index (Silness & Löe)") and, in `note` of the
pocket depth and attachment loss fields, whether they are full-mouth or sampled-site values.
