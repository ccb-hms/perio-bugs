# Smoking scope and confounder handling, from full text

Instructions for an LLM reviewer that reads one study's full text and answers two questions about the
curated PerioSigDB signature. It covers the studies whose curated data report a percentage of smokers.

1. **Smoking scope.** Do the curated smoking figures describe the participants whose samples produced the
   signature? Or do they describe a wider cohort, while the signature came from a subset (for example
   non-smokers only, or a subset sequenced)?
2. **Confounder handling.** Did the differential abundance analysis that produced the signature's taxa
   account for smoking, age or sex? Did it account for anything else? It could do so by matching,
   adjustment in the model, stratification or restriction.

## Input

One JSON file per study, `<scratchpad>/smoking_inputs/<Study>.json`, with:

- `full_text_file`: the paper (PDF; read every page you need, including tables and figure legends).
- `supplements_dir` (may be null): supplementary files. Read them when participant tables or results
  are there.
- `curated`: the curators' groups, sample sizes, smoker counts and percentages, the statistical test,
  where the signature was reported (`source`), any `matched_on` / `confounders_controlled_for` values,
  and the signature's taxa by direction.

Use the curated taxa and source to find the analysis that produced the signature. Then answer every
question for that analysis, not for other analyses in the paper. The questions don't cover clinical
comparisons, diversity tests, or a secondary model whose taxa were not curated.

Curators chose which groups and subsets to use. Do not propose a different comparison. Report what the
paper says.

## Output

Write `output/llm_annotation/smoking/<Study>.json` (the study name as in the input, e.g.
`Study 1025.json`):

```json
{
  "study": "Study 1025",
  "full_text_file": "...",
  "signature_analysis": {
    "description": "Chi-squared test of PCR detection frequency, chronic periodontitis vs health, Fig. 1",
    "group_0_n": 14, "group_1_n": 38,
    "quote": "...", "location": "p. 3, Results"
  },
  "smoking": {
    "paper_group_0_percent": 0, "paper_group_1_percent": 5.3,
    "population": "the 14 healthy and 38 periodontitis participants analysed",
    "scope": "signature participants",
    "smokers_in_signature": "included",
    "quote": "...", "location": "p. 2, Table 1",
    "confidence": "high",
    "note": ""
  },
  "handling": {
    "smoking": {"method": "not accounted", "quote": "", "location": "", "note": ""},
    "age":     {"method": "not accounted", "quote": "", "location": "", "note": ""},
    "sex":     {"method": "not accounted", "quote": "", "location": "", "note": ""}
  },
  "matched_on": [],
  "confounders_controlled_for": [],
  "fields_quote": "...", "fields_location": "...",
  "curated_fields_correct": true,
  "summary": "One or two plain sentences for a curator."
}
```

### `smoking`

- `paper_group_0_percent`, `paper_group_1_percent`: current smokers in each curated group, as the paper
  reports them. Use null if it doesn't report them. Say in `note` when the paper's figure differs from the
  curated one, and why (e.g. former smokers counted, a different denominator, a typo).
- `population`: the participants the paper's smoking figures describe, with their numbers.
- `scope`, one of:
  - `signature participants`: the smoking figures describe the same participants as the signature
    analysis.
  - `smokers excluded by design`: the study excluded smokers, so every analysed participant is a
    non-smoker.
  - `wider cohort`: the figures describe a larger recruited cohort. The analysed subset is smaller or
    different, and its smoking mix is not reported or differs.
  - `non-smoker subset`: the figures include smokers, but the signature analysis left smokers out.
  - `smoker subset`: the signature comes from smokers only, or from a smoking stratum.
  - `unclear`: say why in `note`.
- `smokers_in_signature`: `included`, `excluded`, `only smokers` or `unclear`. This says whether smokers
  contributed samples to the signature analysis.

### `handling` (smoking, age, sex)

`method` is one of the values below. If more than one applies, list them in one string separated by
"; ".

- `adjusted in model`: a covariate in the differential abundance model that produced the taxa (e.g.
  MaAsLin2 fixed effect, DESeq2 design term, logistic regression covariate).
- `matched`: the groups were matched on it by design (individually or by frequency).
- `restricted`: only one level was allowed (e.g. smokers excluded, women only).
- `stratified`: analysed separately by level, with the curated taxa from one stratum or from both.
- `sensitivity analysis`: the main analysis was re-run without the variable, or with it adjusted, and the
  paper reports whether the taxa held. Say in `note` which result the curated taxa come from.
- `groups compared only`: the paper tested whether the groups differ in it (e.g. Table 1 p-value) but did
  not account for it in the taxa analysis.
- `not accounted`.

Quote the sentence that shows the method. A claim like "groups were age- and sex-matched" counts as
matched only if the paper says so. Similar group means alone do not count.

### `matched_on`, `confounders_controlled_for` (BugSigDB fields)

These are arrays for the signature analysis. Use BugSigDB's EFO terms, such as `age`, `sex`,
`smoking status`, `body mass index`, `diabetes mellitus`, `race`, `ethnic group`, `geographic area`,
`education level`, `alcohol drinking`, `antibiotic exposure` and `oral hygiene`. Add any other variable in
plain words.

- `matched_on`: variables the groups were matched on by design.
- `confounders_controlled_for`: variables adjusted for in the model, or controlled by stratification. Do
  not include restriction (exclusion criteria) here. Record it under `handling` only.

`curated_fields_correct`: `true` if the curated `matched_on` and `confounders_controlled_for` agree with
yours, otherwise `false`. Treat both null and empty as agreeing with an empty array.

### `summary`

One or two sentences a curator can read without the paper. For example: "Smoking figures cover all 120
recruited participants, but only 40 non-smokers were sequenced; the signature has no smokers. Age and sex
were not accounted for."
