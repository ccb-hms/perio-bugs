# Paper figures

Each figure in the PerioSigDB paper has its own R script here. The scripts only draw. The analysis stays in `reports/metadata_options/report.qmd`, which writes the plotted data to `output/figure_data/` every time it renders.

## Rebuild the figures

You need R with the report's packages (plus ggtext, cowplot, ragg and sf) and Quarto.

```sh
figures/build_all.sh                # render the report, then draw every figure
figures/build_all.sh --skip-report  # redraw from the existing output/figure_data/
cd figures && Rscript fig4.R        # one figure
```

Each figure is written to `figures/output/` as a vector PDF and a 600 dpi PNG.

## Figures

| Script | Content | Previous figure |
|---|---|---|
| `fig1.R` | The contents of PerioSigDB: papers by period and country, taxa and signature sizes, profiling methods, the most reported taxa by method | Figure 1 |
| `fig2.R` | Consensus taxa; pair correlations in healthy HMP plaque; participant age, sex and smoking (C, drawn by `study_groups.R`) | Figure 2 |
| `fig3.R` | Coverage against other resources; profiling method above taxonomic rank, beside A; consistency across studies | Figure S2 (B, C), Figure 4 (D) |
| `fig4.R` | Direction of the periodontitis-associated genera across conditions (A, drawn by `genus_heatmap.R`); whole-signature similarity for the conditions the report chooses (`fig4_rows`) in all four direction pairings; the same conditions as one point each | Figure 5 (left), Figure S4 (rows), Figure 6B |
| `fig5.R` | Colorectal cancer: species behind the shared genera, enrichment ranks, genus fold changes | S3, S6 |
| `figS1.R` | Pair correlations in HMP and the five studies with per-sample data | Figure S1 |
| `figS2.R` | Genera increased (A) and decreased (B) in periodontitis against the number of other conditions reporting them | report section on genera less widely reported elsewhere |
| `figS4.R` | Whole-signature similarity for every condition | Figure S4 |
| `figS5.R` | Genera from all signatures and from sequencing signatures only | Figure S5 |
| `figS6.R` | Direction summary | Figure 6A |

## Things you may want to change

- Figure 3A compares periodontitis coverage with other resources. Their rows come from `data/other_resources.csv` and contain placeholder values until we have the real numbers. Replace them and set `placeholder` to `FALSE`.
- The report chooses the rows of Figure 4B and C by rule (`sim_main_rows()` in `report.qmd`: FDR < 0.05 in any column, against all conditions or within the body site; body site rows as in 4A) and exports them as `fig4_rows`. It leaves colorectal cancer's mouth studies out of Figures 4B, 4C and S4 (`excluded_site_rows`), because they match the periodontitis consensus only in the opposite direction.
- `METHOD` in `fig5.R` picks the enrichment test in Figure 5B: `"ora"` (default) or `"padog"`.
- Point labels in Figures 4B and S6 sit at fixed offsets (`LABEL_OFFSETS`, `OFFSETS`). Adjust them there if the data move a point.
- If you rename anything the report exports (a panel title, a condition label), check the matching script. `fig5.R` stops with an error if a species panel would be empty.
- Colours, fonts, sizes and the similarity panels live in `common.R`. Colours follow the report. Figures 1C and 1D use the report's salmon and slate for increased and decreased, because blue and orange already mean 16S and shotgun in 1E and 1F.
