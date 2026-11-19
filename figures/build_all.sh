#!/usr/bin/env bash
# Rebuild every paper figure: render the report (which writes output/figure_data/), then draw each figure in R.
# Usage: figures/build_all.sh [--skip-report]
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"

if [[ "${1:-}" != "--skip-report" ]]; then
  (cd "$here/../reports/metadata_options" && quarto render report.qmd)
fi

cd "$here"
for script in fig1 fig2 fig3 fig4 fig5 figS1 figS2 figS4 figS5 figS6; do
  Rscript "$script.R"
done
