# Microbial Signatures of Periodontal Disease

*Standardized, accessible, and FAIR data resources of published microbial differential abundance signatures in periodontal health and disease.*

---

## Project Overview

The **Microbial Signatures of Periodontal Disease** project aggregates, standardizes, and disseminates microbial abundance signatures reported in the literature for periodontal health and disease. Developed jointly by the Feres Lab and the Center for Computational Biomedicine (CCB), this effort aims to enable comprehensive data access and reproducible microbiome research in periodontology.

**Main Deliverables:**
- A harmonized, archive-ready dataset of microbial signatures with standardized nomenclature and metadata.
- Public deposition of the dataset on [Zenodo](https://zenodo.org/) in compliance with the FAIR principles (Findability, Accessibility, Interoperability, Reusability).
- An open-source R package to facilitate programmatic access, exploration, and integration of the dataset for scientific users.

## Contributing:

- Place excel files from dropbox (`Cleaned Micro List RY_final.xlsx` and `OVERVIEW_SHEET_RY.xlsx`) in `data/`
- run `scripts/merge_overview.R` to generate `output/overview_merged.rds`
- run `scripts/merge_microbe.R` to generate `output/diff_species.rds`
- work through `scripts/clean_overview.R` to contribute to cleaning up columns

---