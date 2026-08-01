# Official NSCH raw data

Place the two official Census topical Stata archives in this directory:

- `nsch_2023e_topical_Stata.zip`
- `nsch_2024_topical_Stata.zip`

Official URLs:

- https://www2.census.gov/programs-surveys/nsch/datasets/2023/nsch_2023e_topical_Stata.zip
- https://www2.census.gov/programs-surveys/nsch/datasets/2024/nsch_2024_topical_Stata.zip

Run `tools/R/bin/Rscript.exe R/download_raw_data.R` to download, validate, extract, and checksum them. The official archives are now present locally and the full workflow has completed. Raw public-use files remain excluded from Git; only `download_manifest.csv` is tracked as a provenance record.

The full workflow divides each annual `FWC` weight by two and pools the files following the Census Guide to Multi-Year Estimates.
