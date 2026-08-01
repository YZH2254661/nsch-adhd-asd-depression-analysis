# Methodology Sources

- U.S. Census Bureau, 2023 NSCH data release: https://www.census.gov/programs-surveys/nsch/data/datasets/nsch2023.html
- U.S. Census Bureau, 2024 NSCH source and accuracy statement: https://www2.census.gov/programs-surveys/nsch/technical-documentation/source-and-accuracy/2024-NSCH-Source-and-Accuracy-Statement.pdf
- U.S. Census Bureau, Guide to Multi-Year Estimates: https://www2.census.gov/programs-surveys/nsch/technical-documentation/methodology/NSCH-Guide-to-Multi-Year-Estimates.pdf
- U.S. Census Bureau, Guide to Analysis with Multiply Imputed Data: https://www.census.gov/content/dam/Census/programs-surveys/nsch/tech-documentation/methodology/NSCH-Guide-to-Analysis-with-Multiply-Imputed-Data.pdf
- U.S. Census Bureau, NSCH Analytic Guide: https://www2.census.gov/programs-surveys/nsch/technical-documentation/methodology/NSCH-Analytic-Guide.pdf
- U.S. Census Bureau, 2024 Topical Variable List: https://www2.census.gov/programs-surveys/nsch/technical-documentation/codebook/2024-NSCH-Topical-Variable-List.pdf
- CRAN `survey` 4.5 reference manual: https://stat.ethz.ch/CRAN/web/packages/survey/refman/survey.html

The multi-year guide instructs analysts to divide annual weights by the number of pooled years. The source and accuracy statement identifies `FIPSST` and `STRATUM` as strata, `HHID` as the topical-file PSU, and `FWC` as the selected-child weight. The Analytic Guide requires subgroup analyses to retain the complete survey design and identifies six family income-to-poverty ratio implicates (`FPL_I1`-`FPL_I6`). The multiple-imputation guide calls for combining estimates and variances across all six implicates with Rubin's rules. The analysis implements these requirements with `survey::svydesign()`, `survey::svyglm(..., family = quasibinomial())`, and `mitools::MIcombine()`.
