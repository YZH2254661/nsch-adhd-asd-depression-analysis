# Harmonized Analysis Data Dictionary

| Harmonized field | NSCH source | Coding/role |
|---|---|---|
| `depression` | `K2Q32A`, `K2Q32B` | Current depression, 0/1 |
| `adhd` | `K2Q31A`, `K2Q31B` | Current ADHD, 0/1 |
| `asd` | `K2Q35A`, `K2Q35B` | Current ASD, 0/1 |
| `anxiety` | `K2Q33A`, `K2Q33B` | Current anxiety, 0/1 |
| `behavior` | `K2Q34A`, `K2Q34B` | Current behavior/conduct problems, 0/1 |
| `age` | `SC_AGE_YEARS` | Age in years; analysis restricted to 3-17 |
| `sex` | `SC_SEX` | Male/Female |
| `income` | `FPL_I1`-`FPL_I6` | 0-199%, 200-399%, or 400%+ of the family poverty level; six implicates |
| `education` | `HIGRADE_TVIS` | Highest adult education, four levels; unknown is ineligible |
| `weight_status` | `BMICLASS` | Underweight, healthy, overweight/obesity, or not reported/not in universe; official Stata tagged missing values `.m`/`.n` are retained in the last category and ordinary missing is ineligible |
| `weight` | `FWC / 2` | Two-year pooled selected-child weight |
| `strata` | `FIPSST` x `STRATUM` | Combined sampling stratum |
| `psu` | year x `HHID` | Household PSU, made year-unique |
| `analysis_eligible` | Derived | Ages 3-17 with valid exposure, outcome, sex, education, weight-status, weight, strata, and PSU fields |

For each current condition, a valid "No" response to the corresponding ever-diagnosed item is coded as current=0 even when the current-status item is a logical skip. Invalid or missing ever/current combinations remain missing.

The full survey design is declared before subsetting to `analysis_eligible`. For official data, the six income implicates are fitted separately and model estimates and variances are combined with Rubin's rules. The term "family poverty level" refers to the Census income-to-poverty ratio and is not described as a federal poverty guideline.

For the official pooled files, the source N is 106,537, the age 3-17 domain N is 90,712, and the final analysis N is 88,955. These counts are written on every run to `outputs/tables/full_analysis_summary.csv`.
