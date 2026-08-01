source(file.path(project_root, "R", "functions.R"))
source(file.path(project_root, "R", "data_import.R"))

fake_raw <- data.frame(
  K2Q31A = c(1, 2, 1, 1), K2Q31B = c(1, 6, 2, 1),
  K2Q35A = c(2, 2, 1, 1), K2Q35B = c(6, 6, 1, 1),
  K2Q32A = c(1, 2, 1, 1), K2Q32B = c(1, 6, 2, 1), K2Q32C = c(2, 6, 6, 3),
  K2Q33A = c(2, 2, 1, 1), K2Q33B = c(6, 6, 1, 2),
  K2Q34A = c(2, 2, 1, 1), K2Q34B = c(6, 6, 1, 2),
  SC_AGE_YEARS = c(8, 10, 14, 16), SC_SEX = c(1, 2, 1, 2),
  FPL_I1 = c(120, 250, 430, 180), FPL_I2 = c(130, 250, 430, 175),
  FPL_I3 = c(110, 250, 430, 185), FPL_I4 = c(125, 250, 430, 190),
  FPL_I5 = c(115, 250, 430, 170), FPL_I6 = c(135, 250, 430, 180),
  HIGRADE_TVIS = c(1, 2, 3, 4),
  BMICLASS = c(1, 2, 3, 6), FWC = c(10, 20, 30, 40),
  FIPSST = c("01", "01", "02", "02"), STRATUM = c("1", "2A", "1", "2A"),
  HHID = c("a", "b", "c", "d"), stringsAsFactors = FALSE
)

harmonized <- harmonize_nsch_year(fake_raw, year = 2024L)
if (!isTRUE(validate_analysis_data(harmonized))) {
  stop("Harmonized annual data must satisfy the analysis contract")
}
if (!identical(harmonized$adhd, c(1L, 0L, 0L, 1L))) {
  stop("Annual harmonization must combine ever/current items correctly")
}
if (!identical(harmonized$asd, c(0L, 0L, 1L, 1L))) {
  stop("Annual harmonization must construct current ASD correctly")
}
if (!isTRUE(all.equal(harmonized$weight, c(5, 10, 15, 20)))) {
  stop("Two-year pooled weights must equal annual FWC divided by two")
}
if (!identical(as.character(harmonized$sex), c("Male", "Female", "Male", "Female"))) {
  stop("Sex coding must match the NSCH public-use labels")
}
income_columns <- paste0("income_implicate_", 1:6)
if (!all(income_columns %in% names(harmonized))) {
  stop("Annual harmonization must preserve all six family-poverty implicates")
}
if (length(analysis_implicates(harmonized)) != 6L) {
  stop("Official-data analysis must expand to six implicate-specific datasets")
}
if (!all(harmonized$analysis_eligible)) {
  stop("Complete eligible fixture records must be marked for the analysis domain")
}

incomplete_raw <- fake_raw
incomplete_raw$HIGRADE_TVIS[[1]] <- 7
incomplete_raw$BMICLASS[[2]] <- 7
incomplete <- harmonize_nsch_year(incomplete_raw, year = 2024L)
if (incomplete$analysis_eligible[[1]] || incomplete$analysis_eligible[[2]]) {
  stop("Missing education or weight-status covariates must be excluded from the analysis domain")
}

tagged_bmi_raw <- fake_raw
tagged_bmi_raw$BMICLASS <- c(
  1, haven::tagged_na("n"), haven::tagged_na("m"), NA_real_
)
tagged_bmi <- harmonize_nsch_year(tagged_bmi_raw, year = 2024L)
if (!all(
  as.character(tagged_bmi$weight_status[2:3]) == "Not reported/not in universe"
) || !all(tagged_bmi$analysis_eligible[2:3])) {
  stop("Official tagged BMI missing values must remain in the reported/not-in-universe category")
}
if (as.character(tagged_bmi$weight_status[[4]]) != "Unknown" ||
    tagged_bmi$analysis_eligible[[4]]) {
  stop("An untagged missing BMI must remain unknown and ineligible")
}

invalid_design_raw <- fake_raw
invalid_design_raw$HHID[[3]] <- NA_character_
invalid_design <- harmonize_nsch_year(invalid_design_raw, year = 2024L)
if (!is.na(invalid_design$psu[[3]]) || invalid_design$analysis_eligible[[3]]) {
  stop("Missing household PSU values must remain missing and be excluded")
}

invalid_income_raw <- fake_raw
invalid_income_raw$FPL_I6[[4]] <- NA_real_
invalid_income <- harmonize_nsch_year(invalid_income_raw, year = 2024L)
if (invalid_income$analysis_eligible[[4]]) {
  stop("Records missing any required poverty implicate must be excluded")
}

duplicate_dir <- tempfile("nsch-import-")
dir.create(file.path(duplicate_dir, "nsch_2023e_topical_Stata"), recursive = TRUE)
top_level_file <- file.path(duplicate_dir, "nsch_2023e_topical.dta")
nested_file <- file.path(
  duplicate_dir, "nsch_2023e_topical_Stata", "nsch_2023e_topical.dta"
)
file.create(top_level_file, nested_file)
selected_file <- find_year_file(duplicate_dir, 2023L)
if (!identical(normalizePath(selected_file), normalizePath(top_level_file))) {
  stop("A unique top-level DTA must take precedence over an extracted archive copy")
}
unlink(duplicate_dir, recursive = TRUE)
