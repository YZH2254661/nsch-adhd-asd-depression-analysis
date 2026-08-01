source(file.path(project_root, "R", "functions.R"))

assert_identical <- function(actual, expected, label) {
  if (!identical(actual, expected)) {
    stop(label, "\nExpected: ", paste(expected, collapse = ", "),
         "\nActual: ", paste(actual, collapse = ", "))
  }
}

assert_true <- function(value, label) {
  if (!isTRUE(value)) {
    stop(label)
  }
}

binary <- recode_yes_no(c(1, 2, 6, 7, 9, NA_real_))
assert_identical(binary, c(1L, 0L, NA_integer_, NA_integer_, NA_integer_, NA_integer_),
                 "NSCH yes/no recoding must preserve only valid 1/2 responses")

current_condition <- recode_current_condition(
  ever = c(1, 1, 2, 2, 7, NA_real_),
  current = c(1, 2, 6, 7, 1, NA_real_)
)
assert_identical(
  current_condition,
  c(1L, 0L, 0L, 0L, NA_integer_, NA_integer_),
  "A valid 'no' to ever diagnosed must be coded as not current despite the current-item skip"
)

groups <- derive_adhd_asd_group(
  adhd = c(0L, 1L, 0L, 1L),
  asd = c(0L, 0L, 1L, 1L)
)
assert_identical(
  as.character(groups),
  c("Neither ADHD nor ASD", "ADHD only", "ASD only", "Co-occurring ADHD-ASD"),
  "The four ADHD/ASD groups must use the manuscript order"
)
assert_identical(
  levels(groups),
  c("Neither ADHD nor ASD", "ADHD only", "ASD only", "Co-occurring ADHD-ASD"),
  "The reference group must be neither ADHD nor ASD"
)

reported <- data.frame(
  term = "adhd:asd",
  estimate = log(0.46),
  std_error = 0.20,
  stringsAsFactors = FALSE
)
interaction_result <- format_interaction_result(reported, confidence_level = 0.95)
assert_true(all(c("term", "log_odds", "interaction_or", "ci_low", "ci_high", "p_value") %in%
                  names(interaction_result)),
            "Interaction output must include estimate, OR, CI, and p-value")
assert_true(interaction_result$interaction_or > interaction_result$ci_low &&
              interaction_result$interaction_or < interaction_result$ci_high,
            "Interaction OR must lie within its confidence interval")

valid <- data.frame(
  depression = c(0L, 1L), adhd = c(0L, 1L), asd = c(0L, 0L),
  age = c(8, 14), sex = c("Male", "Female"), income = c("Low", "High"),
  education = c("High school", "College"), weight_status = c("Healthy", "Overweight"),
  anxiety = c(0L, 1L), behavior = c(0L, 0L), weight = c(1, 2),
  strata = c("01-1", "01-1"), psu = c("a", "b"), year = c(2023L, 2024L),
  stringsAsFactors = FALSE
)
assert_true(isTRUE(validate_analysis_data(valid)), "Valid harmonized input must pass validation")

domain_data <- valid
domain_data$analysis_eligible <- c(FALSE, TRUE)
domain_design <- build_survey_design(domain_data)
assert_true(
  nrow(domain_design$variables) == 1L && domain_design$variables$age[[1]] == 14,
  "Survey design must apply eligibility as a domain after the full design is declared"
)

processed_path <- file.path(tempfile(pattern = "processed-output-"), "nested", "analysis.rds")
save_processed_data(valid, processed_path)
assert_true(file.exists(processed_path), "Processed-data saving must create missing parent directories")
assert_true(identical(readRDS(processed_path), valid), "Saved processed data must round-trip without changes")
