source(file.path(project_root, "R", "functions.R"))

example <- generate_example_data(n = 2500L, seed = 20240729L)
if (nrow(example) != 2500L) {
  stop("Example generator must return the requested number of rows")
}
if (!isTRUE(validate_analysis_data(example))) {
  stop("Generated example data must satisfy the analysis contract")
}

groups <- derive_adhd_asd_group(example$adhd, example$asd)
if (length(unique(stats::na.omit(groups))) != 4L) {
  stop("Example data must contain all four ADHD/ASD groups")
}

design <- build_survey_design(example)
models <- fit_core_models(design)
expected_models <- c("group", "interaction", "age_interaction", "sex_interaction")
if (!all(expected_models %in% names(models))) {
  stop("Core workflow must fit all required models")
}

interaction <- extract_interaction_from_model(models$interaction)
if (nrow(interaction) != 1L || interaction$term[[1]] != "adhd:asd") {
  stop("Formal interaction output must contain exactly the ADHD x ASD term")
}
if (!all(is.finite(unlist(interaction[c("log_odds", "std_error", "interaction_or", "ci_low", "ci_high", "p_value")]))) ) {
  stop("Formal interaction output must contain finite statistics")
}

or_table <- extract_odds_ratios(models$group)
if (!all(c("term", "adjusted_or", "ci_low", "ci_high", "p_value") %in% names(or_table))) {
  stop("Model output must include ORs, confidence intervals, and p-values")
}
