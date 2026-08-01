source(file.path(project_root, "R", "functions.R"))

example_output_data <- generate_example_data(n = 3000L, seed = 17L)
example_output_design <- build_survey_design(example_output_data)
example_output_models <- fit_core_models(example_output_design)

prevalence <- weighted_group_prevalence(example_output_design)
if (nrow(prevalence) != 4L || !all(c("group", "prevalence", "ci_low", "ci_high") %in% names(prevalence))) {
  stop("Weighted prevalence output must contain four groups and confidence intervals")
}

distribution <- weighted_group_distribution(example_output_design)
if (nrow(distribution) != 4L || abs(sum(distribution$proportion) - 1) > 1e-6) {
  stop("Weighted group distribution must contain four proportions summing to one")
}

direct_comparisons <- extract_group_contrasts(example_output_models$group)
if (nrow(direct_comparisons) != 3L ||
    !all(c("comparison", "adjusted_or", "ci_low", "ci_high", "p_value", "fdr_p_value") %in%
         names(direct_comparisons))) {
  stop("Direct-comparison output must contain three unique contrasts with uncertainty")
}

moderator_tests <- extract_moderator_tests(example_output_models)
if (nrow(moderator_tests) != 2L ||
    !all(c("moderator", "f_statistic", "numerator_df", "denominator_df", "p_value", "fdr_p_value") %in%
         names(moderator_tests))) {
  stop("Moderator output must contain age and sex overall interaction tests")
}

severity <- weighted_severity_distribution(example_output_design)
if (!all(c("group", "severity", "proportion", "ci_low", "ci_high") %in% names(severity)) ||
    !all(c("Mild", "Moderate", "Severe", "Unknown/missing") %in% severity$severity)) {
  stop("Severity output must contain all four manuscript categories with confidence intervals")
}
severity_sums <- aggregate(proportion ~ group, severity, sum)
if (any(abs(severity_sums$proportion - 1) > 1e-6)) {
  stop("Severity categories must use all current-depression cases and sum to one within group")
}

secondary <- fit_secondary_models(example_output_design)
if (!all(c("age_3_11", "age_12_17", "male", "female", "age_6_17", "comorbidity_adjusted") %in% names(secondary))) {
  stop("Secondary workflow must include age and sex strata and sensitivity models")
}

age_results <- combine_stratified_results(secondary[c("age_3_11", "age_12_17")])
if (!all(c("stratum", "term", "adjusted_or", "ci_low", "ci_high") %in% names(age_results))) {
  stop("Stratified output must identify stratum and effect estimates")
}

main_plot_path <- tempfile(fileext = ".png")
age_plot_path <- tempfile(fileext = ".png")
save_main_forest_plot(extract_odds_ratios(example_output_models$group), main_plot_path)
age_predictions <- pooled_age_predictions(
  list(example_output_design),
  list(example_output_models$age_interaction)
)
if (nrow(age_predictions) != 8L ||
    !all(c("group", "age_group", "probability", "ci_low", "ci_high") %in% names(age_predictions)) ||
    any(age_predictions$probability < 0 | age_predictions$probability > 1)) {
  stop("Age interaction output must contain eight valid standardized probabilities")
}
save_age_probability_plot(age_predictions, age_plot_path)
if (!file.exists(main_plot_path) || file.info(main_plot_path)$size < 1000) {
  stop("Main forest plot must be generated as a non-empty PNG")
}
if (!file.exists(age_plot_path) || file.info(age_plot_path)$size < 1000) {
  stop("Age interaction plot must be generated as a non-empty PNG")
}

workflow_dir <- tempfile(pattern = "analysis-output-")
run_analysis_workflow(example_output_data, workflow_dir, label = "test")
required_outputs <- c(
  "test_analysis_summary.csv", "test_group_distribution.csv", "test_group_prevalence.csv", "test_group_model.csv",
  "test_direct_comparisons.csv", "test_interaction.csv", "test_moderator_tests.csv",
  "test_stratified.csv", "test_age_predictions.csv", "test_severity.csv", "test_sensitivity.csv",
  "test_group_prevalence.png", "test_main_forest.png", "test_age_interaction.png"
)
if (!all(file.exists(file.path(workflow_dir, required_outputs)))) {
  stop("End-to-end workflow must write every required table and figure")
}

analysis_summary <- utils::read.csv(file.path(workflow_dir, "test_analysis_summary.csv"))
if (!identical(
  names(analysis_summary),
  c("source_n", "age_domain_n", "analysis_n", "income_implicates")
) || analysis_summary$analysis_n[[1]] != nrow(example_output_data)) {
  stop("Analysis summary must expose the manuscript sample-size contract")
}
