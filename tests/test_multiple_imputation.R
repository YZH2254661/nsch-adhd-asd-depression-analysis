source(file.path(project_root, "R", "functions.R"))

mi_data <- generate_example_data(n = 3000L, seed = 20240730L)
income_levels <- levels(mi_data$income)
for (implicate in 1:6) {
  shifted <- as.character(mi_data$income)
  replace_rows <- which(seq_len(nrow(mi_data)) %% 31L == implicate)
  shifted[replace_rows] <- c("0-199% poverty level", "200-399% poverty level", "400%+ poverty level")[
    (replace_rows %% 3L) + 1L
  ]
  mi_data[[paste0("income_implicate_", implicate)]] <- factor(
    shifted,
    levels = unique(c(income_levels, "0-199% poverty level", "200-399% poverty level", "400%+ poverty level"))
  )
}

implicates <- analysis_implicates(mi_data)
if (length(implicates) != 6L) {
  stop("Six poverty implicates must produce six analysis datasets")
}

designs <- lapply(implicates, build_survey_design)
model_sets <- lapply(designs, fit_core_models)
interaction_models <- lapply(model_sets, `[[`, "interaction")
pooled_interaction <- extract_pooled_interaction(interaction_models)

if (nrow(pooled_interaction) != 1L || pooled_interaction$term[[1]] != "adhd:asd" ||
    !all(is.finite(unlist(pooled_interaction[c(
      "log_odds", "std_error", "interaction_or", "ci_low", "ci_high", "p_value"
    )])))) {
  stop("Rubin-pooled interaction output must contain finite estimates and uncertainty")
}

pooled_group <- extract_pooled_odds_ratios(lapply(model_sets, `[[`, "group"))
if (!all(c("adjusted_or", "ci_low", "ci_high", "p_value") %in% names(pooled_group))) {
  stop("Rubin-pooled group model must retain the standard model-output contract")
}

mi_output_dir <- tempfile(pattern = "mi-workflow-")
mi_result <- run_analysis_workflow(mi_data, mi_output_dir, label = "mi")
if (length(mi_result$models) != 6L ||
    !file.exists(file.path(mi_output_dir, "mi_age_predictions.csv")) ||
    !file.exists(file.path(mi_output_dir, "mi_interaction.csv"))) {
  stop("The complete workflow must pool all six implicates and write pooled outputs")
}
