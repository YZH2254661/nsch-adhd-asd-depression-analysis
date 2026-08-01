recode_yes_no <- function(x) {
  result <- rep(NA_integer_, length(x))
  result[!is.na(x) & x == 1] <- 1L
  result[!is.na(x) & x == 2] <- 0L
  result
}

recode_current_condition <- function(ever, current) {
  if (length(ever) != length(current)) {
    stop("ever and current must have equal length")
  }

  result <- rep(NA_integer_, length(ever))
  result[!is.na(ever) & ever == 2] <- 0L
  result[!is.na(ever) & ever == 1 & !is.na(current) & current == 1] <- 1L
  result[!is.na(ever) & ever == 1 & !is.na(current) & current == 2] <- 0L
  result
}

derive_adhd_asd_group <- function(adhd, asd) {
  if (length(adhd) != length(asd)) {
    stop("adhd and asd must have equal length")
  }

  labels <- rep(NA_character_, length(adhd))
  complete <- !is.na(adhd) & !is.na(asd)
  labels[complete & adhd == 0L & asd == 0L] <- "Neither ADHD nor ASD"
  labels[complete & adhd == 1L & asd == 0L] <- "ADHD only"
  labels[complete & adhd == 0L & asd == 1L] <- "ASD only"
  labels[complete & adhd == 1L & asd == 1L] <- "Co-occurring ADHD-ASD"

  factor(
    labels,
    levels = c(
      "Neither ADHD nor ASD",
      "ADHD only",
      "ASD only",
      "Co-occurring ADHD-ASD"
    )
  )
}

format_interaction_result <- function(coefficient_table, confidence_level = 0.95) {
  required <- c("term", "estimate", "std_error")
  missing <- setdiff(required, names(coefficient_table))
  if (length(missing) > 0L) {
    stop("Missing interaction coefficient fields: ", paste(missing, collapse = ", "))
  }
  if (nrow(coefficient_table) != 1L) {
    stop("Exactly one interaction coefficient is required")
  }

  alpha <- 1 - confidence_level
  critical <- stats::qnorm(1 - alpha / 2)
  estimate <- coefficient_table$estimate[[1]]
  std_error <- coefficient_table$std_error[[1]]
  z_value <- estimate / std_error

  data.frame(
    term = coefficient_table$term[[1]],
    log_odds = estimate,
    std_error = std_error,
    interaction_or = exp(estimate),
    ci_low = exp(estimate - critical * std_error),
    ci_high = exp(estimate + critical * std_error),
    p_value = 2 * stats::pnorm(abs(z_value), lower.tail = FALSE),
    stringsAsFactors = FALSE
  )
}

required_analysis_columns <- function() {
  c(
    "depression", "adhd", "asd", "age", "sex", "income", "education",
    "weight_status", "anxiety", "behavior", "weight", "strata", "psu", "year"
  )
}

validate_analysis_data <- function(data) {
  missing <- setdiff(required_analysis_columns(), names(data))
  if (length(missing) > 0L) {
    stop("Analysis input is missing columns: ", paste(missing, collapse = ", "))
  }

  for (name in c("depression", "adhd", "asd", "anxiety", "behavior")) {
    invalid <- !is.na(data[[name]]) & !(data[[name]] %in% c(0L, 1L))
    if (any(invalid)) {
      stop(name, " must contain only 0, 1, or missing values")
    }
  }

  if (any(!is.na(data$weight) & data$weight <= 0)) {
    stop("Survey weights must be positive")
  }
  if ("analysis_eligible" %in% names(data) &&
      (!is.logical(data$analysis_eligible) || any(is.na(data$analysis_eligible)))) {
    stop("analysis_eligible must be a non-missing logical variable")
  }

  TRUE
}

income_implicate_names <- function() {
  paste0("income_implicate_", 1:6)
}

analysis_implicates <- function(data) {
  implicate_columns <- income_implicate_names()
  present <- implicate_columns %in% names(data)
  if (any(present) && !all(present)) {
    stop("Analysis input must contain either none or all six income implicate columns")
  }
  if (!all(present)) {
    return(list(data))
  }

  lapply(implicate_columns, function(column) {
    implicate <- data
    implicate$income <- implicate[[column]]
    implicate
  })
}

save_processed_data <- function(data, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(data, path)
  invisible(path)
}

generate_example_data <- function(n = 5000L, seed = 20240729L) {
  if (n < 500L) {
    stop("Use at least 500 rows so all four exposure groups are represented")
  }

  set.seed(seed)
  year <- sample(c(2023L, 2024L), n, replace = TRUE)
  stratum_number <- sample(seq_len(25L), n, replace = TRUE)
  psu_number <- sample(seq_len(2L), n, replace = TRUE)
  age <- sample(3:17, n, replace = TRUE)
  sex <- factor(sample(c("Male", "Female"), n, replace = TRUE))
  income <- factor(
    sample(c("0-199% poverty level", "200-399% poverty level", "400%+ poverty level", "Unknown"), n,
           replace = TRUE, prob = c(0.34, 0.33, 0.28, 0.05))
  )
  education <- factor(
    sample(c("Less than high school", "High school", "Some college", "College degree"),
           n, replace = TRUE, prob = c(0.08, 0.22, 0.31, 0.39))
  )
  weight_status <- factor(
    sample(c("Underweight", "Healthy weight", "Overweight/obesity", "Not reported/not in universe"),
           n, replace = TRUE, prob = c(0.04, 0.54, 0.24, 0.18))
  )

  adhd_probability <- stats::plogis(-2.55 + 0.055 * (age - 10) + 0.25 * (sex == "Male"))
  adhd <- stats::rbinom(n, 1L, adhd_probability)
  asd_probability <- stats::plogis(-4.05 + 0.95 * adhd + 0.32 * (sex == "Male"))
  asd <- stats::rbinom(n, 1L, asd_probability)

  anxiety_probability <- stats::plogis(-2.6 + 0.7 * adhd + 0.9 * asd + 0.06 * (age - 10))
  anxiety <- stats::rbinom(n, 1L, anxiety_probability)
  behavior_probability <- stats::plogis(-2.8 + 1.15 * adhd + 0.45 * asd - 0.03 * (age - 10))
  behavior <- stats::rbinom(n, 1L, behavior_probability)

  depression_logit <- -4.05 +
    log(7.75) * adhd + log(3.91) * asd + log(0.46) * adhd * asd +
    0.07 * (age - 10) + 0.18 * (sex == "Female") +
    0.25 * (income == "0-199% poverty level")
  depression <- stats::rbinom(n, 1L, stats::plogis(depression_logit))

  severity <- rep(NA_character_, n)
  depressed <- depression == 1L
  severity[depressed] <- sample(
    c("Mild", "Moderate", "Severe"), sum(depressed), replace = TRUE,
    prob = c(0.47, 0.42, 0.11)
  )
  depressed_indices <- which(depressed)
  missing_severity_indices <- depressed_indices[seq_along(depressed_indices) %% 50L == 0L]
  severity[missing_severity_indices] <- NA_character_

  data.frame(
    depression = as.integer(depression),
    adhd = as.integer(adhd),
    asd = as.integer(asd),
    age = age,
    sex = sex,
    income = income,
    education = education,
    weight_status = weight_status,
    anxiety = as.integer(anxiety),
    behavior = as.integer(behavior),
    weight = stats::runif(n, 0.5, 2.5),
    strata = sprintf("%02d", stratum_number),
    psu = sprintf("%d-%02d-%d", year, stratum_number, psu_number),
    year = year,
    depression_severity = factor(severity, levels = c("Mild", "Moderate", "Severe")),
    stringsAsFactors = FALSE
  )
}

build_survey_design <- function(data) {
  validate_analysis_data(data)
  data$group <- derive_adhd_asd_group(data$adhd, data$asd)
  data$age_group <- factor(
    ifelse(data$age <= 11, "3-11 years", "12-17 years"),
    levels = c("3-11 years", "12-17 years")
  )
  data$sex <- factor(data$sex)
  data$income <- factor(data$income)
  data$education <- factor(data$education)
  data$weight_status <- factor(data$weight_status)

  design <- survey::svydesign(
    ids = ~psu,
    strata = ~strata,
    weights = ~weight,
    data = data,
    nest = TRUE
  )
  if ("analysis_eligible" %in% names(data)) {
    design <- subset(design, analysis_eligible)
  }
  for (name in c("group", "age_group", "sex", "income", "education", "weight_status")) {
    design$variables[[name]] <- droplevels(design$variables[[name]])
  }
  design
}

fit_core_models <- function(design) {
  family <- stats::quasibinomial()
  list(
    group = survey::svyglm(
      depression ~ group + age + sex + income + education + weight_status,
      design = design, family = family
    ),
    interaction = survey::svyglm(
      depression ~ adhd * asd + age + sex + income + education + weight_status,
      design = design, family = family
    ),
    age_interaction = survey::svyglm(
      depression ~ group * age_group + sex + income + education + weight_status,
      design = design, family = family
    ),
    sex_interaction = survey::svyglm(
      depression ~ group * sex + age + income + education + weight_status,
      design = design, family = family
    )
  )
}

extract_odds_ratios <- function(model, confidence_level = 0.95) {
  coefficient_matrix <- summary(model)$coefficients
  intervals <- stats::confint(model, level = confidence_level)
  terms <- rownames(coefficient_matrix)
  p_column <- grep("^Pr\\(", colnames(coefficient_matrix), value = TRUE)
  p_values <- if (length(p_column) == 1L) coefficient_matrix[, p_column] else rep(NA_real_, length(terms))

  result <- data.frame(
    term = terms,
    log_odds = coefficient_matrix[, "Estimate"],
    std_error = coefficient_matrix[, "Std. Error"],
    adjusted_or = exp(coefficient_matrix[, "Estimate"]),
    ci_low = exp(intervals[, 1]),
    ci_high = exp(intervals[, 2]),
    p_value = as.numeric(p_values),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  result[result$term != "(Intercept)", , drop = FALSE]
}

pooled_model_parameters <- function(models) {
  if (length(models) == 0L) {
    stop("At least one fitted model is required")
  }
  if (length(models) == 1L) {
    coefficients <- stats::coef(models[[1]])
    return(list(
      coefficients = coefficients,
      variance = stats::vcov(models[[1]]),
      df = stats::setNames(rep(models[[1]]$df.residual, length(coefficients)), names(coefficients))
    ))
  }

  complete_df <- min(vapply(models, function(model) model$df.residual, numeric(1)))
  combined <- mitools::MIcombine(models, df.complete = complete_df)
  coefficients <- stats::coef(combined)
  degrees_freedom <- combined$df
  if (is.null(names(degrees_freedom))) {
    names(degrees_freedom) <- names(coefficients)
  }
  list(
    coefficients = coefficients,
    variance = stats::vcov(combined),
    df = degrees_freedom
  )
}

extract_pooled_odds_ratios <- function(models, confidence_level = 0.95) {
  parameters <- pooled_model_parameters(models)
  terms <- names(parameters$coefficients)
  standard_errors <- sqrt(diag(parameters$variance))
  degrees_freedom <- parameters$df[terms]
  critical <- stats::qt(1 - (1 - confidence_level) / 2, df = degrees_freedom)
  statistics <- parameters$coefficients / standard_errors

  result <- data.frame(
    term = terms,
    log_odds = as.numeric(parameters$coefficients),
    std_error = as.numeric(standard_errors),
    adjusted_or = exp(as.numeric(parameters$coefficients)),
    ci_low = exp(as.numeric(parameters$coefficients) - critical * standard_errors),
    ci_high = exp(as.numeric(parameters$coefficients) + critical * standard_errors),
    p_value = 2 * stats::pt(abs(statistics), df = degrees_freedom, lower.tail = FALSE),
    stringsAsFactors = FALSE
  )
  result[result$term != "(Intercept)", , drop = FALSE]
}

extract_pooled_interaction <- function(models, confidence_level = 0.95) {
  result <- extract_pooled_odds_ratios(models, confidence_level)
  result <- result[result$term == "adhd:asd", , drop = FALSE]
  if (nrow(result) != 1L) {
    stop("The fitted models must contain exactly one adhd:asd interaction term")
  }
  names(result)[names(result) == "adjusted_or"] <- "interaction_or"
  result
}

extract_interaction_from_model <- function(model, confidence_level = 0.95) {
  coefficient_matrix <- summary(model)$coefficients
  term <- "adhd:asd"
  if (!(term %in% rownames(coefficient_matrix))) {
    stop("The fitted model does not contain the adhd:asd interaction term")
  }

  intervals <- stats::confint(model, level = confidence_level)
  p_column <- grep("^Pr\\(", colnames(coefficient_matrix), value = TRUE)
  p_value <- if (length(p_column) == 1L) coefficient_matrix[term, p_column] else NA_real_

  data.frame(
    term = term,
    log_odds = coefficient_matrix[term, "Estimate"],
    std_error = coefficient_matrix[term, "Std. Error"],
    interaction_or = exp(coefficient_matrix[term, "Estimate"]),
    ci_low = exp(intervals[term, 1]),
    ci_high = exp(intervals[term, 2]),
    p_value = as.numeric(p_value),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

weighted_group_prevalence <- function(design) {
  estimate <- survey::svyby(
    ~depression,
    ~group,
    design,
    survey::svymean,
    na.rm = TRUE,
    vartype = c("se", "ci"),
    keep.names = FALSE
  )

  data.frame(
    group = as.character(estimate$group),
    prevalence = as.numeric(estimate$depression),
    std_error = as.numeric(estimate$se),
    ci_low = as.numeric(estimate$ci_l),
    ci_high = as.numeric(estimate$ci_u),
    stringsAsFactors = FALSE
  )
}

weighted_group_distribution <- function(design) {
  estimate <- survey::svymean(~group, design, na.rm = TRUE)
  intervals <- stats::confint(estimate)
  terms <- names(stats::coef(estimate))

  data.frame(
    group = sub("^group", "", terms),
    proportion = as.numeric(stats::coef(estimate)),
    std_error = as.numeric(survey::SE(estimate)),
    ci_low = as.numeric(intervals[, 1]),
    ci_high = as.numeric(intervals[, 2]),
    stringsAsFactors = FALSE
  )
}

extract_group_contrasts <- function(model, confidence_level = 0.95) {
  extract_group_contrasts_from_parameters(
    pooled_model_parameters(list(model)),
    confidence_level = confidence_level
  )
}

extract_pooled_group_contrasts <- function(models, confidence_level = 0.95) {
  extract_group_contrasts_from_parameters(
    pooled_model_parameters(models),
    confidence_level = confidence_level
  )
}

extract_group_contrasts_from_parameters <- function(parameters, confidence_level = 0.95) {
  coefficients <- parameters$coefficients
  covariance <- parameters$variance
  terms <- c(
    adhd = "groupADHD only",
    asd = "groupASD only",
    both = "groupCo-occurring ADHD-ASD"
  )
  missing <- setdiff(unname(terms), names(coefficients))
  if (length(missing) > 0L) {
    stop("Group model is missing contrast terms: ", paste(missing, collapse = ", "))
  }

  definitions <- list(
    list(label = "ASD only vs ADHD only", positive = terms[["asd"]], negative = terms[["adhd"]]),
    list(label = "Co-occurring ADHD-ASD vs ADHD only", positive = terms[["both"]], negative = terms[["adhd"]]),
    list(label = "Co-occurring ADHD-ASD vs ASD only", positive = terms[["both"]], negative = terms[["asd"]])
  )
  rows <- lapply(definitions, function(definition) {
    contrast <- rep(0, length(coefficients))
    names(contrast) <- names(coefficients)
    contrast[[definition$positive]] <- 1
    contrast[[definition$negative]] <- -1
    estimate <- sum(contrast * coefficients)
    std_error <- sqrt(as.numeric(t(contrast) %*% covariance %*% contrast))
    degrees_freedom <- min(parameters$df[c(definition$positive, definition$negative)])
    critical <- stats::qt(1 - (1 - confidence_level) / 2, df = degrees_freedom)
    statistic <- estimate / std_error
    data.frame(
      comparison = definition$label,
      log_odds = estimate,
      std_error = std_error,
      adjusted_or = exp(estimate),
      ci_low = exp(estimate - critical * std_error),
      ci_high = exp(estimate + critical * std_error),
      p_value = 2 * stats::pt(abs(statistic), df = degrees_freedom, lower.tail = FALSE),
      stringsAsFactors = FALSE
    )
  })
  result <- do.call(rbind, rows)
  result$fdr_p_value <- stats::p.adjust(result$p_value, method = "BH")
  result
}

extract_moderator_tests <- function(models) {
  specifications <- list(
    list(name = "Age group", model = models$age_interaction, terms = ~group:age_group),
    list(name = "Sex", model = models$sex_interaction, terms = ~group:sex)
  )
  rows <- lapply(specifications, function(specification) {
    test <- survey::regTermTest(specification$model, specification$terms, method = "Wald")
    data.frame(
      moderator = specification$name,
      f_statistic = as.numeric(test$Ftest),
      numerator_df = as.numeric(test$df),
      denominator_df = as.numeric(test$ddf),
      p_value = as.numeric(test$p),
      stringsAsFactors = FALSE
    )
  })
  result <- do.call(rbind, rows)
  result$fdr_p_value <- stats::p.adjust(result$p_value, method = "BH")
  result
}

extract_pooled_moderator_tests <- function(model_sets) {
  if (length(model_sets) == 1L) {
    return(extract_moderator_tests(model_sets[[1]]))
  }
  specifications <- list(
    list(name = "Age group", model_name = "age_interaction", pattern = ":age_group"),
    list(name = "Sex", model_name = "sex_interaction", pattern = ":sex")
  )
  rows <- lapply(specifications, function(specification) {
    parameters <- pooled_model_parameters(lapply(model_sets, `[[`, specification$model_name))
    terms <- grep(specification$pattern, names(parameters$coefficients), value = TRUE, fixed = TRUE)
    if (length(terms) == 0L) {
      stop("No pooled interaction terms found for ", specification$name)
    }
    coefficients <- parameters$coefficients[terms]
    covariance <- parameters$variance[terms, terms, drop = FALSE]
    numerator_df <- length(terms)
    denominator_df <- min(parameters$df[terms])
    statistic <- as.numeric(t(coefficients) %*% solve(covariance, coefficients)) / numerator_df
    data.frame(
      moderator = specification$name,
      f_statistic = statistic,
      numerator_df = numerator_df,
      denominator_df = denominator_df,
      p_value = stats::pf(statistic, numerator_df, denominator_df, lower.tail = FALSE),
      stringsAsFactors = FALSE
    )
  })
  result <- do.call(rbind, rows)
  result$fdr_p_value <- stats::p.adjust(result$p_value, method = "BH")
  result
}

weighted_severity_distribution <- function(design) {
  if (!("depression_severity" %in% names(design$variables))) {
    stop("Analysis input must contain depression_severity for the severity table")
  }
  updated <- update(
    design,
    severity_mild = as.integer(!is.na(depression_severity) & depression_severity == "Mild"),
    severity_moderate = as.integer(!is.na(depression_severity) & depression_severity == "Moderate"),
    severity_severe = as.integer(!is.na(depression_severity) & depression_severity == "Severe"),
    severity_unknown = as.integer(is.na(depression_severity))
  )
  depressed <- subset(updated, depression == 1)
  variable_names <- c(
    Mild = "severity_mild",
    Moderate = "severity_moderate",
    Severe = "severity_severe",
    "Unknown/missing" = "severity_unknown"
  )

  pieces <- lapply(names(variable_names), function(level) {
    variable <- variable_names[[level]]
    estimate <- survey::svyby(
      stats::as.formula(paste0("~", variable)),
      ~group,
      depressed,
      survey::svymean,
      na.rm = TRUE,
      vartype = c("se", "ci"),
      keep.names = FALSE
    )
    data.frame(
      group = as.character(estimate$group),
      severity = level,
      proportion = as.numeric(estimate[[variable]]),
      std_error = as.numeric(estimate$se),
      ci_low = as.numeric(estimate$ci_l),
      ci_high = as.numeric(estimate$ci_u),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, pieces)
}

fit_group_model <- function(design, extra_terms = NULL, exclude_terms = NULL) {
  right_hand_side <- c("group", "age", "sex", "income", "education", "weight_status", extra_terms)
  right_hand_side <- setdiff(right_hand_side, exclude_terms)
  formula <- stats::as.formula(paste("depression ~", paste(right_hand_side, collapse = " + ")))
  survey::svyglm(formula, design = design, family = stats::quasibinomial())
}

fit_secondary_models <- function(design) {
  list(
    age_3_11 = fit_group_model(subset(design, age >= 3 & age <= 11)),
    age_12_17 = fit_group_model(subset(design, age >= 12 & age <= 17)),
    male = fit_group_model(subset(design, sex == "Male"), exclude_terms = "sex"),
    female = fit_group_model(subset(design, sex == "Female"), exclude_terms = "sex"),
    age_6_17 = fit_group_model(subset(design, age >= 6 & age <= 17)),
    comorbidity_adjusted = fit_group_model(design, extra_terms = c("anxiety", "behavior"))
  )
}

group_term_label <- function(term) {
  labels <- c(
    "groupADHD only" = "ADHD only",
    "groupASD only" = "ASD only",
    "groupCo-occurring ADHD-ASD" = "Co-occurring ADHD-ASD"
  )
  unname(labels[term])
}

combine_stratified_results <- function(models) {
  stratum_labels <- c(
    "age_3_11" = "3-11 years",
    "age_12_17" = "12-17 years",
    "male" = "Male",
    "female" = "Female"
  )

  pieces <- lapply(names(models), function(name) {
    result <- extract_odds_ratios(models[[name]])
    result <- result[grepl("^group", result$term), , drop = FALSE]
    result$comparison <- group_term_label(result$term)
    result$stratum <- if (name %in% names(stratum_labels)) stratum_labels[[name]] else name
    result
  })
  result <- do.call(rbind, pieces)
  result$fdr_p_value <- stats::p.adjust(result$p_value, method = "BH")
  result
}

combine_pooled_stratified_results <- function(secondary_sets) {
  stratum_labels <- c(
    "age_3_11" = "3-11 years",
    "age_12_17" = "12-17 years",
    "male" = "Male",
    "female" = "Female"
  )
  pieces <- lapply(names(stratum_labels), function(name) {
    models <- lapply(secondary_sets, `[[`, name)
    result <- extract_pooled_odds_ratios(models)
    result <- result[grepl("^group", result$term), , drop = FALSE]
    result$comparison <- group_term_label(result$term)
    result$stratum <- stratum_labels[[name]]
    result
  })
  result <- do.call(rbind, pieces)
  result$fdr_p_value <- stats::p.adjust(result$p_value, method = "BH")
  result
}

save_main_forest_plot <- function(or_table, path) {
  plot_data <- or_table[grepl("^group", or_table$term), , drop = FALSE]
  plot_data$comparison <- group_term_label(plot_data$term)
  plot_data$comparison <- factor(
    plot_data$comparison,
    levels = rev(c("ADHD only", "ASD only", "Co-occurring ADHD-ASD"))
  )

  plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = adjusted_or, y = comparison)) +
    ggplot2::geom_vline(xintercept = 1, color = "#6B7280", linewidth = 0.6, linetype = 2) +
    ggplot2::geom_errorbar(
      ggplot2::aes(xmin = ci_low, xmax = ci_high),
      width = 0.16, linewidth = 0.9, color = "#176B87", orientation = "y"
    ) +
    ggplot2::geom_point(size = 3.1, shape = 21, fill = "#D1495B", color = "white", stroke = 0.7) +
    ggplot2::scale_x_log10() +
    ggplot2::labs(
      x = "Adjusted odds ratio (log scale)", y = NULL,
      title = "Adjusted association with current depression",
      subtitle = "Reference: neither ADHD nor ASD; bars show 95% confidence intervals"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold"),
      plot.title.position = "plot"
    )

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(path, plot, width = 7.4, height = 4.5, dpi = 320, bg = "white")
  invisible(path)
}

save_age_stratified_or_plot <- function(stratified_table, path) {
  plot_data <- stratified_table
  plot_data$comparison <- factor(
    plot_data$comparison,
    levels = rev(c("ADHD only", "ASD only", "Co-occurring ADHD-ASD"))
  )
  plot_data$stratum <- factor(plot_data$stratum, levels = c("3-11 years", "12-17 years"))

  plot <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = adjusted_or, y = comparison, color = stratum, shape = stratum)
  ) +
    ggplot2::geom_vline(xintercept = 1, color = "#6B7280", linewidth = 0.6, linetype = 2) +
    ggplot2::geom_errorbar(
      ggplot2::aes(xmin = ci_low, xmax = ci_high),
      width = 0.15, linewidth = 0.8, orientation = "y",
      position = ggplot2::position_dodge(width = 0.42)
    ) +
    ggplot2::geom_point(size = 2.8, position = ggplot2::position_dodge(width = 0.42)) +
    ggplot2::scale_x_log10() +
    ggplot2::scale_color_manual(values = c("3-11 years" = "#176B87", "12-17 years" = "#D1495B")) +
    ggplot2::labs(
      x = "Adjusted odds ratio (log scale)", y = NULL, color = "Age group", shape = "Age group",
      title = "Age-stratified associations with current depression",
      subtitle = "Reference within each age stratum: neither ADHD nor ASD"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      legend.position = "top",
      plot.title = ggplot2::element_text(face = "bold"),
      plot.title.position = "plot"
    )

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(path, plot, width = 7.6, height = 4.8, dpi = 320, bg = "white")
  invisible(path)
}

standardized_age_predictions <- function(design, model) {
  variables <- design$variables
  group_levels <- levels(variables$group)
  age_levels <- levels(variables$age_group)
  coefficients <- stats::coef(model)
  covariance <- stats::vcov(model)
  model_terms <- stats::delete.response(stats::terms(model))
  sampling_weights <- as.numeric(stats::weights(design, type = "sampling"))
  sampling_weights <- sampling_weights / sum(sampling_weights)

  rows <- lapply(age_levels, function(age_level) {
    do.call(rbind, lapply(group_levels, function(group_level) {
      new_data <- variables
      new_data$group <- factor(group_level, levels = group_levels)
      new_data$age_group <- factor(age_level, levels = age_levels)
      model_matrix <- stats::model.matrix(
        model_terms,
        data = new_data,
        contrasts.arg = model$contrasts,
        xlev = model$xlevels
      )
      model_matrix <- model_matrix[, names(coefficients), drop = FALSE]
      linear_predictor <- as.numeric(model_matrix %*% coefficients)
      fitted_probability <- stats::plogis(linear_predictor)
      probability <- sum(sampling_weights * fitted_probability)
      gradient <- colSums(
        model_matrix * as.numeric(sampling_weights * fitted_probability * (1 - fitted_probability))
      )
      variance <- as.numeric(t(gradient) %*% covariance %*% gradient)
      data.frame(
        group = group_level,
        age_group = age_level,
        probability = probability,
        variance = variance,
        stringsAsFactors = FALSE
      )
    }))
  })
  do.call(rbind, rows)
}

pool_scalar_estimates <- function(estimates, variances, complete_df) {
  if (length(estimates) == 1L) {
    return(list(estimate = estimates[[1]], variance = variances[[1]], df = complete_df))
  }
  results <- lapply(estimates, function(value) stats::setNames(value, "estimate"))
  covariance <- lapply(variances, function(value) matrix(value, nrow = 1L, ncol = 1L))
  combined <- mitools::MIcombine(results, covariance, df.complete = complete_df)
  list(
    estimate = as.numeric(stats::coef(combined)),
    variance = as.numeric(stats::vcov(combined)),
    df = as.numeric(combined$df)
  )
}

pooled_age_predictions <- function(designs, models, confidence_level = 0.95) {
  if (length(designs) != length(models) || length(designs) == 0L) {
    stop("Design and model lists must be non-empty and have equal length")
  }
  predictions <- Map(standardized_age_predictions, designs, models)
  keys <- paste(predictions[[1]]$group, predictions[[1]]$age_group, sep = "\r")
  complete_df <- min(vapply(designs, survey::degf, numeric(1)))

  rows <- lapply(seq_along(keys), function(index) {
    pooled <- pool_scalar_estimates(
      lapply(predictions, function(table) table$probability[[index]]),
      lapply(predictions, function(table) table$variance[[index]]),
      complete_df
    )
    standard_error <- sqrt(pooled$variance)
    critical <- stats::qt(1 - (1 - confidence_level) / 2, df = pooled$df)
    data.frame(
      group = predictions[[1]]$group[[index]],
      age_group = predictions[[1]]$age_group[[index]],
      probability = pooled$estimate,
      std_error = standard_error,
      ci_low = max(0, pooled$estimate - critical * standard_error),
      ci_high = min(1, pooled$estimate + critical * standard_error),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

save_group_prevalence_plot <- function(prevalence_table, path) {
  plot_data <- prevalence_table
  plot_data$group <- factor(
    plot_data$group,
    levels = c("Neither ADHD nor ASD", "ADHD only", "ASD only", "Co-occurring ADHD-ASD"),
    labels = c("Neither", "ADHD\nonly", "ASD\nonly", "ADHD-ASD\nco-occurring")
  )

  plot <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = group, y = prevalence, fill = group)
  ) +
    ggplot2::geom_col(width = 0.68) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = ci_low, ymax = ci_high),
      width = 0.14, linewidth = 0.75
    ) +
    ggplot2::scale_fill_manual(values = c("#6B7280", "#176B87", "#E3A018", "#D1495B")) +
    ggplot2::scale_y_continuous(
      labels = function(value) sprintf("%.0f%%", 100 * value),
      expand = ggplot2::expansion(mult = c(0, 0.08))
    ) +
    ggplot2::labs(
      x = NULL, y = "Weighted prevalence",
      title = "Current depression prevalence by ADHD/ASD group",
      subtitle = "Bars show survey-weighted estimates; error bars show 95% confidence intervals"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      legend.position = "none",
      plot.title = ggplot2::element_text(face = "bold"),
      plot.title.position = "plot"
    )

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(path, plot, width = 7.6, height = 4.8, dpi = 320, bg = "white")
  invisible(path)
}

save_age_probability_plot <- function(prediction_table, path) {
  plot_data <- prediction_table
  plot_data$group <- factor(
    plot_data$group,
    levels = c("Neither ADHD nor ASD", "ADHD only", "ASD only", "Co-occurring ADHD-ASD"),
    labels = c("Neither", "ADHD\nonly", "ASD\nonly", "ADHD-ASD\nco-occurring")
  )
  plot_data$age_group <- factor(plot_data$age_group, levels = c("3-11 years", "12-17 years"))
  dodge <- ggplot2::position_dodge(width = 0.42)

  plot <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = group, y = probability, color = age_group)
  ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = ci_low, ymax = ci_high),
      width = 0.12, linewidth = 0.75, position = dodge
    ) +
    ggplot2::geom_point(size = 3.1, position = dodge) +
    ggplot2::scale_color_manual(values = c("3-11 years" = "#176B87", "12-17 years" = "#D1495B")) +
    ggplot2::scale_y_continuous(labels = function(value) sprintf("%.0f%%", 100 * value)) +
    ggplot2::labs(
      x = NULL, y = "Covariate-standardized probability", color = "Age group",
      title = "Adjusted probability of current depression by age group",
      subtitle = "Points show survey-weighted marginal predictions; bars show 95% confidence intervals"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      legend.position = "top",
      plot.title = ggplot2::element_text(face = "bold"),
      plot.title.position = "plot"
    )

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(path, plot, width = 7.6, height = 4.8, dpi = 320, bg = "white")
  invisible(path)
}

run_analysis_workflow <- function(data, output_dir, label = "analysis", figure_dir = output_dir) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
  validate_analysis_data(data)
  old_lonely_psu <- getOption("survey.lonely.psu")
  options(survey.lonely.psu = "adjust")
  on.exit(options(survey.lonely.psu = old_lonely_psu), add = TRUE)

  implicates <- analysis_implicates(data)
  designs <- lapply(implicates, build_survey_design)
  model_sets <- lapply(designs, fit_core_models)
  secondary_sets <- lapply(designs, fit_secondary_models)
  design <- designs[[1]]

  prevalence <- weighted_group_prevalence(design)
  distribution <- weighted_group_distribution(design)
  group_results <- extract_pooled_odds_ratios(lapply(model_sets, `[[`, "group"))
  group_rows <- grepl("^group", group_results$term)
  group_results$fdr_p_value <- NA_real_
  group_results$fdr_p_value[group_rows] <- stats::p.adjust(
    group_results$p_value[group_rows], method = "BH"
  )
  direct_comparisons <- extract_pooled_group_contrasts(lapply(model_sets, `[[`, "group"))
  interaction <- extract_pooled_interaction(lapply(model_sets, `[[`, "interaction"))
  moderator_tests <- extract_pooled_moderator_tests(model_sets)
  stratified_results <- combine_pooled_stratified_results(secondary_sets)
  age_predictions <- pooled_age_predictions(
    designs,
    lapply(model_sets, `[[`, "age_interaction")
  )
  severity <- weighted_severity_distribution(design)

  sensitivity_pieces <- lapply(c("age_6_17", "comorbidity_adjusted"), function(name) {
    result <- extract_pooled_odds_ratios(lapply(secondary_sets, `[[`, name))
    result <- result[grepl("^group", result$term), , drop = FALSE]
    result$comparison <- group_term_label(result$term)
    result$analysis <- name
    result
  })
  sensitivity <- do.call(rbind, sensitivity_pieces)
  sensitivity$fdr_p_value <- ave(
    sensitivity$p_value,
    sensitivity$analysis,
    FUN = function(p_values) stats::p.adjust(p_values, method = "BH")
  )

  analysis_summary <- data.frame(
    source_n = nrow(data),
    age_domain_n = sum(!is.na(data$age) & data$age >= 3 & data$age <= 17),
    analysis_n = nrow(design$variables),
    income_implicates = length(implicates)
  )

  utils::write.csv(analysis_summary, file.path(output_dir, paste0(label, "_analysis_summary.csv")), row.names = FALSE)
  utils::write.csv(distribution, file.path(output_dir, paste0(label, "_group_distribution.csv")), row.names = FALSE)
  utils::write.csv(prevalence, file.path(output_dir, paste0(label, "_group_prevalence.csv")), row.names = FALSE)
  utils::write.csv(group_results, file.path(output_dir, paste0(label, "_group_model.csv")), row.names = FALSE)
  utils::write.csv(direct_comparisons, file.path(output_dir, paste0(label, "_direct_comparisons.csv")), row.names = FALSE)
  utils::write.csv(interaction, file.path(output_dir, paste0(label, "_interaction.csv")), row.names = FALSE)
  utils::write.csv(moderator_tests, file.path(output_dir, paste0(label, "_moderator_tests.csv")), row.names = FALSE)
  utils::write.csv(stratified_results, file.path(output_dir, paste0(label, "_stratified.csv")), row.names = FALSE)
  utils::write.csv(age_predictions, file.path(output_dir, paste0(label, "_age_predictions.csv")), row.names = FALSE)
  utils::write.csv(severity, file.path(output_dir, paste0(label, "_severity.csv")), row.names = FALSE)
  utils::write.csv(sensitivity, file.path(output_dir, paste0(label, "_sensitivity.csv")), row.names = FALSE)

  save_group_prevalence_plot(prevalence, file.path(figure_dir, paste0(label, "_group_prevalence.png")))
  save_main_forest_plot(group_results, file.path(figure_dir, paste0(label, "_main_forest.png")))
  save_age_probability_plot(age_predictions, file.path(figure_dir, paste0(label, "_age_interaction.png")))

  invisible(list(
    design = if (length(designs) == 1L) design else designs,
    models = if (length(model_sets) == 1L) model_sets[[1]] else model_sets,
    secondary_models = if (length(secondary_sets) == 1L) secondary_sets[[1]] else secondary_sets,
    prevalence = prevalence,
    distribution = distribution,
    analysis_summary = analysis_summary,
    direct_comparisons = direct_comparisons,
    interaction = interaction,
    age_predictions = age_predictions,
    analysis_n = nrow(design$variables)
  ))
}
