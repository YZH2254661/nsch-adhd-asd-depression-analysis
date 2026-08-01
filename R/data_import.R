numeric_values <- function(x) {
  suppressWarnings(as.numeric(x))
}

require_nsch_columns <- function(data) {
  required <- c(
    "K2Q31A", "K2Q31B", "K2Q35A", "K2Q35B", "K2Q32A", "K2Q32B",
    "K2Q32C", "K2Q33A", "K2Q33B", "K2Q34A", "K2Q34B", "SC_AGE_YEARS",
    "SC_SEX", paste0("FPL_I", 1:6), "HIGRADE_TVIS", "BMICLASS", "FWC", "FIPSST",
    "STRATUM", "HHID"
  )
  missing <- setdiff(required, names(data))
  if (length(missing) > 0L) {
    stop("NSCH file is missing required variables: ", paste(missing, collapse = ", "))
  }
  invisible(TRUE)
}

categorize_income_ratio <- function(x) {
  category <- cut(
    numeric_values(x),
    breaks = c(-Inf, 199, 399, Inf),
    labels = c("0-199% poverty level", "200-399% poverty level", "400%+ poverty level"),
    right = TRUE
  )
  factor(
    ifelse(is.na(category), "Unknown", as.character(category)),
    levels = c(
      "0-199% poverty level", "200-399% poverty level", "400%+ poverty level", "Unknown"
    )
  )
}

harmonize_nsch_year <- function(raw_data, year) {
  names(raw_data) <- toupper(names(raw_data))
  require_nsch_columns(raw_data)

  adhd <- recode_current_condition(
    numeric_values(raw_data$K2Q31A), numeric_values(raw_data$K2Q31B)
  )
  asd <- recode_current_condition(
    numeric_values(raw_data$K2Q35A), numeric_values(raw_data$K2Q35B)
  )
  depression <- recode_current_condition(
    numeric_values(raw_data$K2Q32A), numeric_values(raw_data$K2Q32B)
  )
  anxiety <- recode_current_condition(
    numeric_values(raw_data$K2Q33A), numeric_values(raw_data$K2Q33B)
  )
  behavior <- recode_current_condition(
    numeric_values(raw_data$K2Q34A), numeric_values(raw_data$K2Q34B)
  )

  age <- numeric_values(raw_data$SC_AGE_YEARS)
  sex_code <- numeric_values(raw_data$SC_SEX)
  sex <- factor(
    ifelse(sex_code == 1, "Male", ifelse(sex_code == 2, "Female", "Unknown")),
    levels = c("Male", "Female", "Unknown")
  )

  income_implicates <- lapply(1:6, function(index) {
    categorize_income_ratio(raw_data[[paste0("FPL_I", index)]])
  })
  income <- income_implicates[[1]]

  education_code <- numeric_values(raw_data$HIGRADE_TVIS)
  education_labels <- c(
    "1" = "Less than high school", "2" = "High school",
    "3" = "Some college", "4" = "College degree"
  )
  education <- unname(education_labels[as.character(education_code)])
  education[is.na(education)] <- "Unknown"
  education <- factor(
    education,
    levels = c("Less than high school", "High school", "Some college", "College degree", "Unknown")
  )

  bmi_code <- numeric_values(raw_data$BMICLASS)
  tagged_bmi_missing <- haven::is_tagged_na(raw_data$BMICLASS)
  weight_status <- rep("Unknown", length(bmi_code))
  weight_status[!is.na(bmi_code) & bmi_code == 1] <- "Underweight"
  weight_status[!is.na(bmi_code) & bmi_code == 2] <- "Healthy weight"
  weight_status[!is.na(bmi_code) & bmi_code %in% c(3, 4)] <- "Overweight/obesity"
  weight_status[tagged_bmi_missing | (!is.na(bmi_code) & bmi_code == 6)] <-
    "Not reported/not in universe"
  weight_status <- factor(
    weight_status,
    levels = c(
      "Underweight", "Healthy weight", "Overweight/obesity",
      "Not reported/not in universe", "Unknown"
    )
  )

  severity_code <- numeric_values(raw_data$K2Q32C)
  severity <- c("1" = "Mild", "2" = "Moderate", "3" = "Severe")[as.character(severity_code)]
  severity[depression != 1L] <- NA_character_

  fips <- as.character(raw_data$FIPSST)
  stratum <- as.character(raw_data$STRATUM)
  hhid <- as.character(raw_data$HHID)
  psu <- ifelse(is.na(hhid) | !nzchar(hhid), NA_character_, paste(year, hhid, sep = "-"))

  harmonized <- data.frame(
    depression = depression,
    adhd = adhd,
    asd = asd,
    age = age,
    sex = sex,
    income = income,
    education = education,
    weight_status = weight_status,
    anxiety = anxiety,
    behavior = behavior,
    weight = numeric_values(raw_data$FWC) / 2,
    strata = interaction(fips, stratum, drop = TRUE, sep = "-"),
    psu = psu,
    year = as.integer(year),
    depression_severity = factor(severity, levels = c("Mild", "Moderate", "Severe")),
    stringsAsFactors = FALSE
  )
  for (index in 1:6) {
    harmonized[[paste0("income_implicate_", index)]] <- income_implicates[[index]]
  }
  income_valid <- Reduce(
    `&`,
    lapply(income_implicates, function(value) !is.na(value) & value != "Unknown")
  )
  harmonized$analysis_eligible <- income_valid & with(
    harmonized,
    age >= 3 & age <= 17 &
      !is.na(depression) & !is.na(adhd) & !is.na(asd) &
      !is.na(age) & sex != "Unknown" &
      education != "Unknown" & weight_status != "Unknown" &
      !is.na(weight) & weight > 0 &
      !is.na(strata) & !is.na(psu)
  )
  harmonized$analysis_eligible[is.na(harmonized$analysis_eligible)] <- FALSE
  harmonized
}

extract_nsch_archives <- function(raw_dir) {
  archives <- list.files(raw_dir, pattern = "\\.zip$", full.names = TRUE)
  for (archive in archives) {
    destination <- file.path(raw_dir, tools::file_path_sans_ext(basename(archive)))
    if (!dir.exists(destination)) {
      dir.create(destination, recursive = TRUE)
      utils::unzip(archive, exdir = destination)
    }
  }
  invisible(raw_dir)
}

find_year_file <- function(raw_dir, year) {
  files <- list.files(raw_dir, pattern = "\\.dta$", recursive = TRUE, full.names = TRUE)
  candidates <- files[grepl(as.character(year), basename(files)) & grepl("topical", basename(files), ignore.case = TRUE)]
  if (length(candidates) > 1L) {
    normalized_raw_dir <- normalizePath(raw_dir, winslash = "/", mustWork = TRUE)
    top_level_candidates <- candidates[
      dirname(normalizePath(candidates, winslash = "/", mustWork = TRUE)) == normalized_raw_dir
    ]
    if (length(top_level_candidates) == 1L) {
      return(top_level_candidates[[1]])
    }
  }
  if (length(candidates) != 1L) {
    stop(
      "Expected exactly one ", year, " topical Stata file under ", raw_dir,
      "; found ", length(candidates), ". See data/raw/README.md."
    )
  }
  candidates[[1]]
}

prepare_combined_nsch <- function(raw_dir) {
  extract_nsch_archives(raw_dir)
  file_2023 <- find_year_file(raw_dir, 2023L)
  file_2024 <- find_year_file(raw_dir, 2024L)

  data_2023 <- harmonize_nsch_year(haven::read_dta(file_2023), 2023L)
  data_2024 <- harmonize_nsch_year(haven::read_dta(file_2024), 2024L)
  combined <- rbind(data_2023, data_2024)
  validate_analysis_data(combined)
  combined
}
