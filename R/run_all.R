#!/usr/bin/env Rscript

all_args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", all_args[grepl("^--file=", all_args)])
project_root <- normalizePath(file.path(dirname(file_arg), ".."))
args <- commandArgs(trailingOnly = TRUE)

mode_position <- match("--mode", args)
mode <- if (!is.na(mode_position) && length(args) >= mode_position + 1L) args[[mode_position + 1L]] else "example"
if (!(mode %in% c("example", "full"))) {
  stop("--mode must be either example or full")
}

library_dir <- file.path(project_root, "tools", "R-library")
if (dir.exists(library_dir)) {
  .libPaths(c(library_dir, .libPaths()))
}

source(file.path(project_root, "R", "functions.R"))
source(file.path(project_root, "R", "data_import.R"))

table_dir <- file.path(project_root, "outputs", "tables")
figure_dir <- file.path(project_root, "outputs", "figures")
log_dir <- file.path(project_root, "outputs", "logs")
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

if (mode == "example") {
  data <- generate_example_data(n = 5000L, seed = 20240729L)
  example_path <- file.path(project_root, "data", "example", "nsch_example.csv")
  dir.create(dirname(example_path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(data, example_path, row.names = FALSE, na = "")
  label <- "example"
} else {
  data <- prepare_combined_nsch(file.path(project_root, "data", "raw"))
  save_processed_data(data, file.path(project_root, "data", "processed", "nsch_2023_2024_analysis.rds"))
  label <- "full"
}

result <- run_analysis_workflow(
  data = data,
  output_dir = table_dir,
  figure_dir = figure_dir,
  label = label
)

log_path <- file.path(log_dir, paste0(label, "_session_info.txt"))
session_lines <- sub("[[:space:]]+$", "", capture.output(sessionInfo()))
writeLines(session_lines, log_path)
message("Completed ", mode, " workflow with ", result$analysis_n, " analysis records")
message("Interaction result: ", paste(capture.output(print(result$interaction)), collapse = " "))
