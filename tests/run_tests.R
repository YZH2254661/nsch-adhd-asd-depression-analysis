#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grepl("^--file=", args)])
project_root <- normalizePath(file.path(dirname(file_arg), ".."))

library_dir <- file.path(project_root, "tools", "R-library")
if (dir.exists(library_dir)) {
  .libPaths(c(library_dir, .libPaths()))
}

test_files <- list.files(
  file.path(project_root, "tests"),
  pattern = "^test_.*\\.R$",
  full.names = TRUE
)

if (length(test_files) == 0L) {
  stop("No test files found")
}

for (test_file in test_files) {
  message("Running ", basename(test_file))
  sys.source(test_file, envir = new.env(parent = globalenv()))
}

message("All tests passed")
