#!/usr/bin/env Rscript

project_root <- normalizePath(file.path(dirname(commandArgs(trailingOnly = FALSE)[1]), ".."), mustWork = FALSE)
if (!file.exists(file.path(project_root, "SPEC.md"))) {
  project_root <- normalizePath(getwd())
}

library_dir <- file.path(project_root, "tools", "R-library")
dir.create(library_dir, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(library_dir, .libPaths()))

required <- c("survey", "mitools", "haven", "ggplot2")
missing <- required[!vapply(required, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]

if (length(missing) > 0L) {
  install.packages(
    missing,
    repos = "https://cloud.r-project.org",
    lib = library_dir,
    dependencies = c("Depends", "Imports", "LinkingTo")
  )
}

still_missing <- required[!vapply(required, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
if (length(still_missing) > 0L) {
  stop("Package installation failed: ", paste(still_missing, collapse = ", "))
}

message("Required R packages are available in: ", library_dir)
