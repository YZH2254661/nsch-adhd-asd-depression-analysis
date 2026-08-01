#!/usr/bin/env Rscript

all_args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", all_args[grepl("^--file=", all_args)])
project_root <- normalizePath(file.path(dirname(file_arg), ".."))
raw_dir <- file.path(project_root, "data", "raw")
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)

sources <- c(
  nsch_2023e_topical_Stata.zip = "https://www2.census.gov/programs-surveys/nsch/datasets/2023/nsch_2023e_topical_Stata.zip",
  nsch_2024_topical_Stata.zip = "https://www2.census.gov/programs-surveys/nsch/datasets/2024/nsch_2024_topical_Stata.zip"
)

for (name in names(sources)) {
  destination <- file.path(raw_dir, name)
  if (!file.exists(destination)) {
    partial <- paste0(destination, ".part")
    tryCatch(
      {
        utils::download.file(sources[[name]], partial, mode = "wb", quiet = FALSE)
        if (nrow(utils::unzip(partial, list = TRUE)) == 0L) {
          stop("Downloaded archive contains no files")
        }
        if (!file.rename(partial, destination)) {
          stop("Could not finalize downloaded archive")
        }
      },
      error = function(error) {
        if (file.exists(partial)) {
          unlink(partial)
        }
        stop(
          "Failed to download ", name, ". Place the official archive in data/raw/ ",
          "manually and rerun. Original error: ", conditionMessage(error),
          call. = FALSE
        )
      }
    )
  }
  if (nrow(utils::unzip(destination, list = TRUE)) == 0L) {
    stop("Archive is empty or invalid: ", destination)
  }
  utils::unzip(destination, exdir = file.path(raw_dir, tools::file_path_sans_ext(name)))
}

manifest <- data.frame(
  file = names(sources),
  source_url = unname(sources),
  md5 = unname(tools::md5sum(file.path(raw_dir, names(sources)))),
  stringsAsFactors = FALSE
)
utils::write.csv(manifest, file.path(raw_dir, "download_manifest.csv"), row.names = FALSE)
print(manifest)
