#' Create a new blog post
#'
#' Creates a dated directory under posts/ with an index.qmd stub.
#'
#' @param slug short kebab-case name, e.g. "gdal-warp-tricks"
#' @param title post title (defaults to humanised slug)
#' @param categories character vector of categories
#' @param open open the file for editing (RStudio/Positron)
#' @examples
#' new_post("gdal-warp-tricks", "GDAL warp tricks", c("code", "news"))
new_post <- function(slug,
                     title = gsub("-", " ", slug),
                     categories = c("code", "news"),
                     open = interactive()) {

  date <- format(Sys.Date(), "%Y-%m-%d")
  dir_name <- paste0(date, "_", slug)
  dir_path <- file.path("posts", dir_name)

  if (dir.exists(dir_path)) {
    stop(dir_path, " already exists", call. = FALSE)
  }
  dir.create(dir_path, recursive = TRUE)

  cats <- paste0('  - ', categories, collapse = '\n')
  yaml <- sprintf('---
title: "%s"
author: "Michael Sumner"
date: "%s"
categories:
%s
---

', title, date, cats)

  qmd_path <- file.path(dir_path, "index.qmd")
  writeLines(yaml, qmd_path)
  message("Created ", qmd_path)

  if (open && requireNamespace("rstudioapi", quietly = TRUE)) {
    if (rstudioapi::isAvailable()) {
      rstudioapi::navigateToFile(qmd_path)
    }
  }
  invisible(qmd_path)
}
