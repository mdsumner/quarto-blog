## update_categories.R
## Run from the quarto-blog root directory
## Backs up each file before modifying

library(here)

# Map post folder names to new categories
category_map <- list(
  "2026-02-07_cryptic-curvilinear-detection" = c("gdal", "raster", "netcdf", "projections"),
  "2025-12-21_vaster-grid-logic"             = c("raster", "vaster"),
  "2025-09-04_broken-netcdf"                 = c("netcdf", "gdal"),
  "2025-03-12_r-py-multidim"                 = c("gdal", "zarr", "netcdf"),
  "2024-12-11_conservative_gdal"             = c("gdal", "raster"),
  "2024-12-05_idea_update"                   = c("overview"),
  "2024-12-04_plot_native"                   = c("raster", "gdal"),
  "2023-04-22_gdal_image_tiles"              = c("gdal", "r-packages", "ceramic"),
  "2022-12-09_netcdf-degenerate-rectilinear" = c("netcdf", "raster"),
  "2022-04-05_gdal_raster_blocks"            = c("gdal", "raster"),
  "2022-04-25_gdalwarper-in-r"               = c("gdal", "raster", "projections"),
  "2019-05-29_recent-rgl-format-changes"     = c("mesh", "rgl"),
  "2017-09-01_gdal-in-r"                     = c("gdal", "overview"),
  "2017-07-25_erddap-in-sf-and-ggplot2"      = c("netcdf", "r-packages"),
  "2017-01-10_r-spatial-2017"                = c("overview"),
  "2015-12-28_gis3d"                         = c("mesh", "rgl", "projections"),
  "2014-04-17_r-matrices-and-image"          = c("raster")
)

update_post_categories <- function(post_dir, new_cats) {
  # find the qmd file - could be index.qmd or named after the slug
  qmd_files <- list.files(
    here("posts", post_dir),
    pattern = "\\.qmd$",
    full.names = TRUE
  )
  if (length(qmd_files) == 0) {
    message("  No .qmd found in: ", post_dir)
    return(invisible(NULL))
  }
  # if multiple qmd files, prefer index.qmd, else take the first

  if (length(qmd_files) > 1) {
    idx <- grep("index\\.qmd$", qmd_files)
    if (length(idx) > 0) {
      qmd_files <- qmd_files[idx[1]]
    } else {
      qmd_files <- qmd_files[1]
    }
  }
  f <- qmd_files[1]
  lines <- readLines(f, warn = FALSE)

  # find the YAML block (between first --- and second ---)
  fence_lines <- which(lines == "---")
  if (length(fence_lines) < 2) {
    message("  Can't find YAML fences in: ", f)
    return(invisible(NULL))
  }
  yaml_start <- fence_lines[1]
  yaml_end   <- fence_lines[2]

  # find existing categories line within the YAML
  cat_idx <- grep("^categories:", lines[yaml_start:yaml_end]) + yaml_start - 1

  new_cat_line <- paste0("categories: [", paste(new_cats, collapse = ", "), "]")

  if (length(cat_idx) == 1) {
    # backup
    writeLines(lines, paste0(f, ".bak"))
    lines[cat_idx] <- new_cat_line
    message("  Updated: ", basename(f), " -> [", paste(new_cats, collapse = ", "), "]")
  } else if (length(cat_idx) == 0) {
    # no categories line - insert one before the closing ---
    writeLines(lines, paste0(f, ".bak"))
    lines <- append(lines, new_cat_line, after = yaml_end - 1)
    message("  Inserted categories in: ", basename(f), " -> [", paste(new_cats, collapse = ", "), "]")
  } else {
    message("  Multiple categories lines in: ", f, " — skipping")
    return(invisible(NULL))
  }

  writeLines(lines, f)
}

# run it
message("Updating post categories...\n")
for (post_dir in names(category_map)) {
  message(post_dir)
  if (!dir.exists(here("posts", post_dir))) {
    message("  Directory not found, skipping")
    next
  }
  update_post_categories(post_dir, category_map[[post_dir]])
}

message("\nDone. Backup files saved as .qmd.bak")
message("Review with: git diff posts/")
