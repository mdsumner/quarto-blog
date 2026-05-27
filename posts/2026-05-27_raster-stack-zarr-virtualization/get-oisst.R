library(glue)

date  <- seq(as.Date("2025-01-01"), by = "16 days", length.out = 23)
base0 <- "https://www.ncei.noaa.gov/data/sea-surface-temperature-optimum-interpolation/v2.1/access/avhrr"
ym    <- format(date, "%Y%m")
ymd   <- format(date, "%Y%m%d")
urls  <- glue("{base0}/{ym}/oisst-avhrr-v02r01.{ymd}.nc")

dir   <- tempdir()
files <- file.path(dir, basename(urls))
download.file(urls, files, method = "libcurl", mode = "wb")
