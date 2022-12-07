#' #' https://stackoverflow.com/questions/74705976/how-to-import-sinusoidal-remotesensed-data-into-raster-in-r
#' #' 
#' #' https://gist.github.com/mdsumner/dbade9bb2a52bc53ce5413bf20c274e2
#' #' 
#' #' 
#' #' that looks like L3bin to me, not a raster but a ragged array
#' #' 
#' #' 
#' #' I've got stuff in this package: https://github.com/hypertidy/L3bin
#' #' 
#' #' Integerized Sinusoidal Binning Scheme for Level 3 Data
#' #' Website
#' #' https://hypertidy.github.io/L3bin/
#' #' it's not exactly obvious, or even easy to use - but it's very cool and readily useable once you understand what L3b is (generally the nasa ocean colour files have "L3m" (for mapped, global grid) vs "L3b" (for L3bin, sparse integerized sinusoidal grid) in the filename (L3m is derived from L3bin, which is stored that way to preserve statistical properties as one step up from L2 swaths summarized into daily bins) - there's a classic SeaWiFS tech paper about the stats and bins
#' #' 
#' #' 
#' #' um, but also not easy to read in - I've use various forms of HDF5 packages in the past, you have to navigate the compound types and groups in the NetCDF - if you don't get any feedback I'm happy to show some stuff
#' #' 
#' #'     looks like you can get the bin values from terra, but not the bin index, it's in a different group in the file and GDAL won't see it)
#' #' 
#' #' here's my old read code https://github.com/sosoc/croc/blob/master/R/read_L3_file_hdf5.R but I very much expect that won't work out of the box!
#' #' 
#' #'  read_L3_file_hdf5.R
#' #' #' Read Level-3 ocean colour.
#' #' #'
#' #' #'
#' #' #' Read the compound types (i.e. tables of variables) from ocean colour L3 NetCDF files.
#' #'                 #'
#' #' #' `read_binlist` for just the 'BinList'
#' #' e
#' #' <https://github.com/sosoc/croc|sosoc/croc>sosoc/croc | Added by GitHub
#'  
#'    
#'    Michael Sumner
#'    6 hours ago
#'    oh I see there are lon and lat vars in there, and the bin index is there too but hidden
#'    
#'    
#'    Michael Sumner
#'    6 hours ago
#'    I'l be back with an answer :grinning:
#' #' 
#' #' 
#' #' here's enough to get by on https://gist.github.com/mdsumner/dbade9bb2a52bc53ce5413bf20c274e2, just assuming a longlat grid but you could obviously pick any, I have more crafy bin-summarizing code in other projects IYI
#'    :heart:
#'      1
#'    
#'    
#'    
#'    Philippe Massicotte
#'    4 hours ago
#'    As usual, 
#'    @mdsumner
#'    you have a simple and elegant solution. Going to experiment it today!
#'   
#'       I did years of that kind of stuff, the cell thing in raster was a game changer (and fasterize made it blazing)
#'   
#'    fwiw the lon,lat is entirely redundant given nrows of the grid and bin index, and each bin has a specific bounds based on that (and all are equal area), just there might be more cunning aggregates for you to do based on the bin as group :pray:
#' #' 

f <- "~/ESACCI-OC-L3S-CHLOR_A-MERGED-1D_DAILY_4km_SIN_PML_OCx-20070101-fv5.0.nc"

```
library(tidync)
##
## the bins, this is every single bin for this sinusoidal grid
bins <- tidync::tidync(f) |> activate("D1") |>
 hyper_tibble()


minbin <- bins |> dplyr::filter(lat >=40) |> summarize(bin = min(bin_index)) |> dplyr::pull(bin)
## the bin is for joining on which bin_index is used here
d <- tidync::tidync(f) |> activate("D1,D0") |>
  hyper_filter(bin_index = bin_index >= minbin) |> 
  hyper_tibble(select_var = c("chlor_a")) |> 
  dplyr::inner_join(bins, "bin_index")
  
d$time <- NULL

plot(d$lon, d$lat, pch = ".", col = palr::chl_pal(d$chlor_a), asp = 1/cos(mean(d$lat) * pi/180))
  

## define a raster
library(terra)
r <- terra::rast(terra::ext(c(-180, 180, min(d$lat), max(d$lat))), nrows = 90, ncols = 180, crs = "OGC:CRS84")

cells <- tibble::tibble(cell = terra::cellFromXY(r, cbind(d$lon, d$lat)), chlor_a = d$chlor_a) |> 
  group_by(cell) |> summarize(chlor_a = mean(chlor_a))
r[cells$cell] <- cells$chlor_a
pal <- palr::chl_pal(palette = TRUE)
image(r, col = pal$cols[-1], breaks = pal$breaks)

```
