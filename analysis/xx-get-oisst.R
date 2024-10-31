library(devtools)
library(dplyr)
library(terra)
library(gstat)
library(sf)
library(stars)
# library(ncdf4)
library(ggplot2)
library(concaveman)
library(stringr)
library(pacea)

sf_use_s2(FALSE)  # remove spherical geometry (s2) for sf operations

d <- oisst_month %>%
  st_transform(crs = "EPSG:3005")

surf_dat <- sfc_as_cols(d)


# co-ordinate system so far
surf_hotssea_cave <- surf_dat %>%
  na.omit() %>%
  concaveman::concaveman()
# plot(surf_hotssea_cave)  # used to plot as a rectangle-ish when we had 0's
# (that we interpreted as real values), but now a concave
# outline around everything (one single outline, no islands) because we've used NA's

## mask with coastline, I think this is kind of a fix as we've used surface info
surf_hotssea_buff <- surf_dat %>%
  na.omit() %>%
  st_geometry() %>%
  st_buffer(dist = 1500) %>%
  st_union() %>%
  st_as_sf()
# plot(surf_hotssea_buff) # This is now the non-NA values (when they were 0's
# it loked like hotssea_cave, and so shows islands.

# TODO put back in when possible
# rm(snc_dat, snc_lon, snc_lat, svar, sdat)
# END parameters
#####




for(i in nc_filenames[1]){  # TODO put back in for all of them
  i <- nc_filenames[2]   # for running line by line, doing temp

  # Automatically create object name, and then the variable name to extract
  obj_name <- stringr::str_replace(i, "1980to2018_", "") %>%
    stringr::str_replace(".nc", "")

  # If an average over depths then add in 'avg' for consistency with bccm
  if(!(stringr::str_detect(i, "surface")) & !(stringr::str_detect(i, "bottom"))){
    obj_name <- stringr::str_replace(obj_name,
                                     "oisst_month_grid")
  }


  j <- "sst"


  # Put sst into dataframe and sf object
  dat <- data.frame(x = nc_lon,
                    y = nc_lat) %>%
    cbind(nc_varmat)
  dat_sf <- st_as_sf(dat,
                     coords = c("x", "y"),
                     crs = "EPSG:4326")    # Okay for Greig's
  tdat_sf <- st_transform(dat_sf,
                          crs = "EPSG: 3005")                 # BC Albers

  # Calculations earlier were for surface to give the surf_hotssea_cave and
  # surf_hotssea_buff that get used below. Presume they're still needed. Also
  # surface calculations give the cnames (column names.
  # Then here inside the loop
  # they're done for each object (because deep ones won't have the same coverage).
  # Some looks like overkill but I think the conversions were needed to get to the
  # same format.
  hotssea_cave <- tdat_sf %>%
    na.omit() %>%
    concaveman::concaveman()

  hotssea_buff <- tdat_sf %>%
    na.omit() %>%
    st_geometry() %>%
    st_buffer(dist = 1500) %>%
    st_union() %>%
    st_as_sf()

  # This took 6 minutes (which is shorter now using hotssea_buff not _poly):
  output2 <- point2rast(data = tdat_sf,
                        spatobj = hotssea_buff,
                        loc = llnames,
                        cellsize = 1500,       # Want 1500 not 2000
                        nnmax = nmax,
                        as = "SpatRast")

  # This is a "SpatRaster" object, doesn't plot well
  # plot(output2) # with roms_buff gave fancy artwork. Looks wrong but could be the
  # plotting as it's a SpatRaster.
  # plot(output2)  - now gives something sensible, using my new hotssea_poly

  # crop out grid cells with polygon masks
  t2_sf <- output2 %>%
    terra::mask(hotssea_poly) %>%      # Back to hotssea_poly
    stars::st_as_stars() %>%  ## check here for converting to points (not raster)
    st_as_sf()

  # This is needed as we used hotssea_poly above, and t2_sf has 31951 features.
  #  This may not all be needed, but just
  #  leave in as it's all a bit subtle and Travis spent a lot of time figuring it out.
  ##### BC MASK OPTION 2 - Using roms outline
  # 1. use roms_cave
  t2_sfb <- t2_sf[hotssea_cave, ]

  # 2. use roms_buff to get haida gwaii outline and shore - do, as likely
  # needed for elsewhere:
  t2_sfb <- t2_sfb[hotssea_buff,]

  # 3. use default surface roms_cave
  t2_sfb <- t2_sfb[surf_hotssea_cave,]

  # 4. use default surface roms_buff
  t2_sf <- t2_sfb[surf_hotssea_buff,]    # Carefull, going back to t2_sf
  # With NA stuff is now:
  # Simple feature collection with 10731 features and 468 fields
  # Whereas was:
  # Simple feature collection with 34515 features and 468 fields
  # Geometry type: POLYGON

  # assign column names as year_month
  names(t2_sf)[1:(ncol(t2_sf) - 1)] <- cnames

  # round to 6 decimal places to reduce file size
  t3_sf <- t2_sf %>%
    st_drop_geometry() %>%
    round(digits = 6) %>%
    st_as_sf(geometry = st_geometry(t2_sf))

  class(t3_sf) <- c("pacea_st",
                    "sf",
                    "tbl_df",
                    "tbl",
                    "data.frame")

  # assign units attribute
  attr(t3_sf, "units") <- jvars_table[which(jvars_table[, 1] == j), 3]
  attr(t3_sf, "restrict_plotting_range") <- TRUE      # To then use to automatically restrict
  # the plotting
  attr(t3_sf, "salinity_unit") <- "PSU"      # To automate the axes labels

  filename <- paste0(pacea_data_dir,
                     obj_name,
                     "_",
                     version,
                     ".rds")
  assign(obj_name, t3_sf)

  do.call("save", list(as.name(obj_name), file = filename, compress = "xz"))

  # Don't worry about timing, probably for when Travis was testing.
  # end <- Sys.time()
  # jtime <- end-start
  # print(jtime)
  # names(jtime) <- paste(depth_range_i, tj, sep="_")
  # proctimes <- c(proctimes, jtime)

  # Manually add names to data-raw/data-key/hotssea_data_list.csv
  # remove files
  # TODO update this:
  #rm(dat, dat_sf, tdat_sf, roms_cave, roms_buff,
  #   output2, output6, t2_sf2, t2_sf6, t2_sf26,
  #   t2_sf26a, t2_sf26b, t3_sf26, nc_var, nc_varmat)
  # rm(list = objname)   # TODO add back in when have saved and reloaded .rds
  gc()
}
