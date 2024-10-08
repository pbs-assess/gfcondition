# get roms covariates from pacea-data-main and using their spatial_average function
# updated here with a default polygon for whole outer coast because I couldn't install the package
# remotes::install_github("pbs-assess/pacea") ## not working so retrieved files from github
library(pacea)
library(tidyverse)

# download all variables
bccm_all_variables()


spatial_average <- function(pacea_st_obj,
                            area = list(matrix(c(-134, -131, -128, -124, -125.5, -134, -134,
                                                 54.4, 54.4, 50.5, 48, 47.5, 52, 54.4),
                                               ncol = 2))
){
  stopifnot("pacea_st_obj must be of class pacea_st" =
              ("pacea_st" %in% class(pacea_st_obj)))

  # convert area to a simple features object, with the correct projection
  area_sf <- sf::st_sfc(sf::st_polygon(area),
                        crs = 4326) %>%
    sf::st_as_sf() %>%
    sf::st_transform(crs = 3005)

  # this filters to just the required area
  obj_area <- pacea_st_obj[area_sf, ]

  obj_area_drop <- sf::st_drop_geometry(obj_area) %>%
    as_tibble()

  avg <- colMeans(obj_area_drop)

  obj_area_tib <- tibble::tibble(value = avg)

  obj_area_tib$year <- as.numeric(substr(names(avg),
                                         1,
                                         4))
  obj_area_tib$month <- as.numeric(substr(names(avg),
                                          6,
                                          7))

  obj_area_tib <- dplyr::relocate(obj_area_tib,
                                  year,
                                  month)
  obj_area_tib
}


load_coast <- function(xlim_ll, ylim_ll, buffer = 0) {
  data("nepacLLhigh", package = "PBSmapping", envir = environment())
  np <- PBSmapping::clipPolys(nepacLLhigh,
                              xlim = xlim_ll + c(-buffer, buffer),
                              ylim = ylim_ll + c(-buffer, buffer)
  )
  # ll2utm(np, utm_zone = utm_zone)
}

area <- list(matrix(c(-134.5, -131, -127, -128.25, -124, -125.5, -132, -134.5,
                      54.4, 54.4,   51,   50.75, 48.5, 47.75, 52, 54.4),
            ncol = 2))


coast <- load_coast(range(area[[1]][,1]) + c(-0.2, 0.2),
                    range(area[[1]][,2]) + c(-0.2, 0.2)
)

area_xy <- as.data.frame(area)
area_xy$X <- area_xy$X1
area_xy$Y <- area_xy$X2

ggplot(area_xy, aes(X, Y)) +
  geom_polygon(alpha =0.3, fill = "skyblue") +
  geom_polygon(
    data = coast, aes_string(x = "X", y = "Y", group = "PID"),
    fill = "grey87", col = "grey70", lwd = 0.2
  ) +
  xlab("Longitude") + ylab("Latitude")

ggsave("figs/bccm-area.png", width = 3, height = 3)

# area_sf <- sf::st_sfc(sf::st_polygon(area),
#                       crs = 4326) %>%
#   sf::st_as_sf() %>%
#   sf::st_transform(crs = 3005)
#
# plot(area_sf)

# load("../pacea-data-main/data/bccm_primaryproduction_01.rds")
# pp <- spatial_average(bccm_primaryproduction, area = area)
pp <- spatial_average(bccm_primaryproduction(), area = area)
saveRDS(pp, "data-raw/cw_primary_production.rds")

# load("../pacea-data-main/data/bccm_phytoplankton_01.rds")
pp <- spatial_average(bccm_phytoplankton(), area = area)
saveRDS(pp, "data-raw/cw_phytoplankton.rds")

# load("../pacea-data-main/data/bccm_bottom_oxygen_01.rds")
pp <- spatial_average(bccm_bottom_oxygen(), area = area)
saveRDS(pp, "data-raw/cw_bottom_oxygen.rds")

# load("../pacea-data-main/data/bccm_surface_oxygen_01.rds")
pp <- spatial_average(bccm_surface_oxygen(), area = area)
saveRDS(pp, "data-raw/cw_surface_oxygen.rds")

# load("../pacea-data-main/data/bccm_bottom_temperature_01.rds")
pp <- spatial_average(bccm_bottom_temperature(), area = area)
saveRDS(pp, "data-raw/cw_bottom_temperature.rds")

# load("../pacea-data-main/data/bccm_surface_temperature_01.rds")
pp <- spatial_average(bccm_surface_temperature(), area = area)
saveRDS(pp, "data-raw/cw_surface_temperature.rds")



# pacea_long <- function(data, names_to = "date", values_to = "value") {
#
#   dat <- data %>%
#     tidyr::pivot_longer(cols = !last_col(), cols_vary = "slowest", names_to = names_to, values_to = values_to)  %>%
#     mutate(year = as.numeric(substr(get(names_to), 1, 4)),
#            month = as.numeric(substr(get(names_to), 6, 7))) %>%
#     dplyr::select(year, month, value, geometry)
#
#   return(dat)
# }
