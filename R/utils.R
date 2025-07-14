.ggsave <- function(filename, ...) {
  # if (FRENCH) filename <- file.path("fr", filename)
  op <- options()
  if (exists("FRENCH")){if (FRENCH) options(OutDec = ",")}
  ggplot2::ggsave(filename, ...)
  options(op)
}


#' helper functions
#' @export
#'
update_mesh <- function(x) { # x = fitted sdmTMB model
  make_mesh(x$data, x$spde$xy_cols, mesh = x$spde$mesh)
}

#' functions for getting final model structures
#' @export
get_model_structure <- function(fit){
  if(sdmTMB:::is_delta(fit)){
    tibble(`Range shared` = range_shared(fit),
       `Spatial field on 1st component` = spatial_estimated(fit)[1],
       `Spatial field on 2nd component` = spatial_estimated(fit)[2],
       `Spatiotemporal field on 1st component` = spatiotemporal_estimated(fit)[1],
       `Spatiotemporal field on 2nd component` = spatiotemporal_estimated(fit)[2],
       `Delta model` = sdmTMB:::is_delta(fit),
       Anisotropy = fit$call$anisotropy,
       Family = ifelse(sdmTMB:::is_delta(fit), fit$family$clean_name,fit$family$family)
    )
  } else {
    # browser()
    # can be simplified as long as column names match
    tibble(`Range shared` = range_shared(fit),
           `Spatial field on 1st component` = spatial_estimated(fit)[1],
           # `Spatial field on 2nd component` = spatial_estimated(fit)[2],
           `Spatiotemporal field on 1st component` = spatiotemporal_estimated(fit)[1],
           # `Spatiotemporal field on 2nd component` = spatiotemporal_estimated(fit)[2],
           # `Delta model` = sdmTMB:::is_delta(fit),
           Anisotropy = fit$call$anisotropy,
           Family = fit$family$family
    )
  }
}

#' @export
#'
spatial_estimated <- function(x) {
  p <- get_pars(x)
  !p$ln_tau_O == 0
}

#' @export
#'
spatiotemporal_estimated <- function(x) {
  p <- get_pars(x)
  !p$ln_tau_E == 0
}

#' @export
#'
range_shared <- function(x) {
  p <- get_pars(x)
  (length(unique(p$ln_kappa)) == 1 && !sdmTMB:::is_delta(x)) ||
    (length(unique(p$ln_kappa)) == 2 && sdmTMB:::is_delta(x))
}

#' @export
#'
format_survey_labels <- function(data) {
  data |> mutate(
      survey_abbrev = ifelse(survey_series_id == 79, "TRIENNIAL", survey_abbrev),
      survey_abbrev = ifelse(survey_series_id == 68, "HAKE", survey_abbrev),
      survey_abbrev = ifelse(survey_series_id == 5, "HS PCOD", survey_abbrev),
      survey_abbrev = ifelse(survey_series_id == 11, "THORNYHEAD", survey_abbrev),
      # survey_area = ifelse(survey_abbrev == "HS MSA", "SYN HS",
      #                 ifelse(survey_abbrev == "MSSM QCS", "SYN QCS",
      #                 ifelse(survey_abbrev == "MSSM WCVI", "SYN WCVI",
      #                 survey_abbrev))),
      survey_type = as.factor(
        case_when(
          survey_abbrev %in% c("HBLL OUT S", "HBLL OUT N")~"HBLL",
          survey_abbrev == "HS MSA"~"MSA",
          survey_abbrev %in% c("MSSM WCVI", "MSSM QCS") & year>2002 & year<=2005~"MSSM<=05",
          survey_abbrev %in% c("MSSM WCVI", "MSSM QCS") & year>2005~"MSSM>05",
          survey_abbrev %in% c("MSSM WCVI", "MSSM QCS") & year <= 2002~"MSSM <03",
          survey_abbrev %in% c("SYN HS", "SYN QCS", "SYN WCHG", "SYN WCVI")~"SYN",
          survey_abbrev %in% c("EUL N", "EUL S")~"EUL",
          TRUE~survey_abbrev
        ))
    )
}


label_yrs <- function(x, start_year = yrs[1]) {
  x + start_year - 1
}

#'
#' @export
#'
sfc_as_cols <- function(x, names = c("x","y")) {
  stopifnot(inherits(x,"sf") #&& inherits(sf::st_geometry(x),"sfc_POINT")
  )
  ret <- sf::st_coordinates(x)
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  x <- x[ , !names(x) %in% names]
  ret <- setNames(ret,names)
  dplyr::bind_cols(x,ret)
}

#'
#' @export
#'
plot_index <- function(dat, species, group_name, model_string, filename, remove_extra_years = NULL){
  if (!file.exists(filename)) {
    i <- get_index(dat, area = 4, bias_correct = TRUE)
    i$species <- species
    i$group <- group_name
    i$model_string <- model_string

  } else {
    i <- readRDS(filename)
    i$species <- species
    i$group <- group_name
    i$model_string <- model_string
  }

  if(!is.null(remove_extra_years)) {
    i <- filter(i, !(year %in% remove_extra_years))
  }

  saveRDS(i, filename)

  ggplot(i, aes(year, est)) +
    geom_line() +
    geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.4) +
    xlab("Year") +
    ylab("Biomass estimate (kg)")
}

#'
#' @export
#'
split_index_by_survey <- function(model, grid, species, group_name){

  grid <- filter(grid, year %in% c(sort(unique(model$data$year))))

  p <- grid |>
    split(grid$survey) |>
    lapply(function(x) predict(model, re_form_iid = NA, newdata = x,
                               return_tmb_object = TRUE))
  i <- purrr::map_dfr(p, get_index, area = 4, .id = "survey")
  i$surveys <- paste0(unique(model$data$survey_type), collapse=", ")
  i$species <- species
  i$group <- group_name
  i$index <- paste0(i$group, "\n(", i$surveys, ")")
  i$model <- paste0(
    ifelse(isTRUE(model$family$delta), model$family$clean_name, paste0(model$family[1], "(link = 'log')")),
    "\nspatial (", model[["spatial"]][1], ", ", model[["spatial"]][2], ")")

  saveRDS(i, paste0("data-generated/density-split-ind/temp-index-split-",
                    gsub(" ", "-", gsub("\\/", "-", tolower(species))), "-",
                    gsub(" ", "-", group_name), ".rds"))

  return(i)
}



#'
#' @export
#'
map_density <- function(dat, filename, variable = "density_trimmed",
                        variable_label = "Density \n(kg/ha)",
                        col_trans = fourth_root_power_trans()
) {

 if(!is(dat, "data.frame")){
  if (length(dat$fit_obj$family$family)>1) {
    p1 <- dat$data %>% mutate(
      density = dat$fit_obj$family[[1]]$linkinv(est1) * dat$fit_obj$family[[2]]$linkinv(est2),
      density_trimmed = ifelse(density > quantile(density, 0.995),
                               quantile(density, 0.995),
                               density))
  } else {
    p1 <- dat$data %>% mutate(density = dat$fit_obj$family$linkinv(est),
                              density_trimmed = ifelse(density > quantile(density, 0.995),
                                                       quantile(density, 0.995),
                                                       density))
  }
   saveRDS(p1, filename)

 } else {
   p1 <- dat
 }
  ggplot(p1, aes_string("X", "Y", colour = variable, fill = variable)) +
    geom_tile(width = 2, height = 2, alpha = 1) +
    facet_wrap(~year) +
    scale_fill_viridis_c(trans = col_trans) +
    scale_colour_viridis_c(trans = col_trans) +
    labs(x = "", y = "", colour = variable_label, fill = variable_label)


}


get_wraper <- function(x) {
  lapply(strwrap(x, width = 30, simplify = FALSE), paste, collapse="\n")
}


Flatfish <- c(
  "Curlfin Sole",#
  "Butter Sole",
  "Sand Sole",#
  "Petrale Sole", #
  "Arrowtooth Flounder", #
  "English Sole",#
  "Dover Sole",#
  "Rex Sole", #
  "Flathead Sole",#
  "Southern Rock Sole",#
  "Slender Sole",#
  "Pacific Sanddab",#
  "Pacific Halibut"#
)

Rockfish <- c(
  "Pacific Ocean Perch",
  "Bocaccio",
  "Canary Rockfish",
  "Redstripe Rockfish", # MSA added with mean > 4
  "Rougheye/Blackspotted Rockfish Complex", # WILL NEED UPDATE FOR ALL MAT CLASSES
  "Silvergray Rockfish", # MSA added with mean > 5
  "Shortspine Thornyhead",
  "Widow Rockfish", # hake would need mean > 1, mssm1 > 4
  "Yelloweye Rockfish",
  "Yellowmouth Rockfish", #
  "Yellowtail Rockfish"
)

flatfish <- tolower(Flatfish)
rockfish <- tolower(Rockfish)

