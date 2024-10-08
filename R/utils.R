#' helper functions
#' @export
#'
update_mesh <- function(x) { # x = fitted sdmTMB model
  make_mesh(x$data, x$spde$xy_cols, mesh = x$spde$mesh)
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
    labs(x = "", y = "")
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

