#' Trim predictions to represent a percentage of the mean total abundance
#'
#' @param data A grid in which cells must be defined by "X", "Y", time variable is "year",
#'    and biomass/abundance stored as "density"
#' @param prop_threshold Default is 0.01, which keeps 99% of the predicted abundance
#'
#' @return
#' @export
#'
#' @examples
#'
trim_predictions <- function(data, prop_threshold = 0.01) {

  # calculate average density for each unique location/cell
  out <- data %>% group_by(X, Y) %>%
    mutate(mean_density = mean(density)) %>% ungroup()

  # select any one time slice (year)
  .data <- filter(out, year == max(year))

  # calculate the cumulative abundance threshold that represents chosen percent of total
  abund_threshold <- sum(.data$mean_density, na.rm = TRUE) * prop_threshold

  # sort and cumsum the mean densities from from one time slice (year)
  s <- sort(.data$mean_density)
  abund_sum <- cumsum(s)

  # find threshold for density that cumulatively accounts for chosen percent of total
  lower_density_threshold <- s[which(abund_sum >= abund_threshold)[1]]

  # filter full dataset to exclude cells below this threshold
  out <- filter(out, mean_density > lower_density_threshold)
  out
}

#' Trim predictions separately for each year
#'
#' @export
trim_predictions_by_year <- function(data, prop_threshold = 0.01) {
    out <- data %>%
      split(data$year) %>%
      purrr::map_dfr(trim_predictions, prop_threshold = prop_threshold, .id = "year")
    out$year <- as.numeric(out$year)
    out
}
