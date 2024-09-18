#' Plot the output from a geostatistical models
#'
#' Takes a prediction grid and creates a map of the model
#' predictions and/or the raw data. Includes a number of options for customizing
#' the map including the ability to rotate the map.
#'
#' @param pred_dat The `predictions`.
#' @param raw_dat The `data` element of the model.
#' @param fill_column The name of the column to plot. Must be in natural space.
#' @param fill_scale A ggplot `scale_fill_*` object.
#' @param colour_scale A ggplot `scale_colour_*` object. You likely want this to
#'   match `fill_scale` unless you want the map to look strange.
#' @param pos_pt_col The color for positive set location points.
#' @param bin_pt_col The color for binary set location points.
#' @param pos_pt_fill The fill color for positive set location points.
#' @param pt_size_range The range of point sizes for positive set location
#'   points.
#' @param show_legend Logical for whether or not to show the legend.
#' @param extrapolate_depth Logical for whether or not to show predictions
#'   across all depths in the survey domain (the default) or to not extrapolate
#'   beyond the range of the observed sets in the data set.
#' @param extrapolation_buffer A buffer to add to the minimum and maximum
#'   observed depths if `extrapolate_depth = TRUE`.
#' @param show_model_predictions Logical for whether or not to show the
#'   geostatistical model predictions.
#' @param show_raw_data Logical for whether or not to show the raw data.
#' @param utm_zone The UTM zone of grid and model data used.
#' @param fill_label A label to use in the legend for the fill color.
#' @param pt_label A label to use in the legend for the point size.
#' @param rotation_angle An angle to rotate the entire map. Can be useful to
#'   make a map of the BC coast take up less. Defaults to not rotating the map.
#'   The groundfish synopsis report uses `rotation_angle = 40`.
#' @param rotation_center The coordinates around which to rotate the mouth.
#'   These should be in UTM coordinates.
#' @param show_axes Logical for whether or not to show the axes.
#' @param xlim X axis limits in UTM coordinates. The synopsis report uses
#'   `c(360, 653)`. Defaults to the range of the data.
#' @param ylim Y axis limits in UTM coordinates. The synopsis report uses
#'   `c(5275, 6155)`. Defaults to the range of the data.
#' @param x_buffer A buffer in UTM coordinates to extend the X axis. Mostly
#'   useful if the axis limits aren't explicitly specified.
#' @param y_buffer A buffer in UTM coordinates to extend the Y axis. Mostly
#'   useful if the axis limits aren't explicitly specified.
#' @param bounds dataframe of coordinates from which to get limits.
#' @param north_symbol Logical for whether to include a north symbol.
#' @param north_symbol_coord Coordinates for the north symbol in UTM
#'   coordinates.
#' @param north_symbol_length Length of the north assemble arrow.
#' @param cell_size The size of the grid cells for the model predictions.
#' @param circles Logical for whether to plot the model predictions in circles.
#'   This analysis report uses this for the IPHC survey.
#'
#' @return
#' A ggplot object.
#'
#' @export
#' @examples
plot_predictions <- function(pred_dat, raw_dat,
                             fill_column = "density",
                             fill_scale =
                               ggplot2::scale_fill_viridis_c(),
                             colour_scale =
                               ggplot2::scale_colour_viridis_c(),
                             pt_column = "density",
                             pos_pt_col = "#FFFFFF60",
                             bin_pt_col = "#FFFFFF40",
                             pos_pt_fill = "#FFFFFF05",
                             pt_size_range = c(0.5, 9),
                             show_legend = TRUE,
                             extrapolate_depth = TRUE,
                             extrapolation_buffer = 0,
                             show_model_predictions = TRUE,
                             show_raw_data = TRUE,
                             utm_zone = 9,
                             fill_label = "Predicted\nbiomass\ndensity (kg/m^2)",
                             pt_label = "Tow density (kg/m^2)",
                             rotation_angle = 0,
                             rotation_center = c(500, 5700),
                             show_axes = FALSE,
                             xlim = NULL,
                             ylim = NULL,
                             bounds = NULL,
                             x_buffer = c(-0.3, 0.5),
                             y_buffer = c(-0.1, 0.1),
                             include_bath = FALSE,
                             north_symbol = FALSE,
                             north_symbol_coord = c(130, 5975),
                             north_symbol_length = 30,
                             cell_size = 2, circles = FALSE) {
  # fill_column <- match.arg(fill_column)
  if (!extrapolate_depth) {
    pred_dat <- filter(
      pred_dat,
      depth >= min(raw_dat$depth, na.rm = TRUE) - extrapolation_buffer,
      depth <= max(raw_dat$depth, na.rm = TRUE) + extrapolation_buffer,
      depth > 0
    )
  }

  pred_dat$id <- NA # for circles

  # if(fill_column == "density"){pred_dat$cond_fac <- NA}
  # if(fill_column == "cond_fac"){pred_dat$density <- NA}

  if(!("year" %in% colnames(pred_dat))){pred_dat$year <- NA}

  if (show_model_predictions && !circles) {
    # turn grid into explicit rectangles for possible rotation:
    pred_dat <- lapply(seq_len(nrow(pred_dat)), function(i) {
      row_dat <- pred_dat[i, , drop = FALSE]
      X <- row_dat$X
      Y <- row_dat$Y
      data.frame(
        X = c(
          X - cell_size / 2, X + cell_size / 2,
          X + cell_size / 2, X - cell_size / 2
        ),
        Y = c(
          Y - cell_size / 2, Y - cell_size / 2,
          Y + cell_size / 2, Y + cell_size / 2
        ),
        fill_column = row_dat[fill_column],
        # density = row_dat$density,
        # cond_fac = row_dat$cond_fac,
        # bin = row_dat$bin,
        # pos = row_dat$pos,
        year = row_dat$year,
        id = i
      )
    }) %>% bind_rows()
  }
# browser()
  if (north_symbol) {
    north <- data.frame(
      X = c(north_symbol_coord[1], north_symbol_coord[1]),
      Y = c(north_symbol_coord[2], north_symbol_coord[2] + north_symbol_length)
    )
    north_lab_coord <- c(north$X[1], north$Y[1] - 15)

    north <- gfplot:::rotate_df(north, rotation_angle, rotation_center)

    north_sym <- data.frame(
      X = north$X[1],
      Xend = north$X[2],
      Y = north$Y[1],
      Yend = north$Y[2]
    )

    r <- gfplot:::rotate_coords(north_lab_coord[1], north_lab_coord[2],
                       rotation_angle = rotation_angle,
                       rotation_center = rotation_center
    )
    north_lab_coord <- c(r$x, r$y)
  }

  coast <- gfplot:::load_coastline(range(raw_dat$lon) + c(-1, 1),
                          range(raw_dat$lat) + c(-1, 1),
                          utm_zone = utm_zone
  )
  coast <- gfplot:::rotate_df(coast, rotation_angle, rotation_center)


  isobath <- gfplot:::load_isobath(range(raw_dat$lon) + c(-5, 5),
                          range(raw_dat$lat) + c(-5, 5),
                          bath = c(100, 200, 500), utm_zone = 9
  )
  isobath <- gfplot:::rotate_df(isobath, rotation_angle, rotation_center)

  pred_dat <- gfplot:::rotate_df(pred_dat, rotation_angle, rotation_center)
  raw_dat <- gfplot:::rotate_df(raw_dat, rotation_angle, rotation_center)

  if (is.null(xlim) || is.null(ylim)) {
    if(!is.null(bounds)){
      bounds <- gfplot:::rotate_df(bounds, rotation_angle, rotation_center)
      xlim <- range(bounds$X) + x_buffer
      ylim <- range(bounds$Y) + y_buffer
    } else {
    xlim <- range(pred_dat$X) + x_buffer
    ylim <- range(pred_dat$Y) + y_buffer
  }
  }


  g <- ggplot()

  if(length(unique(pred_dat$year))>1){
  g <- g + facet_wrap(~ year)
}
  if (show_model_predictions && !circles) {
    g <- g + ggplot2::geom_polygon(
      data = pred_dat, aes_string("X", "Y",
                                  fill = fill_column,
                                  colour = fill_column, group = "id"
      )
    ) +
      fill_scale + colour_scale
  }
  if (show_raw_data) {
    g <- g +
      geom_point(
        data = filter(raw_dat, present == 0),
        aes_string(x = "X", y = "Y"),
        col = if (show_model_predictions) bin_pt_col else "grey50",
        pch = 4, size = 0.5
      ) +
      geom_point(
        data = filter(raw_dat, present == 1),
        aes_string(
          x = "X", y = "Y",
          size = pt_column
        ), fill = pos_pt_fill,
        col = if (show_model_predictions) pos_pt_col else "grey30", pch = 21
      )
  }

  g <- g +
    ggplot2::scale_size_continuous(range = pt_size_range) +
    gfplot::theme_pbs() +
    coord_equal(xlim = xlim, ylim = ylim) +
    guides(
      shape = ggplot2::guide_legend(override.aes = list(colour = "grey30")),
      size = ggplot2::guide_legend(override.aes = list(colour = "grey30"))
    ) +
    geom_polygon(
      data = coast, aes_string(x = "X", y = "Y", group = "PID"),
      fill = "grey87", col = "grey70", lwd = 0.2
    ) +
    guides(shape = "none", colour = "none") +
    labs(size = pt_label, fill = fill_label) +
    ylab("Northing") +
    xlab("Easting")

  if (!show_legend) {
    g <- g + theme(legend.position = "none")
  }

  if (!show_axes) {
    g <- g + theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    )
  }

  if(include_bath){
  suppressWarnings({
    suppressMessages({
      g <- g + geom_path(
        data = isobath, aes_string(
          x = "X", y = "Y",
          group = "paste(PID, SID)"
        ),
        inherit.aes = FALSE, lwd = 0.4, col = "grey70", alpha = 0.4
      )})})
  }

  # plot circles on top of land for inlets:
  if (show_model_predictions && circles) {
    g <- g + ggplot2::geom_point(
      data = pred_dat, aes_string("X", "Y",
                                  fill = fill_column, colour = fill_column, group = "id"
      ), size = cell_size, pch = 21
    ) +
      fill_scale + colour_scale
  }

  if (north_symbol) {
    g <- g + ggplot2::geom_segment(
      data = north_sym,
      aes_string(x = "X", y = "Y", xend = "Xend", yend = "Yend"),
      inherit.aes = FALSE, colour = "grey30", lwd = 0.8,
      arrow = ggplot2::arrow(length = unit(0.7, "char"))
    )
    g <- g + ggplot2::annotate("text",
                               label = "N", colour = "grey30",
                               x = north_lab_coord[1], y = north_lab_coord[2]
    )
  }

  g
}
