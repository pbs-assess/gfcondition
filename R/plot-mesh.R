#' Plot mesh
#'
#' @param mesh_obj Mesh object from sdmTMB
#' @param data_obj Raw data
#' @param catch_var Variable name in " " to be plotted
#' @param group Title for plot
#'
#' @export
#'
plot_mesh <- function(
    mesh_obj,
    data_obj,
    catch_var = "catch_weight",
    group = "Total") {
  g <- ggplot() +
    inlabru::gg(mesh_obj$mesh) +
    coord_fixed() +
    geom_point(aes(X, Y),
               shape = "x",
               size = 0.75,
               data = data_obj
    ) +
    geom_point(
      aes(X, Y,
          fill = .data[[catch_var]],
          colour = .data[[catch_var]],
          size = .data[[catch_var]],
          shape = survey_type
      ),
      data = filter(data_obj, .data[[catch_var]] != 0)
    ) +
    facet_wrap(~year, ncol = 5) +
    scale_fill_viridis_c(trans = "fourth_root_power") +
    scale_color_viridis_c(trans = "fourth_root_power") +
    scale_shape_manual(values=seq(0,length(unique(data_obj$survey_type))))+
    scale_size_continuous(guide = NULL) +
    ggtitle(paste0(group, "")) +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
    )
  if (catch_var == "density_kgha") {
    g + labs(
      fill = "Density (kg/ha)",
      colour = "Density (kg/ha)",
      shape = "Survey"
    )
  } else {
    if (catch_var == "n_fish_sampled") {
      g + labs(
        fill = "N sampled",
        colour = "N sampled",
        shape = "Survey"
      )
    } else {
    g
    }
  }
}
