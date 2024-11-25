# plot condition prediction maps with and without sample counts and species detections
library(tidyverse)
devtools::load_all(".")

source("stock-specific/00-set-options.R")
spp <- gsub(" ", "-", gsub("\\/", "-", tolower(species)))

model_names <- list.files(paste0("stock-specific/", spp, "/output/cond-pred"),
                          pattern = "", full.names = FALSE)

# for now just plotting first model type
# in this case the density agnositic version
model_name <- model_names[1]

# all sets
dset <- readRDS(paste0("stock-specific/", spp, "/data/tidy-survey-sets-", spp, ".rds")) %>%
  filter(!is.na(longitude)&!is.na(latitude)) %>%
  sdmTMB::add_utm_columns(.,
                          ll_names = c("longitude", "latitude"),
                          utm_crs = set_utm_crs)
unique(dset$survey_abbrev)

set_list <- dset %>%
  select(fishing_event_id, longitude, latitude, X, Y, year, catch_weight, catch_count) %>%
  distinct() %>%
  mutate(
    fishing_event_id = as.factor(fishing_event_id),
    lon = longitude, lat = latitude)

for(i in seq_along(model_name)) {
pred_files <- list.files(paste0("stock-specific/", spp, "/output/cond-pred/", model_name[i]),
                          pattern = "", full.names = TRUE)
model_files <- list.files(paste0("stock-specific/", spp, "/output/condition-models/", model_name[i]),
                         pattern = "", full.names = TRUE)
for (j in seq_along(pred_files)){

  rm(m)
  rm(p2)
  m <- readRDS(model_files[j])

  p1 <- readRDS(pred_files[j])


  # filter to plot only cells representing 99% of mean predicted biomass
  # cells must be defined by "X", "Y", time by "year", and biomass/abundance stored as "density"
  p2 <- trim_predictions_by_year(p1, 0.001)

  # Map model predictions ----

  # # just sampled sets
  # set_list <- m$data %>% select(fishing_event_id, longitude, latitude, X, Y) %>% distinct() %>%
  #   mutate(lon = longitude, lat = latitude)

  model_dat <- m$data %>%
    group_by(fishing_event_id) %>%
    mutate(
      fishing_event_id = as.factor(fishing_event_id),
      count = n()
    )

  model_dat <- left_join(set_list, model_dat, multiple = "all") %>%
    filter(year >= min(p2$year), year <= max(p2$year)) %>%
    mutate(
    density = group_catch_weight,
    caught = ifelse(catch_count > 0 | catch_weight > 0, 1, 0),
    count = ifelse(is.na(count), 0, count),
    present = ifelse(count > 0, 1, ifelse(caught == 1, 0, NA_integer_))
  )

  # model_dat %>% group_by(present, caught) %>% summarise(n = n()) %>% View()
  p2$log_cond <- log(p2$cond)
  p2 <- p2 %>% mutate(cond_trim = ifelse(cond > quantile(p2$cond, 0.99),
                                         quantile(p2$cond, 0.99), cond
  ))

  g <- plot_predictions(p2, model_dat,
                        fill_column = "cond_trim",
                        fill_label = "Condition \nfactor",
                        fill_scale =
                          ggplot2::scale_fill_viridis_c(),
                        bounds = p1,
                        rotation_angle = 30,
                        show_raw_data = FALSE
  )

  g <- g + facet_wrap(~year, ncol = 8) +
    ggtitle(paste0(species, ": ", unique(m$data$group_name), " ", model_name[i]))

  ggsave(paste0("stock-specific/", spp, "/figs/cond-", model_name[i], "/condition-pred-wide-", spp, "-", unique(m$data$group_name), "-",
                model_name[i], "-", knot_distance, "-km.png"),
         height = 8, width = 8
  )


  g <- g + facet_wrap(~year, ncol = 12) +
    ggtitle(paste0(species, ": ", unique(m$data$group_name), " ", model_name[i]))

  ggsave(paste0("stock-specific/", spp, "/figs/cond-", model_name[i], "/condition-pred-wide-", spp, "-", unique(m$data$group_name), "-",
                model_name[i], "-", knot_distance, "-km.png"),
         height = 6, width = 11
  )


  g <- plot_predictions(p2, model_dat, # extrapolate_depth = FALSE,
                        # fill_column = "log_cond",
                        fill_column = "cond_trim",
                        fill_label = "Condition \nfactor",
                        pt_column = "count",
                        pt_label = "Fish \nsampled",
                        pt_size_range = c(0.5, 4),
                        pos_pt_fill = NA,
                        bin_pt_col = "black",
                        pos_pt_col = "red",
                        # x_buffer = c(-0, 0),
                        # y_buffer = c(-0, 0),
                        fill_scale =
                          ggplot2::scale_fill_viridis_c(),
                        # ggplot2::scale_fill_viridis_c(trans = "log10"),
                        bounds = p1,
                        rotation_angle = 30, show_raw_data = TRUE
  )

  g <- g + facet_wrap(~year, ncol = 8) +
    ggtitle(paste0(species, ": ", unique(m$data$group_name), " ", model_name[i]))

  ggsave(paste0("stock-specific/", spp, "/figs/cond-", model_name[i], "/condition-map-", spp, "-", unique(m$data$group_name), "-",
                model_name[i], "-", knot_distance, "-km.png"),
         height = 8, width = 8
  )

  g <- g + facet_wrap(~year, ncol = 12) +
    ggtitle(paste0(species, ": ", unique(m$data$group_name), " ", model_name[i]))

  ggsave(paste0("stock-specific/", spp, "/figs/cond-", model_name[i], "/condition-map-wide-", spp, "-", unique(m$data$group_name), "-",
                model_name[i], "-", knot_distance, "-km.png"),
         height = 6, width = 11
  )

}
}
