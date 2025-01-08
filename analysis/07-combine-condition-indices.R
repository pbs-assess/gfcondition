# plots combining density indices for all species
library(tidyverse)
library(sdmTMB)
library(ggsidekick)
devtools::load_all(".")
theme_set(ggsidekick:::theme_sleek()+
  theme(plot.margin = unit(c(0.15, 0.15, 0.15, 0.15), "inches")))

source("analysis/00-species-list.R")

# model_name1 <- "2024-09-w-pcod-before-10pyr" # conference version
model_name1 <- "2024-09" # with new halibut data

# model_name2 <- "2024-09-doy-ld0c-w-pcod-before-10pyr"
model_name2 <- "2024-09-doy-ld0c" # with new halibut data


f1 <- list.files(paste0("data-generated/cond-index/",
                        model_name1), pattern = ".rds", full.names = TRUE)

d1 <- purrr::map_dfr(f1, readRDS) |>
  filter(!(species %in% species_to_remove)) |>
  mutate(species = ifelse(species == "North Pacific Spiny Dogfish",
                          "Pacific Spiny Dogfish", species),
         species = ifelse(species == "Rougheye/Blackspotted Rockfish Complex",
                          "Rougheye/Blackspotted", species),
         # group = forcats::fct_relevel(group, "immatures", "mature males", "mature females"),
         group = factor(group, levels = c("immatures", "mature males", "mature females"),
                        labels = c("Immatures", "Mature males", "Mature females")))

d1$model <- "Density-agnostic"


f2 <- list.files(paste0("data-generated/cond-index/",
                        model_name2), pattern = ".rds", full.names = TRUE)

d2 <- purrr::map_dfr(f2, readRDS) |>
  filter(!(species %in% species_to_remove)) |>
  mutate(species = ifelse(species == "North Pacific Spiny Dogfish",
                              "Pacific Spiny Dogfish", species),
         species = ifelse(species == "Rougheye/Blackspotted Rockfish Complex",
                          "Rougheye/Blackspotted", species),
         # group = forcats::fct_relevel(group, "immatures", "mature males", "mature females"),
         group = factor(group, levels = c("immatures", "mature males", "mature females"),
                        labels = c("Immatures", "Mature males", "Mature females")))
d2$model <- "Excluding density-dependence"


min_est <- min(d1$est, d2$est)
max_est <- quantile(c(d1$est, d2$est), 0.9999)
d1 <- d1 |> mutate(upr_trimmed = ifelse(upr > max_est, max_est, upr))
d2 <- d2 |> mutate(upr_trimmed = ifelse(upr > max_est, max_est, upr))


species_to_plot <- species_list
set_name <- "all-"
fig_height <- 8*1.4
fig_width <- 10
set_ncol <- 5
set_legend_position <- c(0.6,0.06)


### run individually for talks
# set_name <- "all2-wide-"
# set_ncol <- 8
# set_legend_position <- c(0.75,0.08)
# fig_height <- 8
# fig_width <- 16
#
# species_to_plot <- Flatfish
# set_name <- "flatfish-"
# # #
# species_to_plot <- species_list[!(species_list %in% c(Rockfish, Flatfish))]
# set_name <- "other-"
# # #
# fig_height <- 4
# fig_width <- 10
# set_ncol <- 5
# set_legend_position <- "none"
# #
# species_to_plot <- Rockfish
# set_name <- "rockfish-"
# fig_height <- 5.5
# fig_width <- 12
# set_ncol <- 6
# # set_legend_position <- c(0.7, 0.1)

d1 |> bind_rows(d2)  |>
  filter(species %in% species_to_plot) |>
  # filter(!(species== "Pacific Halibut")) |>
  ggplot( aes(year, est, fill = group)) +
  geom_hline(yintercept = 1,
             # linetype = "dotted",
             colour = "grey") +
  geom_line(aes(colour = group, linetype = model)) +
  geom_ribbon(data = filter(d1, species %in% species_to_plot),
              aes(ymin = lwr, ymax = upr_trimmed, alpha = group)) +
  # scale_y_log10() +
  facet_wrap(~species,
             scales = "free_y",
             ncol = set_ncol) +
  # coord_cartesian(expand = FALSE, ylim = c(min_est, max_est)) +
  scale_linetype_manual(
    values=c(1,2)
  ) +
  scale_alpha_discrete(range = c(0.1, 0.3)) +
  scale_color_viridis_d(option = "D", direction =1) +
  scale_fill_viridis_d(option = "D", direction =1) +
  theme(legend.position = "top",
  # theme(legend.position = "inside", legend.position.inside = set_legend_position,
        legend.direction = "vertical", legend.box = "horizontal") +
  labs(
    x = "",
    y = "Condition index",
    alpha = "",
    fill = "",
    colour = "",
    linetype = "")

ggsave(paste0("figs/",
              "man/",
              "all-condition-indices-",
              set_name,
              model_name2,
              "-free2", # halibut updated to include iphc
              ".png"),
       height = fig_height,
       width = fig_width
)

