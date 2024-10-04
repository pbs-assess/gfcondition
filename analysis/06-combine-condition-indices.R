# plots combining density indices for all species
library(tidyverse)
library(sdmTMB)
library(ggsidekick)
devtools::load_all(".")
theme_set(ggsidekick:::theme_sleek())

fig_height <- 4 * 2
fig_width <- 5 * 2

source("analysis/00-species-list.R")

# model_name1 <- "apr-2024"
model_name1 <- "2024-09"

f1 <- list.files(paste0("data-generated/cond-index/",
                        model_name1), pattern = ".rds", full.names = TRUE)

d1 <- purrr::map_dfr(f1, readRDS) |>
# filter(!(species %in% c("Slender Sole", "Pacific Hake") & group == "immatures")) |>
  # filter(!(species %in% c(Flatfish))) |>
  # filter(!(species %in% c(Flatfish, Rockfish))) |>
  # filter(species %in% Flatfish) |>
  filter(!(species %in% species_to_remove)) |>
  mutate(species = ifelse(species == "Rougheye/Blackspotted Rockfish Complex",
                          "Rougheye/Blackspotted", species),
         # group = forcats::fct_relevel(group, "immatures", "mature males", "mature females"),
         group = factor(group, levels = c("immatures", "mature males", "mature females"),
                        labels = c("Immatures", "Mature males", "Mature females")))

d1$model <- "Density-agnostic"

# model_name2 <- "apr-2024-density"
# model_name2 <- "apr-2024-not-total-density"
# model_name2 <- "apr-2024-cell-means"
# # model_name2 <- "may-2024-doy-ld0c"
model_name2 <- "2024-09-doy-ld0c"
f2 <- list.files(paste0("data-generated/cond-index/",
                        model_name2), pattern = ".rds", full.names = TRUE)

d2 <- purrr::map_dfr(f2, readRDS) |>
  # filter(!(species %in% c(Flatfish))) |>
  # filter(!(species %in% c(Flatfish, Rockfish))) |>
  # filter(species %in% Flatfish) |>
  filter(!(species %in% species_to_remove)) |>
  # filter(!(species %in% c("Slender Sole", "Pacific Hake") & group == "immatures")) |>
  mutate(species = ifelse(species == "Rougheye/Blackspotted Rockfish Complex",
                          "Rougheye/Blackspotted", species),
         # group = forcats::fct_relevel(group, "immatures", "mature males", "mature females"),
         group = factor(group, levels = c("immatures", "mature males", "mature females"),
                        labels = c("Immatures", "Mature males", "Mature females")))
d2$model <- "Excluding density-dependence"


min_est <- min(d1$est, d2$est)
# max_est <- max(d1$est)
max_est <- quantile(c(d1$est, d2$est), 0.9999)
d1 <- d1 |> mutate(upr_trimmed = ifelse(upr > max_est, max_est, upr))
d2 <- d2 |> mutate(upr_trimmed = ifelse(upr > max_est, max_est, upr))


d1 |> bind_rows(d2)  |>
# d1 |>
# d2 |>
  ggplot( aes(year, est, fill = group)) +
  geom_hline(yintercept = 1,
             # linetype = "dotted",
             colour = "grey") +
  geom_line(aes(colour = group, linetype = model)#, linewidth = 1
  ) +
  geom_ribbon(data = d1, aes(ymin = lwr, ymax = upr_trimmed, alpha = group)) +
  # scale_y_log10() +
  facet_wrap(~species,
             scales = "free_y",
             ncol = 5) +
  # coord_cartesian(expand = FALSE, ylim = c(min_est, max_est)) +
  scale_linetype_manual(
    values=c(1,2)
  ) +
  scale_alpha_discrete(range = c(0.1, 0.3)) +
  scale_color_viridis_d(option = "D", direction =1) +
  scale_fill_viridis_d(option = "D", direction =1) +
  # ggtitle(paste(model_name))+
  # theme(legend.position = "none") +
  theme(legend.position = c(0.6,0.06),
        legend.direction = "vertical", legend.box = "horizontal") +
  labs(
    x = "",
    y = "Condition index",
    # y = "Condition index excluding effect of density-dependence",
    # x = "Year",
    alpha = "",
    fill = "",
    colour = "",
    linetype = "")

ggsave(paste0("figs/",
              "all-condition-indices-",
              # "flatfish-",
              # "not-flat-",
              "all-",
              # model_name1,
              model_name2,
              "-with-pcod",
              "-free",
              ".png"),
       height = fig_height*1.4,
       # height = fig_height*1,
       width = fig_width*1
)

