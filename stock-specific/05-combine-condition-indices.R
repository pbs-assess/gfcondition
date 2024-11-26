# plots combining density indices for all species
library(tidyverse)
library(sdmTMB)
devtools::load_all(".")

source("stock-specific/00-set-options.R")
spp <- gsub(" ", "-", gsub("\\/", "-", tolower(species)))

theme_set(ggsidekick:::theme_sleek()+
  theme(plot.margin = unit(c(0.15, 0.2, 0.2, 0.15), "inches")))

model_names <- list.files(paste0("stock-specific/", spp, "/output/cond-pred"),
                          pattern = "", full.names = FALSE)

source("analysis/00-species-list.R")

model_name1 <- model_names[1]
model_name2 <- model_names[2]

f1 <- list.files(paste0("stock-specific/", spp, "/output/cond-index/",
                        model_name1), pattern = ".rds", full.names = TRUE)

d1 <- purrr::map_dfr(f1, readRDS) |>
  mutate(group = factor(group, levels = c("immatures", "mature males", "mature females"),
                        labels = c("Immatures", "Mature males", "Mature females")))

d1$model <- "Density-agnostic"


f2 <- list.files(paste0("stock-specific/", spp, "/output/cond-index/",
                        model_name2), pattern = ".rds", full.names = TRUE)

d2 <- purrr::map_dfr(f2, readRDS) |>
  mutate(species = ifelse(species == "Rougheye/Blackspotted Rockfish Complex",
                          "Rougheye/Blackspotted", species),
         # group = forcats::fct_relevel(group, "immatures", "mature males", "mature females"),
         group = factor(group, levels = c("immatures", "mature males", "mature females"),
                        labels = c("Immatures", "Mature males", "Mature females")))
d2$model <- "Excluding density-dependence"


min_est <- min(d1$est, d2$est)
max_est <- quantile(c(d1$est, d2$est), 0.9999)
d1 <- d1 |> mutate(upr_trimmed = ifelse(upr > max_est, max_est, upr))
d2 <- d2 |> mutate(upr_trimmed = ifelse(upr > max_est, max_est, upr))


set_ncol <- 1
set_legend_position <- c(0.65,0.18)
fig_height <- 4
fig_width <- 6

d1 |> bind_rows(d2)  |>
  ggplot( aes(year, est, fill = group)) +
  geom_hline(yintercept = 1,
             # linetype = "dotted",
             colour = "grey") +
  geom_line(aes(colour = group, linetype = model)) +
  geom_ribbon(data = d1,
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
  theme(legend.position = set_legend_position,
        legend.direction = "vertical", legend.box = "horizontal") +
  labs(
    x = "",
    y = "Condition index",
    alpha = "",
    fill = "",
    colour = "",
    linetype = "")

ggsave(paste0("stock-specific/", spp, "/figs/",
              "all-condition-indices-",
              model_name2,
              ".png"),
       height = fig_height,
       width = fig_width
)

