# plots combining density indices for all species
library(tidyverse)
library(sdmTMB)
library(patchwork)
devtools::load_all(".")

# # works for lingcod
# set_legend_position <- c(0.65,0.18)

set_legend_position <- c(0.4,0.9)


# source("stock-specific/00-set-options.R")
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
  mutate(species = ifelse(species == "Rougheye/Blackspotted Rockfish Complex",
                          "Rougheye/Blackspotted", species),
         group = factor(group, levels = c("immatures", "mature males", "mature females"),
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




min_est <- min(d1$est, d2$est,
               d1$lwr, d2$lwr)
max_est <- max(d1$est, d2$est,
               d1$upr, d2$upr
)


# min_est <- min(d1$est, d2$est)
# max_est <- quantile(c(d1$est, d2$est), 0.9999)
# d1 <- d1 |> mutate(upr_trimmed = ifelse(upr > max_est, max_est, upr))
# d2 <- d2 |> mutate(upr_trimmed = ifelse(upr > max_est, max_est, upr))


set_ncol <- 1
fig_height <- 4
fig_width <- 6

g1 <- d1 |> bind_rows(d2)  |>
  mutate(
    group = rosettafish::en2fr(group, FRENCH),
    model = rosettafish::en2fr(model, FRENCH)
  ) |>
  ggplot(aes(year, est, fill = group)) +
  geom_hline(yintercept = 1,
             # linetype = "dotted",
             colour = "grey") +
  geom_line(aes(colour = group, linetype = model)) +
  geom_ribbon(data = d1 |> mutate(group = rosettafish::en2fr(group, FRENCH),
                                  model = rosettafish::en2fr(model, FRENCH)),
              aes(ymin = lwr, ymax = upr, alpha = group)) +
  # scale_y_log10() +
  # facet_wrap(~species,
  #            scales = "free_y",
  #            ncol = set_ncol) +
  # coord_cartesian(expand = FALSE, ylim = c(min_est, max_est)) +
  scale_linetype_manual(
    values=c(1,2)
  ) +
  scale_alpha_discrete(range = c(0.1, 0.3)) + #, guide = FALSE) +
  scale_color_viridis_d(option = "D", direction =1) + #, guide = FALSE) +
  scale_fill_viridis_d(option = "D", direction =1) + #, guide = FALSE) +
  coord_cartesian(ylim = c(min_est-0.005, max_est+0.005), expand = FALSE) +
  theme(
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    # legend.position = "none",
    legend.position = "top",
    # legend.position = "inside",
    legend.position.inside = set_legend_position,
    legend.direction = "vertical", legend.box = "horizontal"
    ) +
  labs(
    x = "",
    y = rosettafish::en2fr("Condition index", FRENCH),
    alpha = "",
    fill = "",
    colour = "",
    linetype = "")

.ggsave(paste0("stock-specific/", spp, "/figs", if(FRENCH){"-french"},
              "/",
              spp, "-all-condition-indices-",
              model_name2,
              ".png"),
       height = fig_height,
       width = fig_width
)

# browser()

(gl <- d1 |> bind_rows(d2)  |>
    mutate(
      group = rosettafish::en2fr(group, FRENCH),
      model = rosettafish::en2fr(model, FRENCH)
    ) |>
  ggplot( aes(year, log_est, fill = group)) +
  geom_hline(yintercept = 0,
             # linetype = "dotted",
             colour = "grey") +
  geom_line(aes(colour = group, linetype = model)) +
  # geom_ribbon(data = d1 |> mutate(group = rosettafish::en2fr(group, FRENCH),
  #             model = rosettafish::en2fr(model, FRENCH)),
  #             aes(ymin = lwr, ymax = upr, alpha = group)) +
  geom_ribbon(data = d1 |> mutate(group = rosettafish::en2fr(group, FRENCH),
                                  model = rosettafish::en2fr(model, FRENCH)),
              aes(ymin = log(lwr), ymax = log(upr), alpha = group)) +
  # scale_y_log10() +
  # facet_wrap(~species,
  #            scales = "free_y",
  #            ncol = set_ncol) +
  # coord_cartesian(expand = FALSE, ylim = c(min_est, max_est)) +
  scale_linetype_manual(
    values=c(1,2)
  ) +
  scale_alpha_discrete(range = c(0.1, 0.3)) + #, guide = FALSE) +
  scale_color_viridis_d(option = "D", direction =1) + #, guide = FALSE) +
  scale_fill_viridis_d(option = "D", direction =1) + #, guide = FALSE) +
  # coord_cartesian(ylim = c(min_est-0.005, max_est+0.005), expand = FALSE) +
  theme(
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    # legend.position = "none",
    legend.position = "top",
    # legend.position = "inside",
    legend.position.inside = set_legend_position,
    legend.direction = "vertical", legend.box = "horizontal"
  ) +
  labs(
    x = "",
    y = rosettafish::en2fr("Condition index (log scale)", FRENCH),
    # y = "Condition index (log scale)",
    alpha = "",
    fill = "",
    colour = "",
    linetype = ""))

.ggsave(paste0("stock-specific/", spp, "/figs", if(FRENCH){"-french"},
              "/",
              spp, "-log-condition-indices-",
              model_name2,
              ".png"),
       height = fig_height,
       width = fig_width
)


f3 <- list.files(paste0("stock-specific/", spp, "/output/cond-index-by-survey/",
                        model_name1), pattern = ".rds", full.names = TRUE)

d3 <- purrr::map_dfr(f3, readRDS) |>
  mutate(species = ifelse(species == "Rougheye/Blackspotted Rockfish Complex",
                          "Rougheye/Blackspotted", species),
         region = factor(region, levels = c("SYN WCHG", "SYN HS", "SYN QCS", "SYN WCVI")),
         group = factor(group, levels = c("immatures", "mature males", "mature females"),
                        labels = c("Immatures", "Mature males", "Mature females")))

d3$model <- "Density-agnostic"


f4 <- list.files(paste0("stock-specific/", spp, "/output/cond-index-by-survey/",
                        model_name2), pattern = ".rds", full.names = TRUE)

d4 <- purrr::map_dfr(f4, readRDS) |>
  mutate(species = ifelse(species == "Rougheye/Blackspotted Rockfish Complex",
                          "Rougheye/Blackspotted", species),
         region = factor(region, levels = c("SYN WCHG", "SYN HS", "SYN QCS", "SYN WCVI")),
         group = factor(group, levels = c("immatures", "mature males", "mature females"),
                        labels = c("Immatures", "Mature males", "Mature females")))

d4$model <- "Excluding density-dependence"


### probably only necessary for multispecies plots?
# min_est <- min(d1$est, d2$est, d3$est, d4$est)
# max_est <- max(d1$est, d2$est, d3$est, d4$est)
# # max_est <- quantile(c(d1$est, d2$est, d3$est, d4$est), 0.9999)
min_est <- quantile(c(d1$lwr, d2$lwr, d3$lwr, d4$lwr), 0.025)
max_est <- quantile(c(d1$upr, d2$upr, d3$upr, d4$upr), 0.975)

# d3 <- d3 |> mutate(upr_trimmed = ifelse(upr > max_est, max_est, upr))
# d4 <- d4 |> mutate(upr_trimmed = ifelse(upr > max_est, max_est, upr))

g2 <- d3 |> bind_rows(d4)  |>
  mutate(
    group = rosettafish::en2fr(group, FRENCH),
    model = rosettafish::en2fr(model, FRENCH)
  ) |>
  ggplot( aes(year, est, fill = group)) +
  geom_hline(yintercept = 1,
             # linetype = "dotted",
             colour = "grey") +
  geom_line(aes(colour = group, linetype = model)) +
  geom_ribbon(data = d3 |> mutate(group = rosettafish::en2fr(group, FRENCH),
                                  model = rosettafish::en2fr(model, FRENCH)),
              aes(ymin = lwr, ymax = upr, alpha = group)) +
  # scale_y_log10() +
  facet_grid(region~group) +
  # coord_cartesian(expand = FALSE, ylim = c(min_est, max_est)) +
  scale_linetype_manual(
    values=c(1,2)
  ) +
  scale_alpha_discrete(range = c(0.1, 0.3)) +
  scale_color_viridis_d(option = "D", direction =1) +
  scale_fill_viridis_d(option = "D", direction =1) +
  coord_cartesian(ylim = c(min_est, max_est), expand = FALSE) +
  theme(
    legend.title = element_blank(),
    legend.position = "none",
    # legend.position = "bottom",
    legend.direction = "vertical", legend.box = "horizontal"
  ) +
  labs(
    x = "",
    y = rosettafish::en2fr("Condition index", FRENCH),
    alpha = "",
    fill = "",
    colour = "",
    linetype = "")

.ggsave(paste0("stock-specific/", spp, "/figs", if(FRENCH){"-french"},
              "/",
              spp, "-all-condition-indices-split-",
              model_name2,
              ".png"),
       height = fig_height*1.5,
       width = fig_width
)


(g1 + ggtitle(if(FRENCH){"A. Partout sur la côte (stock entier)"}else{"A. Coastwide (entire stock)"})) / (g2+ggtitle(if(FRENCH){"B. Fractionnés par zone d'enquête"}else{"B. Split by survey area"})) + plot_layout(heights = c(0.6,1))


.ggsave(paste0("stock-specific/", spp, "/figs", if(FRENCH){"-french"},
              "/C-00-",
              spp, "-all-condition-indices-w-split-",
              model_name2,
              ".png"),
       height = fig_height*2,
       width = fig_width
)
