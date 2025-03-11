# plots for flow chart

# total biomass sdm
devtools::load_all()
library(tidyverse)
library(gfplot)
library(ggsidekick)
library(patchwork)

source("stock-specific/00-set-options.R")

density_quantile <- 0.99

density_fill_scale <- ggplot2::scale_fill_viridis_c(trans="sqrt", option = "C")
density_colour_scale <- ggplot2::scale_colour_viridis_c(trans="sqrt", option = "C")
# density_colour_scale <- ggplot2::scale_fill_viridis_c(trans=fourth_root_power_trans())
# density_colour_scale <- ggplot2::scale_fill_viridis_c(trans="log10")
# density_colour_scale <- ggplot2::scale_fill_viridis_c(trans="log")

spp <- gsub(" ", "-", gsub("\\/", "-", tolower(species)))

dir.create(paste0("stock-specific/", spp, "/figs/flow-chart/"), showWarnings = FALSE)
# dir.create(paste0("stock-specific/", spp, "/output/", "density-predictions/"), showWarnings = FALSE)
# dir.create(paste0("stock-specific/", spp, "/output/", "density-index/"), showWarnings = FALSE)

## use definitions from the set-options file, for example:
# dens_model_total <- "dln-2024-11" # this is for total
# dens_model_name1 <- "dln-split-2024-11" # these are `all catches' models
# dens_model_name2 <- "dln-only-sampled-2024-11" # these are `sampled catches' models

dens_model_total
dens_model_name1



m <- paste0(spp, "-total-", dens_model_total, "-", knot_distance, "-km")
pfn <- paste0("stock-specific/", spp, "/output/", "density-predictions/p-", m, ".rds")

# 1. total biomass maps----

p0 <- readRDS(pfn) #|> filter(year %in% c(2000, 2001, 2002, 2003, 2004,
                                         # 2020, 2021, 2022, 2023, 2024))
m0 <- readRDS(paste0("stock-specific/", spp, "/output/density-models/", dens_model_total, "/total/", m, ".rds"))

m0$data$lat <- m0$data$latitude
m0$data$lon <- m0$data$longitude


p0f <- p0 |>
  filter(# year %in% c(2000, 2001, 2002, 2003, 2004, 2020, 2021, 2022, 2023, 2024)
         # year %in% c(2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018, 2020)
         year %in% c(2000, 2008, 2016, 2024)
         # year %in% c(2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021)
         # year %in% c(2000, 2004, 2008, 2012, 2016, 2020, 2024)
         # year %in% c(2006, 2008, 2010, 2012, 2014, 2016, 2018, 2020, 2022, 2024)
         # year %in% c(2000, 2001, 2004, 2005, 2008, 2016, 2017, 2020, 2021, 2024)
         ) |>
  mutate(density_trimmed = ifelse(density > quantile(density, density_quantile),
                                  quantile(density, density_quantile),
                                  density))


g <- plot_predictions(p0f, m0$data,
                      fill_column = "density_trimmed",
                      fill_label = "Total biomass",
                      fill_scale = density_fill_scale,
                      colour_scale = density_colour_scale,
                      bounds = NULL,
                      # rotation_angle = 10,
                      show_raw_data = FALSE
)

g <- g + facet_wrap(~year, ncol = 4) + theme(
  legend.position = "none",
  panel.spacing.y = unit(0, "lines"),
  panel.spacing.x = unit(0.1, "lines"))
# g

ggsave(paste0("stock-specific/", spp, "/figs/flow-chart/01-total-dens-4yrs.png"),
       height = 1.5, width = 4
)

# ggsave(paste0("stock-specific/", spp, "/figs/total-dens-4yrs.png"),
#        height = 2.5, width = 8
# )



# 2. Maturity ----
mat <- readRDS(paste0("stock-specific/", spp, "/output/split-catch-data-", spp, ".rds"))
# mat$maturity_plot$layers[[4]] <- NULL

mat$maturity_plot + theme(
  legend.position = "inside", legend.position.inside = c(0.8, 0.5),
  plot.title = element_blank())

ggsave(paste0("stock-specific/", spp, "/figs/flow-chart/02-maturity.png"),
       height = 1.7, width = 2.25
)

# 3. Le Crens----

dat <- readRDS(paste0("stock-specific/", spp, "/data/tidy-survey-samples-", spp, ".rds"))
## remove the sablefish survey because this survey is at different time of year than all the others
dat <- filter(dat, !(survey_abbrev %in% c("SABLE")))
## temporary fix for IPHC because of sample size imbalance over time and
## uncertainty about how weight was measured
if(species == "Pacific Halibut"){
  dat <- filter(dat, survey_abbrev != "IPHC FISS")
}

ds <- readRDS(paste0("stock-specific/", spp, "/output/condition-data-", spp, "-mat-", mat_threshold, ".rds"))

ggplot(
  dat |> filter(
    sex %in% c(1, 2)
  ) |>
    mutate(
      sex_label = ifelse(sex == 1, "Male", "Female"),
      weight = weight / 1000,
      vline = ifelse(sex == 1, mat$m$mat_perc$m.p0.5, mat$m$mat_perc$f.p0.5)
    ),
  aes(length, weight)
) +
  geom_point(size = 0.5, colour = "red") +
  geom_point(size = 0.5, colour = "white", data = ds) +
  geom_point(aes(colour = cond_fac),
             data = filter(ds, sex %in% c(1, 2)) |> mutate(sex_label = ifelse(sex == 1, "Male", "Female")),
             size = 0.5,
             alpha = 0.8
  ) +
  facet_wrap(~sex_label) +
  geom_vline(aes(xintercept = vline,
                 #linetype = sex_label,
                 # alpha = sex_label
                 ),
             linetype = "dashed",
             show.legend = F) +
  scale_alpha_discrete(range = c(1,0.5)) +
  labs(
    colour = "Le Cren's", shape = "Sex",
    x = "Length (cm)", y = "Weight (kg)"
  ) +
  scale_colour_viridis_c() +
  scale_shape_discrete(guide = NULL) +
  ggsidekick::theme_sleek() + theme(
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.3, 'cm'),
    legend.position = c(0.13,0.65)
    )


ggsave(paste0("stock-specific/", spp, "/figs/flow-chart/03-le-crens.png"),
       height = 3.5, width = 5.5
)


# 4. Split biomass maps----

m1 <- paste0(spp, "-mat-fem-", dens_model_name1, "-", knot_distance, "-km")
m2 <- paste0(spp, "-mat-m-", dens_model_name1, "-", knot_distance, "-km")
m3 <- paste0(spp, "-imm-", dens_model_name1, "-", knot_distance, "-km")

pmfn <- paste0("stock-specific/", spp, "/output/", "density-predictions/p-", m1, ".rds")
pmfn2 <- paste0("stock-specific/", spp, "/output/", "density-predictions/p-", m2, ".rds")
pifn <- paste0("stock-specific/", spp, "/output/", "density-predictions/p-", m3, ".rds")

p1 <- readRDS(pmfn) |> filter(year %in% c(2024)) |> mutate(year = "Mature female\n2024")
p2 <- readRDS(pmfn2) |> filter(year %in% c(2024))|> mutate(year = "Mature male\n2024")
p3 <- readRDS(pifn) |> filter(year %in% c(2024)) |> mutate(year = "Immature\n2024")

p2024 <- bind_rows(p1,p2,p3)|>
  mutate(density_trimmed = ifelse(density > quantile(density, density_quantile),
                                  quantile(density, density_quantile),
                                  density),
         year = factor(year, levels = c("Immature\n2024", "Mature male\n2024", "Mature female\n2024")))

g <- plot_predictions(p2024, m0$data,
                      fill_column = "density_trimmed",
                      fill_label = "Biomass\ndensity",
                      fill_scale = density_fill_scale,
                      colour_scale = density_colour_scale,
                      bounds = NULL,
                      # rotation_angle = 30,
                      show_raw_data = FALSE
)
g <- g + facet_wrap(~year, ncol = 3) + theme(
  panel.spacing = unit(0.2, "lines"),
  legend.position = "none")
# g

ggsave(paste0("stock-specific/", spp, "/figs/flow-chart/04-split-dens.png"),
       height = 1.9, width = 4
)


# 5. Biomass index----

i0 <- paste0("stock-specific/", spp, "/output/", "density-index/", dens_model_total, "/total/i-", m, ".rds")
i1 <- paste0("stock-specific/", spp, "/output/", "density-index/", dens_model_name1, "/mat-fem/i-", m1, ".rds")
i2 <- paste0("stock-specific/", spp, "/output/", "density-index/", dens_model_name1, "/mat-m/i-", m2, ".rds")
i3 <- paste0("stock-specific/", spp, "/output/", "density-index/", dens_model_name1, "/imm/i-", m3, ".rds")

m1 <- paste0(spp, "-mat-fem-", dens_model_name2, "-", knot_distance, "-km")
m2 <- paste0(spp, "-mat-m-", dens_model_name2, "-", knot_distance, "-km")
m3 <- paste0(spp, "-imm-", dens_model_name2, "-", knot_distance, "-km")

i0 <- paste0("stock-specific/", spp, "/output/", "density-index/", dens_model_total, "/total/i-", m, ".rds")
i1 <- paste0("stock-specific/", spp, "/output/", "density-index/", dens_model_name2, "/mat-fem/i-", m1, ".rds")
i2 <- paste0("stock-specific/", spp, "/output/", "density-index/", dens_model_name2, "/mat-m/i-", m2, ".rds")
i3 <- paste0("stock-specific/", spp, "/output/", "density-index/", dens_model_name2, "/imm/i-", m3, ".rds")


d0 <- readRDS(i0)
d <- readRDS(i1) |> bind_rows(readRDS(i2)) |> bind_rows(readRDS(i3))

ds <- d |>
  select(-group, -type) |>
  group_by(model_string, year, species) |>
  summarise_all("sum")

ds$group <- "Sum of split indices"

(g <- d |> ggplot(aes(year, est/1000*4,
               ymin = lwr/1000*4,
               ymax = upr/1000*4)) +#convert to tons, multiply by grid cell area
    geom_ribbon(data = d0, alpha = 0.08) +
    geom_line(aes(colour = group, linetype = "Split by sex and maturity")) +
    geom_ribbon(aes(alpha = group, fill = group)) +
    geom_line(data = ds, colour = "black", aes(linetype = "Sum of split indices")) +
    geom_line(data = d0, colour = "black", aes(linetype = "Index of total biomass")) +
    scale_alpha_discrete(range = c(0.45, 0.25)) +
    scale_color_viridis_d(
      # end = 0.99,
      direction =-1,
      option = "D") +
    scale_linetype_manual(
      values=c(1,2,3)
    ) +
    scale_fill_viridis_d(# end = 0.99,
      direction =-1,
      option = "D") +
    coord_cartesian(expand = FALSE) +
    theme(
      axis.title.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position = "inside", legend.position.inside = c(0.5, 0.8),
          legend.direction = "vertical", legend.box = "horizontal")+
    labs(
      fill = NULL,
      alpha = NULL,
      colour = NULL,
      linetype = NULL,
      x="Year", y="Relative biomass"))


ggsave(paste0("stock-specific/", spp, "/figs/flow-chart/05-density-indices.png"),
       height = 1.7, width = 3.6
)


m1 <- paste0(spp, "-mat-fem-", dens_model_name2, "-", knot_distance, "-km")
m2 <- paste0(spp, "-mat-m-", dens_model_name2, "-", knot_distance, "-km")
m3 <- paste0(spp, "-imm-", dens_model_name2, "-", knot_distance, "-km")

i0 <- paste0("stock-specific/", spp, "/output/", "density-index/", dens_model_total, "/total/i-", m, ".rds")
i1 <- paste0("stock-specific/", spp, "/output/", "density-index/", dens_model_name2, "/mat-fem/i-", m1, ".rds")
i2 <- paste0("stock-specific/", spp, "/output/", "density-index/", dens_model_name2, "/mat-m/i-", m2, ".rds")
i3 <- paste0("stock-specific/", spp, "/output/", "density-index/", dens_model_name2, "/imm/i-", m3, ".rds")


d0 <- readRDS(i0)
d <- readRDS(i1) |> bind_rows(readRDS(i2)) |> bind_rows(readRDS(i3))

ds <- d |>
  select(-group, -type) |>
  group_by(model_string, year, species) |>
  summarise_all("sum")

ds$group <- "Sum of split indices"

(g <- d |> ggplot(aes(year, est/1000*4,
                      ymin = lwr/1000*4,
                      ymax = upr/1000*4)) +#convert to tons, multiply by grid cell area
    geom_ribbon(data = d0, alpha = 0.08) +
    geom_line(aes(colour = group, linetype = "Split by sex and maturity")) +
    geom_ribbon(aes(alpha = group, fill = group)) +
    geom_line(data = ds, colour = "black", aes(linetype = "Sum of split indices")) +
    geom_line(data = d0, colour = "black", aes(linetype = "Index of total biomass")) +
    scale_alpha_discrete(range = c(0.45, 0.25)) +
    scale_color_viridis_d(
      # end = 0.99,
      direction =-1,
      option = "D") +
    scale_linetype_manual(
      values=c(1,2,3)
    ) +
    scale_fill_viridis_d(# end = 0.99,
      direction =-1,
      option = "D") +
    coord_cartesian(expand = FALSE) +
    theme(
      axis.title.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position = "inside", legend.position.inside = c(0.5, 0.8),
      legend.direction = "vertical", legend.box = "horizontal")+
    labs(
      fill = NULL,
      alpha = NULL,
      colour = NULL,
      linetype = NULL,
      x="Year", y="Relative biomass"))


ggsave(paste0("stock-specific/", spp, "/figs/flow-chart/05-density-indices-only-sampled.png"),
       height = 2.7, width = 4.6
)


# 6. Condition maps----
cond_model_names <- list.files(paste0("stock-specific/", spp, "/output/cond-pred"),
                          pattern = "", full.names = FALSE)

# for now just plotting first model type
# in this case the density agnositic version
cond_model_name <- cond_model_names[1]

f1 <- list.files(paste0("stock-specific/", spp, "/output/cond-pred/", cond_model_name), pattern = ".rds", full.names = TRUE)

# pc <- purrr::map_dfr(f1, readRDS) |> filter(year %in% c(2024)) #|> mutate(year = paste0(group, "\n2024"))

p1 <- readRDS(f1[2]) |> filter(year %in% c(2024)) |> mutate(group = "Mature female\n2024")
p2 <- readRDS(f1[3]) |> filter(year %in% c(2024))|> mutate(group = "Mature male\n2024")
p3 <- readRDS(f1[1]) |> filter(year %in% c(2024)) |> mutate(group = "Immature\n2024")
pc <- list(p1,p2,p3)
# filter to plot only cells representing 99% of mean predicted biomass
# cells must be defined by "X", "Y", time by "year", and biomass/abundance stored as "density"
# p2 <- trim_predictions_by_year(p3)

p2 <- purrr::map_dfr(pc, trim_predictions_by_year)

p2$log_cond <- log(p2$cond)
p2 <- p2 %>% mutate(cond_trim = ifelse(cond > quantile(p2$cond, 0.975),
                                       quantile(p2$cond, 0.975), cond),
                    group = factor(group, levels = c("Immature\n2024", "Mature male\n2024", "Mature female\n2024")),
                    year = group)

g <- plot_predictions(p2, m0$data,
                      fill_column = "cond_trim",
                      fill_label = "Le Cren's",
                      fill_scale =
                        ggplot2::scale_fill_viridis_c(),
                      bounds = NULL,
                      # rotation_angle = 30,
                      show_raw_data = FALSE
)
g <- g + facet_wrap(~year, ncol = 3) + theme(
  panel.spacing = unit(0.2, "lines"),
  # legend.position = "none"
  legend.title = element_text(size = 6),
  legend.text = element_text(size = 5),
  legend.key.size = unit(0.3, 'cm'),
  legend.position = "inside", legend.position.inside = c(0.94, 0.7)
  )
g

ggsave(paste0("stock-specific/", spp, "/figs/flow-chart/06-condition-maps.png"),
       height = 1.9, width = 4
)

# 7. Condition indices----

f2 <- list.files(paste0("stock-specific/", spp, "/output/cond-index/", cond_model_name), pattern = ".rds", full.names = TRUE)

d2 <- purrr::map_dfr(f2, readRDS) |>
  mutate(group = factor(group, levels = c("immatures", "mature males", "mature females"),
                        labels = c("Immatures", "Mature males", "Mature females")))

min_est <- min(d2$est)
max_est <- max(d2$est)

g <- d2 |>
  ggplot(aes(year, est, fill = group)) +
  geom_hline(yintercept = 1,
             # linetype = "dotted",
             colour = "grey") +
  geom_line(aes(colour = group)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, alpha = group)) +
  scale_linetype_manual(
    values=c(1,2)
  ) +
  scale_alpha_discrete(range = c(0.1, 0.3)) +
  scale_color_viridis_d(option = "D", direction =1) +
  scale_fill_viridis_d(option = "D", direction =1) +
  facet_wrap(~group) +
  coord_cartesian(expand = FALSE, ylim = c(min_est-0.005, max_est+0.005)) +
  theme(#plot.margin = unit(c(0.1, 0.15, 0.1, 0), "inches"),
    axis.title = element_blank(),
    legend.position = "none") +
  labs(
    x = "",
    y = "Condition index")
g

ggsave(paste0("stock-specific/", spp, "/figs/flow-chart/07-condition-indices.png"),
       height = 1.6, width = 4.5
)
