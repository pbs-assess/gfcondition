# prepare coastwide covariates for DFA

library(tidyverse)
library(pacea)
devtools::load_all()
theme_set(ggsidekick::theme_sleek())

# check that this matches current range for condition indices?
yrs <- 2002:2024

npgo0 <- npgo |>
  filter(month %in% c(1,2,3,4,5,6)) |> # not sure why 7 was included before?
  group_by(year) |> summarise(value = mean(anomaly, na.rm = TRUE)) |>
  filter(year %in% yrs, !is.na(value)) |>
  mutate(time = seq_along(year),
         value_raw = value,
         value = (value - mean(value))/ sd(value),
         type = "NPGO")
hist(npgo0$value)


  # load("data-raw/npi_monthly.rda")
  npi0 <- npi_monthly |>
    filter(month %in% c(1,2,3,4,5,6)) |> # not sure why 7 was included before?
    group_by(year) |> summarise(value = mean(value, na.rm = TRUE)) |>
    filter(year %in% yrs, !is.na(value)) |>
    mutate(time = seq_along(year),
           value_raw = value,
           value = (value - mean(value))/ sd(value),
           type = "NPI")
  hist(npi0$value)

  # load("data-raw/soi.rda")
  soi0 <- soi |>
    filter(month %in% c(1,2,3,4,5,6)) |>
    group_by(year) |> summarise(value = mean(anomaly, na.rm = TRUE)) |>
    filter(year %in% yrs, !is.na(value)) |>
    mutate(time = seq_along(year),
           value_raw = value,
           value = (value - mean(value))/ sd(value),
           type = "SOI")
  hist(soi0$value)

  soi1 <- soi  |> group_by(year) |>
    summarise(value = mean(anomaly, na.rm = TRUE)) |>
    mutate(year = year + 1) |>
    filter(year %in% yrs, !is.na(value)) |>
    mutate(time = seq_along(year),
           value_raw = value,
           value = (value - mean(value))/ sd(value),
           type = "SOI (prior year)")
  hist(soi1$value)


  ## ONI - not correlated
  # load("data-raw/oni.rda")
  oni0 <- oni |>
    filter(month %in% c(1,2,3,4,5,6)) |>
    group_by(year) |> summarise(value = mean(anomaly, na.rm = TRUE)) |>
    filter(year %in% yrs, !is.na(value)) |>
    mutate(time = seq_along(year),
           value_raw = value,
           value = (value - mean(value))/ sd(value),
           type = "ENSO (ONI)")
  hist(oni0$value)

  # if(var_type == "ONI" & set_lag == 0){
  #   agg_var <- "mean"
  #   covar <- oni0
  #   var_label <- " Jan-Jun ONI (red line)"
  # }

  oni1 <- oni |> group_by(year) |>
    summarise(value = mean(anomaly, na.rm = TRUE)) |>
    mutate(year = year + 1) |>
    filter(year %in% yrs, !is.na(value)) |>
    mutate(time = seq_along(year),
           value_raw = value,
           value = (value - mean(value))/ sd(value),
           type = "ONI (prior year)")
  hist(oni1$value)

  # if(var_type == "ONI" & set_lag == 1){
  #   agg_var <- "mean"
  #   var_type <- "ONI"
  #   covar <- oni1
  #   var_label <- " ONI (red line)"
  # }
  #

  # load("data-raw/pdo.rda")
  pdo0 <- pdo |>
    filter(month %in% c(1,2,3,4,5,6)) |>
    group_by(year) |> summarise(value = mean(anomaly, na.rm = TRUE)) |>
    filter(year %in% yrs, !is.na(value)) |>
    mutate(time = seq_along(year),
           value_raw = value,
           value = (value - mean(value))/ sd(value),
           type = "PDO")
  hist(pdo0$value)

  pdo1 <- pdo |> group_by(year) |>
    summarise(value = mean(anomaly, na.rm = TRUE)) |>
    mutate(year = year + 1) |>
    filter(year %in% yrs, !is.na(value)) |>
    mutate(time = seq_along(year),
           value_raw = value,
           value = (value - mean(value))/ sd(value),
           type = "PDO (prior year)")
  hist(pdo1$value)

  # if(var_type == "PDO" & set_lag == 1){
  #   agg_var <- "mean"
  #   var_type <- "PDO"
  #   covar <- pdo1
  #   var_label <- " PDO (red line)"
  # }
  #
  # if(var_type == "PDO" & set_lag == 0){
  #   agg_var <- "mean"
  #   var_type <- "PDO"
  #   covar <- pdo0
  #   var_label <- " Jan-Jun PDO (red line)"
  # }

  ## primary -- positively correlated (more so w trend 2)
  pp_monthly <- readRDS("data-raw/cw_primary_production.rds")
  pp0 <- pp_monthly |>
    filter(month %in% c(1,2,3,4,5,6), !is.na(value)) |>
    group_by(year) |> summarise(value = mean(value, na.rm = TRUE)) |>
    filter(year %in% yrs, !is.na(value)) |>
    mutate(time = seq_along(year),
           value_raw = value,
           value = (value - mean(value))/ sd(value),
           type = "Primary production")
  hist(pp0$value)


  pp1 <- pp_monthly |>
    group_by(year) |> summarise(value = mean(value, na.rm = TRUE)) |>
    mutate(year = year + 1) |>
    filter(year %in% yrs, !is.na(value)) |>
    mutate(
      time = seq_along(year),
      value_raw = value,
      value = (value - mean(value))/ sd(value),
      type = "Production (prior year)")
  hist(pp1$value)


  # if(var_type == "production" & set_lag == 0){
  #   agg_var <- "mean"
  #   var_label <- " primary production (red line)"
  #   covar <- pp0
  # }

  ## phytoplankton -- positively correlated (more so w trend 2)
  pt_monthly <- readRDS("data-raw/cw_phytoplankton.rds")
  pt0 <- pt_monthly |>
    filter(month %in% c(1,2,3,4,5,6), !is.na(value)) |>
    group_by(year) |> summarise(value = mean(value, na.rm = TRUE)) |>
    filter(year %in% yrs, !is.na(value)) |>
    mutate(time = seq_along(year),
           value_raw = value,
           value = (value - mean(value))/ sd(value),
           type = "Phytoplankton")

  hist(pt0$value)

  # if(var_type == "phytoplankton" & set_lag == 0){
  #   agg_var <- "mean"
  #   var_label <- " phytoplankton (red line)"
  #   covar <- pt0
  #   set_lag <- 0
  # }

  ## SST - not correlated - OISST
  sst_monthly <- readRDS("data-raw/cw_surface_temperature_oi.rds")
  sst0 <- sst_monthly |>
    filter(month %in% c(4,5,6), !is.na(value)) |>
    group_by(year) |> summarise(value = mean(value, na.rm = TRUE)) |>
    filter(year %in% yrs, !is.na(value)) |>
    mutate(time = seq_along(year),
           value_raw = value,
           value = (value - mean(value))/ sd(value),
           type = "SST (OI)")
  hist(sst0$value)

  ## SST - ROMS
  sst_monthly2 <- readRDS("data-raw/cw_surface_temperature.rds")
  sst2 <- sst_monthly2 |>
    filter(month %in% c(4,5,6), !is.na(value)) |>
    group_by(year) |> summarise(value = mean(value, na.rm = TRUE)) |>
    filter(year %in% yrs, !is.na(value)) |>
    mutate(time = seq_along(year),
           value_raw = value,
           value = (value - mean(value))/ sd(value),
           type = "SST (BCCM)")
  hist(sst2$value)


  # if(var_type == "SST" & set_lag == 0){
  #   agg_var <- "mean"
  #   var_type <- "SST"
  #   var_label <- " Jan-Jun SST (red line)"
  #   covar <- sst0
  # }

  sst1 <- sst_monthly |>
    group_by(year) |> summarise(value = mean(value, na.rm = TRUE)) |>
    mutate(year = year + 1) |>
    filter(year %in% yrs, !is.na(value)) |>
    mutate(
      time = seq_along(year),
      value_raw = value,
      value = (value - mean(value))/ sd(value),
      type = "SST (prior year)")
  hist(sst1$value)

  ## TOB - not correlated
  tob_monthly <- readRDS("data-raw/cw_bottom_temperature.rds")
  tob0 <- tob_monthly |>
    filter(month %in% c(4,5,6)) |>
    group_by(year) |> summarise(value = mean(value, na.rm = TRUE)) |>
    filter(year %in% yrs, !is.na(value)) |>
    mutate(time = seq_along(year),
           value_raw = value,
           value = (value - mean(value))/ sd(value),
           type = "TOB (BCCM)")
  hist(tob0$value)

  tob1 <- tob_monthly |> group_by(year) |> summarise(value = mean(value, na.rm = TRUE)) |>
    mutate(year = year + 1) |>
    filter(year %in% yrs, !is.na(value)) |>
    mutate(
      time = seq_along(year),
      value_raw = value,
      value = (value - mean(value))/ sd(value),
      type = "TOB (prior year)")
  hist(tob1$value)

  ## BO2 - negatively correlated with 2
  do_monthly <- readRDS("data-raw/cw_bottom_oxygen.rds")
  o20 <- do_monthly |>
    filter(month %in% c(4,5,6), !is.na(value)) |>
    group_by(year) |> summarise(value = mean(value, na.rm = TRUE)) |>
    filter(year %in% yrs, !is.na(value)) |>
    mutate(time = seq_along(year),
           value_raw = value,
           value = (value - mean(value))/ sd(value),
           # type = "Bottom O2"
           value = -value,
           type = "O2 depletion"
    )
  hist(o20$value)

  o2p0 <- do_monthly |>
    filter(month %in% c(4,5,6), !is.na(value)) |>
    group_by(year) |> summarise(value = mean(value, na.rm = TRUE)) |>
    filter(year %in% yrs, !is.na(value)) |>
    mutate(time = seq_along(year),
           value_raw = value,
           value = (value - mean(value))/ sd(value),
           type = "O2 available"
    )

  ## BO2 - negatively correlated with 2
  so2_monthly <- readRDS("data-raw/cw_surface_oxygen.rds")
  so20 <- so2_monthly |>
    filter(month %in% c(4,5,6), !is.na(value)) |>
    group_by(year) |> summarise(value = mean(value, na.rm = TRUE)) |>
    filter(year %in% yrs, !is.na(value)) |>
    mutate(time = seq_along(year),
           value_raw = value,
           value = (value - mean(value))/ sd(value),
           type = "Surface O2")
  hist(so20$value)


  library(readxl)
  pink <- read_excel("data-raw/pink/N Pacific Pink Salmon Abundance Data for Luke Rogers 4 May 2021.xlsx", col_types = c("numeric", "numeric", "skip"))

  pink2 <- pink |>
    mutate(year = `Return Year`,
           value = `Pink Returns (thousands)`) |>
    filter(year %in% yrs, !is.na(value)) |>
    mutate(time = seq_along(year),
           value_raw = value,
           value = (value - mean(value))/ sd(value),
           type = "Pink salmon adults")
  hist(pink2$value)

  pink1 <- pink |>
    mutate(year = `Return Year`-1,
           value = `Pink Returns (thousands)`) |>
    filter(year %in% yrs, !is.na(value)) |>
    mutate(time = seq_along(year),
           value_raw = value,
           value = (value - mean(value))/ sd(value),
           type = "Pink salmon juveniles")
  hist(pink1$value)


  ev1 <- bind_rows(pdo0, oni0, npgo0#, npi0
                   ) |>
    mutate(type = factor(type, levels = c("ENSO (ONI)", "PDO", "NPGO", "NPI"))) %>%
    ggplot() +
    geom_line(aes(time, value, colour = type), alpha = 0.7, linewidth = 1) +
    # geom_point(aes(time, value, colour = type), size = 2) +
    # scale_color_viridis_d()+
    # scale_color_brewer(palette = "Paired")+
    scale_colour_manual(values = RColorBrewer::brewer.pal(n = 10, name = "Paired")[c(9,10,1
                                                                                     #,8
                                                                                     )]) +
    scale_x_continuous(limits = c(0, 24),label = label_yrs ) +
    theme(
      axis.title = element_blank(),
      legend.justification=c(0, 1)) +
    # legend.position = "none")+
    labs(x = "Year", y = "Standardized index", colour = "Climate Index")

  # ggsave("figs/climate-indices.png", width = 4, height = 2)
  ev1

  ev2 <- bind_rows(sst0, tob0,
                   sst2, #pp0,  pt0,
  ) |>
    #mutate(type = factor(type, levels = c("ENSO (ONI)", "PDO", "NPI", "SOI"))) %>%
    ggplot() +
    geom_line(aes(time, value, colour = type), alpha = 0.7, linewidth = 1) +
    # geom_point(aes(time, value, colour = type), size = 2) +
    # scale_color_viridis_d()+
    # scale_color_brewer(palette = "Paired")+
    scale_colour_manual(values = RColorBrewer::brewer.pal(n = 10, name = "Paired")[c(#4,3,
      # 1,
      5,
      6,
      # 2,
      7)])  +
    scale_x_continuous(limits = c(0, 24), label = label_yrs ) +
    theme(
      axis.title.y = element_blank(),
      legend.justification=c(0, 1)) +
    # legend.position = "none")+
    labs(x = "Year", y = "Standardized value", colour = "Temperature")
  ev2

  ev3 <- bind_rows(pp0,  #pt0,
                   # so20,
                   o20#, pink1, pink2
  ) |>
    #mutate(type = factor(type, levels = c("ENSO (ONI)", "PDO", "NPI", "SOI"))) %>%
    ggplot() +
    geom_line(aes(time, value, colour = type), alpha = 0.7, linewidth = 1) +
    # geom_point(aes(time, value, colour = type), size = 2) +
    # scale_color_viridis_d()+
    # scale_color_brewer(palette = "Paired")+
    scale_colour_manual(values = RColorBrewer::brewer.pal(n = 12, name = "Paired")[c(1,#3,
                                                                                     4#,2
                                                                                     #6,5,
    )])  +
    scale_x_continuous(limits = c(0, 24), label = label_yrs ) +
    theme(
      axis.title.y = element_blank(),
      legend.justification=c(0, 1)) +
    # legend.position = "none")+
    labs(x = "Year", y = "Standardized value", colour = "Other BCCM")

  ev3

  # ggsave("figs/ev-indices.png", width = 4, height = 2)

  y_lab_big <- ggplot() +
    annotate(geom = "text", x = 1, y = 1, size = 4,
             colour = "grey30",
             label = "Standardized Environmental Variable", angle = 90) +
    coord_cartesian(clip = "off")+
    theme_void()

  y_lab_big + (ev1/ev2/ev3) + patchwork::plot_layout(width = c(0.1,1))

  ggsave("figs/man/ev-indices-spr.png", width = 6, height = 4.3)
