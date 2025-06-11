# prepare coastwide covariates for DFA

library(tidyverse)
library(patchwork)
library(pacea)
devtools::load_all()
theme_set(ggsidekick::theme_sleek())

# check that this matches current range for condition indices?
yrs <- 2002:2024

npgo2 <- npgo |>
  group_by(year) |>
  summarise(value = mean(anomaly, na.rm = TRUE)) |>
  mutate(year = year + 2) |>
  filter(year %in% yrs, !is.na(value)) |>
  mutate(
    time = seq_along(year),
    value_raw = value,
    value = (value - mean(value)) / sd(value),
    # value = value_raw,
    type = "NPGO (lag 2)"
  )
hist(npgo2$value)

npgo0 <- npgo |>
  filter(month %in% c(1, 2, 3, 4, 5, 6)) |>
  group_by(year) |>
  summarise(value = mean(anomaly, na.rm = TRUE)) |>
  filter(year %in% yrs, !is.na(value)) |>
  mutate(
    time = seq_along(year),
    value_raw = value,
    value = (value - mean(value)) / sd(value),
    # value = value_raw,
    type = "NPGO"
  )
hist(npgo0$value)

npgoF <- npgo2 |> mutate(value = -value, type = "Inverted NPGO (lag 2)")


## ONI - not correlated
oni0 <- oni |>
  filter(month %in% c(1, 2, 3, 4, 5, 6)) |>
  group_by(year) |>
  summarise(value = mean(anomaly, na.rm = TRUE)) |>
  filter(year %in% yrs, !is.na(value)) |>
  mutate(
    time = seq_along(year),
    value_raw = value,
    value = (value - mean(value)) / sd(value),
    type = "ENSO (ONI)"
  )
hist(oni0$value)


oni1 <- oni |>
  group_by(year) |>
  summarise(value = mean(anomaly, na.rm = TRUE)) |>
  mutate(year = year + 1) |>
  filter(year %in% yrs, !is.na(value)) |>
  mutate(
    time = seq_along(year),
    value_raw = value,
    value = (value - mean(value)) / sd(value),
    type = "ONI (lag 1)"
  )
hist(oni1$value)

pdo0 <- pdo |>
  filter(month %in% c(1, 2, 3, 4, 5, 6)) |>
  group_by(year) |>
  summarise(value = mean(anomaly, na.rm = TRUE)) |>
  filter(year %in% yrs, !is.na(value)) |>
  mutate(
    time = seq_along(year),
    value_raw = value,
    value = (value - mean(value)) / sd(value),
    type = "PDO"
  )
hist(pdo0$value)

pdoF <- pdo0 |> mutate(value = -value, type = "Inverted PDO")

pdo1 <- pdo |>
  group_by(year) |>
  summarise(value = mean(anomaly, na.rm = TRUE)) |>
  mutate(year = year + 1) |>
  filter(year %in% yrs, !is.na(value)) |>
  mutate(
    time = seq_along(year),
    value_raw = value,
    value = (value - mean(value)) / sd(value),
    type = "PDO (lag 1)"
  )
hist(pdo1$value)

## primary -- positively correlated (more so w trend 2)
pp_monthly <- readRDS("data-raw/cw_primary_production.rds")
pp0 <- pp_monthly |>
  filter(month %in% c(1, 2, 3, 4, 5, 6), !is.na(value)) |>
  group_by(year) |>
  summarise(value = mean(value, na.rm = TRUE)) |>
  filter(year %in% yrs, !is.na(value)) |>
  mutate(
    time = seq_along(year),
    value_raw = value,
    value = (value - mean(value)) / sd(value),
    type = "Primary production"
  )
hist(pp0$value)


pp1 <- pp_monthly |>
  group_by(year) |>
  summarise(value = mean(value, na.rm = TRUE)) |>
  mutate(year = year + 1) |>
  filter(year %in% yrs, !is.na(value)) |>
  mutate(
    time = seq_along(year),
    value_raw = value,
    value = (value - mean(value)) / sd(value),
    type = "Production (lag 1)"
  )
hist(pp1$value)


## SST - not correlated - OISST
sst_monthly <- readRDS("data-raw/cw_surface_temperature_oi.rds")
sst0 <- sst_monthly |>
  filter(month %in% c(4, 5, 6), !is.na(value)) |>
  group_by(year) |>
  summarise(value = mean(value, na.rm = TRUE)) |>
  filter(year %in% yrs, !is.na(value)) |>
  mutate(
    time = seq_along(year),
    value_raw = value,
    value = (value - mean(value)) / sd(value),
    type = "SST (OI)"
  )
hist(sst0$value)

## SST - ROMS
sst_monthly2 <- readRDS("data-raw/cw_surface_temperature.rds")
sst2 <- sst_monthly2 |>
  filter(month %in% c(4, 5, 6), !is.na(value)) |>
  group_by(year) |>
  summarise(value = mean(value, na.rm = TRUE)) |>
  filter(year %in% yrs, !is.na(value)) |>
  mutate(
    time = seq_along(year),
    value_raw = value,
    value = (value - mean(value)) / sd(value),
    type = "SST (BCCM)"
  )
hist(sst2$value)

sst1 <- sst_monthly |>
  group_by(year) |>
  summarise(value = mean(value, na.rm = TRUE)) |>
  mutate(year = year + 1) |>
  filter(year %in% yrs, !is.na(value)) |>
  mutate(
    time = seq_along(year),
    value_raw = value,
    value = (value - mean(value)) / sd(value),
    type = "SST (lag 1)"
  )
hist(sst1$value)

## TOB - not correlated
tob_monthly <- readRDS("data-raw/cw_bottom_temperature.rds")
tob0 <- tob_monthly |>
  filter(month %in% c(4, 5, 6)) |>
  group_by(year) |>
  summarise(value = mean(value, na.rm = TRUE)) |>
  filter(year %in% yrs, !is.na(value)) |>
  mutate(
    time = seq_along(year),
    value_raw = value,
    value = (value - mean(value)) / sd(value),
    type = "TOB (BCCM)"
  )
hist(tob0$value)

tob1 <- tob_monthly |>
  group_by(year) |>
  summarise(value = mean(value, na.rm = TRUE)) |>
  mutate(year = year + 1) |>
  filter(year %in% yrs, !is.na(value)) |>
  mutate(
    time = seq_along(year),
    value_raw = value,
    value = (value - mean(value)) / sd(value),
    type = "TOB (lag 1)"
  )
hist(tob1$value)

do_monthly <- readRDS("data-raw/cw_bottom_oxygen.rds")
o20 <- do_monthly |>
  filter(month %in% c(4, 5, 6), !is.na(value)) |>
  group_by(year) |>
  summarise(value = mean(value, na.rm = TRUE)) |>
  filter(year %in% yrs, !is.na(value)) |>
  mutate(
    time = seq_along(year),
    value_raw = value,
    value = (value - mean(value)) / sd(value),
    # type = "Bottom O2"
    value = -value,
    type = "O2 depletion"
  )
hist(o20$value)

o2p0 <- do_monthly |>
  filter(month %in% c(4, 5, 6), !is.na(value)) |>
  group_by(year) |>
  summarise(value = mean(value, na.rm = TRUE)) |>
  filter(year %in% yrs, !is.na(value)) |>
  mutate(
    time = seq_along(year),
    value_raw = value,
    value = (value - mean(value)) / sd(value),
    type = "O2 available"
  )

ev1 <- bind_rows(pdo0, oni0, npgo2) |>
  mutate(type = factor(type, levels = c("ENSO (ONI)", "PDO", "NPGO (lag 2)"))) %>%
  ggplot() +
  geom_line(aes(time, value, colour = type), alpha = 0.7, linewidth = 1) +
  scale_colour_manual(values = RColorBrewer::brewer.pal(n = 10, name = "Paired")[c(9, 10, 1)]) +
  scale_x_continuous(limits = c(0, 24), label = label_yrs) +
  theme(
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.justification = c(0, 1)
  ) +
  # legend.position = "none")+
  labs(x = "Year", y = "Standardized index", colour = "Climate Index")

# ggsave("figs/climate-indices.png", width = 4, height = 2)
ev1

ev2 <- bind_rows(sst0, tob0, sst2, ) |>
  ggplot() +
  geom_line(aes(time, value, colour = type), alpha = 0.7, linewidth = 1) +
  scale_colour_manual(values = RColorBrewer::brewer.pal(n = 10, name = "Paired")[c( # 4,3,
    # 1,
    5,
    6,
    # 2,
    7
  )]) +
  scale_x_continuous(limits = c(0, 24), label = label_yrs) +
  theme(
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.justification = c(0, 1)
  ) +
  # legend.position = "none")+
  labs(x = "Year", y = "Standardized value", colour = "Temperature")
ev2

ev3 <- bind_rows(
  pp0,
  pp1,
  o20
) |>
  ggplot() +
  geom_line(aes(time, value, colour = type), alpha = 0.7, linewidth = 1) +
  scale_colour_manual(values = RColorBrewer::brewer.pal(n = 12, name = "Paired")[c(2, 4, 3)]) +
  scale_x_continuous(limits = c(0, 24), label = label_yrs) +
  theme(
    axis.title = element_blank(),
    legend.justification = c(0, 1)
  ) +
  # legend.position = "none")+
  labs(x = "Year", y = "Standardized value", colour = "Other BCCM")

ev3

# ggsave("figs/ev-indices.png", width = 4, height = 2)

y_lab_big <- ggplot() +
  annotate(
    geom = "text", x = 1, y = 1, size = 4,
    colour = "grey30",
    label = "Standardized Environmental Variable", angle = 90
  ) +
  coord_cartesian(clip = "off") +
  theme_void()

y_lab_big + (ev1 / ev2 / ev3) + plot_layout(width = c(0.1, 1))

ggsave("figs/man/ev-indices-spr.png", width = 6, height = 4.3)


## make a small version for the flow chart
y_lab_big <- ggplot() +
  annotate(
    geom = "text", x = 1, y = 1, size = 4,
    colour = "grey30",
    label = "Standardized value", angle = 90
  ) +
  coord_cartesian(clip = "off") +
  theme_void()
y_lab_big + (ev1 / ev2 / ev3) + patchwork::plot_layout(width = c(0.1, 1)) & theme(legend.title = element_blank())

ggsave("figs/man/chart-ev-indices.png", width = 4.25, height = 2.7)
