# update bayesDFA

# devtools::install_github("fate-ewi/bayesdfa")

library(tidyverse)
library(dplyr)
library(bayesdfa)
library(patchwork)
library(pacea)

devtools::load_all()

dir.create(paste0("figs/DFA/"), showWarnings = FALSE)
dir.create(paste0("figs/man/"), showWarnings = FALSE)

# load overall species list
source("analysis/00-species-list.R")

theme_set(ggsidekick::theme_sleek())
options(mc.cores = parallel::detectCores())
fig_height <- 4 * 2
fig_width <- 5 * 2



## Choose model ----
## DFA settings

ctrl <- list(adapt_delta = 0.98, max_treedepth = 12)
# ctrl <- list(adapt_delta = 0.98, max_treedepth = 12)
set_iter <- 4000 # min 2000
set_chains <- 3


# ## choose response variable
## are we doing a DFA of biomass density rather than condition
# density <- TRUE
density <- FALSE
### if condition
# adjusted_for_density <- FALSE
adjusted_for_density <- TRUE

# trend_count <- 1
trend_count <- 2
# trend_count <- 3

no_covs <- TRUE
# no_covs <- FALSE

# set_group <- "immatures"
# set_group <- "mature males"
set_group <- "mature females"

if(no_covs) {
  if(adjusted_for_density){
  # model_name <- "apr-2024-not-total-density"
  # model_name <- "apr-2024-cell-means"
    model_name <- "2024-09-doy-ld0c"
  if(set_group == "immatures"){
    y_label <- "Immature condition indices (adjusting for density)"
    which_flip <- 0L
    which_flip2 <- 0L
  }
  if(set_group == "mature males") {
    y_label <- "Mature male condition indices (adjusting for density)"
    which_flip <- 0L
    which_flip2 <- 2L
  }
  if(set_group ==  "mature females") {
    y_label <- "Mature female condition indices (adjusting for density)"
    which_flip <- 1L
    which_flip2 <- 2L
  }
  } else {
  # model_name <- "apr-2024"
  model_name <- "2024-09"
  if(set_group == "immatures") {
    y_label <- "Immature condition indices (not controlling for density)"
    which_flip <- 1L
    which_flip2 <- 2L
  }
  if(set_group == "mature males") {
    y_label <- "Mature male condition indices (not controlling for density)"
    which_flip <- 1L
    which_flip2 <- 2L
  }
  if(set_group ==  "mature females") {
    y_label <- "Mature female condition indices (not controlling for density)"
    which_flip <- 1L
    which_flip2 <- 0L
  }
  }
  } else{
    # model_name <- "apr-2024"
    model_name <- "2024-09"
  if(set_group == "immatures") {
    y_label <- "Immature condition indices (biomass as observation covariate)"
    which_flip <- 0L
    which_flip2 <- 0L
  }
  if(set_group == "mature males") {
    y_label <- "Mature male condition indices (biomass as observation covariate)"
    which_flip <- 0L
    which_flip2 <- 0L
  }
  if(set_group ==  "mature females") {
    y_label <- "Mature female condition indices (biomass as observation covariate)"
    which_flip <- 0L
    which_flip2 <- 0L
  }
  }


if (trend_count == 3) trend_label <- "3 trends"
if (trend_count == 2) trend_label <- "2 trends"
if (trend_count == 1) trend_label <- "1 trend"

## Load and structure data ----


# or if density?
if(density){

  set_group <- "total"
  # model_name <- "dln-dln-all-mar-2024"
  model_name <- "dln-all-2024-09"
  y_label <- "Total density indices"

  f <- list.files(
    paste0("data-generated/density-index/", model_name, "/total"),
    pattern = ".rds",
    full.names = TRUE)

  d0 <- purrr::map_dfr(f, readRDS)

  dg <- d0

} else{
  f <- list.files(paste0("data-generated/cond-index/",
                         model_name), pattern = ".rds",
                  full.names = TRUE)

  d <- purrr::map_dfr(f, readRDS)

  dg <- filter(d, group == set_group)
}


# # ## remove species?
# # # better convergence but weaker patterns?
# species_to_remove <- c(
#   #"Slender Sole",
#   # "Curlfin Sole",
#   # "Sand Sole",
#   "Butter Sole" # samples only from HS and strange behaviour of RF
#   )

dg <- filter(dg, !(species %in% species_to_remove))
# d0 <- filter(d0, !(species %in% species_to_remove))


dg <- dg %>% mutate(
  taxa_group = case_when(species %in% Flatfish~"Flatfish",
                         species %in% Rockfish~"Rockfish",
                         TRUE~"Other"
  ),
  species = ifelse(species == "Rougheye/Blackspotted Rockfish Complex",
                   "Rougheye/Blackspotted", species
  )
)



ggplot(d, aes(year, est)) + facet_wrap(~paste(species, group)) +
  geom_line()

ggplot(d, aes(year, est, colour = species)) + facet_wrap(~group) +
  geom_line()

ggplot(dg, aes(year, log_est)) +
  facet_wrap(~paste(species, group)) +
  geom_line()

## wide dataframe structure
# dw <- tidyr::pivot_wider(dg, id_cols = c(year), names_from = species, values_from = est) |>
#   select(-year) |> t()
# dw

## long dataframe structure
yrs <- sort(unique(dg$year))
spp <- unique(dg$species)
spp_grouped <- select(dg, species, taxa_group) %>% distinct()
taxa_gr <- spp_grouped$taxa_group
yr_lu <- data.frame(year = sort(unique(dg$year)), time = seq_along(yrs))
dg <- left_join(dg, yr_lu)

dd <- tibble(
  obs = dg$log_est,
  # obs = dg$est,
  year = dg$year,
  time = dg$time,
  ts = dg$species,
  se = dg$se
)

## Weights ----
range(dg$se)
range(1/dg$se^2)

# SD/weight not SD^2/weight in model code, so try
# dd$weights <- 1 / (dd$se) # this is the SD
# dd$weights_scaled <- dd$weights / mean(dd$weights)
dd$weights <- (1 / dd$se)^2 # this would be the variance
dd$weights_scaled <- (dd$weights / mean(dd$weights))
# dd$weights_scaled <- sqrt(dd$weights / mean(dd$weights)) # ruins effects
# dd$weights_scaled <- dd$weights / median(dd$weights)
hist(dd$weights)
hist(dd$weights_scaled)
mean(dd$weights_scaled)

plot((sort(dd$weights)))
plot((sort(dd$weights_scaled/2)))

plot((sort(dd$weights_scaled)))
ggplot(dd) + geom_point(aes(se, (weights_scaled)))

# DFA without a covariate ----
if(no_covs){
m <- fit_dfa(
  y = dd,
  iter = set_iter,
  chains = set_chains,
  num_trends = trend_count,
  inv_var_weights = "weights_scaled",
  estimate_process_sigma = FALSE,
  estimate_nu = FALSE,
  scale = "zscore",
  data_shape = "long",
  # seed = 298191,
  seed = 298,
  control = ctrl
)
# m
} else{

  if(set_group == "immature") {

    spp_list <- dg %>% select(species, dens_model_name) %>% distinct()

    f <- list(
      paste0("data-generated/density-index/", spp_list$dens_model_name, "/imm/i-",
             gsub(" ", "-", gsub("\\/", "-", tolower(species))), "-",
             spp_list$dens_model_name, "-20-km.rds")
    )

  } else {

    if(obs_covs) {
    # if we were to control for overall biomass as a covariate unique to each species
    f <- list.files(
      paste0("data-generated/density-index/dln-all-mar-2024/total"),
      pattern = ".rds",
      full.names = TRUE)

  }

  d0 <- purrr::map_dfr(f, readRDS)

  d0 <- d0 %>% mutate(
    taxa_group = case_when(species %in% Flatfish~"Flatfish",
                           species %in% Rockfish~"Rockfish",
                           TRUE~"Other"
    ),
    species = ifelse(species == "Rougheye/Blackspotted Rockfish Complex",
                     "Rougheye/Blackspotted", species
    )
  )

  spp_yrs <- dd %>% select(time, year, species = ts) %>% distinct()

  d0 <- left_join(spp_yrs, d0) #use years from dg

  obs_cov <- d0 %>% group_by(species) %>%
    mutate(value = (log_est - mean(log_est))/sd(log_est)) %>%
    select(time, timeseries = species, value)
  obs_cov$covariate <- 1

  # ctrl <- list(adapt_delta = 0.98, max_treedepth = 12)
  obs_cov$timeseries <- as.integer(as.factor(obs_cov$timeseries)) # must be numeric; should match `dd` (which my quick hack may not!!)
  obs_cov <- as.data.frame(obs_cov) # make not a tibble because of different default `drop` arguments, yes, should probably be made robust to that... otherwise get error:
  # dims declared=(578); dims found=(578,1)


  dd$timeseries <- dd$ts
  # save(dd, obs_cov, file = "dfa.rda")
}

  if(proc_covs){








  }



m <- fit_dfa(
  y = dd,
  # iter = 500,
  # chains = 1,
  # num_trends = 2,
  iter = set_iter,
  chains = set_chains,
  num_trends = trend_count,
  inv_var_weights = "weights_scaled",
  estimate_process_sigma = FALSE,
  estimate_nu = FALSE,
  scale = "zscore",
  obs_covar = obs_cov,
  data_shape = "long",
  seed = 298,
  control = ctrl
)
}



range(m$monitor$Bulk_ESS)
range(m$monitor$Rhat)
# row.names(m$monitor)

# # this isn't working anymore
# checks <- m$monitor |> filter(Rhat >1)
# checks |> View()
# # Error in `[.simsummary`(x, sliceStart:sliceEnd) :
# # argument "j" is missing, with no default

r <- rotate_trends(m)

label_yrs <- function(x) {
  x + yrs[1] - 1
}

plot_trends(r) + scale_x_continuous(label = label_yrs )

plot_loadings(r, names = spp) +
  ggtitle(paste0("DFA for ", set_group, " with no covariates"))

flip_trend <- function(rotated_modelfit, trend = 1L) {
  rflip <- rotated_modelfit
  rflip$trends_mean[trend,] <- -1 * rotated_modelfit$trends_mean[trend,]
  rflip$trends_lower[trend,] <- -1 * rotated_modelfit$trends_lower[trend,]
  rflip$trends_upper[trend,] <- -1 * rotated_modelfit$trends_upper[trend,]
  for (i in seq_len(dim(rotated_modelfit$Z_rot)[1])) {
    rflip$Z_rot[i,,trend] <- -1 * rotated_modelfit$Z_rot[i,,trend]
    rflip$trends[i,,trend] <- -1 * rotated_modelfit$trends[i,,trend]
  }
  rflip
}

plot_loadings(r, names = spp)
# plot_trends(r, years = yrs)

if(which_flip == 0L) {
  rflip <- r
} else {
  rflip <- flip_trend(r, which_flip)
}
rflip <- flip_trend(rflip, which_flip2)

plot_trends(r, years = yrs)
plot_trends(rflip, years = yrs)
plot_loadings(rflip, names = spp)

if(density) {

(p1 <- plot_trends(rflip) +
    scale_x_continuous(label = label_yrs ) +
    # geom_line(data = covars, aes_string(x = "time", y = "value", colour = "Variable"),
    #           alpha = 0.8,
    #           linewidth = 1.5) +
    # scale_colour_brewer(palette = "Paired") +
    # labs(colour = "", ylab = "Standardized value" ) +
    # theme(legend.position = c(0.2,0.85), axis.title.y = element_text())+
    # theme(legend.position = "none") +
    facet_wrap(~trend_number, nrow = 2) +
    ggsidekick::theme_sleek() +
    scale_fill_manual(values = RColorBrewer::brewer.pal(n = 6, name = "Paired")[c(6,2)]) +
    theme(legend.position='top', legend.justification='left', legend.direction='horizontal',
          plot.margin = unit(c(0.2,0.5,0.2,0.2), "cm"),
          legend.text=element_text(size=rel(0.8))) +
    # ggtitle(paste0("DFA of ", set_group, " with no covariates"))
    ylab("Standardized value") +
    ggtitle(paste0("DFA of biomass indices"))
)

(p2 <- plot_loadings(rflip, names = spp)+
    # scale_fill_manual(values = RColorBrewer::brewer.pal(n = 6, name = "Paired")[c(6,2)]) +
    scale_fill_manual(values = c("grey20","grey20")) +
    guides(alpha=guide_legend(override.aes = list(fill = "grey20")),
           fill = "none") +
    labs(alpha = "Prob not 0") +
    theme(legend.position = "none",
          plot.margin = unit(c(0.2,0.5,0.2,0.2), "cm"),
          axis.title.y = element_blank()) )

wrap_elements(p1) + p2 + plot_layout(ncol = 2)

ggsave("figs/DFA-tends-total.png", width = 8, height = 5)

}

# DFA fit plot ----

# plot_fitted(m2, names = spp)

source("R/dfa_fitted.R")
df1 <- dfa_fitted(m, conf_level = 0.95, names = spp)
# cols <- viridis::viridis(length(unique((df$ID))), end = 0.8)

(pf <- ggplot(df1) +
    geom_ribbon(aes(x = time,
                           ymin = lower, ymax = upper
                           #, fill = "ID"
    ),
    alpha = 0.4) +
    geom_line(aes(x = time, y = estimate#, colour = "ID"
    )) +
    # # geom_line(data = pro_covar, aes_string(x = "time", y = "value"), colour = "red") +
    # geom_line(data = covars, aes_string(x = "time", y = "value", colour = "Variable"),
    #           alpha = 0.7,
    #           linewidth = 0.5) +
    # # scale_colour_manual(values = RColorBrewer::brewer.pal(n = 4, name = "Paired")[c(2,4)]
    # # ) +
    geom_point(aes(x = time, y = y),
               #col = "blue",
               size = 0.5, alpha = 0.7) +
    # scale_color_manual(values = cols) +
    # scale_fill_manual(values = cols) +
    facet_wrap(~ID#, scales = "free_y"
    ) +
    xlab("Time") + ylab(y_label) +
    scale_x_continuous(label = label_yrs ) +
    # theme(legend.position = c(.87,.1)) +
    # theme(legend.position = "none") +
    # labs(colour= "Species", fill = "Species")+
    ggtitle(paste0("DFA ", y_label, " with ", trend_label))
)

# # only needed if including covariates
# if(trend_count>1){
#   (pf <- pf + scale_colour_manual(values = RColorBrewer::brewer.pal(n = 4, name = "Paired")) )
# } else {
#   pf <- pf + scale_colour_manual(values = RColorBrewer::brewer.pal(n = 4, name = "Paired")[c(2,4)])
# }


ggsave(paste0("figs/man/DFA-fits-",  trend_count,
              "trends-no-cov-", no_covs, "-",
              gsub(" ", "-", set_group),
              "-", model_name, "-4k.png"),
       height = fig_height*0.7, width = fig_width)



## Get coastwide covariates ----

## choose 1 covariate
# set_lag <- 1
# agg_var <- "mean"
# set_lag <- 0
# # agg_var <- "max" # not good fit for immature
# agg_var <- "mean"
# agg_var <- "min"
#
# if (set_lag == 0) lag_label <- ""
# if (set_lag == 1) lag_label <- "previous year's "

if(!exists("so20")){

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
         type = "SOI (previous year)")
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
         type = "ONI (previous year)")
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
         type = "PDO (previous year)")
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

## SST - not correlated
sst_monthly <- readRDS("data-raw/cw_surface_temperature.rds")
sst0 <- sst_monthly |>
  filter(month %in% c(1,2,3,4,5,6), !is.na(value)) |>
  group_by(year) |> summarise(value = mean(value, na.rm = TRUE)) |>
  filter(year %in% yrs, !is.na(value)) |>
  mutate(time = seq_along(year),
         value_raw = value,
         value = (value - mean(value))/ sd(value),
         type = "SST")
hist(sst0$value)
#
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
    type = "SST (previous year)")
hist(sst1$value)

## TOB - not correlated
tob_monthly <- readRDS("data-raw/cw_bottom_temperature.rds")
tob0 <- tob_monthly |>
  filter(month %in% c(1,2,3,4,5,6)) |>
  group_by(year) |> summarise(value = mean(value, na.rm = TRUE)) |>
  filter(year %in% yrs, !is.na(value)) |>
  mutate(time = seq_along(year),
         value_raw = value,
         value = (value - mean(value))/ sd(value),
         type = "TOB")
hist(tob0$value)

tob1 <- tob_monthly |> group_by(year) |> summarise(value = mean(value, na.rm = TRUE)) |>
  mutate(year = year + 1) |>
  filter(year %in% yrs, !is.na(value)) |>
  mutate(
    time = seq_along(year),
    value_raw = value,
    value = (value - mean(value))/ sd(value),
    type = "TOB (previous year)")
hist(tob1$value)

## BO2 - negatively correlated with 2
do_monthly <- readRDS("data-raw/cw_bottom_oxygen.rds")
o20 <- do_monthly |>
  filter(month %in% c(1,2,3,4,5,6), !is.na(value)) |>
  group_by(year) |> summarise(value = mean(value, na.rm = TRUE)) |>
  filter(year %in% yrs, !is.na(value)) |>
  mutate(time = seq_along(year),
         value_raw = value,
         value = (value - mean(value))/ sd(value),
         # type = "Bottom O2"
         value = -value,
         type = "Upwelling (O2 depletion)"
  )
hist(o20$value)

## BO2 - negatively correlated with 2
so2_monthly <- readRDS("data-raw/cw_surface_oxygen.rds")
so20 <- so2_monthly |>
  filter(month %in% c(1,2,3,4,5,6), !is.na(value)) |>
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
}

# if(var_type == "TOB" & set_lag == 0){
#   agg_var <- "mean"
#   var_type <- "TOB"
#   var_label <- " Jan-Jun TOB (red line)"
#   covar <- tob0
#   set_lag <- 0
# }
# if(var_type == "TOB" & set_lag == 1){
#   agg_var <- "mean"
#   var_type <- "TOB"
#   var_label <- " previous year's TOB (red line)"
#   covar <- tob1
#   set_lag <- 1
# }
#
# arrow_tob1 <- readRDS("data/all-productivity-longer-2023-04-14-2.rds") |>
#   filter(species == "Arrowtooth Flounder",
#          variable_type == "TOB",
#          months == "1to12", climate_model == "roms")|>
#   select(year, agg_type, value, lag) |>
#   # make sure first year represented and no missing years in sequence
#   filter(year %in% yrs) |>
#   filter(agg_type == agg_var) |>
#   # filter(agg_type == "max") |>
#   # filter(lag == 0) |>
#   filter(lag == 1) |>
#   mutate(time = seq_along(year),
#          trend = 1,
#          covariate = 1,
#          # var_mean = mean(value),
#          # var_sd = sd(value),
#          # value = (value - mean(value))/sd(value)
#          ) |>
#   select(-year, -agg_type, -lag) |>
#   as.data.frame(stringsAsFactors = FALSE)
#
# # check_tob <- pro_tob1 |> rename(value1 = value) |> left_join(pro_covar)
# # plot(value~value1, data = check_tob)
# tob1$covariate <- "arrowtooth TOB"
# arrow_tob1$covariate <- "BCCM TOB"
# check_tob <- bind_rows(tob1, arrow_tob1)
# ggplot(check_tob) + geom_line(aes(time, value, colour = covariate))
#
# ## explore
# plot(pp0$value, pt0$value)
# plot(pt0$year,pt0$value)
#
# plot(pp0$year,pp0$value)
# points(pt0$value~pt0$year, col = "red")

# all_vars <- bind_rows(pdo0, pdo1, oni0, oni1)
# all_vars <- bind_rows(pdo0, oni0, soi0)
# all_vars <- bind_rows(pdo1, oni1, soi1)
# plot_trends(r, years = pro_covar$time) +
#   geom_line(data = all_vars, aes_string(x = "time", y = "value", colour = "type")) +
#   scale_color_viridis_d()
## formate an annual var



# One covariate ----
# # posthoc correlation: (simple, propagates uncertainty via 'trend_samples')
# set.seed(1)
# fake_dat <- rnorm(length(yrs), 0, 1)
# correlation <- trend_cor(r, fake_dat, trend_samples = 100)
# hist(correlation)

## alternate set up for just one at a time
# pro_covar <- covar |>
#   mutate(trend = 1,
#          value = (value - mean(value))/ sd(value),
#          covariate = 1
#   ) |>
#   select(-year, -type)|>
#   as.data.frame(stringsAsFactors = FALSE)


# Explore covariates ----

ev1 <- bind_rows(pdo0, oni0, npi0, soi0) |>
  mutate(type = factor(type, levels = c("ENSO (ONI)", "PDO", "NPI", "SOI"))) %>%
  ggplot() +
  geom_line(aes(time, value, colour = type), alpha = 0.7, linewidth = 1) +
  # geom_point(aes(time, value, colour = type), size = 2) +
  # scale_color_viridis_d()+
  # scale_color_brewer(palette = "Paired")+
  scale_colour_manual(values = RColorBrewer::brewer.pal(n = 8, name = "Paired")[c(1,2,7,8)]) +
  scale_x_continuous(label = label_yrs ) +
  theme(
    axis.title = element_blank(),
    legend.justification=c(0, 1)) +
    # legend.position = "none")+
  labs(x = "Year", y = "Standardized index", colour = "Climate Index")

# ggsave("figs/climate-indices.png", width = 4, height = 2)


ev2 <- bind_rows(sst0, tob0, #pp0,  pt0,
                 so20, o20) |>
  #mutate(type = factor(type, levels = c("ENSO (ONI)", "PDO", "NPI", "SOI"))) %>%
  ggplot() +
  geom_line(aes(time, value, colour = type), alpha = 0.7, linewidth = 1) +
  # geom_point(aes(time, value, colour = type), size = 2) +
  # scale_color_viridis_d()+
  # scale_color_brewer(palette = "Paired")+
  scale_colour_manual(values = RColorBrewer::brewer.pal(n = 10, name = "Paired")[c(#4,3,
                                                                                   6,
                                                                                   9,
                                                                                   5,
                                                                                   10)])  +
  scale_x_continuous( limits = c(1, 22), label = label_yrs ) +
  theme(
    axis.title.y = element_blank(),
    legend.justification=c(0, 1)) +
  # legend.position = "none")+
  labs(x = "Year", y = "Standardized value", colour = "ROMS Variable")
ev2

ev3 <- bind_rows(pp0,  pt0, pink1, pink2) |>
  #mutate(type = factor(type, levels = c("ENSO (ONI)", "PDO", "NPI", "SOI"))) %>%
  ggplot() +
  geom_line(aes(time, value, colour = type), alpha = 0.7, linewidth = 1) +
  # geom_point(aes(time, value, colour = type), size = 2) +
  # scale_color_viridis_d()+
  # scale_color_brewer(palette = "Paired")+
  scale_colour_manual(values = RColorBrewer::brewer.pal(n = 12, name = "Paired")[c(4,6,5,3)])  +
  scale_x_continuous( limits = c(1, 22), label = label_yrs ) +
  theme(
    axis.title.y = element_blank(),
    legend.justification=c(0, 1)) +
  # legend.position = "none")+
  labs(x = "Year", y = "Standardized value", colour = "Biotic Variable")

ev3

# ggsave("figs/ev-indices.png", width = 4, height = 2)

y_lab_big <- ggplot() +
  annotate(geom = "text", x = 1, y = 1, size = 4,
           colour = "grey30",
           label = "Standardized value", angle = 90) +
  coord_cartesian(clip = "off")+
  theme_void()

y_lab_big + (ev1/ev2/ev3) + patchwork::plot_layout(width = c(0.1,1))

ggsave("figs/man/ev-indices.png", width = 6, height = 6)

# Select 2 covariates ----

  # pro_covar1 <- pt0 |>
  # pro_covar1 <- pp0 |> # mat?
  # pro_covar1 <- pdo1 |>
  pro_covar1 <- pdo0 |> #* imm & mat
  # pro_covar1 <- pink1 |>
  # pro_covar1 <- npi0 |> #~
  # pro_covar1 <- oni0 |>
  # pro_covar1 <- oni1 |>
  # pro_covar1 <- soi0 |>
  # pro_covar1 <- sst0 |> #* imm 3
  # pro_covar1 <- tob0 |>
  # pro_covar1 <- o20 |>
  # pro_covar1 <- so20 |> #*
  mutate(trend_number = "Trend 1",
         Variable = type
  ) |>
  select(-year, -type)|>
  as.data.frame(stringsAsFactors = FALSE)

if(set_group == "total"){
  pro_covar1 <- tob0 |>
  #   # pro_covar1 <- tob1 |>
  # pro_covar1 <- sst0 |>
    # pro_covar1 <- sst1 |>
  # pro_covar1 <- o20 |>
  # pro_covar1 <- so20 |> #*
  mutate(trend_number = "Trend 1",
         Variable = type
  ) |>
  select(-year, -type)|>
  as.data.frame(stringsAsFactors = FALSE)
}

if(set_group == "mature females"){
    pro_covar1 <- tob0 |>
    # pro_covar1 <- tob1 |>
    # pro_covar1 <- sst0 |>
    # pro_covar1 <- o20 |>
    # pro_covar1 <- so20 |> #*
    mutate(trend_number = "Trend 1",
           Variable = type
    ) |>
    select(-year, -type)|>
    as.data.frame(stringsAsFactors = FALSE)
}

correlation <- trend_cor(r, pro_covar1$value,
                         time_window = seq_len(max(pro_covar1$time)),
                         trend = 1, trend_samples = 1000)
hist(correlation)
cor1 <- as.data.frame(correlation)

## flip didn't work here
# correlation_f <- trend_cor(rflip, pro_covar1$value,
#                          time_window = seq_len(max(pro_covar1$time)),
#                          trend = 1, trend_samples = 100)
# hist(correlation_f)


 pro_covar1b <- oni0 |> # mat
    # pro_covar1b <- so20 |> #*imm
  # pro_covar1b <- npi0 |>
  # pro_covar1b <- soi0 |>
  # pro_covar1b <- sst0 |>
  # pro_covar1b <- so20 |>
  # pro_covar1b <- pp0 |>
  # pro_covar1b <- tob0 |>
  # pro_covar1b <- o20 |>
    mutate(trend_number = "Trend 1",
         # value = (value - mean(value))/ sd(value),
         Variable = type
  ) |>
  select(-year, -type)|>
  as.data.frame(stringsAsFactors = FALSE)

  if(set_group == "total"){
    # pro_covar1b <- pdo0 |> # mat
      # pro_covar1b <- pdo1 |> # mat
      # pro_covar1b <- tob0 |>
    pro_covar1b <- o20 |>
      mutate(trend_number = "Trend 1",
             # # value = (value - mean(value))/ sd(value),
             Variable = type
      ) |>
      select(-year, -type)|>
      as.data.frame(stringsAsFactors = FALSE)
  }

 if(set_group == "mature females"){
   # pro_covar1b <- tob0 |>
   # pro_covar1b <- tob1 |>
     pro_covar1b <- sst0 |>
     # pro_covar1b <- o20 |>
     # pro_covar1b <- so20 |> #*
     mutate(trend_number = "Trend 1",
            Variable = type
     ) |>
     select(-year, -type)|>
     as.data.frame(stringsAsFactors = FALSE)
 }

correlation <- trend_cor(r, pro_covar1b$value,
                         time_window = seq_len(max(pro_covar1b$time)),
                         trend = 1, trend_samples = 1000)
hist(correlation)
cor1b <- as.data.frame(correlation)

covars <- pro_covar1
covarsb <- pro_covar1b

if(trend_count>1){

  # pro_covar2 <- pt0 |>
  pro_covar2 <- pp0 |> #~ males and imm
  # pro_covar2 <- pink1 |>
  # pro_covar2 <- sst0 |> # females
  # pro_covar2 <- tob0 |> #~
  # pro_covar2 <- o20 |>
  # pro_covar2 <- so20 |>
  mutate(trend_number = "Trend 2",
         # value = (value - mean(value))/ sd(value),
         Variable = type
  ) |>
  select(-year, -type)|>
  as.data.frame(stringsAsFactors = FALSE)

  if(set_group == "mature females"){
      # pro_covar2 <- pt0 |>
      pro_covar2 <- pdo0 |> #~ males and imm
      # # pro_covar2 <- pp0 |> #~ males and imm
      # pro_covar2 <- sst0 |> # females
      # # pro_covar2 <- tob0 |> #~
      # pro_covar2 <- o20 |>
      # pro_covar2 <- so20 |>
      mutate(trend_number = "Trend 2",
             # value = (value - mean(value))/ sd(value),
             Variable = type
      ) |>
      select(-year, -type)|>
      as.data.frame(stringsAsFactors = FALSE)
  }

  correlation <- trend_cor(r, pro_covar2$value,
                           time_window = seq_len(max(pro_covar2$time)),
                           trend = 2, trend_samples = 1000)
  hist(correlation)
  # (mpp <- mean(correlation))
  cor2 <- as.data.frame(correlation)


  pro_covar2b <- pt0 |> # imm and males
  # pro_covar2b <- pp0 |>
    # pro_covar2b <- pink1 |>
  # pro_covar2b <- sst0 |>
  # pro_covar2b <- tob0 |> # males
  # pro_covar2b <- so20 |> # females
    mutate(trend_number = "Trend 2",
           # # value = (value - mean(value))/ sd(value),
           Variable = type
    ) |>
    select(-year, -type)|>
    as.data.frame(stringsAsFactors = FALSE)

  if(set_group == "mature females"){
    # pro_covar2b <- pt0 |> # imm and males
      # pro_covar2b <- pp0 |>
      # pro_covar2b <- sst0 |>
      # pro_covar2b <- tob0 |> # males
      # pro_covar2b <- so20 |> # females
      pro_covar2b <- oni0 |> # females
      mutate(trend_number = "Trend 2",
             # # value = (value - mean(value))/ sd(value),
             Variable = type
      ) |>
      select(-year, -type)|>
      as.data.frame(stringsAsFactors = FALSE)
  }
  correlation <- trend_cor(r, pro_covar2b$value,
                           time_window = seq_len(max(pro_covar2b$time)),
                           trend = 2, trend_samples = 1000)
  hist(correlation)
  # (mpp <- mean(correlation))
  cor2b <- as.data.frame(correlation)

  covars <- bind_rows(pro_covar1, pro_covar2, pro_covar1b, pro_covar2b)

  covars$Variable <- factor(covars$Variable, levels = c(
    pro_covar1b$Variable[1],
    pro_covar1$Variable[1],
    pro_covar2b$Variable[1],
    pro_covar2$Variable[1]
  ))

}

if(trend_count>2){

  pro_covar3 <- pdo0 |> # imm
  # pro_covar3 <- pt0 |>
  # pro_covar3 <- pp0 |> #~ males and imm
  # pro_covar3 <- sst0 |> # females
    # pro_covar3 <- tob0 |> #~
    # pro_covar3 <- o20 |>
    # pro_covar3 <- so20 |>
    mutate(trend_number = "Trend 3",
           # value = (value - mean(value))/ sd(value),
           Variable = type
    ) |>
    select(-year, -type)|>
    as.data.frame(stringsAsFactors = FALSE)

  correlation <- trend_cor(r, pro_covar3$value,
                           time_window = seq_len(max(pro_covar3$time)),
                           trend = 3, trend_samples = 1000)
  hist(correlation)
  # (mpp <- mean(correlation))
  cor3 <- as.data.frame(correlation)

  pro_covar3b <- oni0 |> # imm
  # pro_covar3b <- pt0 |> # imm and males
  # pro_covar3b <- pp0 |>
  # pro_covar3b <- sst0 |>
  # pro_covar3b <- tob0 |> # males
  # pro_covar3b <- so20 |> # females
    mutate(trend_number = "Trend 3",
           # # value = (value - mean(value))/ sd(value),
           Variable = type
    ) |>
    select(-year, -type)|>
    as.data.frame(stringsAsFactors = FALSE)

  correlation <- trend_cor(r, pro_covar3b$value,
                           time_window = seq_len(max(pro_covar3b$time)),
                           trend = 3, trend_samples = 1000)
  hist(correlation)
  # (mpp <- mean(correlation))
  cor3b <- as.data.frame(correlation)

  covars <- bind_rows(pro_covar1, pro_covar2, pro_covar1b,
                      pro_covar2b, pro_covar3 , pro_covar3b
                      )

  covars$Variable <- factor(covars$Variable, levels = c(
    pro_covar3b$Variable[1],
    pro_covar3$Variable[1],
    pro_covar2b$Variable[1],
    pro_covar2$Variable[1],
    pro_covar1b$Variable[1],
    pro_covar1$Variable[1]
  ))

}


if(which_flip == 1L) {
  cor1$correlation <- -cor1$correlation
  cor1b$correlation <- -cor1b$correlation
}

cor1$var <- pro_covar1$Variable[1]
cor1b$var <- pro_covar1b$Variable[1]

cor1$Label <- cor1b$Label <- paste0("Trend 1 ~ ", pro_covar1$Variable[1], " / ", pro_covar1b$Variable[1])
correlations <- bind_rows(cor1, cor1b)

# correlations$var <- factor(correlations$var, levels = c(
#   pro_covar1b$Variable[1],
#   pro_covar1$Variable[1]
#   ))

if(trend_count>1){
  if(which_flip == 2L|which_flip2 == 2L) {
    cor2$correlation <- -cor2$correlation
    cor2b$correlation <- -cor2b$correlation
  }

  cor2$var <- pro_covar2$Variable[1]
  cor2b$var <- pro_covar2b$Variable[1]
  cor2$Label <- cor2b$Label <- paste0("Trend 2 ~ ", pro_covar2$Variable[1], " / ", pro_covar2b$Variable[1])
  correlations <- bind_rows(cor1, cor2, cor1b, cor2b)

  correlations$var <- factor(correlations$var, levels = c(
    pro_covar1b$Variable[1],
    pro_covar1$Variable[1],
    pro_covar2b$Variable[1],
    pro_covar2$Variable[1]
  ))
}

if(trend_count>2){
  if(which_flip == 3L) {
    cor3$correlation <- -cor2$correlation
    cor3b$correlation <- -cor2b$correlation
  }

  cor3$var <- pro_covar3$Variable[1]
  cor3b$var <- pro_covar3b$Variable[1]
  cor3$Label <- cor3b$Label <- paste0("Trend 3 ~ ", pro_covar3$Variable[1], " / ", pro_covar3b$Variable[1])
  correlations <- bind_rows(cor1, cor2, cor1b, cor2b, cor3, cor3b)

  correlations$var <- factor(correlations$var, levels = c(
    pro_covar3b$Variable[1],
    pro_covar3$Variable[1],
    pro_covar2b$Variable[1],
    pro_covar2$Variable[1],
    pro_covar1b$Variable[1],
    pro_covar1$Variable[1]
  ))
}



# DFA summary plots ----

(p1 <- plot_trends(rflip) +
  scale_x_continuous(label = label_yrs ) +
    geom_line(data = covars,
              aes(x = .data[["time"]], y = .data[["value"]], colour =.data[["Variable"]]),
            alpha = 0.8,
            linewidth = 1.5) +
    # scale_colour_brewer(palette = "Paired") +
    labs(colour = "", ylab = "Standardized value" ) +
    # theme(legend.position = c(0.2,0.85), axis.title.y = element_text())+
    # theme(legend.position = "none") +
    theme(legend.position='top', legend.justification='left', legend.direction='horizontal',
          plot.margin = unit(c(0,0,0,0), "cm"),
          legend.text=element_text(size=rel(0.8))) +
    # ggtitle(paste0("DFA of ", set_group, " with no covariates"))
    ggtitle(paste0("DFA of ", y_label, ""))
  )

if(trend_count>1){
  if(trend_count>2){
  (p1 <- p1 + scale_colour_manual(values = RColorBrewer::brewer.pal(n = 6, name = "Paired")[c(1,2,3,4,5,6)]))
  } else{
    if(pro_covar1b$Variable[1]=="SST"){
      (p1 <- p1 + scale_colour_manual(values = RColorBrewer::brewer.pal(n = 6, name = "Paired")[c(5,6,1,2)]) )
    }else{
      if(pro_covar1$Variable[1]=="TOB (previous year)"|pro_covar1$Variable[1]=="TOB"){

        (p1 <- p1 + scale_colour_manual(values = RColorBrewer::brewer.pal(n = 6, name = "Paired")[c(5,6,3,4)]) )
      } else {
      (p1 <- p1 + scale_colour_manual(values = RColorBrewer::brewer.pal(n = 4, name = "Paired")) )
      }
    }
  }
} else {
  p1 <- p1 + scale_colour_manual(values = RColorBrewer::brewer.pal(n = 4, name = "Paired")[c(2,4)])
}



(p2 <- ggplot(correlations) +
  geom_histogram(aes(correlation, fill = var), alpha = 0.7, position="identity") +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "darkgrey") +
  xlab("Post-hoc correlation coefficients") + ylab("Count") +
  facet_wrap(~Label#, scales = "free"
             ) +
  theme(
    plot.margin = unit(c(0.2,0,0,0), "cm"),
    axis.title.y = element_blank(),
         axis.ticks.y = element_blank(), axis.text.y = element_blank(),
         legend.position = "none"))

if(trend_count>2){
  (p2 <- p2 + scale_fill_manual(values = RColorBrewer::brewer.pal(n = 6, name = "Paired")[c(1,2,3,4,5,6)]) )
} else {
if(trend_count>1){
  if(pro_covar1b$Variable[1]=="SST"){
      (p2 <- p2 + scale_fill_manual(values = RColorBrewer::brewer.pal(n = 6, name = "Paired")[c(5,6,1,2)]) )
  }else{
    if(pro_covar1$Variable[1]=="TOB (previous year)"|pro_covar1$Variable[1]=="TOB"){
    (p2 <- p2 + scale_fill_manual(values = RColorBrewer::brewer.pal(n = 6, name = "Paired")[c(5,6,3,4)]) )
    } else {
    (p2 <- p2 + scale_fill_manual(values = RColorBrewer::brewer.pal(n = 4, name = "Paired")) )
    }
  }
} else {
  p2 <- p2 + scale_fill_manual(values = RColorBrewer::brewer.pal(n = 4, name = "Paired")[c(2,4)])
}
}

(p3 <- plot_loadings(rflip, names = spp)+
    guides(alpha=guide_legend(override.aes = list(fill = "grey20")),
           fill = "none") +
    labs(alpha = "Prob not 0") +
    theme(legend.position = "none",
          axis.title.y = element_blank()) )

if(trend_count>2){
  (p3 <- p3 + scale_fill_manual(values = RColorBrewer::brewer.pal(n = 6, name = "Paired")[c(5,4,2)]))
} else {
  if(pro_covar1b$Variable[1]=="SST"){#5,6,1,2
  (p3 <- p3 + scale_fill_manual(values = RColorBrewer::brewer.pal(n = 6, name = "Paired")[c(6,2)]))
  }else{
    if(pro_covar1$Variable[1]=="TOB (previous year)"|pro_covar1$Variable[1]=="TOB"){
      (p3 <- p3 + scale_fill_manual(values = RColorBrewer::brewer.pal(n = 6, name = "Paired")[c(6,4)]) )
    } else {
    (p3 <- p3 + scale_fill_manual(values = RColorBrewer::brewer.pal(n = 6, name = "Paired")[c(2,4)]))
    }
  }
}


# wide version for talks
if(trend_count>2){
  wrap_elements(p1 / p2 + plot_layout(nrow = 2, widths= c(1), heights = c(1,0.5))) + wrap_elements(p3) + plot_layout(nrow = 1, widths= c(1.5, 1))

}else{
wrap_elements(p1 / p2 + plot_layout(nrow = 2, widths= c(1), heights = c(1,0.5))) + wrap_elements(p3) + plot_layout(nrow = 1, widths= c(1.75, 1))
# & theme(axis.title.y = element_blank())
}

if(trend_count>2){
    ggsave(paste0("figs/DFA-", trend_count, "trends-no-cov-",
                  no_covs, "-",
                  gsub(" ", "-", set_group), "-", model_name, "-",
                  gsub(" ", "-", pro_covar1$Variable[1]), "-",
                  gsub(" ", "-", pro_covar2$Variable[1]),
                  gsub(" ", "-", pro_covar3$Variable[1]), "-filtered.png"),
           height = fig_height/1.65, width = fig_width)
}else{
  if(trend_count>1){
    ggsave(paste0("figs/DFA-", trend_count, "trends-no-cov-",
                  no_covs, "-",
              gsub(" ", "-", set_group), "-", model_name, "-",
              gsub(" ", "-", pro_covar1$Variable[1]), "-",
              gsub(" ", "-", pro_covar2$Variable[1]), "-filtered.png"),
       height = fig_height/1.65, width = fig_width*1)

 } else {
ggsave(paste0("figs/DFA-", trend_count, "trends-no-cov-",
              no_covs, "-",
              gsub(" ", "-", set_group), "-", model_name, "-",
              gsub(" ", "-", pro_covar1$Variable[1]), "-filtered.png"),
       height = fig_height/1.65, width = fig_width*0.9)
}
}


# tall version for manuscript
wrap_elements(p1 / p2 + plot_layout(nrow = 2, widths= c(1), heights = c(1,0.7))& theme(
  # plot.margin = unit(c(0.2,0,0,0), "cm"),
  plot.title = element_blank())) /
  wrap_elements(p3+
                  theme(
    legend.position='right', legend.justification='left', #legend.direction='horizontal',
    plot.margin = unit(c(0.2,1,0,0.5), "cm"),
    plot.title = element_blank(),
    strip.background = element_blank(),
    strip.text.x = element_blank()
    )
  ) + plot_layout(nrow = 2, widths= c(1), heights = c(1,1.1))


if(trend_count>2){
  ggsave(paste0("figs/man/DFA-", trend_count, "trends-no-cov-",
                gsub(" ", "-", set_group), "-", model_name, "-",
                gsub(" ", "-", pro_covar1$Variable[1]), "-",
                gsub(" ", "-", pro_covar2$Variable[1]),
                gsub(" ", "-", pro_covar3$Variable[1]), ".png"),
         height = fig_height*1.1, width = fig_width/1.5)
}else{
  if(trend_count>1){
    ggsave(paste0("figs/man/DFA-", trend_count, "trends-no-cov-",
                  gsub(" ", "-", set_group), "-", model_name, "-",
                  gsub(" ", "-", pro_covar1$Variable[1]), "-",
                  gsub(" ", "-", pro_covar2$Variable[1]), "-4k.png"),
           height = fig_height*1.1, width = fig_width/1.5)

  } else {
    ggsave(paste0("figs/man/DFA-", trend_count, "trends-no-cov-",
                  gsub(" ", "-", set_group), "-", model_name, "-",
                  gsub(" ", "-", pro_covar1$Variable[1]), ".png"),
           height = fig_height*1.1, width = fig_width/1.5)
  }
}







# # # spaghetti
# (p5 <- ggplot(df) +
#   geom_line(aes_string(x = "time", y = "y", group = "ID"),
#             color = "grey50", linewidth = 0.5) +
#   geom_line(aes_string(x = "time", y = "estimate",
#                        group = "ID", color = "ID"), linewidth = 1.2) +
#   scale_color_manual(values = cols) +
#   xlab("Time") # + theme(legend.position = "none")
#   )
#



# DFA with covariate ----
# with process covariate when fitting:
# https://cran.r-project.org/web/packages/bayesdfa/vignettes/a3_covariates.html

# if we were to control for maturity-specific biomass as a covariate unique to each species

#
#
#
# ctrl <- list(adapt_delta = 0.98, max_treedepth = 12)
#
# mcov <- fit_dfa(
#   y = dd,
#   # iter = 500,
#   # chains = 1,
#   # num_trends = 2,
#   iter = set_iter,
#   chains = set_chains,
#   num_trends = trend_count,
#   inv_var_weights = "weights_scaled",
#   estimate_process_sigma = FALSE,
#   estimate_nu = FALSE,
#   scale = "zscore",
#   obs_covar = obs_cov,
#   data_shape = "long",
#   seed = 298,
#   control = ctrl
# )
#
#
# range(mcov$monitor$Bulk_ESS)
# range(mcov$monitor$Rhat)
# # names(mcov$samples_permuted)
# rc <- rotate_trends(mcov)
# #
# # hist(mcov$samples_permuted$b_pro[,1,1])
# #
# plot_trends(rc, years = unique(dd$year)) #+
#   #geom_line(data = pdo0, aes_string(x = "year", y = "value"), colour = "red")
#
# #
# plot_loadings(rc, names = unique(dd$ts)) #+
  # ggtitle(paste0("DFA for ", set_group, " with ", lag_label, agg_var, var_type))
#
# # plot_fitted(m2, names = spp)
# df2 <- dfa_fitted(mcov, conf_level = 0.95, names = unique(dd$ts))
# cols <- viridis::viridis(length(unique((df$ID))), end = 0.8)
#
# # # # spgehetti
# # (p1 <- ggplot(df) +
# #     geom_line(aes_string(x = "time", y = "y", group = "ID"),
# #               color = "grey50", linewidth = 0.5) +
# #     geom_line(aes_string(x = "time", y = "estimate",
# #                          group = "ID", color = "ID"), linewidth = 1.2) +
# #     scale_color_manual(values = cols) +
# #     xlab("Time") # + theme(legend.position = "none")
# # )
#
# (p2 <- ggplot(df2) +
#     geom_ribbon(aes_string(x = "time",
#                            ymin = "lower", ymax = "upper", fill = "ID"),
#                 alpha = 0.4) +
#     geom_line(aes_string(x = "time", y = "estimate", colour = "ID")) +
#     geom_line(data = pro_covar, aes_string(x = "time", y = "value"), colour = "red") +
#     geom_point(aes_string(x = "time", y = "y"),
#                #col = "blue",
#                size = 0.5, alpha = 0.7) +
#     scale_color_manual(values = cols) +
#     scale_fill_manual(values = cols) +
#     facet_wrap("ID" #, scales = "free_y"
#                ) +
#     xlab("Time") + ylab(y_label) +
#     theme(legend.position = "none") +
#     # labs(colour= "Species", fill = "Species")+
#     ggtitle(paste0("DFA with ",  trend_label, " and ", lag_label, agg_var, var_label))
# )
#
# ggsave(paste0("figs/DFA-",  trend_count, "trends-lag", set_lag, "-", agg_var, var_type, ".png"),
#        height = fig_height, width = fig_width)
#
#


# # plot_fitted(m2, names = spp)
# df <- dfa_fitted(m2, conf_level = 0.95, names = spp)
# cols <- viridis::viridis(length(unique((df$ID))), end = 0.8)
#
# (p <- ggplot(df) +
#     geom_ribbon(aes_string(x = "time",
#                            ymin = "lower", ymax = "upper"),
#                 alpha = 0.4) +
#   geom_line(aes_string(x = "time", y = "estimate")) +
#   geom_point(aes_string(x = "time", y = "y"), col = "red",
#              size = 0.5, alpha = 0.4) +
#   facet_wrap("ID", scales = "free_y") +
#   xlab("Time") + ylab("")
#   )

## model selection not working
# m3 <- find_dfa_trends(
#   y = dd,
#   iter = 1000,
#   chains = 6,
#   weights = "weights_scaled",
#   estimate_process_sigma = TRUE,
#   scale = "zscore",
#   data_shape = "long",
#   seed = 298191,
#   kmin = 1, kmax = 3,
#   # compare_normal = TRUE,
#   # variance = c("equal", "unequal"),
#   control = ctrl
# )
# m3


# pro_covar <- data.frame(
#   time = seq_along(yrs),
#   trend = 1,
#   covariate = 1,
#   value = rnorm(length(yrs)), # make sure this is standardized!
#   stringsAsFactors = FALSE
# )
# pro_cov = expand.grid("trend"=1:2, "time"=yr_lu$time, "covariate"=1, ts = spp)
# pro_cov <- left_join(pro_cov, dd0, relationship = "many-to-many")
