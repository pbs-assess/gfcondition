
# devtools::install_github("fate-ewi/bayesdfa")
library(tidyverse)
library(bayesdfa)
library(patchwork)

devtools::load_all()
# load overall species list
source("analysis/00-species-list.R")
source("analysis/x-load-dfa-covariates.R")

theme_set(ggsidekick::theme_sleek())
options(mc.cores = parallel::detectCores())
fig_height <- 4 * 2
fig_width <- 5 * 2

dir.create(paste0("figs/DFA/"), showWarnings = FALSE)
dir.create(paste0("figs/man/"), showWarnings = FALSE)
dir.create(paste0("data-generated/DFA/"), showWarnings = FALSE)

set_group <- "immatures"
# set_group <- "mature males"
# set_group <- "mature females"

# dfa_by_class <- function(set_group){ ### not sure why but this didn't work
## Choose model ----
## DFA settings

# ## these work with /mean(weights)
# ctrl <- list(adapt_delta = 0.99, max_treedepth = 20) # default settings
# set_iter <- 5000 # min 2000
# set_chains <- 1 # 1 works


# ctrl <- list(adapt_delta = 0.95, max_treedepth = 12) # this works for imm not accounting for density
# ctrl <- list(adapt_delta = 0.99, max_treedepth = 20) # default settings
ctrl <- list(adapt_delta = 0.95, max_treedepth = 20)
set_iter <- 10000 # min 2000
set_chains <- 5 # 1 works
estimate_sigma <- FALSE
# equal_sigma <- TRUE
# use_expansion_prior <- FALSE
# use_expansion_prior <- TRUE

# ## choose response variable
## are we doing a DFA of biomass density rather than condition
# density <- TRUE
density <- FALSE

### if condition
# adjusted_for_density <- FALSE
# just_trend_version <- TRUE
just_trend_version <- FALSE
adjusted_for_density <- TRUE

# trend_count <- 1
trend_count <- 2
# trend_count <- 3

no_covs <- TRUE
# no_covs <- FALSE

if(!no_covs) {
obs_covs <- TRUE
proc_covs <- FALSE
}


if(no_covs) {
  if(adjusted_for_density){
  # model_name <- "apr-2024-not-total-density"
  # model_name <- "apr-2024-cell-means"
    # model_name <- "2024-09-doy-ld0c-w-pcod-before-10pyr"
    model_name <- "2024-09-doy-ld0c"

  if(set_group == "immatures"){
    y_label <- "Immature condition indices (adjusting for density)"
    # use_expansion_prior <- TRUE
    # ctrl <- list(adapt_delta = 0.99, max_treedepth = 20)
    which_flip <- 0L
    which_flip2 <- 2L
  }
  if(set_group == "mature males") {
    y_label <- "Mature male condition indices (adjusting for density)"
    which_flip <- 0L
    which_flip2 <- 2L
  }
  if(set_group ==  "mature females") {
    y_label <- "Mature female condition indices (adjusting for density)"
    which_flip <- 0L
    which_flip2 <- 2L
  }
  } else {
  # # model_name <- "apr-2024"
  # model_name <- "2024-09-w-pcod-before-10pyr"
    model_name <- "2024-09"

  if(set_group == "immatures") {
    y_label <- "Immature condition indices (not controlling for density)"
    which_flip <- 1L
    which_flip2 <- 2L
  }
  if(set_group == "mature males") {
    y_label <- "Mature male condition indices (not controlling for density)"
    which_flip <- 0L
    which_flip2 <- 0L
  }
  if(set_group ==  "mature females") {
    y_label <- "Mature female condition indices (not controlling for density)"
    which_flip <- 0L
    which_flip2 <- 0L
  }
  }
  } else{
    # model_name <- "apr-2024"
    # model_name <- "2024-09-w-pcod-before-10pyr"
    model_name <- "2024-09"

    adjusted_for_density <- FALSE
    ctrl <- list(adapt_delta = 0.90, max_treedepth = 12)
  if(set_group == "immatures") {
    # not converging at the moment
    y_label <- "Immature condition indices (biomass as observation covariate)"
    which_flip <- 0L
    which_flip2 <- 0L
  }
  if(set_group == "mature males") {
    y_label <- "Mature male condition indices (biomass as observation covariate)"
    which_flip <- 0L
    which_flip2 <- 2L
  }
  if(set_group ==  "mature females") {
    y_label <- "Mature female condition indices (biomass as observation covariate)"
    which_flip <- 0L
    which_flip2 <- 0L
  }
  }


if (trend_count == 4) trend_label <- "4 trends"
if (trend_count == 3) trend_label <- "3 trends"
if (trend_count == 2) trend_label <- "2 trends"
if (trend_count == 1) trend_label <- "1 trend"

## Load and structure data ----


# or if density?
if(density){

  set_group <- "total"
  # model_name <- "dln-dln-all-mar-2024"
  # model_name <- "dln-all-2024-09"
  model_name <- "dln-ss5-2024-09"
  y_label <- "Total density indices"

  f <- list.files(
    paste0("data-generated/density-index/", model_name, "/total"),
    pattern = ".rds",
    full.names = TRUE)

  d <- purrr::map_dfr(f, readRDS)
  d <- filter(d, !(species %in% species_to_remove))
  dg <- d

} else{
  f <- list.files(paste0("data-generated/cond-index/",
                         model_name), pattern = ".rds",
                  full.names = TRUE)

  d <- purrr::map_dfr(f, readRDS)
  d <- filter(d, !(species %in% species_to_remove))
  dg <- filter(d, group == set_group)
}


dg <- dg %>% mutate(
  taxa_group = case_when(species %in% Flatfish~"Flatfish",
                         species %in% Rockfish~"Rockfish",
                         TRUE~"Other"
  ),
  species = ifelse(species == "Rougheye/Blackspotted Rockfish Complex",
                   "Rougheye/Blackspotted", species
  ),
  species = ifelse(species == "North Pacific Spiny Dogfish",
                   "Pacific Spiny Dogfish", species
  )
)

### input spaghetti ----

cols <- viridis::viridis(length(unique((d$species))), end = 0.8)
(ps <- d |> group_by(species, group) |>
    mutate(obs_scaled = (log_est-mean(log_est))/ sd(log_est),
           group = factor(group, levels = c("immatures", "mature males", "mature females"),
                          labels = c("Immatures", "Mature males", "Mature females"))
           ) |>
    ggplot() +
    geom_line(aes(x = year, y = obs_scaled,
                         color =species, group = species), linewidth = 0.5) +
    scale_color_manual(values = cols) +
    ylab(paste0(ifelse(
      density, "Biomass index",
      paste0("Scaled condition index ",
             ifelse(adjusted_for_density,
                    "(adjusting for density)","(not controlling for density)"))))
      ) +
    xlab("") +
    # xlab("Time") +
    theme(legend.position = "none",
          plot.margin = unit(c(0.15, 0.15, 0.15, 0.15), "inches"))
 )

if(!density){
  ps + facet_wrap(~group, ncol = 1)
}

ggsave(paste0("figs/man/DFA-spaghetti-", model_name, ".png"),
       height = fig_height*0.5, width = fig_width*0.5)

### wide dataframe structure
# dw <- tidyr::pivot_wider(dg, id_cols = c(year), names_from = species, values_from = est) |>
#   select(-year) |> t()
# dw

### long dataframe structure  ----
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

saveRDS(dd, paste0(set_group, "-", model_name, "-condition-indices.rds"))



## Weights ----

## could put all condition indices on the same scale but not centred...
## not sure this makes sense?

# but will scale the se to obs variability for each species
dd <- dd %>% group_by(ts) %>%
  mutate(mean_abs_obs = mean(abs(obs)),
         obs_range = max(obs)-min(obs),
         sd_obs = sd(obs),
         se_scaled = se/sd(obs)
  ) |> ungroup()


dd$weights1 <- (1 / dd$se)^2 # this would be the variance
# dd$weights <- (1 / dd$se_scaled)^2 # this would be the variance
# dd$weights_scaled <- (dd$weights / mean(dd$weights))
# dd$weights_scaled <- (dd$weights / sum(dd$weights))*100
dd$weights_scaled1 <- (dd$weights1 /sum(dd$weights1))*100
# dd$weights_scaled2 <- log(dd$weights +1)
# ggplot(dd) + geom_point(aes(se, (weights_scaled1), colour = sd_obs))
# plot(weights_scaled1~weights_scaled, data = dd)


# DFA: has model already been run ----

# bayesdfa_config <- "lhw"
# bayesdfa_config <- "lhw1-ar1"
bayesdfa_config <- "w1-ar1" # works for imm
# bayesdfa_config <- "w1"
# bayesdfa_config <- "log(scaledSE)"
# bayesdfa_config <- "none" # doesn't converge well at all

if(density) {
  file_name <- paste0("data-generated/DFA/",
                      "total-biomass-",
                      model_name,
                      "-", set_chains, "chains",
                      # "-", use_expansion_prior,
                      # "-", set_iter,
                      "-", trend_count,
                      "-", bayesdfa_config,
                      ".rds")
} else {
  if(no_covs){
    file_name <- paste0("data-generated/DFA/",
                        gsub(" ", "-", set_group),
                        "-", model_name,
                        "-", set_chains, "chains",
                        "-", trend_count,
                        "-", bayesdfa_config,
                        ".rds")
  } else {
  file_name <- paste0("data-generated/DFA/Obscov-",
                    gsub(" ", "-", set_group),
                    "-", model_name,
                    "-", set_chains, "chains",
                    # "-", use_expansion_prior,
                    # "-", set_iter,
                    "-", trend_count,
                    "-", bayesdfa_config,
                    ".rds")
  }
}
rm(m)
try(m <- readRDS(file_name))

if (!exists("m")) {
if(no_covs){
# DFA without a covariate ----
m <- fit_dfa(
  y = dd,
  iter = set_iter,
  chains = set_chains,
  num_trends = trend_count,
  inv_var_weights = "weights_scaled1",
  # likelihood_weights = "weights_scaled2",
  estimate_trend_ar = TRUE,
  # estimate_process_sigma = estimate_sigma, # helps for imm
  # equal_process_sigma = equal_sigma,
  estimate_nu = FALSE,
  scale = "zscore",
  data_shape = "long",
  # expansion_prior = use_expansion_prior,
  # seed = 298191,
  seed = 298,
  control = ctrl
)
# m
} else{

  if(obs_covs) {
    # DFA with observation covariates ----
  if(set_group == "immature") {

    spp_list <- dg %>% select(species, dens_model_name) %>% distinct()

    f <- list(
      paste0("data-generated/density-index/", spp_list$dens_model_name, "/imm/i-",
             gsub(" ", "-", gsub("\\/", "-", tolower(species))), "-",
             spp_list$dens_model_name, "-20-km.rds")
    )
  } else {
    # if we were to control for overall biomass as a covariate unique to each species
    f <- list.files(
      paste0("data-generated/density-index/dln-ss5-2024-09/total"),
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
    ),
    species = ifelse(species == "North Pacific Spiny Dogfish",
                     "Pacific Spiny Dogfish", species
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

m <- fit_dfa(
  y = dd,
  # iter = 500,
  # chains = 1,
  # num_trends = 2,
  iter = set_iter,
  chains = 1,
  num_trends = trend_count,
  estimate_trend_ar = TRUE,
  inv_var_weights = "weights_scaled1",
  # likelihood_weights = "weights_scaled",
  # estimate_process_sigma = FALSE,
  estimate_nu = FALSE,
  scale = "zscore",
  obs_covar = obs_cov,
  data_shape = "long",
  control = ctrl,
  # control = list(adapt_delta = 0.995, max_treedepth = 20),
  seed = 298
)
  }

  if(proc_covs){
    # DFA with process covariates ----
    ### TODO: not added yet
  }
}
}

# Model diagnostics ----
# mi <- invert_chains(m$model)
# is_converged(mi, threshold = 1.1)
is_converged(m, threshold = 1.1)

# library(bayesplot)
# bayesplot::mcmc_trace(m$samples, regex_pars = c("sigma","lp"))
# bayesplot::mcmc_trace(m$samples, regex_pars = c("Z"))
# # bayesplot::mcmc_trace(m$samples, regex_pars = c("Z"), size = 0.5,
# #                       # facet_args = list(nrow = 2),
# #                       np = nuts_params(m$model),
# #                       np_style = trace_style_np(div_color = "black", div_size = 0.5))
#
# range(m$monitor$Bulk_ESS)
# range(m$monitor$Rhat)
# row.names(m$monitor)

checks <- m$monitor |>
  filter(Rhat >1.1|Bulk_ESS<300|Tail_ESS<300) |>
  as_tibble(rownames = "var")
checks |> View()
# xstar are random walk trends 1 year forecasting in the future so can be ignored

r <- rotate_trends(m)
plot_loadings(r) + scale_y_continuous(limits = c(-2,2))

# only save if converged
if(nrow(checks)<22) {
  if(no_covs){
    saveRDS(m, file_name)
  }
}

# Posthoc covariate tests -----
if(nrow(checks)<22) {
r <- rotate_trends(m)

# plot_trends(r) + scale_x_continuous(label = label_yrs )
# plot_loadings(r, names = spp) +
#   ggtitle(paste0("DFA for ", set_group, " with no covariates"))

if(which_flip == 0L) {
  rflip <- r
} else {
  rflip <- flip_trend(r, which_flip)
}
rflip <- flip_trend(rflip, which_flip2)

plot_trends(r, years = yrs)
plot_trends(rflip, years = yrs)
plot_loadings(rflip, names = spp)

# Biomass trend plots ----
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
    facet_wrap(~trend_number, nrow = 1) +
    ggsidekick::theme_sleek() +
    scale_fill_manual(values = RColorBrewer::brewer.pal(n = 6, name = "Paired")[c(6,2)]) +
   coord_cartesian(expand = FALSE) +
   # ggtitle(paste0("DFA of biomass indices"))+
   # ggtitle(paste0("DFA of ", set_group, " with no covariates"))+
   # ylab("Standardized biomass index") +
    theme(legend.position='top', legend.justification='left', legend.direction='horizontal',
          plot.margin = unit(c(0.2,0.5,0.2,0.2), "cm"),
          legend.text=element_text(size=rel(0.8)),
          axis.title=element_blank()
          )
)

(p2 <- plot_loadings(rflip, names = spp,
                     conf_level = 0.90,
                     ordered_by = 2,
                     flip_order = TRUE)+
    # scale_fill_manual(values = RColorBrewer::brewer.pal(n = 6, name = "Paired")[c(6,2)]) +
    scale_fill_manual(values = c("grey20","grey20")) +
    guides(alpha=guide_legend(override.aes = list(fill = "grey20")),
           fill = "none") +
    labs(alpha = "Prob not 0") +
    theme(#legend.position = "none",

          plot.margin = unit(c(0.2,0.5,0.2,0.2), "cm"),
          strip.text.x = element_blank(),
          axis.title.y = element_blank()) )

# wrap_elements(p1) + p2 + plot_layout(ncol = 2)
  # ggsave("figs/DFA-tends-total.png", width = 8, height = 5)

p1 + tagger::tag_facets(tag = "panel",
                        tag_prefix = "", tag_suffix = "", #position = "tl",
                        tag_pool = c("(a)", "(b)")) +
p2 + tagger::tag_facets(tag = "panel",
                           tag_prefix = "", tag_suffix = "", #position = "tl",
                           tag_pool = c("(c)", "(d)")) +
plot_layout(ncol = 1, heights = c(0.35, 1))&theme(tagger.panel.tag.text = element_text(color = "grey30", size = 10))

ggsave("figs/man/DFA-tends-total.png", width = 8, height = 7)



f <- find_dfa_trends(
  y = dd,
  iter = set_iter,
  chains = set_chains,
  inv_var_weights = "weights_scaled1",
  estimate_trend_ar = TRUE,
  scale = "zscore",
  data_shape = "long",
  seed = 298,
  control = ctrl,
  kmin = 1, kmax = 5,
  compare_normal = TRUE,
  variance = c("equal", "unequal")
)




} else {

# DFA fit plots ----
source("R/dfa_fitted.R")
df1 <- dfa_fitted(m, conf_level = 0.95, names = spp)

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

# Explore covariates ----

# TODO: need a more systematic way to decide which covars to use


# Correlations by trend ----
    pro_covar1 <- sst0
  # # pro_covar1 <- pt0
  # # pro_covar1 <- pp0  # mat?
  # # pro_covar1 <- pdo1
  # # pro_covar1 <- pdo0   #* imm & mat
  # # pro_covar1 <- pink1
  # # pro_covar1 <- npi0   #~
  # # pro_covar1 <- oni0
  # # pro_covar1 <- oni1
  # # pro_covar1 <- soi0
  # pro_covar1 <- sst0   #* imm 3
  # # pro_covar1 <- tob0
  # # pro_covar1 <- o20
  # # pro_covar1 <- so20   #*


if(set_group == "total"){
  pro_covar1 <- tob0
  # pro_covar1 <- tob1
  # pro_covar1 <- sst0
  # pro_covar1 <- sst1
  # pro_covar1 <- o20
  # pro_covar1 <- so20
}

# if(set_group == "mature females"){
#   # pro_covar1 <- tob0
#   # pro_covar1 <- pdo0
#   pro_covar1 <- sst0
#   # pro_covar1 <- o20
#   # pro_covar1 <- pt0
#   # pro_covar1 <- pp0   #
#   # pro_covar1 <- so20   #*
#   # pro_covar1 <- npi0   #~
#   # pro_covar1 <- oni0
#   # pro_covar1 <- soi0
# }

pro_covar1 <- format_process_cov(pro_covar1)

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


  pro_covar1b <- pdo0 # mat
  # pro_covar1b <- oni0   # mat
  # pro_covar1b <- so20   #*imm
  # pro_covar1b <- npi0
  # pro_covar1b <- soi0
  # pro_covar1b <- sst0
  # pro_covar1b <- so20
  # pro_covar1b <- pp0
  # pro_covar1b <- tob0
  # pro_covar1b <- o20

  if(set_group == "total"){
    # pro_covar1b <- pdo0   # mat
      # pro_covar1b <- pdo1   # mat
      # pro_covar1b <- tob0
    pro_covar1b <- o20
  }

 if(set_group == "mature females"){
   # pro_covar1b <- tob0
   pro_covar1b <- pp1
     # pro_covar1b <- sst0
     # pro_covar1b <- o20
     # pro_covar1b <- so20   #*
   # pro_covar1b <- pdo0   #*
   # pro_covar1b <- npi0   #~
   # pro_covar1b <- oni0
   # pro_covar1b <- soi0
 }

pro_covar1b <- format_process_cov(pro_covar1b)
correlation <- trend_cor(r, pro_covar1b$value,
                         time_window = seq_len(max(pro_covar1b$time)),
                         trend = 1, trend_samples = 1000)
hist(correlation)
cor1b <- as.data.frame(correlation)

covars <- pro_covar1
covarsb <- pro_covar1b

if(trend_count>1){

  # pro_covar2 <- pt0
  # pro_covar2 <- pp0   #~ males and imm
  # pro_covar2 <- pink1
  # pro_covar2 <- sst0   # females
  # pro_covar2 <- tob0   #~
  # pro_covar2 <- o20
  pro_covar2 <- npgo0
  # pro_covar2 <- so20

  if(set_group == "mature females"){
      # pro_covar2 <- pt0
      pro_covar2 <- pdo0   #~ males and imm
      # # pro_covar2 <- pp0   #~ males and imm
      # pro_covar2 <- sst0   # females
      # # pro_covar2 <- tob0   #~
      # pro_covar2 <- o20
      # pro_covar2 <- so20
  }

  pro_covar2 <- format_process_cov(pro_covar2, trend_title = "Trend 2")
  correlation <- trend_cor(r, pro_covar2$value,
                           time_window = seq_len(max(pro_covar2$time)),
                           trend = 2, trend_samples = 1000)
  hist(correlation)
  cor2 <- as.data.frame(correlation)


  # pro_covar2b <- pt0   # imm and males
  pro_covar2b <- pp0
    # pro_covar2b <- pink1
  # pro_covar2b <- sst0
  # pro_covar2b <- tob0   # males
  # pro_covar2b <- so20

  if(set_group == "mature females"){
    # pro_covar2b <- pdo1
      # pro_covar2b <- pp0
        # pro_covar2b <- o2p0
        pro_covar2b <- npgo0
      # pro_covar2b <- sst1
      # pro_covar2b <- tob1   # males
      # pro_covar2b <- so20   # females
      # pro_covar2b <- oni0
  }

  pro_covar2b <- format_process_cov(pro_covar2b, trend_title = "Trend 2")
  correlation <- trend_cor(r, pro_covar2b$value,
                           time_window = seq_len(max(pro_covar2b$time)),
                           trend = 2, trend_samples = 1000
                           )
  hist(correlation)
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

  pro_covar3 <- pdo0   # imm
  # pro_covar3 <- pt0
  # pro_covar3 <- pp0   #~ males and imm
  # pro_covar3 <- sst0   # females
  # pro_covar3 <- tob0   #~
  # pro_covar3 <- o20
  # pro_covar3 <- so20

  pro_covar3 <- format_process_cov(pro_covar3, trend_title = "Trend 3")

  correlation <- trend_cor(r, pro_covar3$value,
                           time_window = seq_len(max(pro_covar3$time)),
                           trend = 3, trend_samples = 1000)
  hist(correlation)
  cor3 <- as.data.frame(correlation)

  pro_covar3b <- oni0   # imm
  # pro_covar3b <- pt0   # imm and males
  # pro_covar3b <- pp0
  # pro_covar3b <- sst0
  # pro_covar3b <- tob0   # males
  # pro_covar3b <- so20

  pro_covar3b <- format_process_cov(pro_covar3b, trend_title = "Trend 3")
  correlation <- trend_cor(r, pro_covar3b$value,
                           time_window = seq_len(max(pro_covar3b$time)),
                           trend = 3, trend_samples = 1000)
  hist(correlation)
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

cor1$Label <- cor1b$Label <- paste0("Trend 1 ~ ", pro_covar1b$Variable[1], " or ", pro_covar1$Variable[1])
correlations <- bind_rows(cor1, cor1b)

if(trend_count>1){
  if(which_flip == 2L|which_flip2 == 2L) {
    cor2$correlation <- -cor2$correlation
    cor2b$correlation <- -cor2b$correlation
  }

  cor2$var <- pro_covar2$Variable[1]
  cor2b$var <- pro_covar2b$Variable[1]
  cor2$Label <- cor2b$Label <- paste0("Trend 2 ~ ", pro_covar2b$Variable[1], " or ", pro_covar2$Variable[1])
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

# colour sets
mixed_set <- c(10,6,4,1)
mixed_set2 <- c(3,6,1,10) # used for females mostly
# mixed_set2 <- c(9,6,4,10)

if(just_trend_version) {
  (p1 <- plot_trends(rflip) +
     scale_x_continuous(label = label_yrs ) +
     # scale_colour_brewer(palette = "Paired") +
     labs(colour = "", ylab = "Standardized value" ) +
     # theme(legend.position = c(0.2,0.85), axis.title.y = element_text())+
     # theme(legend.position = "none") +
     theme(legend.position='top', legend.justification='left', legend.direction='horizontal',
           plot.margin = unit(c(0,0,0,0), "cm"),
           axis.title = element_blank(),
           legend.text=element_text(size=rel(0.8))) +
     # ggtitle(paste0("DFA of ", set_group, " with no covariates"))
     ggtitle(paste0("DFA of ", y_label, ""),
             subtitle = paste0("    Species condition index ~ Trend1 x Loading1 + Trend2 x Loading2 + Noise"))
  )
} else {
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
          axis.title = element_blank(),
          legend.text=element_text(size=rel(0.8))) +
    # ggtitle(paste0("DFA of ", set_group, " with no covariates"))
    ggtitle(paste0("DFA of ", y_label, ""),
            subtitle = paste0("    Species condition index ~ Trend1 x Loading1 + Trend2 x Loading2 + Noise"))
  )



if(trend_count>1){
  if(trend_count>2){
  (p1 <- p1 + scale_colour_manual(values = RColorBrewer::brewer.pal(n = 6, name = "Paired")[c(1,2,3,4,5,6)]))
  } else{
    if(set_group == "mature females"){
      (p1 <- p1 + scale_colour_manual(values = RColorBrewer::brewer.pal(n = 10, name = "Paired")[mixed_set2]) )
    }else{
      if(pro_covar1$Variable[1]=="TOB (previous year)"|pro_covar1$Variable[1]=="TOB"){

        (p1 <- p1 + scale_colour_manual(values = RColorBrewer::brewer.pal(n = 6, name = "Paired")[c(5,6,3,4)]) )
      } else {

        # pro_covar1b$Variable[1]=="ENSO (ONI)"
        (p1 <- p1 + scale_colour_manual(values = RColorBrewer::brewer.pal(n = 10, name = "Paired")[mixed_set]) )
      # (p1 <- p1 + scale_colour_manual(values = RColorBrewer::brewer.pal(n = 4, name = "Paired")) )
      }
    }
  }
} else {
  p1 <- p1 + scale_colour_manual(values = RColorBrewer::brewer.pal(n = 4, name = "Paired")[c(2,4)])
}

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
  if(set_group == "mature females"){
      (p2 <- p2 + scale_fill_manual(values = RColorBrewer::brewer.pal(n = 10, name = "Paired")[mixed_set2]) )
  }else{
    if(pro_covar1$Variable[1]=="TOB (previous year)"|pro_covar1$Variable[1]=="TOB"){
    (p2 <- p2 + scale_fill_manual(values = RColorBrewer::brewer.pal(n = 6, name = "Paired")[c(5,6,3,4)]) )
    } else {

      # pro_covar1b$Variable[1]=="ENSO (ONI)"
    (p2 <- p2 + scale_fill_manual(values = RColorBrewer::brewer.pal(n = 10, name = "Paired")[mixed_set]) )
    }
  }
} else {
  p2 <- p2 + scale_fill_manual(values = RColorBrewer::brewer.pal(n = 4, name = "Paired")[c(2,4)])
}
}

(p3 <- plot_loadings(rflip,
                     # violin = FALSE,
                     conf_level = 0.90,
                     ordered_by = 1,
                     flip_order = TRUE,
                     names = spp)+
    guides(alpha=guide_legend(override.aes = list(fill = "grey20")),
           fill = "none") +
    scale_colour_manual(values = c(rep("black", trend_count)))+
    scale_fill_manual(values = c(rep("black", trend_count)))+
    labs(alpha = "Prob not 0") +
    scale_y_continuous(limits = c(-2.1,2.1)) +
    theme(legend.position = "none",
          axis.title.y = element_blank()) )

# if(trend_count>2){
#   (p3 <- p3 + scale_fill_manual(values = RColorBrewer::brewer.pal(n = 6, name = "Paired")[c(5,4,2)]))
# } else {
#   (p3 <- p3 + scale_fill_manual(values = RColorBrewer::brewer.pal(n = 10, name = "Paired")[mixed_set[c(2,3)]])+
#      scale_colour_manual(values = RColorBrewer::brewer.pal(n = 10, name = "Paired")[mixed_set[c(2,3)]]))
#   if(set_group == "mature females"){
#   (p3 <- p3 + scale_fill_manual(values = RColorBrewer::brewer.pal(n = 10, name = "Paired")[mixed_set2[c(2,4)]])+
#      scale_colour_manual(values = RColorBrewer::brewer.pal(n = 10, name = "Paired")[mixed_set2[c(2,4)]]))
#   }
#   if(set_group == "mature males" #& no_covs
#      ){
#     (p3 <- p3 + scale_fill_manual(values = RColorBrewer::brewer.pal(n = 10, name = "Paired")[mixed_set[c(2,4)]])+
#        scale_colour_manual(values = RColorBrewer::brewer.pal(n = 10, name = "Paired")[mixed_set[c(2,4)]]))
#   }
# }
#

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
                  gsub(" ", "-", pro_covar3$Variable[1]), "-",
                  bayesdfa_config,
                  ".png"),
           height = fig_height/1.65, width = fig_width)
}else{
  if(trend_count>1){
    ggsave(paste0("figs/DFA-", trend_count, "trends-no-cov-",
                  no_covs, "-",
              gsub(" ", "-", set_group), "-", model_name, "-",
              gsub(" ", "-", pro_covar1$Variable[1]), "-",
              gsub(" ", "-", pro_covar2$Variable[1]), "-", bayesdfa_config, ".png"),
       height = fig_height/1.55, width = fig_width*1)

 } else {
ggsave(paste0("figs/DFA-", trend_count, "trends-no-cov-",
              no_covs, "-",
              gsub(" ", "-", set_group), "-", model_name, "-",
              gsub(" ", "-", pro_covar1$Variable[1]), "-", bayesdfa_config, ".png"),
       height = fig_height/1.65, width = fig_width*0.9)
}
}

# manuscript_ready <- FALSE
manuscript_ready <- TRUE
if(manuscript_ready) {
# tall version for manuscript
wrap_elements(p1 / p2 + plot_layout(nrow = 2, widths= c(1), heights = c(1,0.7))& theme(
  # plot.margin = unit(c(0.2,0,0,0), "cm"),
  plot.subtitle = element_blank(),
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
}
}
}
# }


dfa_by_class(set_group)

# FIGURES FOR FLOW CHART -----
if(just_trend_version & set_group == "immatures" & !adjusted_for_density) {
p1 + theme(plot.title = element_blank(), plot.subtitle = element_blank())

ggsave(paste0("figs/man/chart-DFA-", trend_count, "trends-no-cov-",
              gsub(" ", "-", set_group), "-", model_name, ".png"),
       height = 1, width = 4)
}

if(set_group == "immatures" & !adjusted_for_density) {
p2

ggsave(paste0("figs/man/chart-DFA-hist-", trend_count, "trends-no-cov-",
              gsub(" ", "-", set_group), "-", model_name, ".png"),
       height = 1.25, width = 4.4)
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

# (p2 <- ggplot(df2) +
#     geom_ribbon(aes_string(x = "time",
#                            ymin = "lower", ymax = "upper", fill = "ID"),
#                 alpha = 0.4) +
#     geom_line(aes_string(x = "time", y = "estimate", colour = "ID")) +
#     # geom_line(data = pro_covar, aes_string(x = "time", y = "value"), colour = "red") +
#     geom_point(aes_string(x = "time", y = "y"),
#                #col = "blue",
#                size = 0.5, alpha = 0.7) +
#     # scale_color_manual(values = cols) +
#     # scale_fill_manual(values = cols) +
#     facet_wrap("ID" #, scales = "free_y"
#                ) +
#     xlab("Time") + ylab(y_label) +
#     theme(legend.position = "none")
#     # labs(colour= "Species", fill = "Species")+
#     # ggtitle(paste0("DFA with ",  trend_label, " and ", lag_label, agg_var, var_label))
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

