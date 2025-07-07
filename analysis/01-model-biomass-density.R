# 1. Create a total biomass distribution model (to use for prediction grid)
# 2. Create prediction grid with density and depth
devtools::load_all()

options(scipen = 100, digits = 4)

library(future)
library(tidyverse)
library(sdmTMB)
library(gfplot)
library(ggsidekick)
library(patchwork)
# library(gfenvirocor)

dir.create(paste0("figs/"), showWarnings = FALSE)
dir.create(paste0("data-generated/density-models/"), showWarnings = FALSE)
dir.create(paste0("data-generated/density-predictions/"), showWarnings = FALSE)
dir.create(paste0("data-generated/density-index/"), showWarnings = FALSE)

# load overall species list
source("analysis/00-species-list.R")

# # # # or override with custom subset
# species_list <- list(
# "Lingcod"
# )

# Function for running species density models --------

fit_all_distribution_models <- function(species, only_sampled) {

  set_utm_crs <- 32609

  ## for generating split data and some exploratory plots
  stop_early <- TRUE
  # stop_early <- FALSE

  ## this only affects maturity specific models (only needed if not using function)
  # only_sampled <- FALSE
  # only_sampled <- TRUE ## need to reun for halibut and pcod only

  options(scipen = 100, digits = 4)
  theme_set(theme_sleek())
  fig_height <- 4 * 2
  fig_width <- 5 * 2

  dens_model_name_long <- "depth, DOY, and survey type"

  set_min_sample_number <- 6
  mat_threshold <- 0.5
  knot_distance <<- 20
  set_priors <<- sdmTMBpriors(
    matern_s = pc_matern(
      range_gt = knot_distance,
      sigma_lt = 2
    ),
    matern_st = pc_matern(
      range_gt = knot_distance,
      sigma_lt = 2
    )
  )

  set_family <- delta_lognormal()
  set_family2 <- tweedie()

  # current model name
  dens_model_name0 <- "dln-ss5"

  if(only_sampled) {
    dens_model_name <- "dln-only-sampled" # current version
    ## if updating only sampled version
    # dens_model_name <- paste0(dens_model_name0, "-only-sampled")
  } else {
    dens_model_name <- paste0(dens_model_name0, "-split")
  }

  ### add date -----
  sysdate <- unlist(strsplit(as.character(Sys.Date()), "-"))

  ## add data run?
  # dens_model_name0 <- paste0(dens_model_name0, sysdate[1], "-", sysdate[2], "")
  # dens_model_name <- paste0(dens_model_name, sysdate[1], "-", sysdate[2], "")

  dens_model_name0 <- paste0(dens_model_name0, "-2024-09")
  dens_model_name <- paste0(dens_model_name, "-2024-09")

  # subset of surveys used for density models
  surveys_included <- c(
    "HS MSA",
    "HS PCOD",
    "THORNYHEAD",
    "SYN HS", "SYN QCS",
    "SYN WCHG", "SYN WCVI",
    "MSSM QCS", "MSSM WCVI", # already filtered to avoid duplication
    "HAKE",
    "OTHER" # already filtered to two older bottom trawl surveys + hake
  )

  if (length(set_family$family) > 1) {
    set_spatial <- "on"
    set_spatiotemporal <- list("rw", "rw")
  } else {
    set_spatial <- "on"
    set_spatiotemporal <- "rw"
  }

  custom_maturity_code <- NULL
  custom_length_threshold <- NULL

  source("analysis/00-custom-maturities.R")
  replace_with_custom_maturity(species)

  spp <- gsub(" ", "-", gsub("\\/", "-", tolower(species)))


  # # Load data ----
  dset <- readRDS("data-generated/all-sets-used.rds") %>%
    filter(species_common_name == tolower(species)) %>%
    filter(survey_abbrev %in% surveys_included) %>%
    distinct()

  dsamp <- readRDS("data-generated/all-samples-used.rds") %>%
    filter(species_common_name == tolower(species)) %>%
    distinct()

  check_for_duplicates <- dset[duplicated(dset$fishing_event_id), ]
  if(nrow(check_for_duplicates)>0){ stop(paste(species, "has duplicate event ids."))}

  check_for_duplicates2 <- dsamp[duplicated(dsamp$specimen_id), ]
  if(nrow(check_for_duplicates2)>0){ stop(paste(species, "has duplicate specimen ids."))}

  ## not using sample_id so shouldn't be any unless something else can cause this
  ## manual checks?
  # unique(dset$survey_abbrev)
  # unique(dsamp$survey_abbrev)
  # unique(dsamp$maturity_code)

  maturity_possible <- TRUE

  if(spp == "curlfin-sole"){
    maturity_possible <- FALSE
  }

  # temporary filter until lengths in wrong units fixed
  if(spp == "lingcod"){
  dsamp <- filter(dsamp, length > 8)
  }

  sets_mat_m <- filter(dsamp, !is.na(maturity_code), !is.na(length), maturity_code != 0, sex == 1)
  sets_mat_f <- filter(dsamp, !is.na(maturity_code), !is.na(length), maturity_code != 0, sex == 2)

  if(min(length(unique(sets_mat_m$fishing_event_id)), length(unique(sets_mat_f$fishing_event_id))) >= 20){
    set_sample_id_re <- TRUE
  }else{
    set_sample_id_re <- FALSE
  }


# Split by maturity ----
  dss <- gfplot::split_catch_by_sex(dset, dsamp,
    # catch_variable = "est_catch_count", # could use this to avoid biomass ~ condition issue
    # split_by_weight = FALSE, # automatically switches to TRUE for common weight-based catch variables
    sample_id_re = set_sample_id_re, # used for maturity ogives
    # sample_id_re = FALSE,
    # survey = surveys_included, # turning this off allows all samples provided to be used for maturity split
    immatures_pooled = TRUE,
    # use_median_ratio = TRUE,
    min_sample_number = set_min_sample_number,
    split_by_maturity = maturity_possible,
    custom_maturity_at = custom_maturity_code,
    custom_length_thresholds = custom_length_threshold,
    p_threshold = mat_threshold,
    plot = maturity_possible
  )

  dir.create(paste0("data-generated/split-catch-data/"), showWarnings = FALSE)

  saveRDS(dss, paste0("data-generated/split-catch-data/", spp, ".rds"))

  # dss$maturity_plot
  # gfplot::plot_mat_ogive(dss$m)
  # dss$weight_plot
  # dss$data %>% filter(group_catch_est >0) %>%
  #     ggplot() + geom_histogram(aes(log(group_catch_est)))

  meaneffort1 <- dss$data %>%
    filter(group_name %in% c("Mature", "Females", "Mature females") &
      usability_code == 1) %>%
    mutate(
      doorspread_m = ifelse(doorspread_m == 0|doorspread_m > 150, NA_real_, doorspread_m),
      # convert mouth widths to doorspread for hake data only
      mouth_width_m = ifelse(mouth_width_m == 0, NA_real_, mouth_width_m),
      doorspread_m = ifelse(survey_series_id == 68 & is.na(doorspread_m), mouth_width_m * 0.74 + 40, doorspread_m),
      speed_mpm = ifelse(speed_mpm == 0, NA_real_, speed_mpm)
      ) %>%
    group_by(year, survey_id, survey_series_id) %>%
    summarise(
      mean_doorspread = mean(doorspread_m, na.rm = TRUE),
      mean_speed = mean(speed_mpm, na.rm = TRUE)
    )

  meaneffort2 <- dss$data %>%
    filter(group_name %in% c("Mature","Males", "Mature females") &
             usability_code == 1) %>%
    mutate(
      doorspread_m = ifelse(doorspread_m == 0, NA_real_, doorspread_m),
      # mouth_width_m = ifelse(mouth_width_m == 0, NA_real_, mouth_width_m),
      # doorspread_m = ifelse(survey_series_id == 68 & is.na(doorspread_m), mouth_width_m * 0.74 + 40, doorspread_m),
      speed_mpm = ifelse(speed_mpm == 0, NA_real_, speed_mpm)
      ) %>%
    group_by(survey_series_id) %>%
    summarise(n = n(),
      mean_series_doorspread = mean(doorspread_m, na.rm = TRUE),
              mean_series_speed = mean(speed_mpm, na.rm = TRUE))

  ds <- dss$data %>%
    left_join(meaneffort1) %>%
    left_join(meaneffort2) %>%
    mutate(
      DOY = as.numeric(strftime(time_deployed, format = "%j")),
      days_to_solstice = DOY - 172,
      fishing_event_id = as.factor(fishing_event_id),
      vessel_cap_combo = factor(paste0(vessel_id, "-", captain_id)),
      vessel = as.factor(vessel_id),
      captain = as.factor(ifelse(is.na(captain_id),
        paste0(vessel_id, "-", year), captain_id
      )),
      usability_name = paste(usability_code, "-", usability_desc),
      usability_name = fct_reorder(usability_name, usability_code),
      # correct outlier duration for Hake, looks like date problem
      duration_min = ifelse(duration_min > 1000, 30, duration_min),
      duration_min = ifelse(duration_min == 0, NA_real_, duration_min), #new
      # get rid of false 0 and huge outliers for doorspread
      doorspread_m = ifelse(doorspread_m == 0|doorspread_m > 150, NA_real_, doorspread_m),
      mouth_width_m = ifelse(mouth_width_m == 0, NA_real_, mouth_width_m),
      # convert mouth widths to doorspread for hake data only
      doorspread_m = ifelse(survey_series_id == 68 & is.na(doorspread_m), mouth_width_m * 0.74 + 40, doorspread_m),
      # use survey or survey series means with missing doorspread and speed
      mean_doorspread = ifelse(is.na(mean_doorspread), mean_series_doorspread, mean_doorspread),
      doorspread_m = ifelse(is.na(doorspread_m), mean_doorspread, doorspread_m),
      mean_speed = ifelse(is.na(mean_speed), mean_series_speed, mean_speed),
      speed_mpm = ifelse(speed_mpm == 0|is.na(speed_mpm), mean_speed, speed_mpm),
      log_depth = log(depth_m),
      log_depth_c = log_depth - 5, # mean and median for whole data set
      area_swept = ifelse(is.na(tow_length_m),
        doorspread_m * duration_min * speed_mpm,
        doorspread_m * tow_length_m
      )
    )

  ds <- ds[,which(unlist(lapply(ds, function(x)!all(is.na(x))))),with=FALSE]

  # Select what data to include ----
  ds <- ds %>%
    # keep only usabilities that seem reasonable
    # filter(usability_code %in% c(0, 1, 6, 16, 22)) %>% # old version
    filter(usability_code %in% c(0, 1, 2, 6, 22)) %>%
    # 16 includes bouncing nets, so should be excluded
    # 2 could be added, since the definition of fail is reason specific
    # if speed recorded, it isn't too slow
    filter(is.na(speed_mpm) | speed_mpm >= 50) %>%
    # if time recorded, it was at least 10 min
    filter(is.na(duration_min) | duration_min == 0 | duration_min >= 10) %>%
    # if tow length available it's at least 500m
    filter(is.na(tow_length_m) | tow_length_m > 500) %>%  filter(area_swept > 0) %>%
    # filter(year > 1983) # too slow to fit using only MSSM from 1975 to 1983
    # early MSSM not reliable for small fish
    filter(survey_type != "MSSM <03" & year >= 2000)

  ds <- ds %>% filter(!is.na(catch_weight))
  ds <- ds %>% filter(!is.na(depth_m))
  ds <- ds %>% filter(!is.na(area_swept))
  ds <- ds %>% filter(!is.na(latitude))
  ds <- ds %>% filter(!is.na(longitude))

  ds$area_swept_km2 <- ds$area_swept / 1000000  # converts m2 to km2
  ds$offset <- log(ds$area_swept_km2*100) # offset in ha
  ds$survey_type <- relevel(ds$survey_type, "SYN")


  # Which surveys to include? ----
  # keep only surveys that caught this species within years of interest
  # currently at least 20 times total or an average of 4 times per year
  which_surv_all <- which_surveys(ds)

  dir.create(paste0("data-generated/surv-summary/"), showWarnings = FALSE)
  saveRDS(which_surv_all, paste0("data-generated/surv-summary/", spp, ".rds"))

  which_surv <- which_surveys(ds) %>% ungroup() %>%
    select(-group_name) %>%
    group_by(survey_type) %>%
    summarize_all(max) %>%
    filter(
      prop_pos >= 0.01 &
      max_pos_by_year >= 3 &
      !(prop_years_w_0 > 0.5 & max_pos_by_year < 5)
     )

  ds <- filter(ds, survey_type %in% which_surv$survey_type)


  # Plot usabilities ----
  ds %>%
      mutate(group_name = ifelse(is.na(group_catch_est), "Unsampled", group_name),
             group_catch_est = ifelse(group_name == "Unsampled", catch_weight, group_catch_est)
      ) %>%
    filter(group_name %in% c("Mature", "Females", "Mature females", "Unsampled")) %>%
    distinct() %>%
    ggplot() +
    geom_point(aes(area_swept, usability_name,size = log(catch_weight + 1)),alpha = 0.3) +
    scale_y_discrete(limits = rev) +
    facet_grid(~survey_type, scales = "free") +
    theme(axis.title.y = element_blank())
  ggsave(paste0("figs/0-usabilities/all-usabilities-", spp, ".png"),
         width = 17, height = 3.5)


  unique(ds$survey_abbrev)
  unique(ds$survey_type)
  sort(unique(ds$year))

  # check that offset doesn't contain NAs of Inf
  range(ds$offset)
  range(ds$area_swept)
  mean(ds$offset)

  ggplot(ds) +
    geom_histogram(aes(offset)) +
    facet_wrap(~year)

  # Make grid ----
  ds$X <- NULL
  ds$Y <- NULL

  d <- sdmTMB::add_utm_columns(ds,
         ll_names = c("longitude", "latitude"),
         utm_crs = set_utm_crs)

  dir.create(paste0("data-generated/density-data/"), showWarnings = FALSE)
  saveRDS(d, paste0("data-generated/density-data/", spp, ".rds"))

  if(stop_early) {
    return(which_surv_all)
  }

  # Set naming conventions ----
  dir.create(paste0("data-generated/density-models/", dens_model_name, "/"), showWarnings = FALSE)
  dir.create(paste0("data-generated/density-models/", dens_model_name0, "/"), showWarnings = FALSE)

  dir0 <-paste0("data-generated/density-models/", dens_model_name0, "/total/")
  dir1 <-paste0("data-generated/density-models/", dens_model_name, "/mat-fem/")
  dir2 <-paste0("data-generated/density-models/", dens_model_name, "/mat-m/")
  dir3 <-paste0("data-generated/density-models/", dens_model_name, "/imm/")

  ### folder names for all density models
  dir.create(dir0, showWarnings = FALSE)
  dir.create(dir1, showWarnings = FALSE)
  dir.create(dir2, showWarnings = FALSE)
  dir.create(dir3, showWarnings = FALSE)

  ### directories for all generated indices
  dir.create(paste0("data-generated/density-index/", dens_model_name0), showWarnings = FALSE)
  dir.create(paste0("data-generated/density-index/", dens_model_name), showWarnings = FALSE)
  dir.create(paste0("data-generated/density-index/", dens_model_name0,
                    "/total/"), showWarnings = FALSE)
  dir.create(paste0("data-generated/density-index/", dens_model_name,
                    "/mat-fem/"), showWarnings = FALSE)
  dir.create(paste0("data-generated/density-index/", dens_model_name,
                    "/mat-m/"), showWarnings = FALSE)
  dir.create(paste0("data-generated/density-index/", dens_model_name,
                    "/imm/"), showWarnings = FALSE)
  dir.create(paste0("data-generated/density-split-ind/"), showWarnings = FALSE)

  m0 <- paste0(spp, "-total-", dens_model_name0, "-", knot_distance, "-km")
  m1 <- paste0(spp, "-mat-fem-", dens_model_name, "-", knot_distance, "-km")
  m2 <- paste0(spp, "-mat-m-", dens_model_name, "-", knot_distance, "-km")
  m3 <- paste0(spp, "-imm-", dens_model_name, "-", knot_distance, "-km")

  fm <- paste0(dir0, m0, ".rds")
  fmf <- paste0(dir1, m1, ".rds")
  fmm <- paste0(dir2, m2, ".rds")
  fmi <- paste0(dir3, m3, ".rds")

  pfn <- paste0("data-generated/density-predictions/p-", m0, ".rds")
  pmfn <- paste0("data-generated/density-predictions/p-", m1, ".rds")
  pmfn2 <- paste0("data-generated/density-predictions/p-", m2, ".rds")
  pifn <- paste0("data-generated/density-predictions/p-", m3, ".rds")

  i0 <- paste0("data-generated/density-index/", dens_model_name0, "/total/i-", m0, ".rds")
  i1 <- paste0("data-generated/density-index/", dens_model_name, "/mat-fem/i-", m1, ".rds")
  i2 <- paste0("data-generated/density-index/", dens_model_name, "/mat-m/i-", m2, ".rds")
  i3 <- paste0("data-generated/density-index/", dens_model_name, "/imm/i-", m3, ".rds")

  # Generate synoptic grid for all modelled years ----
    grid <- replicate_df(gfplot::synoptic_grid,
      time_name = "year",
      time_values = min(d$year):max(d$year)
    ) %>%
      mutate(
        fishing_event_id = as.factor(paste(d$fishing_event_id[1])),
        days_to_solstice = 0,
        log_depth = log(depth),
        log_depth_c = log_depth - 5, # mean and median for whole data set
        survey_type = as.factor("SYN"),
        captain = as.factor("408"),
        vessel = as.factor("584"),
        vessel_cap_combo = as.factor("584-408") # has greatest spatial coverage
      )
  # }


  # Total density dataframe ----

    # simplify dataframe to be included with models
    d <- d %>% select(
      species_common_name,
      fishing_event_id,
      catch_weight,
      group_name, group_catch_est,
      n_events_sampled, n_fish_sampled,
      proportion, median_prop_ann,
      X, Y, latitude, longitude,
      year, days_to_solstice, DOY,
      survey_abbrev, survey_type,
      depth_m, log_depth, log_depth_c,
      # area_swept,
      area_swept_km2, offset,
      duration_min, speed_mpm,
      doorspread_m, tow_length_m,
      usability_name,
      captain, vessel, vessel_cap_combo
    )

    d1 <- d %>%
      filter(group_name %in% c("Mature", "Females", "Mature females"))


  ## check what years we have data for ----
  ## used to filter grid
  survey_years <- d1 %>%
    select(survey_abbrev, year) %>%
    distinct() %>%
    mutate(survey = ifelse(survey_abbrev %in% c("HS MSA", "HS PCOD"), "SYN HS",
      ifelse(survey_abbrev == "MSSM QCS", "SYN QCS",
        ifelse(survey_abbrev %in% c("THORNYHEAD", "MSSM WCVI"), "SYN WCVI",
          survey_abbrev))
    ))

  all_years <- unique(d1$year)
  extra_years <- sdmTMB:::find_missing_time(d1$year)

  # # check that my data and grid are on the same XY scale
  # range(grid$year)
  # range(d$year)
  # range(d$X)
  # range(grid$X)
  # range(d$Y)
  # range(grid$Y)

  dp <- d1 %>% filter(catch_weight > 0)

  hist(log(dp$catch_weight))
  range(d$catch_weight)

  rm(dsamp, dset, ds, dp)

  # Make mesh for total density ----
  mesh <- make_mesh(d1, c("X", "Y"), cutoff = knot_distance)

  ## plot mesh ----
  plot_mesh <- function(
    mesh_obj = mesh,
    data_obj = d1,
    catch_var = "catch_weight",
    group = "total"
    ){
  ggplot() +
    inlabru::gg(mesh_obj$mesh) +
    coord_fixed() +
    geom_point(aes(X, Y),
               shape = "x",
               size = 0.75,
               data = data_obj
    ) +
    geom_point(aes(X, Y,
                   size = .data[[catch_var]],
                   shape = survey_type,
                   fill = .data[[catch_var]],
                   colour = .data[[catch_var]]),
      data = filter(data_obj, .data[[catch_var]] > 0)
    ) +
    facet_wrap(~year) +
    scale_fill_viridis_c(trans = "fourth_root_power") +
    scale_color_viridis_c(trans = "fourth_root_power") +
    ggtitle(paste0(species, " (", group, ")")) +
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank())
  }

  d1$density_kgha <- d1$catch_weight/(d1$area_swept_km2*100) # converts to ha

  plot_mesh(
    mesh_obj = mesh,
    data_obj = d1,
    # data_obj = filter(d1, survey_type == "HAKE"), # for inspection
    # data_obj = filter(d1, survey_type %in% c("MSSM<=05", "MSSM>05")), # for inspection
    catch_var = "density_kgha")

  ggsave(paste0("figs/density-mesh-",
                spp,
                "-total-",
                dens_model_name,
                ".png"), width = 14, height = 14)

  d1 %>% ggplot() + geom_histogram(aes(depth_m)) +
    geom_histogram(aes(depth_m), fill = "red", data = filter(d1, catch_weight > 0 )) +
    facet_wrap(~survey_type, scales = "free_y")

  d1 %>% ggplot() + geom_histogram(aes(days_to_solstice)) +
    geom_histogram(aes(days_to_solstice), fill = "red", data = filter(d1, catch_weight > 0)) +
    facet_wrap(~survey_type, scales = "free_y")

  mesh$mesh$n


# Start modeling ----

  ## total abundance model ----
  if (!file.exists(fm)) {
    if(nrow(which_surv)<2){
      m <- sdmTMB(
        catch_weight ~ 1 +
          poly(log_depth_c, 2) +
          poly(days_to_solstice, 2),
        offset = "offset",
        mesh = mesh,
        data = d1,
        spatial = set_spatial,
        spatiotemporal = set_spatiotemporal,
        share_range = FALSE,
        silent = FALSE,
        time = "year",
        extra_time = extra_years,
        family = set_family,
        priors = set_priors
      )
    } else {
    m <- sdmTMB(
      catch_weight ~ 1 + survey_type +
        poly(log_depth_c, 2) +
        poly(days_to_solstice, 2),
      offset = "offset",
      mesh = mesh,
      data = d1,
      spatial = set_spatial,
      spatiotemporal = set_spatiotemporal,
      share_range = FALSE,
      silent = FALSE,
      time = "year",
      extra_time = extra_years,
      family = set_family,
      priors = set_priors
    )
    }
    saveRDS(m, fm)

    if (!all(sanity(m, gradient_thresh = 0.005))) {
      m <- refine_model(m, alternate_family = set_family2, use_priors = set_priors)
    }
    saveRDS(m, fm)
  } else {
    m <- readRDS(fm)
    if (!all(sanity(m, gradient_thresh = 0.005))) {
        m <- refine_model(m, alternate_family = set_family2, use_priors = set_priors)
    }
    saveRDS(m, fm)
  }

  m
  m$sd_report
  tidy(m, "ran_pars", conf.int = TRUE, model = 1)
  try(tidy(m, "ran_pars", conf.int = TRUE, model = 2))

  # TODO: add R2 once it's working for delta models
  # r2_total <- r2.sdmTMB(m)

  if (is.null(extra_years)) {
    grid <- filter(grid, year %in% c(sort(unique(m$data$year))))
  } else {
    # if extra time
    grid <- filter(grid, year %in% sort(union(m$data$year, m$extra_time)))
  }

  if (file.exists(pfn) & file.exists(i0)) {
    p <- readRDS(pfn)
  } else {
    p <- predict(m, re_form_iid = NA, newdata = grid, return_tmb_object = TRUE)

    map_density(p, pfn, variable = "density_trimmed") +
      labs(title = paste0(species, ": total biomass (", dens_model_name, ")"),
        # subtitle = paste0("Variance explained:", TODO: r2_total$R2[1])
      )

    ggsave(paste0("figs/density-map-", m0, ".png"),
      height = fig_height, width = fig_width
    )

    plot_index(p, species, "Total", dens_model_name, i0) +
      ggtitle(paste0(species, ": total biomass (", dens_model_name, ")"))

  }

  if (!file.exists(i0)) {
    plot_index(p, species, "Total", dens_model_name, i0) +
      ggtitle(paste0(species, ": total biomass (", dens_model_name, ")"))

  }

  # certain model formulation functions (like formula() and terms() ) grab the ENTIRE global environment
  # need to purge previous models before building new ones
  set_spatial <- as.list(m[["spatial"]])
  set_spatiotemporal <- as.list(m[["spatiotemporal"]])
  set_family <- m$family

  rm(m, p)

  ## mature female model ----

  d2 <- d1 %>%
    filter(year > 2001) %>%
    filter(!is.na(group_catch_est))

  if(only_sampled){
    # # test pattern without extrapolating to years without annual survey-specific proportions
    d2 <- d2 %>%
      filter(n_events_sampled > set_min_sample_number) %>%
      filter(!is.na(median_prop_ann))
  }

  which_surv <- which_surveys(d2) %>%
    filter(group_name %in% c("Females", "Mature females")) %>%
    filter(prop_pos >= 0.01 &
           max_pos_by_year >= 3 &
           !(prop_years_w_0 > 0.5 & max_pos_by_year < 5)
    )

  d2 <- filter(d2, survey_type %in% which_surv$survey_type)

  mesh2 <- make_mesh(d2, c("X", "Y"), cutoff = knot_distance)

  d2$density_kgha <- d2$group_catch_est/(d2$area_swept_km2*100)

  plot_mesh(
    mesh_obj = mesh2,
    data_obj = d2,
    catch_var = "density_kgha",
    group = "mature females")

  ggsave(paste0("figs/density-mesh-",
                spp,
                "-mat-fem-",
                dens_model_name,
                ".png"), width = 14, height = 14)

  if (!file.exists(fmf)) {

    if(nrow(which_surv)<2){
      mf <- sdmTMB(group_catch_est ~ 1 +
                     poly(log_depth_c, 2) +
                     poly(days_to_solstice, 2),
                   offset = "offset",
                   spatial = set_spatial,
                   spatiotemporal = set_spatiotemporal,
                   share_range = FALSE,
                   time = "year",
                   family = set_family,
                   extra_time = sdmTMB:::find_missing_time(d2$year),
                   priors = set_priors,
                   silent = FALSE,
                   mesh = mesh2, data = d2
      )
    } else {
    mf <- sdmTMB(group_catch_est ~ 1 + survey_type +
      poly(log_depth_c, 2) +
      poly(days_to_solstice, 2),
    offset = "offset",
    spatial = set_spatial,
    spatiotemporal = set_spatiotemporal,
    share_range = FALSE,
    time = "year",
    family = set_family,
    extra_time = sdmTMB:::find_missing_time(d2$year),
    priors = set_priors,
    silent = FALSE,
    mesh = mesh2, data = d2
    )
    }
    saveRDS(mf, fmf)

    if (!all(sanity(mf, gradient_thresh = 0.005))) {
      mf <- refine_model(mf, alternate_family = set_family2, use_priors = set_priors)
    }
    saveRDS(mf, fmf)
  } else {
    mf <- readRDS(fmf)
    if (!all(sanity(mf, gradient_thresh = 0.005))) {
        mf <- refine_model(mf, alternate_family = set_family2, use_priors = set_priors)
      }
    saveRDS(mf, fmf)
  }

  # TODO: add R2 once it's working for delta models
  # r2_mf <- r2.sdmTMB(mf)

  if (file.exists(pmfn) & file.exists(i1)) {
    pf <- readRDS(pmfn)
  } else {

    pf <- predict(mf,
      re_form_iid = NA, # only needed if random intercepts
      newdata = filter(grid, year %in% sort(union(mf$data$year, mf$extra_time))),
      return_tmb_object = TRUE
    )

    map_density(pf, pmfn) +
      labs(title = paste0(species, ": mature female biomass (", dens_model_name, ")"))

    ggsave(paste0("figs/density-map-", m1, ".png"),
      height = fig_height, width = fig_width
    )
  }

  if (!file.exists(i1)) {
    plot_index(pf, species, "Mature female", dens_model_name, i1) +
      ggtitle(paste0(species, ": mature female biomass (", dens_model_name, ")"))
  }

  ## currently sticking with m values
  # set_spatial <- as.list(mf[["spatial"]])
  # set_spatiotemporal <- as.list(mf[["spatiotemporal"]])
  # set_family <- mf$family

  rm(mf, pf, d1, d2)


  ## mature male model ----

    d2b <- d %>%
      filter(group_name %in% c("Males", "Mature males")) %>%
      filter(year > 2001) %>%
      filter(!is.na(group_catch_est))

  if(only_sampled){
    # # test pattern without extrapolating to years without annual survey-specific proportions
    d2b <- d2b %>%
      filter(n_events_sampled > set_min_sample_number) %>%
      filter(!is.na(median_prop_ann))
  }

    which_surv <- which_surveys(d2b) %>%
      filter(group_name %in% c("Males", "Mature males")) %>%
      filter(prop_pos >= 0.01 &
             max_pos_by_year >= 3 &
             !(prop_years_w_0 > 0.5 & max_pos_by_year < 5)
      )

    d2b <- filter(d2b, survey_type %in% which_surv$survey_type)

    mesh2b <- make_mesh(d2b, c("X", "Y"), cutoff = knot_distance)

    d2b$density_kgha <- d2b$group_catch_est/(d2b$area_swept_km2*100)

    plot_mesh(
      mesh_obj = mesh2b,
      data_obj = d2b,
      catch_var = "density_kgha",
      group = "mature males")

    ggsave(paste0("figs/density-mesh-",
                  spp,
                  "-mat-m-",
                  dens_model_name,
                  ".png"), width = 14, height = 14)


    if (!file.exists(fmm)) {

      if(nrow(which_surv)<2){
        mm <- sdmTMB(group_catch_est ~ 1 +
                               poly(log_depth_c, 2) +
                               poly(days_to_solstice, 2),
                             offset = "offset",
                     spatial = set_spatial,
                     spatiotemporal = set_spatiotemporal,
                     share_range = FALSE,
                     time = "year",
                     family = set_family,
                     priors = set_priors,
                     extra_time = sdmTMB:::find_missing_time(d2b$year),
                     mesh = mesh2b,
                     data = d2b
        )
      } else{
        mm <- sdmTMB(group_catch_est ~ 1 + survey_type +
                       poly(log_depth_c, 2) +
                       poly(days_to_solstice, 2),
                     offset = "offset",
                     spatial = set_spatial,
                     spatiotemporal = set_spatiotemporal,
                     share_range = FALSE,
                     time = "year",
                     family = set_family,
                     priors = set_priors,
                     extra_time = sdmTMB:::find_missing_time(d2b$year),
                     mesh = mesh2b,
                     data = d2b
        )
      }
    saveRDS(mm, fmm)

    if (!all(sanity(mm, gradient_thresh = 0.005))) {
      mm <- refine_model(mm, alternate_family = set_family2, use_priors = set_priors)
    }
    saveRDS(mm, fmm)
  } else {
    mm <- readRDS(fmm)
    # mm <- sdmTMB:::update_version(mm)
    if (!all(sanity(mm, gradient_thresh = 0.005))) {
        mm <- refine_model(mm, alternate_family = set_family2, use_priors = set_priors)
      }
    saveRDS(mm, fmm)
  }

  # TODO: add R2 once it's working for delta models
  # r2_mm <- r2.sdmTMB(mm)

  if (file.exists(pmfn2) & file.exists(i2)) {
    pm <- readRDS(pmfn2)
  } else {
    pm <- predict(mm,
      re_form_iid = NA,
      # newdata = filter(grid, year %in% sort(unique(mm$data$year))),
      newdata = filter(grid, year %in% sort(union(mm$data$year, mm$extra_time))),
      return_tmb_object = TRUE
    )

    map_density(pm, pmfn2) +
      labs(title = paste0(species, ": mature male biomass (", dens_model_name, ")"))

    ggsave(paste0("figs/density-map-", m2, ".png"),
      height = fig_height, width = fig_width
    )
  }

    if (!file.exists(i2)) {
      plot_index(pm, species, "Mature male", dens_model_name, i2) +
        ggtitle(paste0(species, ": mature male biomass (", dens_model_name, ")"))

      # ggsave(paste0("figs/density-index-", m2, ".png"),
      # height = fig_height / 2, width = fig_width / 1.5
      # )
    }


    # set_spatial <- as.list(mm[["spatial"]])
    # set_spatiotemporal <- as.list(mm[["spatiotemporal"]])
    # set_family <- mm$family

    rm(mm, pm, d2b)

  ## immature model ----

  # if maturity split used, but too few immature fish to model
  if(spp == "shortraker-rockfish"){
    maturity_possible <- FALSE
  }


  if(maturity_possible) {

    d3 <- d %>%
      filter(group_name %in% c("Immature")) %>%
      filter(year > 2001) %>%
      filter(!is.na(group_catch_est))

    if(only_sampled){
      # # test pattern without extrapolating to years without annual survey-specific proportions
      d3 <- d3 %>%
        filter(n_events_sampled > set_min_sample_number) %>%
        filter(!is.na(median_prop_ann))
    }

    which_surv <- which_surveys(d3) %>%
      filter(group_name %in% c("Immature")) %>%
      filter(
        prop_pos >= 0.01 &
        max_pos_by_year >= 3 &
        !(prop_years_w_0 > 0.5 & max_pos_by_year < 5)
      )

    d3 <- filter(d3, survey_type %in% which_surv$survey_type)

    mesh3 <- make_mesh(d3, c("X", "Y"), cutoff = knot_distance)

    d3$density_kgha <- d3$group_catch_est/(d3$area_swept_km2*100)

    plot_mesh(
      mesh_obj = mesh3,
      data_obj = d3,
      catch_var = "density_kgha",
      group = "immature")
    ggsave(paste0("figs/density-mesh-", spp, "-imm-", dens_model_name, ".png"),
           width = 14, height = 14)


    if (!file.exists(fmi)) {
    if(nrow(which_surv)<2){
      try( mi <- sdmTMB(group_catch_est ~ 1 +
                     poly(log_depth_c, 2) +
                     poly(days_to_solstice, 2),
                   offset = "offset",
                   spatial = set_spatial,
                   spatiotemporal = set_spatiotemporal,
                   share_range = FALSE,
                   time = "year",
                   family = set_family,
                   extra_time = sdmTMB:::find_missing_time(d3$year),
                   priors = set_priors,
                   silent = FALSE,
                   mesh = mesh3, data = d3
      ))
    } else{
    try(mi <- sdmTMB(group_catch_est ~ 1 + survey_type +
                             poly(log_depth_c, 2) +
                             poly(days_to_solstice, 2),
                           offset = "offset",
                           spatial = set_spatial,
                           spatiotemporal = set_spatiotemporal,
                           share_range = FALSE,
                           time = "year",
                           family = set_family,
                           extra_time = sdmTMB:::find_missing_time(d3$year),
                           priors = set_priors,
                           silent = FALSE,
                           mesh = mesh3, data = d3
    ))
    }

    if(exists("mi")){
    saveRDS(mi, fmi)
    if (!all(sanity(mi, gradient_thresh = 0.005))) {
    mi <- refine_model(mi, alternate_family = set_family2,
                         use_priors = set_priors)
    }
     saveRDS(mi, fmi)
    }

  } else {
    mi <- readRDS(fmi)
    if (!all(sanity(mi, gradient_thresh = 0.005))) {
        mi <- refine_model(mi, alternate_family = set_family2,
                           use_priors = set_priors)
      }
    saveRDS(mi, fmi)
  }


  if(exists("mi")){
  # TODO: add R2 once it's working for delta models
  # r2_i <- r2.sdmTMB(mi)

  s <- sanity(mi, gradient_thresh = 0.005)

  if (all(unlist(s))) {
    if (file.exists(pifn) & file.exists(i3)) {
      pi <- readRDS(pifn)
    } else {
      pi <- predict(mi,
        re_form_iid = NA,
        # newdata = filter(grid, year %in% sort(unique(mi$data$year))),
        newdata = filter(grid, year %in% sort(union(mi$data$year, mi$extra_time))),
        return_tmb_object = TRUE
      )

      map_density(pi, pifn) +
        labs(title = paste0(species, ": immature biomass (", dens_model_name, ")"))

      ggsave(paste0("figs/density-map-", m3, ".png"),
        height = fig_height, width = fig_width
      )
    }
  }
}
}

  if (!file.exists(i3)) {
    try(plot_index(pi, species, "Immature", dens_model_name, i3) +
      ggtitle(paste0(species, ": immature biomass (", dens_model_name, ")")))
  }

  ind0 <- readRDS(i0) %>% mutate(index = "Total") %>% mutate(model_string = dens_model_name0)
  ind1 <- readRDS(i1) %>% mutate(index = "Mature female") %>% mutate(model_string = dens_model_name)
  ind2 <- readRDS(i2) %>% mutate(index = "Mature male") %>% mutate(model_string = dens_model_name)
  try(ind3 <- readRDS(i3) %>% mutate(index = "Immature")%>% mutate(model_string = dens_model_name))

  bc_inds <- bind_rows(ind0, ind1, ind2)
  try(bc_inds <- bind_rows(ind0, ind1, ind2, ind3))

  ## Plot coastwide indices ----
  m <- readRDS(fm)

  (p1 <- bc_inds %>%
    mutate(index = fct_relevel(index, rev)) %>%
    ggplot(aes(year, est/1000)) +
    geom_ribbon(aes(ymin = lwr/1000, ymax = upr/1000, fill = index), alpha = 0.3) +
    geom_line(aes(colour = index), linewidth = 0.7) +
    scale_colour_viridis_d(direction = 1, end = 0.8, option = "A") +
    scale_fill_viridis_d(direction = 1, end = 0.8, option = "A") +
    labs(colour = "Biomass Index", fill = "Biomass Index") +
    xlab("Year") +
    ylab("Biomass estimate (kg)") +
    ggtitle(paste0(species), subtitle = paste0(
      "Model: ",
      ifelse(isTRUE(m$family$delta), m$family$clean_name, paste0(m$family[1], "(link = 'log')")),
      ", spatial (", m[["spatial"]][1], ", ", m[["spatial"]][2],
      ") with st RW and ", dens_model_name_long,
      " (", paste(unique(m$data$survey_type), collapse = ", "), ")"
    )))

  # ggsave(paste0("figs/density-index-", spp, "-all",
  # dens_model_name, "-", knot_distance, "-km.png"),
  # height = fig_height / 2, width = fig_width / 1.5
  # )

  # Generate split indices ----
  fsi <- paste0(
    "data-generated/density-split-ind/", spp, "-split-",
    dens_model_name, "-", knot_distance, "-km.rds"
  )

  if (!file.exists(fsi)) {
    mf <- readRDS(fmf)
    mm <- readRDS(fmm)
    inds0 <- split_index_by_survey(m, grid, species, "Total")
    inds1 <- split_index_by_survey(mf, grid, species, "Mature female")
    inds2 <- split_index_by_survey(mm, grid, species, "Mature male")

    all_split_inds <- bind_rows(inds0, inds1, inds2)
    saveRDS(all_split_inds, fsi)

    if (file.exists(i3)) {
      inds3 <- split_index_by_survey(mi, grid, species, "Immature")
      all_split_inds <- bind_rows(inds0, inds1, inds2, inds3)
    }
    saveRDS(all_split_inds, fsi)
  } else {
    all_split_inds <- readRDS(fsi)
  }

  ## Plot split indices ----
  p2 <- all_split_inds %>%
    left_join(survey_years, ., multiple = "all") %>%
    select(-survey_abbrev) %>%
    filter(!is.na(est)) %>%
    distinct() %>%
    filter(group == "Total") %>%
    mutate(index = fct_relevel(index, rev)) %>%
    ggplot(aes(year, est/1000)) +
    geom_ribbon(aes(ymin = lwr/1000, ymax = upr/1000, fill = survey), alpha = 0.3) +
    geom_line(aes(colour = survey)) +
    # facet_wrap(~index, scales = "free") +
    # facet_wrap(~index, scales = "free_x") +
    scale_colour_viridis_d(direction = -1, end = 0.9) +
    scale_fill_viridis_d(direction = -1, end = 0.9) +
    labs(colour = "Area", fill = "Area") +
    coord_cartesian(ylim = c(0, max(all_split_inds$est/1000) * 1.5)) +
    ggtitle("Total") +
    xlab("Year") +
    ylab("Relative biomass estimate (tonnes)")

  p2

  p3dat <- filter(all_split_inds, group != "Total") %>%
    left_join(survey_years, ., multiple = "all") %>%
    select(-survey_abbrev) %>%
    filter(!is.na(est)) %>%
    distinct()

  p3 <- p3dat %>%
    mutate(index = fct_relevel(index, rev)) %>%
    ggplot(aes(year, est/1000)) +
    geom_ribbon(aes(ymin = lwr/1000, ymax = upr/1000, fill = survey), alpha = 0.3) +
    geom_line(aes(colour = survey)) +
    # facet_wrap(~index, scales = "free") +
    facet_wrap(~index, scales = "free_x") +
    scale_colour_viridis_d(direction = -1, end = 0.9) +
    scale_fill_viridis_d(direction = -1, end = 0.9) +
    labs(colour = "Area", fill = "Area") +
    coord_cartesian(ylim = c(0, max(p3dat$est/1000) * 1.25)) +
    # ggtitle(paste0(species, " (", dens_model_name, ")"))+
    xlab("Year") +
    ylab("Relative biomass estimate (tonnes)")

  if (length(unique(all_split_inds$model)) > 1) {
    p3 <- p3 + geom_text(aes(label = model),
      x = 2006, y = max(p3dat$est/1000) * 1.1, size = 2.5, hjust = 0
    )
  }

  p1a <- p1 + theme(axis.text.x = element_blank(), axis.title = element_blank())
  p2a <- p2 + theme(axis.title.x = element_blank())
  p3a <- p3 + theme(axis.title.y = element_blank(), legend.position = "none")

  p1a + p2a + p3a + plot_layout(ncol = 1)

  ggsave(paste0(
    "figs/density-all-indices-", spp, "-",
    dens_model_name, "-", knot_distance, "-km.png"
  ),
  height = fig_height, width = fig_width
  )

}


## Run with pmap -----
arg_list <- list(species = species_list, only_sampled = FALSE)
pmap(arg_list, fit_all_distribution_models)

arg_list2 <- list(species = species_list, only_sampled = TRUE)
pmap(arg_list2, fit_all_distribution_models)

