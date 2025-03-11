# 1. Create a total biomass distribution model (to use for prediction grid)
# 2. Create prediction grid with density and depth
# TODO: get matching depth for use in models and on prediction grids?

devtools::load_all()
library(tidyverse)
library(sdmTMB)
library(gfplot)
library(ggsidekick)
library(patchwork)

make_all_sdms <- function(only_sampled = FALSE) {

fig_height <- 8
fig_width <- 10

source("stock-specific/00-set-options.R")

spp <- gsub(" ", "-", gsub("\\/", "-", tolower(species)))

dir.create(paste0("stock-specific/", spp, "/output/", "density-models/"), showWarnings = FALSE)
dir.create(paste0("stock-specific/", spp, "/output/", "density-predictions/"), showWarnings = FALSE)
dir.create(paste0("stock-specific/", spp, "/output/", "density-index/"), showWarnings = FALSE)

  ### add date to model names -----
  sysdate <- unlist(strsplit(as.character(Sys.Date()), "-"))

  if(only_synoptic) {
    dens_model_name0 <- paste0(dens_model_name0, "syn-")

    if(only_sampled) {
      dens_model_name <- paste0(dens_model_name0, "only-sampled-")
    } else {
      # use SD to choose between aggregating by year or survey first, then survey, then all
      dens_model_name <- paste0(dens_model_name0, "split-")
    }

  } else {
  if(only_sampled) {
    dens_model_name <- paste0(dens_model_name0, "only-sampled-")
  } else {
    # use SD to choose between aggregating by year or survey first, then survey, then all
    dens_model_name <- paste0(dens_model_name0, "split-")
  }
  }

  dens_model_name0 <- paste0(dens_model_name0, sysdate[1], "-", sysdate[2], "")
  dens_model_name <- paste0(dens_model_name, sysdate[1], "-", sysdate[2], "")

  if (length(set_family$family) > 1) {
    set_spatial <- "on"
    set_spatiotemporal <- list("rw", "rw")
  } else {
    set_spatial <- "on"
    set_spatiotemporal <- "rw"
  }

  set_priors <<- sdmTMB::sdmTMBpriors(
    matern_s = pc_matern(
      range_gt = knot_distance, # *1.5
      sigma_lt = 2
    ),
    matern_st = pc_matern(
      range_gt = knot_distance, # *1.5
      sigma_lt = 2
    )
  )

  # check year we should start model
  cond_years <- readRDS(paste0("stock-specific/", spp, "/data/", "tidy-survey-samples-", spp, ".rds")) |>
    filter(!is.na(weight) & !is.na(length) & !is.na(sex)) |>
    group_by(year) |> summarise(n = n()) |>
    filter(n > 30)

  ds <- readRDS(paste0("stock-specific/", spp, "/output/", "split-catch-data-w-effort-", spp, ".rds")) %>%
    ## early MSSM not reliable for fish that aren't rockfish, flatfish, or lingcod...
    ## but probably rare that sufficient condition samples available that early anyway
    # filter(survey_type != "MSSM <03") |>
    # filter(year > 1983) # too slow to fit using only MSSM from 1975 to 1983
    filter(year >= min(cond_years$year)-1)

  # Select what data to include ----
  ds <- ds %>%
    filter(usability_code %in% c(0, 1, 2, 6, 22)) %>%
    # if speed recorded, it isn't too slow
    filter(is.na(speed_mpm) | speed_mpm >= 50) %>%
    # if time recorded, it was at least 10 min
    filter(is.na(duration_min) | duration_min >= 10) %>%
    # if tow length available it's at least 200m
    filter(is.na(tow_length_m) | tow_length_m > 500) %>%
    filter(area_swept > 0)

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

  saveRDS(which_surv_all, paste0("stock-specific/", spp, "/output/", "surv-summary-", spp, ".rds"))


  which_surv <- which_surveys(ds) %>% ungroup() %>%
    select(-group_name) %>%
    # filter(group_name %in% c("Mature", "Mature females")) %>%
    group_by(survey_type) %>%
    summarize_all(max) %>%
    filter(
      # round(prop_pos, 2) >= 0.01 &
      prop_pos >= 0.01 &
        ## all species-maturity classes met this before I added YE
        # round(max_prop_pos, 2) >= 0.05 &
        max_pos_by_year >= 3 &
        !(prop_years_w_0 > 0.5 & max_pos_by_year < 5)
    )

  if(only_synoptic) {
  which_surv <- filter(which_surv, survey_type %in% c("SYN"))
  }

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
  ggsave(paste0("stock-specific/", spp, "/figs/all-usabilities-", spp, ".png"),
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

  ds$X <- NULL
  ds$Y <- NULL

  d <- sdmTMB::add_utm_columns(ds,
                               ll_names = c("longitude", "latitude"),
                               utm_crs = set_utm_crs)

  saveRDS(d, paste0("stock-specific/", spp, "/output/", "density-data-", spp, ".rds"))

  if(stop_early) {
    return(which_surv_all)
  }

  # Set naming conventions ----
  dir.create(paste0("stock-specific/", spp, "/output/", "density-models/", dens_model_name, "/"), showWarnings = FALSE)
  dir.create(paste0("stock-specific/", spp, "/output/", "density-models/", dens_model_name0, "/"), showWarnings = FALSE)

  dir0 <-paste0("stock-specific/", spp, "/output/", "density-models/", dens_model_name0, "/total/")
  dir1 <-paste0("stock-specific/", spp, "/output/", "density-models/", dens_model_name, "/mat-fem/")
  dir2 <-paste0("stock-specific/", spp, "/output/", "density-models/", dens_model_name, "/mat-m/")
  dir3 <-paste0("stock-specific/", spp, "/output/", "density-models/", dens_model_name, "/imm/")

  ### folder names for all density models
  dir.create(dir0, showWarnings = FALSE)
  dir.create(dir1, showWarnings = FALSE)
  dir.create(dir2, showWarnings = FALSE)
  dir.create(dir3, showWarnings = FALSE)

  ### directories for all generated indices
  dir.create(paste0("stock-specific/", spp, "/output/", "density-index/", dens_model_name0), showWarnings = FALSE)
  dir.create(paste0("stock-specific/", spp, "/output/", "density-index/", dens_model_name), showWarnings = FALSE)
  dir.create(paste0("stock-specific/", spp, "/output/", "density-index/", dens_model_name0,
                    "/total/"), showWarnings = FALSE)
  dir.create(paste0("stock-specific/", spp, "/output/", "density-index/", dens_model_name,
                    "/mat-fem/"), showWarnings = FALSE)
  dir.create(paste0("stock-specific/", spp, "/output/", "density-index/", dens_model_name,
                    "/mat-m/"), showWarnings = FALSE)
  dir.create(paste0("stock-specific/", spp, "/output/", "density-index/", dens_model_name,
                    "/imm/"), showWarnings = FALSE)
  dir.create(paste0("stock-specific/", spp, "/output/", "density-split-ind/"), showWarnings = FALSE)

  m0 <- paste0(spp, "-total-", dens_model_name0, "-", knot_distance, "-km")
  m1 <- paste0(spp, "-mat-fem-", dens_model_name, "-", knot_distance, "-km")
  m2 <- paste0(spp, "-mat-m-", dens_model_name, "-", knot_distance, "-km")
  m3 <- paste0(spp, "-imm-", dens_model_name, "-", knot_distance, "-km")

  fm <- paste0(dir0, m0, ".rds")
  fmf <- paste0(dir1, m1, ".rds")
  fmm <- paste0(dir2, m2, ".rds")
  fmi <- paste0(dir3, m3, ".rds")

  pfn <- paste0("stock-specific/", spp, "/output/", "density-predictions/p-", m0, ".rds")
  pmfn <- paste0("stock-specific/", spp, "/output/", "density-predictions/p-", m1, ".rds")
  pmfn2 <- paste0("stock-specific/", spp, "/output/", "density-predictions/p-", m2, ".rds")
  pifn <- paste0("stock-specific/", spp, "/output/", "density-predictions/p-", m3, ".rds")

  i0 <- paste0("stock-specific/", spp, "/output/", "density-index/", dens_model_name0, "/total/i-", m0, ".rds")
  i1 <- paste0("stock-specific/", spp, "/output/", "density-index/", dens_model_name, "/mat-fem/i-", m1, ".rds")
  i2 <- paste0("stock-specific/", spp, "/output/", "density-index/", dens_model_name, "/mat-m/i-", m2, ".rds")
  i3 <- paste0("stock-specific/", spp, "/output/", "density-index/", dens_model_name, "/imm/i-", m3, ".rds")

  # Make grid ----
  # Use synoptic grid for all modelled years
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

  # if(nrow(d1a)!= nrow(d1)) { print("")}

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

  # check that my data and grid are on the same XY scale
  range(grid$year)
  range(d$year)
  range(d$X)
  range(grid$X)
  range(d$Y)
  range(grid$Y)

  dp <- d1 %>% filter(catch_weight > 0)

  hist(log(dp$catch_weight))
  range(d$catch_weight)

  rm(ds, dp)

  # Make mesh for total density ----

  mesh <- make_mesh(d1, c("X", "Y"), cutoff = knot_distance)

  # browser()
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
      # scale_shape_discrete() +
      scale_fill_viridis_c(trans = "fourth_root_power") +
      scale_color_viridis_c(trans = "fourth_root_power") +
      ggtitle(paste0(species, " (", group, ")")) +
      theme(axis.text = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank())
  }

  # browser()

  d1$density_kgha <- d1$catch_weight/(d1$area_swept_km2*100) # converts to ha

  plot_mesh(
    mesh_obj = mesh,
    data_obj = d1,
    # data_obj = filter(d1, survey_type == "HAKE"), # for inspection
    # data_obj = filter(d1, survey_type %in% c("MSSM<=05", "MSSM>05")), # for inspection
    catch_var = "density_kgha")

  ggsave(paste0("stock-specific/", spp, "/figs/density-mesh-",
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

  # browser()

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
        # control = sdmTMBcontrol(
        #   # start = list(logit_p_mix = qlogis(0.01)),
        #   # map = list(logit_p_mix = factor(NA)),
        #   # nlminb_loops = 1L,
        #   # newton_loops = 1L
        # ),
        priors = set_priors
        # anisotropy = TRUE
      )
    }
    saveRDS(m, fm)

    if (!all(sanity(m, gradient_thresh = 0.005))) {
      m <- refine_model(m, alternate_family = set_family2, use_priors = set_priors)
    }
    saveRDS(m, fm)
  } else {
    m <- readRDS(fm)
    # m <- sdmTMB:::update_version(m)
    # browser()
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
    # grid <- filter(grid, year %in% c(sort(unique(m$data$year))))
    grid <- filter(grid, year %in% sort(union(m$data$year, m$extra_time)))
  }


  if (file.exists(pfn) & file.exists(i0)) {
    p <- readRDS(pfn)
  } else {
    p <- predict(m, re_form_iid = NA, newdata = grid, return_tmb_object = TRUE)

    map_density(p, pfn, variable = "density_trimmed") +
      labs(title = paste0(species, ": total biomass"
                          #(", dens_model_name, ")"
                          ),
           # subtitle = paste0("Variance explained:", TODO: r2_total$R2[1])
      )

    ggsave(paste0("stock-specific/", spp, "/figs/density-map-", m0, ".png"),
           height = fig_height, width = fig_width
    )

    # browser()

    plot_index(p, species, "Total", dens_model_name, i0) +
      ggtitle(paste0(species, ": total biomass (", dens_model_name, ")"))

    # ggsave(paste0("stock-specific/", spp, "/figs/density-index-", m0, ".png"),
    #   height = fig_height / 2, width = fig_width / 1.5
    # )
  }

  if (!file.exists(i0)) {
    plot_index(p, species, "Total", dens_model_name, i0) +
      ggtitle(paste0(species, ": total biomass (", dens_model_name, ")"))

    # ggsave(paste0("stock-specific/", spp, "/figs/density-index-", m0, ".png"),
    # height = fig_height / 2, width = fig_width /1.5
    # )
  }

  # # not working with offset
  # g <- ggeffects::ggeffect(m, paste0("log_depth [",
  #   range(d$log_depth)[1], ":", range(d$log_depth)[2], "by=0.05]"))
  # plot(g)


  # certain model formulation functions (like formula() and terms() ) grab the ENTIRE global environment
  # need to purge previous models before building new ones
  set_spatial <- as.list(m[["spatial"]])
  set_spatiotemporal <- as.list(m[["spatiotemporal"]])
  set_family <- m$family

  # browser()
  rm(m, p)

  ## mature female model ----

  d2 <- d1 %>%
    # filter(year > 2001) %>%
    filter(!is.na(group_catch_est))

  if(only_sampled){
    # # test pattern without extrapolating to years without annual survey-specific proportions
    d2 <- d2 %>%
      filter(n_events_sampled > set_min_sample_number) %>%
      filter(!is.na(median_prop_ann))
  }

  which_surv <- which_surveys(d2) %>%
    filter(group_name %in% c("Females", "Mature females")) %>%
    filter(  #round(prop_pos, 2) >= 0.01 &
      prop_pos >= 0.01 &
        max_pos_by_year >= 3 &
        # round(max_prop_pos, 2) >= 0.05 &
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

  ggsave(paste0("stock-specific/", spp, "/figs/density-mesh-",
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
                   # extra_time = extra_years2,
                   extra_time = sdmTMB:::find_missing_time(d2$year),
                   # extra_time = sdmTMB:::find_missing_time(data$year),
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
                   # extra_time = extra_years2,
                   extra_time = sdmTMB:::find_missing_time(d2$year),
                   # extra_time = sdmTMB:::find_missing_time(data$year),
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
    # mf <- sdmTMB:::update_version(mf)
    if (!all(sanity(mf, gradient_thresh = 0.005))) {
      mf <- refine_model(mf, alternate_family = set_family2, use_priors = set_priors)
    }
    saveRDS(mf, fmf)
  }


  # TODO: add R2 once it's working for delta models
  # r2_mf <- r2.sdmTMB(mf)
  # browser()
  if (file.exists(pmfn) & file.exists(i1)) {
    pf <- readRDS(pmfn)
  } else {
    # pf <- predict(mf, re_form_iid = NA,
    #               newdata = filter(grid, year > 2001),
    #               return_tmb_object = TRUE)

    pf <- predict(mf,
                  re_form_iid = NA, # only needed if random intercepts
                  # newdata = filter(grid, year %in% sort(unique(mf$data$year))),
                  newdata = filter(grid, year %in% sort(union(mf$data$year, mf$extra_time))),
                  return_tmb_object = TRUE
    )

    map_density(pf, pmfn) +
      labs(title = paste0(species, ": mature female biomass"
      # (", dens_model_name, ")"
      ))

    ggsave(paste0("stock-specific/", spp, "/figs/density-map-", m1, ".png"),
           height = fig_height, width = fig_width
    )
  }

  if (!file.exists(i1)) {
    plot_index(pf, species, "Mature female", dens_model_name, i1) +
      ggtitle(paste0(species, ": mature female biomass (", dens_model_name, ")"))

    # ggsave(paste0("stock-specific/", spp, "/figs/density-index-", m1, ".png"),
    # height = fig_height / 2, width = fig_width / 1.5
    # )
  }

  ## currently sticking with m values
  # set_spatial <- as.list(mf[["spatial"]])
  # set_spatiotemporal <- as.list(mf[["spatiotemporal"]])
  # set_family <- mf$family

  rm(mf, pf, d1, d2)


  ## mature male model ----

  # browser()

  d2b <- d %>%
    filter(group_name %in% c("Males", "Mature males")) %>%
    # filter(group_name == "Mature males") %>%
    # filter(year > 2001) %>%
    filter(!is.na(group_catch_est))

  if(only_sampled){
    # # test pattern without extrapolating to years without annual survey-specific proportions
    d2b <- d2b %>%
      filter(n_events_sampled > set_min_sample_number) %>%
      filter(!is.na(median_prop_ann))
  }

  which_surv <- which_surveys(d2b) %>%
    filter(group_name %in% c("Males", "Mature males")) %>%
    filter(#round(prop_pos, 2) >= 0.01 &
      prop_pos >= 0.01 &
        max_pos_by_year >= 3 &
        # round(max_prop_pos, 2) >= 0.05 &
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

  ggsave(paste0("stock-specific/", spp, "/figs/density-mesh-",
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
      labs(title = paste0(species, ": mature male biomass"
      # (", dens_model_name, ")"
      ))

    ggsave(paste0("stock-specific/", spp, "/figs/density-map-", m2, ".png"),
           height = fig_height, width = fig_width
    )
  }

  if (!file.exists(i2)) {
    plot_index(pm, species, "Mature male", dens_model_name, i2) +
      ggtitle(paste0(species, ": mature male biomass (", dens_model_name, ")"))

    # ggsave(paste0("stock-specific/", spp, "/figs/density-index-", m2, ".png"),
    # height = fig_height / 2, width = fig_width / 1.5
    # )
  }


  # set_spatial <- as.list(mm[["spatial"]])
  # set_spatiotemporal <- as.list(mm[["spatiotemporal"]])
  # set_family <- mm$family

  rm(mm, pm, d2b)

  ## immature model ----

  # if maturity split used, but too few immatures to model
  if(spp == "shortraker-rockfish"){
    maturity_possible <- FALSE
  }

  # browser()

  if(maturity_possible) {

    d3 <- d %>%
      filter(group_name %in% c("Immature")) %>%
      # filter(year > 2001) %>%
      filter(!is.na(group_catch_est))

    if(only_sampled){
      # # test pattern without extrapolating to years without annual survey-specific proportions
      d3 <- d3 %>%
        filter(n_events_sampled > set_min_sample_number) %>%
        filter(!is.na(median_prop_ann))
    }

    which_surv <- which_surveys(d3) %>%
      filter(group_name %in% c("Immature")) %>%
      filter(#round(prop_pos, 2) >= 0.01 &
        prop_pos >= 0.01 &
          max_pos_by_year >= 3 &
          # round(max_prop_pos, 2) >= 0.05 &
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
    ggsave(paste0("stock-specific/", spp, "/figs/density-mesh-",
                  spp,
                  "-imm-",
                  dens_model_name,
                  ".png"), width = 14, height = 14)


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
                          # priors = set_priors,
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
                         # priors = set_priors,
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
      # mi <- sdmTMB:::update_version(mi)
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
      # s <- TRUE
      # s$sigmas_ok <- TRUE # ignore small sigma issues
      # s$all_ok <- TRUE # ignore small sigma issues

      # browser()

      if (all(s)) {
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
            labs(title = paste0(species, ": immature biomass"
            # (", dens_model_name, ")"
            ))

          ggsave(paste0("stock-specific/", spp, "/figs/density-map-", m3, ".png"),
                 height = fig_height, width = fig_width
          )
        }
      }
    }
  }

  if (!file.exists(i3)) {
    try(plot_index(pi, species, "Immature", dens_model_name, i3) +
          ggtitle(paste0(species, ": immature biomass (", dens_model_name, ")")))

    # ggsave(paste0("stock-specific/", spp, "/figs/density-index-", m3, ".png"),
    # height = fig_height / 2, width = fig_width / 1.5
    # )
  }

  ind0 <- readRDS(i0) %>% mutate(index = "Total")
  ind1 <- readRDS(i1) %>% mutate(index = "Mature female")
  ind2 <- readRDS(i2) %>% mutate(index = "Mature male")
  try(ind3 <- readRDS(i3) %>% mutate(index = "Immature"))

  bc_inds <- bind_rows(ind0, ind1, ind2)
  try(bc_inds <- bind_rows(ind0, ind1, ind2, ind3))

  # browser()
  # bc_inds <- bc_inds %>% mutate(model_string = dens_model_name)

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

  # p1
  # ggsave(paste0("stock-specific/", spp, "/figs/density-index-", spp, "-all",
  # dens_model_name, "-", knot_distance, "-km.png"),
  # height = fig_height / 2, width = fig_width / 1.5
  # )

  # Generate split indices ----
  fsi <- paste0(
    "stock-specific/", spp, "/output/", "density-split-ind/", spp, "-split-",
    dens_model_name, "-", knot_distance, "-km.rds"
  )

  if (!file.exists(fsi)) {
    # browser()
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

  # browser()
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

  ggsave(paste0("stock-specific/", spp, "/figs/density-all-indices-", spp, "-",
    dens_model_name, "-", knot_distance, "-km.png"
  ),
  height = fig_height, width = fig_width
  )
}

make_all_sdms()

## and then repeat using only surveys with specimen data ----
# ## this only affects maturity specific models
make_all_sdms(only_sampled = TRUE)
