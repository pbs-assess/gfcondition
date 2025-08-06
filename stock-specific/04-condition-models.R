# condition models
library(tidyverse)
library(sdmTMB)
library(ggsidekick)
devtools::load_all(".")

# source("stock-specific/00-set-options.R")

index_list <- expand.grid(maturity = c("imm",
                                       "mat"),
                          males = c(TRUE,
                                    FALSE)
                          ) %>%
  mutate(
    females = ifelse(males == FALSE & maturity == "mat", TRUE, FALSE),
    males = ifelse(maturity == "imm", FALSE, males)
  ) %>%
  distinct()

index_list$add_density <- FALSE
# index_list$add_density <- TRUE

# # do both density agnostic and dependent versions
index_list2 <- index_list
index_list2$add_density <- TRUE
index_list <- bind_rows(index_list, index_list2)

# Function for generating condition indices ----

calc_condition_indices <- function(maturity, males, females, add_density, get_mvn_sims = TRUE) {

  # source("stock-specific/00-set-options.R")
  source("R/refine-model-functions.R", local = TRUE)

  ## internal testing options
  # species <- "Rex Sole"
  # maturity <- "mat"
  # males <- FALSE
  # females <- TRUE
  # add_density <- FALSE
  # add_density <- TRUE
  # stop_early <- TRUE
  stop_early <- FALSE

  rm(m)
  # this only works if called to local environment


  dist <- knot_distance
  fig_height <- 8
  fig_width <- 10


  # spp <- gsub(" ", "-", gsub("\\/", "-", tolower(species)))
  spp <- gsub(" ", "-", gsub("\\/", "-", tolower(stock_name)))

  add_density <- add_density
  mat_class <- maturity
  just_males <- males
  just_females <- females

  message(paste(species, maturity,
              if(maturity=="mat"){ifelse(just_males, "males", "females")},
              ifelse(add_density, "with density", "")
              ))

  # Check which density models to use ----
  id0 <- readRDS(paste0("stock-specific/", spp, "/output/density-index/",
                        dens_model_total, "/total/i-",
                        spp, "-total-", dens_model_total, "-",
                        knot_distance,
                        "-km.rds"))

  if1 <- readRDS(paste0("stock-specific/", spp, "/output/density-index/",
                        dens_model_name1, "/mat-fem/i-",
                        spp, "-mat-fem-", dens_model_name1, "-",
                        knot_distance,
                        "-km.rds"))

  if2 <- readRDS(paste0("stock-specific/", spp, "/output/density-index/",
                        dens_model_name2, "/mat-fem/i-",
                        spp, "-mat-fem-", dens_model_name2, "-",
                        knot_distance,
                        "-km.rds"))
  im1 <- readRDS(paste0("stock-specific/", spp, "/output/density-index/",
                        dens_model_name1, "/mat-m/i-",
                        spp, "-mat-m-", dens_model_name1, "-",
                        knot_distance,
                        "-km.rds"))

  im2 <- readRDS(paste0("stock-specific/", spp, "/output/density-index/",
                        dens_model_name2, "/mat-m/i-",
                        spp, "-mat-m-", dens_model_name2, "-",
                        knot_distance,
                        "-km.rds"))

  # in case imm isn't available
  ii1 <- NULL
  ii2 <- NULL

  try(ii1 <- readRDS(paste0("stock-specific/", spp, "/output/density-index/",
                            dens_model_name1, "/imm/i-",
                            spp, "-imm-", dens_model_name1, "-",
                            knot_distance,
                            "-km.rds")))

  try(ii2 <- readRDS(paste0("stock-specific/", spp, "/output/density-index/",
                            dens_model_name2, "/imm/i-",
                            spp, "-imm-", dens_model_name2, "-",
                            knot_distance,
                            "-km.rds")))

  # id1 <- bind_rows(if1,im1,ii1) %>% group_by(year, model_string) %>% summarise(est = sum(est))
  # id2 <- bind_rows(if2,im2,ii2) %>% group_by(year, model_string) %>% summarise(est = sum(est))
  #
  # ggplot(id0, aes(year, est)) + geom_line() +
  #   geom_line(data = id1, colour= "blue") +
  #   geom_line(data = id2, colour="red")

  isplit <- bind_rows(if1,im1,ii1) %>% bind_rows(., if2,im2,ii2) %>%
    group_by(year, model_string) %>% summarise(combined = sum(est))

  id0 <- id0 %>% select(-model_string)
  iall <- left_join(isplit, id0)

  iall <- iall %>% mutate(diff = combined - est,
                          lwr_diff = combined - lwr,
                          too_low = ifelse(lwr_diff < 0, lwr_diff, 0),
                          upr_diff = combined - upr,
                          too_high = ifelse(upr_diff > 0, upr_diff, 0)
  )

  ggplot(iall, aes(year, combined, colour = model_string)) + geom_line() +
    geom_line(aes(year, est), colour = "black")

  itest <- iall %>% group_by(species, model_string) %>%
    summarise(total_diff = sum(abs(diff)),
              mean_est = mean(est),
              sum_est = sum(est),
              mean_diff = mean(diff/est),
              prop_diff = total_diff/sum(est),
              too_low = sum(too_low),
              too_high = sum(too_high),
              total_ci_error = sum(abs(too_low)+too_high),
              prop_ci_error = total_ci_error/sum_est
    )

  itest$model_total <- dens_model_total

  dir.create(paste0("stock-specific/", spp, "/output/compare-models/"),
             showWarnings = FALSE)
  saveRDS(itest, paste0("stock-specific/", spp, "/output/compare-models/",
                        spp, "-relative-to-", dens_model_total, "-",
                        knot_distance,
                        "-km.rds"))

  itest <- itest %>%
    filter(prop_ci_error < 0.1)

  if(nrow(itest) == 1){
    dens_model_name <- itest$model_string
  } else {
    if(nrow(itest) == 0) { return(paste(species, "doesn't have split indices",
      "that cumulatively exceed CIs by more than 10% of total cumulative biomass."))
    } else {
      dens_model_name <- itest[itest$total_diff==min(itest$total_diff),]$model_string
    }
  }

  # Load condition data and attach lagged density estimates ----

  f <- paste0("stock-specific/", spp, "/output/condition-data-w-dens-",
              spp, "-", mat_class, "-", dens_model_name, ".rds")

  if (!file.exists(f)) {
    ds <- readRDS(paste0("stock-specific/", spp, "/output/condition-data-",
                         spp, "-mat-", mat_threshold, ".rds")) %>%
      ungroup() %>%
      # TODO: move the following 20 lines to data prep?
      mutate(
        log_depth_c = log_depth - 5,
        DOY = as.numeric(strftime(time_deployed, format = "%j")),
        days_to_solstice = DOY - 172
      )

    ds <- ds %>% mutate(
      survey_group = as.factor(
        case_when(
          survey_abbrev %in% c("HBLL OUT N", "HBLL OUT S")~"HBLL",
          survey_abbrev %in% c("MSSM QCS", "MSSM WCVI")~"MSSM",
          survey_abbrev %in% c("HS PCOD", "THORNYHEAD", "HS MSA", "SYN HS", "SYN QCS", "SYN WCHG", "SYN WCVI")~"TRAWL",
          survey_series_id == 68~"HAKE",
          TRUE~survey_abbrev
        ))
    )

    # ds$year_smooth <- ds$year
    ds <- ds %>% filter(!is.na(depth_m))
    ds <- ds %>% filter(!is.na(days_to_solstice))
    ds <- ds %>% filter(!is.na(latitude))
    ds <- ds %>% filter(!is.na(longitude))

    # sg <- ds %>%
    #   group_by(survey_group) %>%
    #   summarise(n = n()) |>
    #   mutate(survey_group = fct_reorder(survey_group, .x = n, .desc = TRUE))

    ds$X <- NULL
    ds$Y <- NULL
    ds2 <- sdmTMB::add_utm_columns(ds,
                                   ll_names = c("longitude", "latitude"),
                                   utm_crs = set_utm_crs)

    nd0 <- ds2 %>%
      select(year, survey_abbrev, fishing_event_id, X, Y, log_depth) %>%
      mutate(
        survey_group = "TRAWL",
        survey_type = "SYN",
        days_to_solstice = 0,
        log_depth_c = log_depth - 5
      ) # can't predict prior to first year of density model

    if(mat_class == "mat") {

      mdf <- readRDS(paste0("stock-specific/", spp, "/output/density-models/",
                            dens_model_name,"/mat-fem/",
                           spp, "-mat-fem-", dens_model_name, "-",
                           knot_distance,
                           "-km.rds"))
      mdm <- readRDS(paste0("stock-specific/", spp, "/output/density-models/",
                            dens_model_name,"/mat-m/",
                           spp, "-mat-m-", dens_model_name, "-",
                           knot_distance,
                           "-km.rds"))

      # mdf <- sdmTMB:::update_version(mdf)
      # mdm <- sdmTMB:::update_version(mdm)

      nd0 <- nd0 %>% filter(year >= max(min(mdf$data$year),min(mdm$data$year)))
      # can't predict prior to first year of density model

      pd0f <- predict(mdf, newdata = nd0)
      pd0m <- predict(mdm, newdata = nd0)

      if (length(mdf$family$family)>1) {
        pd0f <- pd0f %>% mutate(
          density = mdf$family[[1]]$linkinv(est1) * mdf$family[[2]]$linkinv(est2)
        )
      } else {
        pd0f <- pd0f %>% mutate(density = exp(est))
      }

      if (length(mdm$family$family)>1) {
        pd0m <- pd0m %>% mutate(
          density = mdm$family[[1]]$linkinv(est1) * mdm$family[[2]]$linkinv(est2)
        )
      } else {
        pd0m <- pd0m %>% mutate(density = exp(est))
      }

      pd0 <- pd0f
      pd0$density <- pd0f$density + pd0m$density

    } else{

      dens_model_total <- dens_model_name

      fmi <- paste0("stock-specific/", spp, "/output/density-models/",
                    dens_model_name,"/imm/",
                    spp, "-imm-", dens_model_name, "-",
                    knot_distance,
                    "-km.rds")

      if(file.exists(fmi)){
      md <- readRDS(fmi)
      } else {
        return(NA)
      }
      # md <- sdmTMB:::update_version(md)

      nd0 <- nd0 %>% filter(year >= min(md$data$year))
      # can't predict prior to first year of density model

      pd0 <- predict(md, newdata = nd0)

      if (length(md$family$family)>1) {
        pd0 <- pd0 %>% mutate(
          density = md$family[[1]]$linkinv(est1) * md$family[[2]]$linkinv(est2)
        )
      } else {
        pd0 <- pd0 %>% mutate(density = exp(est))
      }
    }

    pd0 <- pd0 %>%
      mutate(
        log_density = log(density),
        model = dens_model_name
      ) %>%
      select(
        year, survey_abbrev, fishing_event_id, X, Y, log_depth,
        density, log_density, model
      ) %>%
      distinct()

    d2 <- left_join(ds2, pd0)
    saveRDS(d2, f)
  } else {
    d2 <- readRDS(f)
  }

  # temporary fix for IPHC because of sample size imbalance over time and
  # uncertainty about how weight was measured
  if(species == "Pacific Halibut"){
    d2 <- filter(d2, survey_abbrev != "IPHC FISS")
  }

  # Select relevant data and grid ----
  if (mat_class == "mat") {
    if (just_males) {

      pf <- paste0(
        "stock-specific/", spp, "/output/density-predictions/p-", spp,
        "-mat-m-", dens_model_name, "-", knot_distance,  "-km.rds"
      )
      if (file.exists(pf)) {
        d <- d2 %>% filter(group_name == "Mature males")
        group_tag <- "mat-m"
        group_label <- "mature males"

        # get current year density to scale condition index with
        gridA <- readRDS(pf) %>%
          select(year, X, Y, survey, depth, days_to_solstice, log_depth, density) %>%
          group_by(year) %>%
          mutate(
            sum_density = sum(density),
            prop_density = density / sum_density
          ) %>%
          ungroup() %>%
          group_by(year, survey) %>%
          mutate(
            survey_density = sum(density),
            prop_density_by_survey = density / survey_density
          )
      } else {
        warning(paste("No density predictions for mature male ", species, "densities."))
        return(NA)
      }
    } else {
      if (just_females) {
        pf <- paste0(
          "stock-specific/", spp, "/output/density-predictions/p-", spp,
          "-mat-fem-", dens_model_name, "-", knot_distance,  "-km.rds"
        )
        if (file.exists(pf)) {
          d <- d2 %>% filter(group_name == "Mature females")
          group_tag <- "mat-fem"
          group_label <- "mature females"

          # get current year density to scale condition index with
          gridA <- readRDS(pf) %>%
            select(year, X, Y, survey, depth, days_to_solstice, log_depth, density) %>%
            group_by(year) %>%
            mutate(
              sum_density = sum(density),
              prop_density = density / sum_density
            ) %>%
            ungroup() %>%
            group_by(year, survey) %>%
            mutate(
              survey_density = sum(density),
              prop_density_by_survey = density / survey_density
            )
        } else {
          warning(paste("No density predictions for mature female ", species, "densities."))
          return(NA)
        }
      } else {
        pf <- paste0(
          "stock-specific/", spp, "/output/density-predictions/p-", spp,
          "-all-mat-", dens_model_name, "-", knot_distance,  "-km.rds"
        )
        if (file.exists(pf)) {
          d <- d2 %>%
            filter(group_name %in% c("Mature females", "Mature males")) %>%
            rename(group_catch_weight_split = group_catch_weight)
          d3 <- d %>%
            group_by(fishing_event_id, group_name) %>%
            select(fishing_event_id, group_name, group_catch_weight_split) %>%
            distinct() %>%
            ungroup() %>%
            group_by(fishing_event_id) %>%
            summarise(group_catch_weight = sum(group_catch_weight_split))
          d <- left_join(d, d3)
          group_tag <- "mat"
          group_label <- "mature (females and males)"

          # get current year density to scale condition index with
          gridA <- readRDS(pf) %>%
            select(year, X, Y, survey, depth, days_to_solstice, log_depth, density) %>%
            group_by(year) %>%
            mutate(sum_density = sum(density),
                   prop_density = density / sum_density
            ) %>%
            ungroup() %>%
            group_by(year, survey) %>%
            mutate(survey_density = sum(density), prop_density_by_survey = density / survey_density)
        } else {
          warning(paste("No density predictions for all mature ", species, "densities."))
          return(NA)
        }
      }
    }
  } else {
    if (mat_class == "imm") {
      pf <- paste0(
        "stock-specific/", spp, "/output/density-predictions/p-", spp,
        "-imm-", dens_model_name, "-", knot_distance,  "-km.rds"
      )
      if (file.exists(pf)) {
        d <- d2 %>% filter(group_name %in% c("Immature"))
        group_tag <- "imm"
        group_label <- "immatures"

        # get current year density to scale condition index with
        gridA <- readRDS(pf) %>%
          select(year, X, Y, survey, depth, days_to_solstice, log_depth, density) %>%
          group_by(year) %>%
          mutate(
            sum_density = sum(density),
            prop_density = density / sum_density#,
            # log_density = log(density),
            # ann_log_density = mean(log_density)
          ) %>%
          ungroup() %>%
          # mutate(dens_dev = log_density - ann_log_density) %>%
          group_by(year, survey) %>%
          mutate(
            survey_density = sum(density),
            prop_density_by_survey = density / survey_density
          )

        # browser()
      } else {
        warning(paste("No density predictions for immature ", species, "densities."))
        return(NA)
      }
    } else {
      pf <- paste0(
        "stock-specific/", spp, "/output/density-predictions/p-", spp,
        "-total-", dens_model_total, "-", knot_distance,  "-km.rds"
      )
      if (file.exists(pf)) {
        # model everything together
        d <- d2

        # get current year density to scale condition index with
        gridA <- readRDS(pf) %>%
          select(year, X, Y, survey, depth, days_to_solstice, log_depth, density) %>%
          group_by(year) %>%
        mutate(sum_density = sum(density),
               prop_density = density / sum_density#,
               # log_density = log(density),
               # ann_log_density = mean(log_density)
        ) %>%
          ungroup() %>%
          # mutate(dens_dev = log_density - ann_log_density
          #      ) %>%
          group_by(year, survey) %>%
          mutate(survey_density = sum(density),
                 prop_density_by_survey = density / survey_density)
        ungroup()
      } else {
        warning(paste("No density predictions for total ", species, "densities."))
        return(NA)
      }
    }
  }

  # Add density covariates to grid ----
  if (add_density) {

    if(mat_class == "mat") {
    # load lagged density predictions for full survey grid if going to be used as covariates condition
    gridBf <- readRDS(paste0(
      "stock-specific/", spp, "/output/density-predictions/p-", spp,
      "-mat-fem-", dens_model_name, "-", knot_distance,  "-km.rds"
    )) %>%
      select(year, X, Y, survey, depth, days_to_solstice, log_depth, density) %>%
      rename(
        density_f = density
      )

    gridBm <- readRDS(paste0(
      "stock-specific/", spp, "/output/density-predictions/p-", spp,
      "-mat-m-", dens_model_name, "-", knot_distance,  "-km.rds"
    )) %>%
      select(year, X, Y, survey, depth, days_to_solstice, log_depth, density) %>%
      rename(
        density_m = density
      )

    gridB <- full_join(gridBf, gridBm) %>% mutate(density = density_f + density_m)

    # browser()

    gridB <- gridB |>
      select(year, X, Y, survey, depth, days_to_solstice, log_depth, density, density_f, density_m) |>
      group_by(year) |>
      mutate(#sum_density = sum(density),
             #prop_density = density / sum_density,
             sum_density_f = sum(density_f),
             prop_density_f = density_f / sum_density_f,
             sum_density_m = sum(density_m),
             prop_density_m = density_m / sum_density_m
      ) |>
      ungroup() |>
      group_by(X, Y) |>
      mutate(
        log_density = log(density),
        cell_mean_density = mean(density),
        log_mean_density = log(cell_mean_density)
      ) |>
      ungroup() |>
      group_by(year, survey) |>
      mutate(survey_density = sum(density),
             prop_density_by_survey = density / survey_density) |>
      ungroup()

    } else{

      gridB <- readRDS(paste0(
        "stock-specific/", spp, "/output/density-predictions/p-", spp,
        "-imm-", dens_model_name, "-", knot_distance,  "-km.rds"
      )) |>
        select(year, X, Y, survey, depth, days_to_solstice, log_depth, density) |>
        group_by(year) |>
        mutate(sum_density = sum(density),
               prop_density = density / sum_density
        ) |>
        ungroup() |>
        group_by(X, Y) |>
        mutate(
          log_density = log(density),
          cell_mean_density = mean(density),
          log_mean_density = log(cell_mean_density)
        ) |>
        ungroup() |>
        group_by(year, survey) |>
        mutate(survey_density = sum(density),
               prop_density_by_survey = density / survey_density) |>
        ungroup()
    }
  }

  # Make mesh ----
  hist(d$cond_fac)
  hist(log(d$cond_fac))

  d <- d %>% filter(year >= min(gridA$year))

  if(!is.null(min_yr_count)) {
  dy <- d %>%
    group_by(year) %>%
    summarise(n = n()) |>
    filter(n > min_yr_count)

  d <- d %>% filter(year %in% dy$year)
  }

  # include only survey groups with > 30 specimens
  sg <- d %>%
    group_by(survey_group) %>%
    summarise(n = n()) |>
    filter(n > 30) |>
    mutate(
      species = species,
      group = group_label,
      survey_group = fct_reorder(droplevels(survey_group), .x = n, .fun = sum, .desc = TRUE))

  if(!is.null(cond_min_sample_count)) {
    # include only survey groups with > 3 samples
    groups_w_samples <- d %>%
      select(sample_id, survey_group) %>%
      distinct() %>%
      group_by(survey_group) %>%
      summarise(n = n()) |>
      filter(n > cond_min_sample_count) |>
      mutate(survey_group = fct_reorder(droplevels(survey_group), .x = n, .fun = sum, .desc = TRUE))

    sg <- filter(sg, survey_group %in% groups_w_samples$survey_group)
    sg$survey_group <- factor(sg$survey_group, levels = levels(groups_w_samples$survey_group))

  }

  dir.create(paste0("stock-specific/", spp, "/output/specimen-counts/"), showWarnings = FALSE)
  saveRDS(sg, paste0("stock-specific/", spp, "/output/specimen-counts/", spp, "-", group_tag, ".rds"))

  if(stop_early) {
    return(itest)
  }

  d <- d %>% filter(survey_group %in% unique(sg$survey_group))

  d$survey_group <- factor(d$survey_group, levels = levels(sg$survey_group))

  mesh <- make_mesh(d, c("X", "Y"), cutoff = knot_distance)

  ggplot() +
    inlabru::gg(mesh$mesh) +
    coord_fixed() +
    geom_point(aes(X, Y, size = total_weight), data = d2) +
    geom_point(aes(X, Y, colour = group_catch_weight), data = d) +
    facet_wrap(~year) +
    scale_color_viridis_c(trans = ggsidekick::fourth_root_power_trans())

  ggplot() +
    inlabru::gg(mesh$mesh) +
    coord_fixed() +
    geom_point(aes(X, Y, size = group_catch_weight), data = d2) +
    geom_point(aes(X, Y, colour = log(cond_fac)), size = 0.5, data = d) +
    facet_wrap(~year) +
    scale_color_gradient2()

  # Estimate condition model ----

    d$sample_multiplier <- 1

    sysdate <- unlist(strsplit(as.character(Sys.Date()), "-"))

    model_name <- paste0(cond_model_prefix, sysdate[1], "-", sysdate[2], "")

    dir.create(paste0("stock-specific/", spp, "/output/condition-models/"),
               showWarnings = FALSE)
    dir.create(paste0("stock-specific/", spp, "/output/cond-index/"),
               showWarnings = FALSE)

  dir.create(paste0("stock-specific/", spp, "/output/condition-models/", model_name, "/"),
             showWarnings = FALSE)
  dir.create(paste0("stock-specific/", spp, "/output/cond-index/", model_name, "/"),
             showWarnings = FALSE)


  mf <- paste0(
    "stock-specific/", spp, "/output/condition-models/", model_name, "/", spp, "-c-",
    group_tag, "-", model_name, "-", knot_distance, "-km.rds"
  )

  if (diff(range(d$days_to_solstice))<60) {
    if (length(unique(d$survey_group)) == 1) {
    cond_formula <- cond_fac ~ 1 + days_to_solstice
    } else {
    cond_formula <- cond_fac ~ survey_group + days_to_solstice
    }
  } else {
    if (length(unique(d$survey_group)) == 1) {
    cond_formula <- cond_fac ~ 1 + poly(days_to_solstice, 2)
    } else {
    cond_formula <- cond_fac ~ survey_group +
      poly(days_to_solstice, 2)
    }
  }

  # Model (density-agnostic) ----

  check_for_duplicates <- d[duplicated(d$specimen_id), ]

  if(nrow(check_for_duplicates)>0){
    stop(paste(species, "has duplicate specimen ids."))
  }

  rm(m) # just in case it was left in the global environment

  # if commented out, will be forced to rerun model
  if (file.exists(mf)) {
    try(m <- readRDS(mf))
  }

  if (!exists("m")) {

    # TODO: Add DOY with cyclic smoother if we add in commercial data?

    try(m <- sdmTMB(cond_formula,
      weights = d$sample_multiplier,
      mesh = mesh,
      data = d,
      spatial = "on",
      spatiotemporal = "rw",
      # spatiotemporal = "off",
      extra_time = sdmTMB:::find_missing_time(d$year),
      share_range = FALSE,
      # silent = FALSE,
      time = "year",
      # reml = TRUE,
      # family = student(df = 5),
      # control = sdmTMBcontrol(newton_loops = 1L),
      ## priors don't work with anisotropy
      # priors = sdmTMBpriors(
      #   matern_s = pc_matern(range_gt = knot_distance,
      #                        sigma_lt = 2),
      #   matern_st = pc_matern(range_gt = knot_distance,
      #                         sigma_lt = 2)
      # ),
      anisotropy = TRUE,
      family = lognormal(link = "log")
    ))

    if (!exists("m")) {

      try(m <- sdmTMB(cond_formula,
                      weights = d$sample_multiplier,
                      mesh = mesh,
                      data = d,
                      spatial = "on",
                      spatiotemporal = "rw",
                      # spatiotemporal = "off",
                      extra_time = sdmTMB:::find_missing_time(d$year),
                      # share_range = FALSE,
                      # silent = FALSE,
                      time = "year",
                      priors = sdmTMBpriors(
                        matern_s = pc_matern(range_gt = knot_distance,
                                             sigma_lt = 2),
                        matern_st = pc_matern(range_gt = knot_distance,
                                              sigma_lt = 2)
                      ),
                      family = lognormal(link = "log")
      ))

    }
    if (!exists("m")) {
    warning(paste(species, maturity,
                if(maturity=="mat"){ifelse(just_males, "males", "females")},
                "model failed completely"))
      return(NA)
    } else {

    m <- refine_cond_model(m, set_formula = cond_formula,
                            dist = knot_distance)
    saveRDS(m, mf)
    }
  } else {

    ## make sure if converged last time
    m <- refine_cond_model(m,
                          set_formula = cond_formula,
                          dist = knot_distance)
    saveRDS(m, mf)
  }

  m
  m$sd_report
  t <- tidy(m, "ran_pars", conf.int = TRUE)
  s <- sanity(m, gradient_thresh = 0.005)

  # p <- get_pars(m)
  # rho <- 2 * plogis(p$rho_time_unscaled) - 1
  # rho

  if(!all(s)){
    warning(paste(species, maturity,
                     if(maturity=="mat"){ifelse(just_males, "males", "females")},
                     "model did not converge"))
    return(NA)
  }

  # browser()
  # Add density dependence to model ----
  ## don't do this for now, but can be used to explore utility of covariates
  if (add_density) {
    d$log_density_c <- d$log_density - mean(d$log_density, na.rm = TRUE)

      d$sample_multiplier <- 1

      model_name <- paste0(model_name, "-ld0c")

    dir.create(paste0("stock-specific/", spp, "/output/condition-models/", model_name, "/"), showWarnings = FALSE)

    dir.create(paste0("stock-specific/", spp, "/output/cond-index/", model_name, "/"),
               showWarnings = FALSE)

    if (diff(range(d$days_to_solstice))<60) {
      if (length(unique(d$survey_group)) == 1) {
        cond_formula2 <- cond_fac ~ 1 +
          days_to_solstice + log_density_c
      } else {
        cond_formula2 <- cond_fac ~ survey_group +
          days_to_solstice + log_density_c
      }
    } else {
      if (length(unique(d$survey_group)) == 1) {
      cond_formula2 <- cond_fac ~ 1 +
        poly(days_to_solstice, 2) +
        log_density_c
        # poly(log_density_c, 2)
        # ann_log_density_c + dens_dev
        # poly(ann_log_density_c, 2) + poly(dens_dev, 2)
        # poly(log_density_lag1_c, 2)
    } else {
      cond_formula2 <- cond_fac ~ survey_group +
        poly(days_to_solstice, 2) +
        log_density_c
      # poly(log_density_c, 2)
        # ann_log_density_c + dens_dev
        # poly(ann_log_density_c, 2) + poly(dens_dev, 2)
        # # poly(log_density_lag1_c, 2)
    }
    }

    mesh2 <- make_mesh(d, c("X", "Y"), cutoff = 15)

    # Model (with density) ----

    mf2 <- paste0(
      "stock-specific/", spp, "/output/condition-models/", model_name, "/",
      spp, "-c-", group_tag, "-", model_name, "-", knot_distance, "-km.rds"
    )

    if (file.exists(mf2)) {
      try(m2 <- readRDS(mf2))
    }

    if (!exists("m2")) {
      try(m2 <- update(m, cond_formula2,
        weights = d$sample_multiplier,
        spatial = "on",
        spatiotemporal = "rw",
        share_range = FALSE,
        # control = sdmTMBcontrol(upper = list(b_j = bx)),
        mesh = mesh2,
        data = d
      ))
    }

    if (!exists("m2")) {
      try(m2 <- update(m, cond_formula2,
                       weights = d$sample_multiplier,
                       spatial = "on",
                       spatiotemporal = "rw",
                       share_range = TRUE,
                       mesh = mesh2,
                       data = d
      ))
    }

    if (!exists("m2")) {

      warning(paste(species, maturity,
                  if(maturity=="mat"){ifelse(just_males, "males", "females")},
                  "model with density failed completely."))
      m <- readRDS(mf)
    } else {
      rm(m)
      saveRDS(m2, mf2)
      m2 <- refine_cond_model(m2, set_formula = cond_formula2,
                              # upper_limits = bx,
                              dist = knot_distance)
      saveRDS(m2, mf2)

    s <- sanity(m2, gradient_thresh = 0.005)
    m2
    m2$sd_report
    tidy(m2, "ran_pars", conf.int = TRUE)

    if(!all(s)){
      warning(paste(species, maturity,
                       if(maturity=="mat"){ifelse(just_males, "males", "females")},
                       "model with density did not converge."))
      m <- readRDS(mf)
    } else {
    ## check if effect of density is negative by more than the SE on the estimate
    ## if not, revert to m1 for index generation
    t <- tidy(m2, conf.int = TRUE)

    if(t$estimate[t$term == "log_density_c"] < - t$std.error[t$term == "log_density_c"]){
    ## maybe this approach would make more sense?
    # if(t$conf.high[t$term == "log_density_c"] < 0){
      m <- m2
    } else {
      warning(paste(species, maturity,
                  if(maturity=="mat"){ifelse(just_males, "males", "females")},
                  "model with density converged, but effect potentially positive."))
      m <- readRDS(mf)
    }
  }
    }
  }

  # Filter grid ----
  if (add_density) {
    grid <- gridB |> mutate(
      # cell mean across years, means that annual variation removed
      # while retaining spatial variability in condition due to density
      # - mean centres it same as predictor variable
      log_density_c = log_mean_density - mean(d$log_density, na.rm = TRUE)
      )
    if(mat_class == "mat") {
      if (just_males) {
      grid <- grid |> mutate(prop_density = prop_density_m)
      }else {
      grid <- grid |> mutate(prop_density = prop_density_f)
      }
    }
  } else {
    grid <- gridA
  }

  grid <- filter(grid, survey %in% survey_grids)
  grid$survey_group <- levels(sg$survey_group)[1]

  sort(unique(m$data$year))
  sort(unique(grid$year))

  # might be redundant
  grid <- filter(grid, year %in% sort(union(unique(m$data$year), m$extra_time)))


  # Check R2

  # browser()
  # sanity(m)
  # m
  # m$sd_report
  # tidy(m, conf.int = TRUE)
  # tidy(m, "ran_pars", conf.int = TRUE)

  # (r2 <- r2.sdmTMB(m))
  #r2$r2[1]

  # Has this model been run before? ----

  i1 <- paste0(
    "stock-specific/", spp, "/output/cond-index/", model_name,
    "/cond-index-", group_tag, "-", spp, "-",
    model_name, "-", knot_distance, "-km.rds"
  )

  dir.create(paste0("stock-specific/", spp, "/output/cond-pred/"),
             showWarnings = FALSE)
  dir.create(paste0("stock-specific/", spp, "/output/cond-pred/", model_name,"/"),
             showWarnings = FALSE)

  pf <- paste0(
    "stock-specific/", spp, "/output/cond-pred/", model_name,
    "/cond-pred-", group_tag, "-", spp, "-",
    model_name, "-", knot_distance, "-km.rds"
  )

  if (!file.exists(i1)|!file.exists(pf)) {

  pc <- predict(m, newdata = grid, return_tmb_object = TRUE)

  p2 <- pc$data %>%
    # filter(!(year == 2020)) %>%
    mutate(cond = exp(est))

  saveRDS(p2, pf)
}

  # Get coastwide index ----

  if (!file.exists(i1)) {

    ind2 <- get_index(pc, area = grid$prop_density, bias_correct = TRUE)
    ind2$species <- species
    ind2$group <- group_label
    ind2$model_string <- model_name
    ind2$dens_model_name <- dens_model_name
    ind2$survey_group <- levels(sg$survey_group)[1]

    saveRDS(ind2, i1)
  } else {
    ind2 <- readRDS(i1)
  #
  #   ind2$species <- species
  #   ind2$group <- group_label
  #   ind2$model_string <- model_name
  #
  #   saveRDS(ind2, i1)
  }

  ggplot(ind2, aes(year, est)) +
    geom_line() +
    geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.4) +
    xlab("Year") +
    ylab("Predicted average condition factor") +
    labs(title = paste0(species, ": ", group_label, " ", model_name)#,
         # subtitle = paste("conditional R2:", r2$r2[1], "marginal R2:", r2$r2[2])
         )

  .ggsave(paste0("stock-specific/", spp, "/figs",
                "/condition-index-", spp, "-",
                group_tag, "-", model_name, "-", knot_distance, "-km.png"),
    height = fig_height / 2, width = fig_width / 2
  )

  # Get MVN simulation samples ----
  if (get_mvn_sims) {
    psims <- predict(m, newdata = grid, nsim = 100)
    isims <- get_index_sims(psims, area = grid$prop_density, return_sims = TRUE)


    dir.create(paste0("stock-specific/", spp, "/output/cond-index-sims/"), showWarnings = FALSE)
    dir.create(paste0("stock-specific/", spp, "/output/cond-index-sims/", model_name, "/"), showWarnings = FALSE)

    saveRDS(isims, paste0(
      "stock-specific/", spp, "/output/cond-index-sims/", model_name,
      "/cond-index-sims-", group_tag, "-", spp, "-",
      model_name, "-", knot_distance, "-km.rds"
    ))
  }


  # Get survey-specific condition indices ----
  if(split_index_by_survey & length(survey_grids)>1) {


    dir.create(paste0("stock-specific/", spp, "/output/cond-index-by-survey/"), showWarnings = FALSE)

    dir.create(paste0("stock-specific/", spp, "/output/cond-index-by-survey/", model_name,"/"), showWarnings = FALSE)

    i2 <- paste0("stock-specific/", spp, "/output/cond-index-by-survey/", model_name,
               "/survey-cond-indices-", spp, "-", group_tag, "-", model_name, "-", knot_distance, "-km.rds")

  if(!file.exists(i2)) {

  ## TODO: if we want to split by stock area, will need to edit grid first to add species specific stock areas
  preds <- grid %>%
    split(.$survey) %>%
    lapply(function(x) predict(m, newdata = x, return_tmb_object = TRUE))

  inds <- purrr::map_dfr(preds, function(.x)
    get_index(.x, area = .x$data$prop_density_by_survey, bias_correct = TRUE), .id = "region")

  survey_years <- m$data %>% select(survey_abbrev, year) %>% distinct() %>%
    mutate(region = ifelse(survey_abbrev == "HS MSA", "SYN HS",
                    ifelse(survey_abbrev == "MSSM QCS", "SYN QCS",
                    ifelse(survey_abbrev == "MSSM WCVI", "SYN WCVI",
                    ifelse(survey_abbrev == "HBLL OUT S", "SYN WCVI",
                    ifelse(survey_abbrev == "HBLL OUT N", "SYN WCHG",
                           survey_abbrev))))))

  ind3 <- left_join(survey_years, inds, multiple = "all") %>%
    filter(!is.na(est))

  ind3$species <- species
  ind3$group <- group_label
  ind3$model_string <- model_name
  ind3$dens_model_name <- dens_model_name
  ind3$survey_group <- levels(sg$survey_group)[1]
  saveRDS(ind3, i2)

  } else {
  ind3 <- readRDS(i2)
  ind3$species <- species
  ind3$group <- group_label
  ind3$model_string <- model_name
  ind3$dens_model_name <- dens_model_name
  ind3$survey_group <- levels(sg$survey_group)[1]
  saveRDS(ind3, i2)
  }

  ggplot(ind3, aes(year, est, fill = region)) +
    geom_line(aes(colour = region)) +
    geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.1) +
    xlab("Year") +
    ylab("Predicted average condition factor") +
    labs(title = paste0(species, ": ", group_label, " ", "-", model_name))

  .ggsave(paste0("stock-specific/", spp, "/figs",
                "/condition-index-", spp, "-split-",
                group_tag, "-", model_name, "-", knot_distance, "-km.png"),
         height = fig_height / 2, width = fig_width/1.5
  )
  }
}

if(!use_parallel) {
# Run with pmap -----

pmap(index_list, calc_condition_indices)

} else {
# Run with furrr ----

library(gfcondition)
library(future)

is_rstudio <- !is.na(Sys.getenv("RSTUDIO", unset = NA))
is_unix <- .Platform$OS.type == "unix"
cores <- round(parallel::detectCores() / 2)
(cores <- parallel::detectCores() - 6L)
if (!is_rstudio && is_unix) plan(multicore, workers = cores) else plan(multisession, workers = cores)

furrr::future_pmap(index_list, calc_condition_indices)
}
