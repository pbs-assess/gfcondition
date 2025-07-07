# condition models
library(tidyverse)
library(sdmTMB)
library(ggsidekick)
# remotes::install_github("seananderson/ggeffects", ref = "sdmTMB")
devtools::load_all(".")

# load overall species list
source("analysis/00-species-list.R")

# # # # or override with custom subset
# species_list <- list(
# "North Pacific Spiny Dogfish"
# )

index_list <- expand.grid(species = species_list,
                          maturity = c("imm",
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

# do both density agnostic and dependent versions
index_list2 <- index_list
index_list2$add_density <- TRUE
index_list <- bind_rows(index_list, index_list2)

# Function for generating condition indices ----

calc_condition_indices <- function(species, maturity, males, females, add_density) {

  source("R/refine-condition-models.R", local = TRUE)

  ## if not using function
  # species <- "Rex Sole"
  # maturity <- "mat"
  # males <- FALSE
  # females <- TRUE
  # add_density <- FALSE
  # add_density <- TRUE

  rm(m)
  # this only works if called to local environment
  set_utm_crs <- 32609

  # stop_early <- TRUE
  stop_early <- FALSE

  # if one wanted to exclude years with too few samples...
  # currently not doing so, as RW st fields may benefit from any available data
  min_yr_count <- NULL

  mat_threshold <- 0.5
  knot_distance <- dist <- 20
  fig_height <- 4 * 2
  fig_width <- 5 * 2
  delta_dens_model <- TRUE

  dens_model_total <- "dln-ss5-2024-09" # this is for total
  dens_model_name1 <- "dln-ss5-split-2024-09" # these are `all catches' models
  dens_model_name2 <- "dln-only-sampled-2024-09" # these are `sampled catches' models

  theme_set(ggsidekick:::theme_sleek())

  spp <- gsub(" ", "-", gsub("\\/", "-", tolower(species)))

  add_density <- add_density
  mat_class <- maturity
  just_males <- males
  just_females <- females

  message(paste(species, maturity,
              if(maturity=="mat"){ifelse(just_males, "males", "females")},
              ifelse(add_density, "with density", "")
              ))

  # Check which density models to use ----
  id0 <- readRDS(paste0("data-generated/density-index/",
                        dens_model_total, "/total/i-",
                        spp, "-total-", dens_model_total, "-",
                        knot_distance,
                        "-km.rds"))

  if1 <- readRDS(paste0("data-generated/density-index/",
                        dens_model_name1, "/mat-fem/i-",
                        spp, "-mat-fem-", dens_model_name1, "-",
                        knot_distance,
                        "-km.rds"))

  if2 <- readRDS(paste0("data-generated/density-index/",
                        dens_model_name2, "/mat-fem/i-",
                        spp, "-mat-fem-", dens_model_name2, "-",
                        knot_distance,
                        "-km.rds"))
  im1 <- readRDS(paste0("data-generated/density-index/",
                        dens_model_name1, "/mat-m/i-",
                        spp, "-mat-m-", dens_model_name1, "-",
                        knot_distance,
                        "-km.rds"))

  im2 <- readRDS(paste0("data-generated/density-index/",
                        dens_model_name2, "/mat-m/i-",
                        spp, "-mat-m-", dens_model_name2, "-",
                        knot_distance,
                        "-km.rds"))

  # in case imm isn't available
  ii1 <- NULL
  ii2 <- NULL

  try(ii1 <- readRDS(paste0("data-generated/density-index/",
                            dens_model_name1, "/imm/i-",
                            spp, "-imm-", dens_model_name1, "-",
                            knot_distance,
                            "-km.rds")))

  try(ii2 <- readRDS(paste0("data-generated/density-index/",
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

  dir.create(paste0("data-generated/compare-models/"),
             showWarnings = FALSE)
  saveRDS(itest, paste0("data-generated/compare-models/",
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
  dir.create(paste0("data-generated/condition-data-w-dens/"),
             showWarnings = FALSE)

  f <- paste0("data-generated/condition-data-w-dens/",
              spp, "-", mat_class, "-condition-best-model.rds")

  if (!file.exists(f)) {
    ds <- readRDS(paste0("data-generated/condition-data-black-swan/",
                         spp, "-mat-", mat_threshold, "-condition.rds")) %>%
      ungroup() %>%
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

      mdf <- readRDS(paste0("data-generated/density-models/",
                            dens_model_name,"/mat-fem/",
                           spp, "-mat-fem-", dens_model_name, "-",
                           knot_distance,
                           "-km.rds"))
      mdm <- readRDS(paste0("data-generated/density-models/",
                            dens_model_name,"/mat-m/",
                           spp, "-mat-m-", dens_model_name, "-",
                           knot_distance,
                           "-km.rds"))


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

      fmi <- paste0("data-generated/density-models/",
                    dens_model_name,"/imm/",
                    spp, "-imm-", dens_model_name, "-",
                    knot_distance,
                    "-km.rds")

      if(file.exists(fmi)){
      md <- readRDS(fmi)
      } else {
        return(NA)
      }

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

  # # fix for IPHC because of sample size imbalance over time and
  # # uncertainty about how weight was measured
  # # currently excluded from analysis
  # if(species == "Pacific Halibut"){
  #   d2 <- filter(d2, survey_abbrev != "IPHC FISS")
  # }

  # Select relevant data and grid ----
  if (mat_class == "mat") {
    if (just_males) {

      pf <- paste0(
        "data-generated/density-predictions/p-", spp,
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
          "data-generated/density-predictions/p-", spp,
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
          "data-generated/density-predictions/p-", spp,
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
        "data-generated/density-predictions/p-", spp,
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
            prop_density = density / sum_density
          ) %>%
          ungroup() %>%
          group_by(year, survey) %>%
          mutate(
            survey_density = sum(density),
            prop_density_by_survey = density / survey_density
          )

      } else {
        warning(paste("No density predictions for immature ", species, "densities."))
        return(NA)
      }
    } else {
      pf <- paste0(
        "data-generated/density-predictions/p-", spp,
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
               prop_density = density / sum_density
        ) %>%
          ungroup() %>%
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
      "data-generated/density-predictions/p-", spp,
      "-mat-fem-", dens_model_name, "-", knot_distance,  "-km.rds"
    )) %>%
      select(year, X, Y, survey, depth, days_to_solstice, log_depth, density) %>%
      rename(
        density_f = density
      )

    gridBm <- readRDS(paste0(
      "data-generated/density-predictions/p-", spp,
      "-mat-m-", dens_model_name, "-", knot_distance,  "-km.rds"
    )) %>%
      select(year, X, Y, survey, depth, days_to_solstice, log_depth, density) %>%
      rename(
        density_m = density
      )

    gridB <- full_join(gridBf, gridBm) %>% mutate(density = density_f + density_m)

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
      ungroup()

    } else{

      gridB <- readRDS(paste0(
        "data-generated/density-predictions/p-", spp,
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
  sg <- d %>%
    group_by(survey_group) %>%
    summarise(n = n()) |>
    filter(n > 30) |>
    mutate(
      species = species,
      group = group_label,
      survey_group = fct_reorder(droplevels(survey_group), .x = n, .fun = sum, .desc = TRUE))

  dir.create(paste0("data-generated/specimen-counts/"), showWarnings = FALSE)
  saveRDS(sg, paste0("data-generated/specimen-counts/", spp, "-", group_tag, ".rds"))

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
    model_name <- "2024-09" # all survey groups

    dir.create(paste0("data-generated/condition-models-",
                      group_tag, "/"),
               showWarnings = FALSE)
    dir.create(paste0("data-generated/cond-index/"),
               showWarnings = FALSE)

  dir.create(paste0("data-generated/condition-models-",
                    group_tag, "/", model_name, "/"),
             showWarnings = FALSE)
  dir.create(paste0("data-generated/cond-index/", model_name, "/"),
             showWarnings = FALSE)


  mf <- paste0(
    "data-generated/condition-models-", group_tag, "/", model_name, "/", spp, "-c-",
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

  rm(m) # just in case it was left in the global environment

  # if commented out, will be forced to rerun model
  if (file.exists(mf)) {
    try(m <- readRDS(mf))
  }

  if (!exists("m")) {

    # TODO: Add DOY with cyclic smoother if enough year-round commercial data ever available

    try(m <- sdmTMB(cond_formula,
      weights = d$sample_multiplier,
      mesh = mesh,
      data = d,
      spatial = "on",
      spatiotemporal = "rw",
      extra_time = sdmTMB:::find_missing_time(d$year),
      share_range = FALSE,
      time = "year",
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
                      extra_time = sdmTMB:::find_missing_time(d$year),
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

  if(!all(unlist(s[1:7]))){
    warning(paste(species, maturity,
                     if(maturity=="mat"){ifelse(just_males, "males", "females")},
                     "model did not converge"))
    return(NA)
  }

  # Add density dependence to model ----
  if (add_density) {
    d$log_density_c <- d$log_density - mean(d$log_density, na.rm = TRUE)

    # option to weight particular samples differently
    # but not needed when weighting the index by density
    d$sample_multiplier <- 1
    model_name <- "2024-09-doy-ld0c"

    dir.create(paste0("data-generated/condition-models-", group_tag,
                      "/", model_name, "/"), showWarnings = FALSE)

    dir.create(paste0("data-generated/cond-index/", model_name, "/"),
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
    } else {
      cond_formula2 <- cond_fac ~ survey_group +
        poly(days_to_solstice, 2) +
        log_density_c
    }
    }

    mesh2 <- make_mesh(d, c("X", "Y"), cutoff = 15)

    # Model (with density) ----
    mf2 <- paste0(
      "data-generated/condition-models-", group_tag, "/", model_name, "/",
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
                              dist = knot_distance)
      saveRDS(m2, mf2)

    s <- sanity(m2, gradient_thresh = 0.005)
    m2
    m2$sd_report
    tidy(m2, "ran_pars", conf.int = TRUE)

    if(!all(unlist(s[1:7]))){
      warning(paste(species, maturity,
                       if(maturity=="mat"){ifelse(just_males, "males", "females")},
                       "model with density did not converge."))
      m <- readRDS(mf)
    } else {
    ## check if effect of density is negative by more than the SE on the estimate
    ## if not, revert to m1 for index generation
    t <- tidy(m2, conf.int = TRUE)

    if(t$estimate[t$term == "log_density_c"] < - t$std.error[t$term == "log_density_c"]){
    ## alternative approach could be to include all -ve estimates for upr CI
    ## would be more conservative than current approach
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

  grid$survey_group <- levels(sg$survey_group)[1]

  sort(unique(m$data$year))
  sort(unique(grid$year))

  # might be redundant
  grid <- filter(grid, year %in% sort(union(unique(m$data$year), m$extra_time)))


  # Has this model been run before? ----

  i1 <- paste0(
    "data-generated/cond-index/", model_name,
    "/cond-index-", group_tag, "-", spp, "-",
    model_name, "-", knot_distance, "-km.rds"
  )

  if (!file.exists(i1)) {

  pc <- predict(m, newdata = grid, return_tmb_object = TRUE)


  p2 <- pc$data %>%
    mutate(cond = exp(est))


  # filter to plot only cells representing 99% of mean predicted biomass
  # cells must be defined by "X", "Y", time by "year", and biomass/abundance stored as "density"
  p2 <- trim_predictions_by_year(p2, 0.001)


  # Map model predictions ----

  dset <- readRDS(paste0("data-generated/all-sets-used.rds")) %>%
      filter(year >= min(m$data$year), year <= max(m$data$year)) %>%
    filter(
      species_common_name == tolower(species)&
      !is.na(longitude)&!is.na(latitude)) %>%
    sdmTMB::add_utm_columns(.,
                            ll_names = c("longitude", "latitude"),
                            utm_crs = set_utm_crs)
  unique(dset$survey_abbrev)

  set_list <- dset %>%
    select(fishing_event_id, longitude, latitude, X, Y, year, catch_weight, catch_count) %>%
    distinct() %>%
    mutate(
      fishing_event_id = as.factor(fishing_event_id),
      lon = longitude, lat = latitude)

  model_dat <- d %>%
    group_by(fishing_event_id) %>%
    mutate(
      fishing_event_id = as.factor(fishing_event_id),
      count = n()
    )

  model_dat <- left_join(set_list, model_dat, multiple = "all") %>% mutate(
    density = group_catch_weight,
    caught = ifelse(catch_count > 0 | catch_weight > 0, 1, 0),
    count = ifelse(is.na(count), 0, count),
    present = ifelse(count > 0, 1, ifelse(caught == 1, 0, NA_integer_))
  ) %>% filter(caught == 1)

  # model_dat %>% group_by(present, caught) %>% summarise(n = n()) %>% View()
  p2$log_cond <- log(p2$cond)
  p2 <- p2 %>% mutate(cond_trim = ifelse(cond > quantile(p2$cond, 0.99),
    quantile(p2$cond, 0.99), cond
  ))
  g <- plot_predictions(p2, model_dat,
    # extrapolate_depth = FALSE,
    # fill_column = "log_cond",
    fill_column = "cond_trim",
    fill_label = "Condition \nfactor",
    pt_column = "count",
    pt_label = "Fish \nsampled",
    pt_size_range = c(0.5, 4),
    pos_pt_fill = NA,
    bin_pt_col = "black",
    pos_pt_col = "red",
    # x_buffer = c(-0, 0),
    # y_buffer = c(-0, 0),
    fill_scale =
      ggplot2::scale_fill_viridis_c(),
    # ggplot2::scale_fill_viridis_c(trans = "log10"),
    bounds = grid,
    rotation_angle = 30, show_raw_data = TRUE
  )

  g <- g + facet_wrap(~year, ncol = 7) +
    ggtitle(paste0(species, ": ", group_label, " ", model_name))

  dir.create(paste0("figs/cond-", model_name, "/"), showWarnings = FALSE)
  ggsave(paste0("figs/cond-", model_name, "/condition-map-wide-", spp, "-", group_tag, "-",
                model_name, "-", knot_distance, "-km.png"),
         height = fig_height*1.5, width = fig_width
  )

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
  }

  ggplot(ind2, aes(year, est)) +
    geom_line() +
    geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.4) +
    xlab("Year") +
    ylab("Predicted average condition factor") +
    labs(title = paste0(species, ": ", group_label, " ", model_name)#,
         # subtitle = paste("conditional R2:", r2$r2[1], "marginal R2:", r2$r2[2])
         )

  ggsave(paste0("figs/cond-", model_name, "/", spp, "-",
                group_tag, "-cond-index-", model_name, "-", knot_distance, "-km.png"),
    height = fig_height / 2, width = fig_width / 2
  )

  # plot_covariates <- TRUE
  plot_covariates <- FALSE

  if (plot_covariates) {

    nd <- data.frame(
      days_to_solstice = seq(min(d$days_to_solstice),
                             max(d$days_to_solstice),
                             length.out = 50
      ),
      survey_group = levels(sg$survey_group)[1],
      log_density_c = 0,
      ann_log_density_c = 0,
      dens_dev = 0,
      year = 2021L # a chosen year
    )
    pd <- predict(m, newdata = nd, se_fit = TRUE, re_form = NA)

    ggplot(pd, aes(days_to_solstice, exp(est),
                   ymin = exp(est - 1.96 * est_se),
                   ymax = exp(est + 1.96 * est_se)
    )) +
      geom_line() +
      geom_ribbon(alpha = 0.4) +
      scale_x_continuous() +
      coord_cartesian(expand = F) +
      labs(x = "Days to solstice", y = "condition") +
      ggtitle(paste0(species, ": ", group_label, " ", model_name))

    ggsave(paste0("figs/cond-", model_name, "/effect-plot-doy-", spp, "-",
                  group_tag, "-", model_name, "-", knot_distance, "-km.png"),
           height = fig_height / 2, width = fig_width / 2
    )

    # Effect plots ----
    if (add_density) {
      nd2 <- data.frame(
        days_to_solstice = 0,
        survey_group = levels(sg$survey_group)[1],
        log_density_c = seq(min(d$log_density_c),
                            max(d$log_density_c),
                            length.out = 50
        ),
        ann_log_density_c = 0,
        dens_dev = 0,
        year = 2021L # a chosen year
      )
      pd <- predict(m, newdata = nd2, se_fit = TRUE, re_form = NA)

      ggplot(pd, aes(log_density_c, exp(est),
                     ymin = exp(est - 1.96 * est_se),
                     ymax = exp(est + 1.96 * est_se)
      )) +
        geom_line() +
        geom_ribbon(alpha = 0.4) +
        scale_x_continuous() +
        coord_cartesian(expand = F) +
        labs(x = "log_density_c", y = "condition") +
        ggtitle(paste0(species, ": ", group_label, " ", model_name))

    }

    ggsave(paste0("figs/cond-", model_name, "/effect-plot-density-", spp, "-",
                  group_tag, "-", model_name, "-", knot_distance, "-km.png"),
           height = fig_height / 2, width = fig_width / 2
    )

    }
  }
}

# Run with pmap -----

# index_list <- index_list[2, ]
# pmap(index_list, calc_condition_indices)

# Run with furrr ----

library(gfcondition)
library(future)

is_rstudio <- !is.na(Sys.getenv("RSTUDIO", unset = NA))
is_unix <- .Platform$OS.type == "unix"
cores <- round(parallel::detectCores() / 2)
(cores <- parallel::detectCores() - 6L)
if (!is_rstudio && is_unix) plan(multicore, workers = cores) else plan(multisession, workers = cores)


furrr::future_pmap(index_list, calc_condition_indices)
