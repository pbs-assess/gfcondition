# 1. Calculate Le Cren’s relative condition factor for each fish sampled
# 2. Remove the most extreme outliers that are likely errors
# 3. Split samples into immature and mature and into length bins
# 4. Calculate ‘weights' (sample multiplier for each fish sampled)
library(tidyverse)
library(gfplot)

# load overall species list
source("analysis/00-species-list.R")

# species_list <- list(
# #   "Sablefish"
# # #   "North Pacific Spiny Dogfish"
# # # # "Slender Sole",
# "Pacific Halibut"
# )

# species_list <- list(species = species_list[27:length(species_list)])
species_list <- list(species = species_list)

get_condition_data <- function(species){

# browser()

update_m <- FALSE
# update_m <- TRUE

## code checking within function
# species <- "Arrowtooth Flounder"
# species <- "Petrale Sole"
# species <- "Pacific Cod"
# species <- "Walleye Pollock"
# species <- "Slender Sole"
# species <- "Pacific Halibut"

# mat_threshold  <-  0.05
mat_threshold <- 0.5
# mat_threshold <- 0.95

spp <- gsub(" ", "-", gsub("\\/", "-", tolower(species)))

# # # Load data ----
dat <- readRDS("data-generated/all-samples-used.rds") %>%
filter(!is.na(longitude), !is.na(latitude),
       species_common_name == tolower(species)
       )

# if(species != "Sablefish"){
  ## remove the combined version of the sablefish survey (only version retained currently)
  ## because this survey is at different time of year than all the others
  dat <- filter(dat, !(survey_abbrev %in% c("SABLE")))
# }


check_for_duplicates <- dat[duplicated(dat$specimen_id), ]

if(nrow(check_for_duplicates)>0){ stop(paste(species, "has duplicate specimen ids."))}

sort(unique(dat$survey_abbrev))

dset <- readRDS("data-generated/all-sets-used.rds") %>%
  filter(species_common_name == tolower(species))
# sort(unique(dset$survey_abbrev))
# unique(select(dset, usability_code, usability_desc)) %>% view()


#TODO: temporary because false zeros were in an older data pull
dat$catch_count <- ifelse(dat$catch_weight > 0 & dat$catch_count == 0, NA, dat$catch_count)
dat$catch_weight <- ifelse(dat$catch_count > 0 & dat$catch_weight == 0, NA, dat$catch_weight)


datf <- filter(dat, !is.na(length))
# datf <- filter(datf, !is.na(weight))
# datf <- filter(datf, year > 2002)

datf <- filter(datf, !is.na(survey_abbrev))
unique(datf$survey_abbrev)
unique(dat$survey_abbrev)


# TODO: decide which surveys to include, for now including all data available
# Have there been any measurement changes or are there differences between HBLL and synoptic in lengths or maturity keys?

fish <- dat


## not needed because only the more common length type populates the length variable
## alternates are included only in their own columns
# dat <- filter(dat, length_type = which.max(table(dat$length_type)))
## but include this check, just in case there's a problem
if(length(unique(fish$length_type))>1){stop("Stop. Two different length types.")}

ggplot(
  filter(fish, !is.na(latitude) & !is.na(longitude)),
  aes(longitude, latitude, colour = survey_abbrev)
) +
  geom_point() +
  facet_wrap(~year)

# 2. Split samples into immature and mature and into length bins ----
# Starting with code lifted from split by maturity function in case we also want to functionalize this
# arguments required:
split_by_maturity <- TRUE
split_by_sex <- TRUE
immatures_pooled <- TRUE

## might be worth getting the same model from the density split

if(!update_m){
# dss <- readRDS(paste0("data-generated/split-catch-data/", spp, ".rds"))
# m <- dss$m
m <- readRDS(paste0("data-generated/split-catch-data/", spp, ".rds"))$m
}

p_threshold <- mat_threshold
custom_maturity <- NULL
custom_length_threshold <- NULL

if(species=="North Pacific Spiny Dogfish") {
  custom_maturity <- c(NA, 55)
}

if (species == "Sandpaper Skate") {
  # Perez 2005
  custom_length_threshold <- c(49.2, 46.7)
}

if (species == "Big Skate") {
  # # McFarlane and King 2006 -- shouldn't be relied on
  # Ebert et al. 2008
  custom_length_threshold <- c(102.9, 113.1)
}
if (species == "Longnose Skate") {
  # # McFarlane and King 2006
  # custom_length_threshold <- c(65, 83)
  # Arrington 2020
  custom_length_threshold <- c(85, 102)
}

if (species == "Spotted Ratfish") {
  # King and McPhie 2015
  custom_length_threshold <- c(30.2, 39.3)
}

if (species == "Shortraker Rockfish") {
  # McDermott 1994:  Hutchinson 2004 F 44.9
  # Conrath 2017 for female,
  # Haldorson and Love reported 47 but based on
  # Westrheim, 1975 for both sexes = 45
  custom_length_threshold <- c(45, 49.9)
}

# if (species == "Pacific Halibut") {
#   # Takada 2017 for females, males less precise.. btw 70-79
#   # no accurate values for males available so wont use for now
#   custom_length_threshold <- c(70, 96.7)
# }


## could use separate estimates for each year
# year_re <- TRUE
## discovered that petrale length at maturity was unusually high in WCVI 2004 and 2006
year_re <- FALSE
sample_id_re <- TRUE

# if(species == "Pacific Halibut") {
# year_re <- FALSE
# sample_id_re <- FALSE
# # fish <- filter(fish, maturity_code != 7 | is.na(maturity_code))
# }

# -----
# does maturity data exist at all for this species?
# browser()
if (split_by_maturity) {
  maturity_codes <- unique(fish$maturity_code)


  if (length(maturity_codes) < 3&is.null(custom_length_threshold)) {
    return("Fewer than 3 maturity codes. Please provide custom length thresholds from literature.")
    # return(list(data = survey_sets, maturity = NULL, weight_model = NULL))
  }

  # Check if only some years without maturity data, and set year_re = FALSE in that case
  years_w_maturity <- fish %>%
    group_by(year) %>%
    mutate(maturity_levels = length(unique(maturity_code)))

  levels_per_year <- unique(years_w_maturity$maturity_levels)


  if (max(levels_per_year) < 3) { # NA plus only one recorded maturity level is max ever recorded
    warning("Maturity data not recorded, so catch not split.", call. = FALSE)
    # return(list(data = survey_sets, model = NULL))
  }

  if (min(levels_per_year) < 3) { # some years lack maturity data

    years_w_maturity %>%
      select(year, maturity_levels) %>%
      distinct() %>%
      arrange(year)
    # fish <- filter(fish, year >= 2002)
    years_w_maturity <- filter(fish, year >= 2002) %>%
      group_by(year) %>%
      mutate(maturity_levels = length(unique(maturity_code)))

    levels_per_year <- unique(years_w_maturity$maturity_levels)
  }

  f_fish <- fish %>%
    filter(sex == 2) %>%
    mutate(year_f = as.character(year))

  m_fish <- fish %>%
    filter(sex == 1) %>%
    mutate(year_f = as.character(year))

  if (min(levels_per_year) < 3) { # some years lack maturity data
    if (length(levels_per_year) < 1) { # TODO: check if this threshold should actually be 2
      warning("Maturity data not recorded, so catch not split.", call. = FALSE)
      # return(list(data = survey_sets, model = NULL))
    } else {
      warning("Some years lack maturity data, but catch still split.", call. = FALSE)

      if(!is.null(custom_length_threshold)){
        f_fish$threshold <- custom_length_threshold[2]
        m_fish$threshold <- custom_length_threshold[1]
      } else {

        if(update_m) {
          m <- fit_mat_ogive(fish,
                             type = "length",
                             sample_id_re = sample_id_re,
                             usability_codes = NULL,
                             custom_maturity_at = custom_maturity,
                             year_re = FALSE
          )

          # browser()
          # gfplot::plot_mat_ogive(m)
        }

      if (p_threshold == 0.5) {
        f_fish$threshold <- m$mat_perc$f.p0.5
        m_fish$threshold <- m$mat_perc$m.p0.5
      }
      if (p_threshold == 0.05) {
        f_fish$threshold <- m$mat_perc$f.p0.05
        m_fish$threshold <- m$mat_perc$m.p0.05
      }
      if (p_threshold == 0.95) {
        f_fish$threshold <- m$mat_perc$f.p0.95
        m_fish$threshold <- m$mat_perc$m.p0.95
      }
      }
    }
  } else {
    if (year_re) {
      # sample_id_re <- FALSE

      if(!is.null(custom_length_threshold)){
        f_fish$threshold <- custom_length_threshold[2]
        m_fish$threshold <- custom_length_threshold[1]
      } else {

      if(update_m) {
      m <- fit_mat_ogive(fish,
        type = "length",
        sample_id_re = sample_id_re,
        usability_codes = NULL,
        custom_maturity_at = custom_maturity,
        year_re = TRUE
      )

      gfplot::plot_mat_annual_ogives(m)
      }

      if (p_threshold == 0.5) {
        f_fish$threshold <- lapply(f_fish$year_f, function(x) m$mat_perc[[x]]$f.p0.5)
        m_fish$threshold <- lapply(m_fish$year_f, function(x) m$mat_perc[[x]]$m.p0.5)
      }
      if (p_threshold == 0.05) {
        f_fish$threshold <- lapply(f_fish$year_f, function(x) m$mat_perc[[x]]$f.p0.05)
        m_fish$threshold <- lapply(m_fish$year_f, function(x) m$mat_perc[[x]]$m.p0.05)
      }
      if (p_threshold == 0.95) {
        f_fish$threshold <- lapply(f_fish$year_f, function(x) m$mat_perc[[x]]$f.p0.95)
        m_fish$threshold <- lapply(m_fish$year_f, function(x) m$mat_perc[[x]]$m.p0.95)
      }
      }
    } else {
      if(!is.null(custom_length_threshold)){
        f_fish$threshold <- custom_length_threshold[2]
        m_fish$threshold <- custom_length_threshold[1]
      } else {

      if(update_m) {
      m <- fit_mat_ogive(fish,
        type = "length",
        sample_id_re = sample_id_re,
        usability_codes = NULL,
        custom_maturity_at = custom_maturity
      )
      }
      # apply global estimates to all catches
      if (p_threshold == 0.5) {
        f_fish$threshold <- m$mat_perc$f.p0.5
        m_fish$threshold <- m$mat_perc$m.p0.5
      }
      if (p_threshold == 0.05) {
        f_fish$threshold <- m$mat_perc$f.p0.05
        m_fish$threshold <- m$mat_perc$m.p0.05
      }
      if (p_threshold == 0.95) {
        f_fish$threshold <- m$mat_perc$f.p0.95
        m_fish$threshold <- m$mat_perc$m.p0.95
      }
      # # or could choose to save sample_id estimates and apply them like this
      # if(p_threshold == 0.5) {
      #   f_fish$threshold <- lapply(f_fish$sample_id, function(x) m$mat_perc[[x]]$f.p0.5)
      #   m_fish$threshold <- lapply(m_fish$sample_id, function(x) m$mat_perc[[x]]$m.p0.5)
      # }
      # if(p_threshold == 0.05) {
      #   f_fish$threshold <- lapply(f_fish$sample_id, function(x) m$mat_perc[[x]]$f.p0.05)
      #   m_fish$threshold <- lapply(m_fish$sample_id, function(x) m$mat_perc[[x]]$m.p0.05)
      # }
      # if(p_threshold == 0.95) {
      #   f_fish$threshold <- lapply(f_fish$sample_id, function(x) m$mat_perc[[x]]$f.p0.95)
      #   m_fish$threshold <- lapply(m_fish$sample_id, function(x) m$mat_perc[[x]]$m.p0.95)
      # }
      }
    }
  }

  if(spp %in% c(" ")){
    f_fish <- mutate(f_fish, mature = 1)
    m_fish <- mutate(m_fish, mature = 1)
  } else {
  # classify each fish as immature or mature based on above thresholds
  f_fish <- mutate(f_fish, mature = if_else(length >= threshold, 1, 0, missing = NULL))
  m_fish <- mutate(m_fish, mature = if_else(length >= threshold, 1, 0, missing = NULL))
  }

  # get unsexed immature fish
  imm_fish <- fish %>%
    filter(!(sex %in% c(1, 2)) &
      length < min(c(
        min(unique(unlist(f_fish$threshold)), na.rm = TRUE),
        min(unique(unlist(m_fish$threshold)), na.rm = TRUE)
      ), na.rm = TRUE)) %>%
    mutate(
      mature = 0,
      year_f = as.character(year)
    )

  # create groups
  if (split_by_sex) {
    if (immatures_pooled) {
      # since not splitting by sex for immatures, the unsexed imm can be added on
      fish_groups <- bind_rows(f_fish, m_fish, imm_fish) %>%
        mutate(group_name = ifelse(mature == 1,
          paste("Mature", ifelse(sex == 1, "males", "females")),
          "Immature"
        ))
    } else {
      fish_groups <- rbind(f_fish, m_fish) %>%
        mutate(group_name = ifelse(mature == 1,
          paste("Mature", ifelse(sex == 1, "males", "females")),
          paste("Immature", ifelse(sex == 1, "males", "females"))
        ))
    }
  } else {
    fish_groups <- rbind(f_fish, m_fish, imm_fish) %>%
      mutate(group_name = ifelse(mature == 1, "Mature", "Immature"))
  }
} else {
  # just split by sex
  fish_groups <- rbind(f_fish, m_fish) %>%
    mutate(group_name = ifelse(sex == 1, "Males", "Females"))
}

if(update_m&is.null(custom_length_threshold)) {

  browser()

  gfplot::plot_mat_ogive(m)

  dir.create(paste0("data-generated/maturity-ogives-02/"), showWarnings = FALSE)
  saveRDS(m, paste0("data-generated/maturity-ogives-02/", spp, "-all.rds"))
}

# 2. Le Cren’s relative condition factor ----
# browser()

fish_groups <- filter(fish_groups, length > 0| is.na(length))
fish_groups <- filter(fish_groups, weight > 0| is.na(weight))

mf <- gfplot::fit_length_weight(fish_groups, sex = "female", usability_codes = NULL)
mm <- gfplot::fit_length_weight(fish_groups, sex = "male", usability_codes = NULL)

## Length-weight plot ----
gfplot::plot_length_weight(object_female = mf, object_male = mm)

## Remove black swan outliers ----

# browser()

sd_threshold <- 2
is_heavy_tail <- TRUE

# sd_threshold <- 3
# is_heavy_tail <- FALSE


filter_lw_outliers <- function(model,
                                  numsd = sd_threshold,
                                  heavy_tailed = is_heavy_tail
                                  ){
  if(heavy_tailed){
  l <- qt(0.025, 3) * exp(model$pars$log_sigma)*numsd
  u <- qt(0.975, 3) * exp(model$pars$log_sigma)*numsd
  }else{
  l <- qnorm(0.025, 0, sd = exp(model$pars$log_sigma)*numsd)
  u <- qnorm(0.975, 0, sd = exp(model$pars$log_sigma)*numsd)
  }
  model$data$resids <- log(model$data$weight) - (model$pars$log_a + model$pars$b * log(model$data$length))
  out <- model$data |> filter(!(resids > u) & !(resids < l))
  out
}

df <- filter_lw_outliers(mf)
dm <- filter_lw_outliers(mm)

df$wbar <- exp(mf$pars$log_a) * df$length^mf$pars$b
dm$wbar <- exp(mm$pars$log_a) * dm$length^mm$pars$b

# include unknown sex individuals for now, because immature individuals can be difficult to sex and differences in growth rate may be slim
du <- dplyr::filter(fish_groups, sex %in% c(0, 3), !is.na(weight), !is.na(length))
# Apply an intermediate slope and intercept to these individuals
# weight is in grams, so convert to kg
du$weight <- du$weight/1000

du$wbar <- exp((mm$pars$log_a + mf$pars$log_a) / 2) * du$length^((mm$pars$b + mf$pars$b) / 2)

dd <- bind_rows(df, dm)
dd$cond_fac <- dd$weight / dd$wbar

# hist(dd$weight)
# hist(du$weight)

du$cond_fac <- du$weight / du$wbar

dd2 <- filter(du, cond_fac >= min(dd$cond_fac) & cond_fac <= max(dd$cond_fac)) |> bind_rows(dd)

# plot(cond_fac~length, data = dd2)


# 4. Calculate ‘weights' (sample multiplier for each fish sampled) ----
ds <- dd2 %>%
  group_by(fishing_event_id, group_name) %>%
  mutate(
    log_depth = log(depth_m),
    group_sampled_weight = sum(weight, na.rm = T),
    group_num_sampled = n()
  ) %>%
  ungroup() %>%
  group_by(fishing_event_id) %>%
  mutate(
    sampled_weight = sum(weight, na.rm = T),
    num_sampled = n(),
    mean_weight = mean(weight, na.rm = T),
    # area_swept = ifelse(
    #   is.na(tow_length_m),
    #   doorspread_m * duration_min * speed_mpm,
    #   doorspread_m * tow_length_m
    #   ),
    total_weight = ifelse(
      is.na(catch_weight) & catch_count > 0, catch_count * mean_weight, ifelse(
        catch_weight > sampled_weight, catch_weight, sampled_weight
        )
      ),
    est_count = round(total_weight / mean_weight),
    est_count = ifelse(num_sampled > est_count, num_sampled, est_count),
    # catch_weight = mean(catch_weight),
    sample_multiplier_by_weight = 1 / (sampled_weight / total_weight), # this is total sampled
    # prop_n_not_sampled = 1 - (num_sampled/est_count), # this is est proportion by count not sampled
    prop_n_in_group = (group_num_sampled / num_sampled), # this is proportion by count
    prop_w_in_group = (group_sampled_weight / sampled_weight), # this is proportion by weight
    group_catch_weight = total_weight * prop_w_in_group, # this is est group catch weight
    # est_num_unsampled_group_members2 = (group_catch_weight - group_sampled_weight)/(group_sampled_weight/group_num_sampled)),
    est_num_unsampled_group_members = (est_count - num_sampled) * prop_n_in_group,
    sample_multiplier = 1 + (est_num_unsampled_group_members / group_num_sampled)
  ) %>%
  unique()

# browser()

# remove some rockfish outliers
dat <- dat %>%
  filter(!(weight > 900 & species_common_name == tolower("Pacific Sanddab")))  %>%
  filter(!(weight > 3500 & species_common_name %in% tolower(c("Yellowmouth Rockfish",
                                                              # "Widow Rockfish",
                                                              "Quillback Rockfish"))))
ds <- ds %>%
  filter(!(weight > 0.900 & species_common_name == tolower("Pacific Sanddab")))  %>%
  filter(!(weight > 3.500 & species_common_name %in% tolower(c("Yellowmouth Rockfish",
                                                              # "Widow Rockfish",
                                                              "Quillback Rockfish"))))


# # investigate results
# ds %>%
#   select(
#     year, survey_abbrev, fishing_event_id,
#     latitude, latitude_end,
#     longitude, longitude_end,
#     depth_m, log_depth,
#     usability_code, area_swept, density_kgpm2,
#     group_name, length, sex, age, weight, specimen_id,
#     wbar, cond_fac,
#     catch_weight, total_weight,
#     sampled_weight, num_sampled, group_num_sampled,
#     prop_w_in_group, group_catch_weight,
#     prop_n_in_group, est_count, est_num_unsampled_group_members, sample_multiplier
#   ) %>%
#   View()
#
# ds %>% select(fishing_event_id, year, length, sex, age, weight, specimen_id,
#               survey_abbrev, wbar, cond_fac) %>% View()
#
# plot(weight ~ length, data = dd, col = "red")
# points(weight ~ length, data = ds)
# # add female maturity in green
# abline(v = m$mat_perc$f.p0.5, col = "green")
# abline(v = m$mat_perc$mean$f.mean.p0.5, col = "green")
# # add male maturity in blue
# abline(v = m$mat_perc$m.p0.5, col = "blue")
# abline(v = m$mat_perc$mean$m.mean.p0.5, col = "blue")


# 5. Outliers plotted ----

# browser()
#
ggplot(dat |> mutate(weight = weight/1000) |> filter(
  sex %in% c(1,2)
  ), aes(length, weight)) +
  geom_point(aes(colour = year)) +
  geom_point(data = ds, colour = "white") +
  geom_point(data = ds, colour = "black", alpha = 0.4) +
  ## checking if specific unit corrections were made
  # geom_point(data = filter(dat,
  #     sample_id %in% c(554878, 406981, 537306, 443178,
  #                      # 150218, # rest of arrowtooth sample
  #                      514880, 536500, 536506, 537170, 532297)|
  #     specimen_id %in% c(
  #       # 5113159, #arrowtooth error
  #       # 12684889,
  #       # 7629026,
  #       16224012, 11018029, 11018030)),
  #     aes(length, weight/1000),
  #     colour = "green") +
  geom_vline(xintercept = f_fish$threshold, col = "#fde725") +
  geom_vline(xintercept = m_fish$threshold, col = "#21908CFF") +
  labs(
    colour = "Year",
    x = "Length (cm)", y = "Weight (kg)") +
  scale_y_log10() +
  scale_x_log10() +
  scale_colour_viridis_c(option = "C") +
  ggtitle(paste0(species
                 , " with filter at ", sd_threshold, " SD",
                 ifelse(is_heavy_tail, " heavy tailed", " normal")
                 #, " (trimmed at ", lower_quantile, " and ", upper_quantile, " quantiles)"
                 )) +
  ggsidekick::theme_sleek() + theme(legend.position = c(0.2,0.8))
# browser()
ggsave(paste0("figs/cond-black-swan-",
              ifelse(is_heavy_tail, "t", "norm"),
              "-", sd_threshold, "sd-",
              "year-",
              # "fixed-",
              spp, ".png"), width = 10, height = 10)


# plot with le crens
ggplot(dat |> mutate(weight = weight/1000) |> filter(
  sex %in% c(1,2)
  # fishing_event_id %in% fishing_event_id
), aes(length, weight, shape = as.factor(sex))) +
  geom_point(colour = "red") +
  geom_point(colour = "white", data = ds) +
  geom_point(aes(colour = cond_fac), data = ds, alpha = 0.4) +
  geom_vline(xintercept = m_fish$threshold, col = "#21908CFF") +
  geom_vline(xintercept = f_fish$threshold, col = "#fde725") +
  labs(
    colour = "Le Cren's", shape = "Sex",
    x = "Length (cm)", y = "Weight (kg)") +
  scale_colour_viridis_c() +
  scale_shape_discrete(guide = NULL) +
  ggtitle(paste0(species
                 , " with filter at ", sd_threshold, " SD",
                 ifelse(is_heavy_tail, " heavy tailed", " normal")
                 #, " (trimmed at ", lower_quantile, " and ", upper_quantile, " quantiles)"
  )) +
  # facet_wrap(~sex) +
  ggsidekick::theme_sleek() + theme(legend.position = c(0.2,0.8))

ggsave(paste0("figs/cond-black-swan-",
              ifelse(is_heavy_tail, "t", "norm"),
              "-", sd_threshold, "sd-",
              spp, ".png"), width = 10, height = 6)


dir.create(paste0("data-generated/condition-data-black-swan/"), showWarnings = FALSE)
saveRDS(ds, paste0("data-generated/condition-data-black-swan/", spp, "-mat-", mat_threshold, "-condition.rds"))

# browser()
}

pmap(species_list, get_condition_data)

