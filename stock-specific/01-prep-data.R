# Prep data and folders for stock specific condition analysis
library(tidyverse)
devtools::load_all()

# source("stock-specific/00-set-options.R")

spp <- gsub(" ", "-", gsub("\\/", "-", tolower(species)))

# create directories ----
dir.create(paste0("stock-specific/", spp, "/"), showWarnings = FALSE)
dir.create(paste0("stock-specific/", spp, "/data/"), showWarnings = FALSE)
dir.create(paste0("stock-specific/", spp, "/figs/"), showWarnings = FALSE)
dir.create(paste0("stock-specific/", spp, "/figs-french/"), showWarnings = FALSE)
# dir.create(paste0("stock-specific/", spp, "/models/"), showWarnings = FALSE)
dir.create(paste0("stock-specific/", spp, "/output/"), showWarnings = FALSE)

# get data if on PBS network ----
if(get_data_at_pbs) {
library(gfdata)
dd <- get_all_survey_sets(species, ssid = NULL,
                       remove_false_zeros = TRUE,
                       usability = NULL)
saveRDS(dd, paste0("stock-specific/", spp, "/data/survey-sets-", spp, ".rds"))

ds <- get_all_survey_samples(species,
                          include_event_info = TRUE,
                          unsorted_only = FALSE)
saveRDS(ds, paste0("stock-specific/", spp, "/data/survey-samples-", spp, ".rds"))
}

# get cached data on PE's Macbook
if(get_PE_cached_data) {
dd <- readRDS("data-raw/survey-sets-all-1.rds") |>
  bind_rows(readRDS("data-raw/survey-sets-all-2.rds")) |>
  filter(species_common_name == tolower(species))

saveRDS(dd, paste0("stock-specific/", spp, "/data/survey-sets-", spp, ".rds"))

ds <- readRDS("data-raw/survey-samples-all-1a.rds") |>
  bind_rows(readRDS("data-raw/survey-samples-all-1b.rds")) |>
  bind_rows(readRDS("data-raw/survey-samples-all-1c.rds")) |>
  filter(species_common_name == tolower(species))


# temporary fix for doorspread and MSSM issues-- shouldn't be needed for any fresh data pulls
ds <- ds |>
  select(-area_swept1, -area_swept2, -area_swept, -area_swept_km2) |>
  group_by(specimen_id) |>
  mutate(speed_mpm = ifelse(is.logical(na.omit(speed_mpm)), NA, na.omit(speed_mpm)),
         doorspread_m = ifelse(is.logical(na.omit(doorspread_m)), NA, na.omit(doorspread_m))) |>
  ungroup()|>
  filter(survey_series_id == survey_series_og)|>
  distinct()

saveRDS(ds, paste0("stock-specific/", spp, "/data/survey-samples-", spp, ".rds"))
}

# tidy data ----
dset <- readRDS(paste0("stock-specific/", spp, "/data/survey-sets-", spp, ".rds")) |>
  filter(survey_abbrev %in% tidy_surveys_included,
         !(survey_abbrev == "OTHER" & !(survey_series_id %in% other_surveys_kept)),
         major_stat_area_code %in% major_areas) |>
  format_survey_labels() |>
  distinct()

dsamp <- readRDS(paste0("stock-specific/", spp, "/data/survey-samples-", spp, ".rds")) |>
  filter(survey_abbrev %in% tidy_surveys_included,
         !(survey_abbrev == "OTHER" & !(survey_series_id %in% other_surveys_kept)),
         major_stat_area_code %in% major_areas) %>%
  format_survey_labels() |>
  distinct()

# check for obvious data problems ----
plot(dsamp$length, dsamp$weight)

# temporary filter until lengths in wrong units fixed
if(spp == "lingcod"){
  dsamp <- filter(dsamp, length > 8)
  dsamp <- filter(dsamp, weight < 50000)
}

# other outliers removed
dsamp <- dsamp %>%
  filter(!(weight > 900 & species_common_name == tolower("Pacific Sanddab")))  %>%
  filter(!(weight > 3500 & species_common_name %in% tolower(c("Yellowmouth Rockfish",
                                                              # "Widow Rockfish",
                                                              "Quillback Rockfish"))))

check_for_duplicates2 <- dsamp[duplicated(dsamp$specimen_id), ]
if(nrow(check_for_duplicates2)>0){
  all_duplicates2 <- dsamp[duplicated(dsamp$specimen_id)|duplicated(dsamp$specimen_id, fromLast=TRUE),]
  stop(paste(species, "has duplicate specimen ids."))}

# dx <- dsamp[duplicated(dsamp$specimen_id) |
#               duplicated(dsamp$specimen_id, fromLast=TRUE), ] |>
#   arrange(specimen_id)
# dx |> View()


saveRDS(dset, paste0("stock-specific/", spp, "/data/tidy-survey-sets-", spp, ".rds"))
saveRDS(dsamp, paste0("stock-specific/", spp, "/data/tidy-survey-samples-", spp, ".rds"))


# # Load tidy data ----
dset <- dset |>
  filter(survey_abbrev %in% sdm_surveys_included)

check_for_duplicates <- dset[duplicated(dset$fishing_event_id), ]
if(nrow(check_for_duplicates)>0){
  all_duplicates <- dset[duplicated(dset$fishing_event_id)|duplicated(dset$fishing_event_id, fromLast=TRUE),]
  stop(paste(species, "has duplicate event ids."))}



sets_mat_m <- filter(dsamp, !is.na(maturity_code), !is.na(length), maturity_code != 0, sex == 1)
sets_mat_f <- filter(dsamp, !is.na(maturity_code), !is.na(length), maturity_code != 0, sex == 2)

if(min(length(unique(sets_mat_m$fishing_event_id)), length(unique(sets_mat_f$fishing_event_id))) >= 20){
  set_sample_id_re <- TRUE
}else{
  set_sample_id_re <- FALSE
}

# Split by maturity ----
library(gfplot)

dss <- split_catch_by_sex(dset, dsamp,
  # catch_variable = "est_catch_count", # could use this to avoid biomass ~ condition issue?
  # split_by_weight = FALSE, # automatically switches to TRUE for common weight-based catch variables
  split_by_sex = split_by_sex,
  immatures_pooled = immatures_pooled,
  sample_id_re = set_sample_id_re, # used for maturity ogives
  year_re = mat_year_re,
  # sample_id_re = FALSE,
  # survey = surveys_included, # turning this off allows all samples provided to be used for maturity split
  # use_median_ratio = TRUE,
  min_sample_number = set_min_sample_number,
  split_by_maturity = maturity_possible,
  custom_maturity_at = custom_maturity_code,
  custom_length_thresholds = custom_length_threshold,
  p_threshold = mat_threshold,
  plot = maturity_possible
  )

## when a background job I get this error
# Error in .Call("rs_sourceCppOnBuild", file, FALSE, FALSE) :
#   C symbol name "rs_sourceCppOnBuild" not in load table
# Calls: .rs.sourceWithProgress ...

saveRDS(dss, paste0("stock-specific/", spp, "/output/", "split-catch-data-", spp, ".rds"))
# dss$maturity_plot
# gfplot::plot_mat_ogive(dss$m)

# dss$weight_plot
# dss$data %>% filter(group_catch_est >0) %>%
#     ggplot() + geom_histogram(aes(log(group_catch_est)))


# additional effort data ----
# use survey (or series) means for speed and door spread

meaneffort1 <- dset %>%
  filter(usability_code == 1) %>%
  mutate(
    doorspread_m = ifelse(doorspread_m == 0|doorspread_m > 150, NA_real_, doorspread_m),
    # convert mouth widths to doorspread for hake data only, not needed for series means
    mouth_width_m = ifelse(mouth_width_m == 0, NA_real_, mouth_width_m),
    doorspread_m = ifelse(survey_series_id == 68 & is.na(doorspread_m), mouth_width_m * 0.74 + 40, doorspread_m),
    doorspread_m = ifelse(survey_series_id == 79 & is.na(doorspread_m), mouth_width_m * 2, doorspread_m), # average conversion according to chatGPT, not critical due to catchability factor
    speed_mpm = ifelse(speed_mpm == 0, NA_real_, speed_mpm)
  ) %>%
  group_by(year, survey_id, survey_series_id) %>%
  summarise(
    mean_doorspread = mean(doorspread_m, na.rm = TRUE),
    mean_speed = mean(speed_mpm, na.rm = TRUE)
  )

meaneffort2 <- dset %>%
  filter(usability_code == 1) %>%
  mutate(
    doorspread_m = ifelse(doorspread_m == 0, NA_real_, doorspread_m),
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
    log_depth_c = log_depth - 5, # close to median for syn data
    area_swept = ifelse(is.na(tow_length_m),
                        doorspread_m * duration_min * speed_mpm,
                        doorspread_m * tow_length_m
    )
  )

ds <- ds[,which(unlist(lapply(ds, function(x)!all(is.na(x))))),with=FALSE]

saveRDS(ds, paste0("stock-specific/", spp, "/output/", "split-catch-data-w-effort-", spp, ".rds"))
