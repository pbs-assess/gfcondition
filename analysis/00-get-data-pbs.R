# retrieve and clean up data

# need to be on PBS network ----

remotes::install_github("pbs-assess/gfdata", ref = "trials")
library(gfdata)

# # load overall species list
source("analysis/00-species-list.R")

## all canadian waters
major_areas <- c("01", "03", "04", "05", "06", "07", "08", "09", "11",
                 "71","72","73","74","75","76","77","99")


dd <- get_all_survey_sets(species_list[1:15],
                          ssid = NULL,
                          major = major_areas,
                          remove_duplicates = TRUE,
                          usability = NULL)

saveRDS(dd, "data-raw/survey-sets-all-1.rds")

dd2 <- get_all_survey_sets(species_list[16:length(species_list)],
                           ssid = NULL,
                           major = major_areas,
                           remove_duplicates = TRUE,
                           usability = NULL)

saveRDS(dd2, "data-raw/survey-sets-all-2.rds")

ds <- get_all_survey_samples(species_list[1:8],
                             ssid = NULL,
                             major = major_areas,
                             include_event_info = TRUE,
                             unsorted_only = FALSE,
                             random_only = FALSE,
                             grouping_only = FALSE,
                             remove_bad_data = TRUE,
                             remove_duplicates = TRUE)
saveRDS(ds, "data-raw/survey-samples-all-1a.rds")

ds1 <- get_all_survey_samples(species_list[9:15],
                              ssid = NULL,
                              major = major_areas,
                             include_event_info = TRUE,
                             unsorted_only = FALSE,
                             random_only = FALSE,
                             grouping_only = FALSE,
                             remove_bad_data = TRUE,
                             remove_duplicates = TRUE)
saveRDS(ds1, "data-raw/survey-samples-all-1b.rds")

ds2 <- get_all_survey_samples(species_list[16:length(species_list)],
                              ssid = NULL,
                              major = major_areas,
                              include_event_info = TRUE,
                              unsorted_only = FALSE,
                              random_only = FALSE,
                              grouping_only = FALSE,
                              remove_bad_data = TRUE,
                              remove_duplicates = TRUE)
saveRDS(ds2, "data-raw/survey-samples-all-2.rds")

# Reminder can be completed off network ----

library(tidyverse)

# if wanting to include IPHC halibut data, if can be downloaded here
# https://www.iphc.int/data/fiss-pacific-halibut-data/
# and wrangled to match PBS data this way
library(readr)


iphc <- read_csv("data-raw/IPHC-halibut-data.csv")
iphc2 <- read_csv("data-raw/IPHC-set-data.csv") |> select(-`Row number`)
iphc <- left_join(iphc,iphc2) |>
  # remove stations in inside waters
  filter(!(Station %in% c(
    2207, 2204, 2203, 2201, 2212, 2211, 2215, 2216, 2219,  2220, 2222, 2223,
    2224, 2225, 2227, 2228, 2229, 2230, 2231, 2234, 2235, 2236, 2238, 2239, 2243,
    2244, 2249, 2259, 2245, 2246
  )))

names(iphc) <- tolower(names(iphc))
iphc <- iphc |> mutate(
  fishing_event_id = stlkey,
  sample_id = stlkey,
  specimen_id = `pacific halibut key`*stlkey,
  sex = case_when(
    sex == "F"~2,
    sex == "M"~1,
    TRUE ~ 0
  ),
  maturity_convention_code = 4,
  maturity_code = case_when(
    maturity == "Resting"~7,
    maturity == "Immature"~1,
    maturity == "Mature"~3,
    maturity == "Spawning"~4,
    TRUE ~ 0
  ),
  length = `length (cm)`,
  weight = `net weight (kg)`*1000,
  latitude = beginlat,
  longitude = beginlon,
  latitude_end = endlat,
  longitude_end = endlon
) |> select(year,month,day,
            fishing_event_id, sample_id, specimen_id,
  sex,
  maturity_convention_code,
  maturity_code,
  length,
  weight,
  latitude,
  longitude,
  latitude_end,
  longitude_end
) |> mutate(
survey_abbrev = "IPHC FISS",
survey_series_id = 14,
survey_id = 66,
species_code = "614",
species_common_name = "pacific halibut"
)

# for condition model (inclusive of those for density models)
surveys_included <- c("HBLL OUT N", "HBLL OUT S",
                      "IPHC FISS",
                      # maybe remove because at different time of year than all the others
                      # for now retaining for length at maturity calculation, but removed from condition analysis
                      "SABLE",  # only have weights for certain species anyway?
                      "MSSM QCS", "MSSM WCVI", "MSSM",
                      "OTHER", # filtered to two older bottom trawl surveys + hake
                      "HS MSA", "SYN HS", "SYN QCS", "SYN WCHG", "SYN WCVI")


dset <- readRDS("data-raw/survey-sets-all-1.rds") %>%
  bind_rows(., readRDS("data-raw/survey-sets-all-2.rds")) %>%
  # this removes duplications and non-Canadian data
  filter(survey_abbrev %in% surveys_included,
         !(survey_abbrev == "OTHER" & !(survey_series_id %in% c(9, 11, 68))),
         major_stat_area_code %in% major_areas) %>%
  mutate(
    survey_abbrev = ifelse(survey_series_id == 68, "HAKE", survey_abbrev),
    # survey_area = ifelse(survey_abbrev == "HS MSA", "SYN HS",
    #                 ifelse(survey_abbrev == "MSSM QCS", "SYN QCS",
    #                 ifelse(survey_abbrev == "MSSM WCVI", "SYN WCVI",
    #                 survey_abbrev))),
    survey_type = as.factor(
      case_when(
        survey_abbrev == "HS MSA"~"MSA",
        survey_abbrev %in% c("MSSM WCVI", "MSSM QCS") & year>2002 & year<=2005~"MSSM<=05",
        survey_abbrev %in% c("MSSM WCVI", "MSSM QCS") & year>2005~"MSSM>05",
        survey_abbrev %in% c("MSSM WCVI", "MSSM QCS") & year <= 2002~"MSSM <03",
        survey_series_id == 68~"HAKE",
        survey_abbrev %in% c("SYN HS", "SYN QCS", "SYN WCHG", "SYN WCVI")~"SYN",
        survey_abbrev %in% c("EUL N", "EUL S")~"EUL",
        TRUE~survey_abbrev
      ))
  ) %>% distinct()

dsamp <- readRDS("data-raw/survey-samples-all-1a.rds") %>%
  bind_rows(., readRDS("data-raw/survey-samples-all-1b.rds")) %>%
  # bind_rows(., readRDS("data-raw/survey-samples-all-2.rds")) %>%
  bind_rows(., iphc) %>%
  filter(survey_abbrev %in% surveys_included,
         !(survey_abbrev == "OTHER" & !(survey_series_id %in% c(9, 11, 68))),
         major_stat_area_code %in% major_areas) %>%
  mutate(
    survey_abbrev = ifelse(survey_series_id == 68, "HAKE", survey_abbrev),
    survey_type = as.factor(
      case_when(
        survey_abbrev == "HS MSA"~"MSA",
        survey_abbrev %in% c("MSSM WCVI", "MSSM QCS") & year>2002 & year<=2005~"MSSM<=05",
        survey_abbrev %in% c("MSSM WCVI", "MSSM QCS") & year>2005~"MSSM>05",
        survey_abbrev %in% c("MSSM WCVI", "MSSM QCS") & year <= 2002~"MSSM <03",
        survey_series_id == 68~"HAKE",
        survey_abbrev %in% c("SYN HS", "SYN QCS", "SYN WCHG", "SYN WCVI")~"SYN",
        survey_abbrev %in% c("EUL N", "EUL S")~"EUL",
        TRUE~survey_abbrev
      ))) %>%
  # # causes duplication for some species with multiple samples collected per individual
  # select(-dna_container_id, -dna_sample_type) %>%
  distinct()

# remove misidentified sandpaper skates from set data
dset <- dset %>% filter(!(species_common_name == tolower("Sandpaper Skate")
    & fishing_event_id %in% c(filter(dsamp, species_common_name == tolower("Sandpaper Skate")&length > 70)$fishing_event_id)))

# and then remove misidentified sandpaper skates from sample data
dsamp <- dsamp %>% filter(!(species_common_name == tolower("Sandpaper Skate")&length > 70))


# temporary fix for doorspread issue
dsamp <- dsamp |>
  select(-area_swept1, -area_swept2, -area_swept, -area_swept_km2) |>
  group_by(specimen_id) |>
  mutate(speed_mpm = ifelse(is.logical(na.omit(speed_mpm)), NA, na.omit(speed_mpm)),
         doorspread_m = ifelse(is.logical(na.omit(doorspread_m)), NA, na.omit(doorspread_m))) |>
  ungroup()

# temp fix for MSSM issure
.d <- dsamp

try(.d[ ((.d$survey_series_id == 7 & .d$major_stat_area_code %in% c("05", "06"))), ]$survey_series_id <- 6, silent = TRUE)
try(.d[ ((.d$survey_series_id == 6 & .d$major_stat_area_code %in% c("03", "04"))), ]$survey_series_id <- 7, silent = TRUE)
try(.d[ ((.d$survey_series_id == 7)), ]$survey_series_desc <- "West Coast Vancouver Island Multispecies Small-mesh Bottom Trawl", silent = TRUE)
try(.d[ ((.d$survey_series_id == 6)), ]$survey_series_desc <- "Queen Charlotte Sound Multispecies Small-mesh Bottom Trawl", silent = TRUE)
try(.d[ ((.d$survey_series_id == 7)), ]$survey_abbrev <- "MSSM WCVI", silent = TRUE)
try(.d[ ((.d$survey_series_id == 6)), ]$survey_abbrev <- "MSSM QCS", silent = TRUE)

dsamp <- .d |> dplyr::distinct()

# save combined processed data
saveRDS(dset, "data-generated/all-sets-used.rds")
saveRDS(dsamp, "data-generated/all-samples-used.rds")

check_for_duplicates <- dsamp[duplicated(dsamp$specimen_id), ]
filter(dsamp, specimen_id %in% check_for_duplicates$specimen_id) |> View()
unique(check_for_duplicates$species_common_name)
unique(check_for_duplicates$survey_series_desc)

dd <- filter(dsamp, specimen_id == 8382976)


dset |> group_by(species_common_name, survey_series_id) |> summarise(n = n(),
                                                   events = length(unique(paste0(fishing_event_id, skate_id)))) |> View()
### check which species are measured by IPHC
### just figured out how to add halibut data direct from IPHC ... halibut should be rerun
d <- dsamp |> filter(survey_abbrev == "IPHC FISS", !is.na(length)) |>
  mutate(DOY = as.numeric(strftime(time_deployed, format = "%j")))
d |> group_by(species_common_name) |> summarise(min = min(year),max = max(year))

d2 <- filter(d, !is.na(weight))
d2 |> group_by(species_common_name) |> summarise(min = min(year),max = max(year))
d2 <- filter(d, maturity_code>0)
d2 |> group_by(species_common_name) |> summarise(min = min(year),max = max(year))


### check which species are measured by HBLL
d <- dsamp |> filter(survey_abbrev %in% c("HBLL OUT N","HBLL OUT S"), !is.na(length)) |>
  mutate(DOY = as.numeric(strftime(time_deployed, format = "%j")))

d |> group_by(species_common_name) |> summarise(min = min(year),max = max(year)) |> View()

d2 <- filter(d, !is.na(weight))
d2 |> group_by(species_common_name) |> summarise(min = min(year),max = max(year))

d2 <- filter(d, maturity_code>0)
d2 |> group_by(species_common_name) |> summarise(min = min(year),max = max(year))

### check which species are measured by SABLE
d <- dsamp |> filter(survey_abbrev %in% c("SABLE", "SABLE INLET",
                                          "SABLE OFF", "SABLE RAND"), !is.na(length)) |>
  mutate(DOY = as.numeric(strftime(time_deployed, format = "%j")))

d |> group_by(species_common_name) |> summarise(min = min(year),max = max(year)) |> View()

d2 <- filter(d, maturity_code>0)
d2 |> group_by(species_common_name) |> summarise(min = min(year),max = max(year), n = n()) |> View()

d2 <- filter(d, !is.na(weight), !is.na(length))
d2 |> group_by(species_common_name) |> summarise(min = min(year),max = max(year), n = n())|> View()


### check which species are measured by HAKE
### discovered all samples since 2018 are missing... should get these and rerun everything!
d <- dsamp |> filter(survey_abbrev %in% c("HAKE"), !is.na(length)) |>
  mutate(DOY = as.numeric(strftime(time_deployed, format = "%j")))

d |> group_by(species_common_name) |> summarise(min = min(year),max = max(year)) |> View()

d2 <- filter(d, maturity_code>0)
d2 |> group_by(species_common_name) |> summarise(min = min(year),max = max(year), n = n()) |> View()

d2 <- filter(d, !is.na(weight), !is.na(length))
d2 |> group_by(species_common_name) |> summarise(min = min(year),max = max(year), n = n())|> View()
