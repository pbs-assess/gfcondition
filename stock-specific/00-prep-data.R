# Prep data and folders for stock specific condition analysis


# choose species and surveys ----
species <- "Lingcod"
species <- "Pacific Halibut"
spp <- gsub(" ", "-", gsub("\\/", "-", tolower(species)))

## all canadian waters
major_areas <- c("01", "03", "04", "05", "06", "07", "08", "09",
                 "11", # bc offshore waters
                 "71","72","73","74","75","76","77","99")


# all survey abbreviations from which any data should be used (maturities, condition, and/or densities)
surveys_included <- c("HBLL OUT N", "HBLL OUT S",
                      "IPHC FISS",
                      # maybe remove because at different time of year than all the others
                      # for now retaining for length at maturity calculation, but removed from condition analysis
                      "SABLE", # only have weights for certain species anyway?
                      "MSSM QCS", "MSSM WCVI",
                      "OTHER", # filtered to two older bottom trawl surveys + hake
                      "HS MSA", "SYN HS", "SYN QCS", "SYN WCHG", "SYN WCVI")

# create directories ----
dir.create(paste0("stock-specific/", spp, "/"), showWarnings = FALSE)
dir.create(paste0("stock-specific/", spp, "/data/"), showWarnings = FALSE)
dir.create(paste0("stock-specific/", spp, "/figs/"), showWarnings = FALSE)
dir.create(paste0("stock-specific/", spp, "/models/"), showWarnings = FALSE)
dir.create(paste0("stock-specific/", spp, "/output/"), showWarnings = FALSE)

# get data if on PBS network ----
library(gfdata)
dd <- get_all_survey_sets(species_list, ssid = NULL,
                       remove_false_zeros = TRUE, usability = NULL)
saveRDS(dd, paste0("stock-specific/", spp, "/data/survey-sets-", spp, ".rds"))

ds <- get_all_survey_samples(species_list,
                          include_event_info = TRUE,
                          unsorted_only = FALSE)
saveRDS(ds, paste0("stock-specific/", spp, "/data/survey-samples-", spp, ".rds"))

# tidy data ----
library(tidyverse)

dset <- readRDS(paste0("stock-specific/", spp, "/data/survey-sets-", spp, ".rds")) %>%
  filter(survey_abbrev %in% surveys_included,
  !(survey_abbrev == "OTHER" & !(survey_series_id %in% c(5, 9, 11, 68))),
major_stat_area_code %in% major_areas) %>%
  mutate(
    survey_abbrev = ifelse(survey_series_id == 68, "HAKE", survey_abbrev),
    survey_abbrev = ifelse(survey_series_id == 5, "HS PCOD", survey_abbrev),
    survey_abbrev = ifelse(survey_series_id == 11, "THORNYHEAD", survey_abbrev),
    # survey_area = ifelse(survey_abbrev == "HS MSA", "SYN HS",
    #                 ifelse(survey_abbrev == "MSSM QCS", "SYN QCS",
    #                 ifelse(survey_abbrev == "MSSM WCVI", "SYN WCVI",
    #                 survey_abbrev))),
    survey_type = as.factor(
      case_when(
        survey_abbrev %in% c("HBLL OUT S", "HBLL OUT N")~"HBLL",
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



dsamp <- readRDS(paste0("stock-specific/", spp, "/data/survey-sets-", spp, ".rds")) %>%
  filter(survey_abbrev %in% surveys_included,
         !(survey_abbrev == "OTHER" & !(survey_series_id %in% c(5, 9, 11, 68))),
         major_stat_area_code %in% major_areas) %>%
  mutate(
    survey_abbrev = ifelse(survey_series_id == 68, "HAKE", survey_abbrev),
    survey_abbrev = ifelse(survey_series_id == 5, "HS PCOD", survey_abbrev),
    survey_abbrev = ifelse(survey_series_id == 11, "THORNYHEAD", survey_abbrev),
    survey_type = as.factor(
      case_when(
        survey_abbrev %in% c("HBLL OUT S", "HBLL OUT N")~"HBLL",
        survey_abbrev == "HS MSA"~"MSA",
        survey_abbrev %in% c("MSSM WCVI", "MSSM QCS") & year>2002 & year<=2005~"MSSM<=05",
        survey_abbrev %in% c("MSSM WCVI", "MSSM QCS") & year>2005~"MSSM>05",
        survey_abbrev %in% c("MSSM WCVI", "MSSM QCS") & year <= 2002~"MSSM <03",
        # survey_series_id == 68~"HAKE",
        survey_abbrev %in% c("SYN HS", "SYN QCS", "SYN WCHG", "SYN WCVI")~"SYN",
        survey_abbrev %in% c("EUL N", "EUL S")~"EUL",
        TRUE~survey_abbrev
      ))) %>%
  # # causes duplication for some species with multiple samples collected per individual
  # select(-dna_container_id, -dna_sample_type) %>%
  distinct()
