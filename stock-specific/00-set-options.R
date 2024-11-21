# Set options for a single species analysis

# If needing raw data, where is it coming from? ----
get_data_at_pbs <- FALSE
get_PE_cached_data <- FALSE

# Species ----
species <- "Lingcod"
# species <- "Pacific Halibut"
# species <- "Dover Sole"

# Data filtering options ----

## set major areas ----
## all canadian waters
major_areas <- c("01", "03", "04", "05", "06", "07", "08", "09",
                 "11", # bc offshore waters
                 "71","72","73","74","75","76","77","99")

## set surveys from which any data can be used (maturities, condition, and/or densities) ----
## use default survey_abbrev
tidy_surveys_included <- c("HBLL OUT N", "HBLL OUT S",
                      "IPHC FISS",
                      "SABLE",
                      "MSSM QCS", "MSSM WCVI",
                      "OTHER", # filtered with other_surveys_kept
                      "HS MSA", "SYN HS", "SYN QCS", "SYN WCHG", "SYN WCVI")

other_surveys_kept <- c(5, # PCOD
                        9, # Rockfish survey pre 2000?
                        11, # THORNYHEAD
                        68) # HAKE

## Notes on SABLE:
## at different time of year than all the others and only has weights for certain species
## retained for length at maturity calculation, but removed from condition analysis


# Output options ----

options(scipen = 100, digits = 4)
ggplot2::theme_set(ggsidekick::theme_sleek())


# Density model options ----

## names ----
dens_model_name_long <- "depth, DOY, and survey type"
# dens_model_name0 <- "dln-"
dens_model_name0 <- "dg-"

## structure ----
# set_family <- sdmTMB::delta_gengamma()
# set_family <- sdmTMB::delta_lognormal()
set_family <- sdmTMB::delta_gamma()
set_family2 <- sdmTMB::tweedie()
knot_distance <<- 20

## all surveys possible for density models ----
sdm_surveys_included <- c(
  "HS MSA",
  "HS PCOD",
  "THORNYHEAD",
  "SYN HS", "SYN QCS",
  "SYN WCHG", "SYN WCVI",
  "MSSM QCS", "MSSM WCVI", # already filtered to avoid duplication
  "HAKE"
)

## or use only surveys with specimen data ----
## this only affects maturity specific models
only_sampled <- FALSE
# only_sampled <- TRUE

## or use only synoptic data for all density models? ----
only_synoptic <- FALSE
# only_synoptic <- TRUE # to see effect of other surveys

if(only_synoptic) dens_model_name_long <- "depth and DOY"


# Split-by-maturity settings ----

mat_threshold <- 0.5
set_min_sample_number <- 6 # sample cutoff for splits, function default is 10

## option for a version that ignores maturity ----
maturity_possible <- TRUE

## only generate split data and some exploratory plots ----
stop_early <- FALSE
# stop_early <- TRUE # to avoid running models

## custom maturity or length thresholds for certain species? ----
custom_maturity_code <- NULL
custom_length_threshold <- NULL

# overwrite for certain species
if (species == "North Pacific Spiny Dogfish") {
  custom_maturity_code <- c(NA, 55)
  # # custom_length_threshold <- c(70.9, 86.2)
  # # set_family <- delta_lognormal_mix()
  # # set_family2 <- delta_lognormal()
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

if (species == "Sandpaper Skate") {
  # Perez 2005
  custom_length_threshold <- c(49.2, 46.7)
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




