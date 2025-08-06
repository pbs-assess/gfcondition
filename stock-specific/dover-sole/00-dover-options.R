# Set options for a single species analysis

# Species ----
species <- "Dover Sole"
stock_name <- "Dover Sole"

# 1. Data retrieval and filtering options ----

## if needing raw data, where is it coming from? ----
get_data_at_pbs <- FALSE
get_PE_cached_data <- TRUE

## set major areas ----
## this defines the "stock"
## here using all canadian waters
major_areas <- c("01", "03", "04", "05", "06", "07", "08", "09",
                 "11", # bc offshore waters
                 "71","72","73","74","75","76","77","99")

survey_grids <- c("SYN QCS", "SYN WCVI", "SYN HS", "SYN WCHG")

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
                        79, # Triennial
                        68 # HAKE
                        )

## Notes on SABLE:
## at different time of year than all the others and only has weights for certain species
## retained for length at maturity calculation, but removed from condition analysis

set_utm_crs <- 32609


# 2. Density model options ----

## structure and name ----
## *name_long is printed on figures along with resulting family
## set_family is the starting choice which is reflected in the *_name0
## set_family2 is the alternate used when the first fails to converge

dens_model_name_long <- "depth, DOY, and survey type"


## if running alternatives can review x-plot-sdm-effects.R figs and model AICs to help choose which models to use
set_family <- sdmTMB::delta_lognormal() # lower AIC for lingcod
dens_model_name0 <- "dln-"


# set_family <- sdmTMB::delta_gamma()
# dens_model_name0 <- "dg-"

# set_family <- sdmTMB::delta_gengamma()
# dens_model_name0 <- "dgg-"

set_family2 <- sdmTMB::tweedie()
knot_distance <<- 20

## all surveys possible for density models ----
sdm_surveys_included <- c(
  "HS MSA",
  "HS PCOD",
  "THORNYHEAD",
  "TRIENNIAL",
  "SYN HS", "SYN QCS",
  "SYN WCHG", "SYN WCVI",
  "MSSM QCS", "MSSM WCVI", # already filtered to avoid duplication
  "HAKE"
)


## or use only synoptic data for all density models? ----
only_synoptic <- FALSE
# only_synoptic <- TRUE # to see effect of other surveys

if(only_synoptic) dens_model_name_long <- "depth and DOY"


# ## or use only longline data for all density models? ----
# only_longline <- FALSE
# # only_longline <- TRUE
# if(only_longline) dens_model_name_long <- "depth, DOY, and survey type"


# 3. Condition model settings ----

# this applies only to units of log_a and b
# set_weight_scale <- 1/1000 # in kg
set_weight_scale <- 1 # in g

# option to add prefix to models otherwise named with "year-month"
cond_model_prefix <- ""


## should we exclude samples from years with fewer than some threshold?
# min_yr_count <- 10 # current main folder, hasn't been run with density yet
min_yr_count <- NULL
# cond_min_sample_count <- NULL # to replicate old behaviour
cond_min_sample_count <- 6

# to run using future package
# gfcondition must be installed

use_parallel <- FALSE
# use_parallel <- TRUE

split_index_by_survey <- TRUE

# A. Global options ----

options(scipen = 100, digits = 4)
ggplot2::theme_set(ggsidekick::theme_sleek())

# B. Split-by-maturity settings ----

## option for a version that ignores maturity ----
# maturity_possible <- FALSE
split_by_maturity <- maturity_possible <- TRUE

split_by_sex <- TRUE
immatures_pooled <- TRUE

mat_year_re <- FALSE
mat_threshold <- 0.5
set_min_sample_number <- 6 # sample cutoff for splits, function default is 10

## only generate split data and some exploratory plots ----
stop_early <- FALSE
# stop_early <- TRUE # to avoid running models

## custom maturity or length thresholds for certain species? ----
custom_maturity_code <- NULL
custom_length_threshold <- NULL


## NEW APPROACH TO RUNNING CODE -----------
# update_models <- TRUE # adds current year and month to density model names
## OR
update_models <- FALSE
# model_date <- "2024-11" # use previous model from this data
model_date <- "2025-07"

if(update_models) {
  sysdate <- unlist(strsplit(as.character(Sys.Date()), "-"))
  # dens_model_total <- paste0(dens_model_name0, sysdate[1], "-", sysdate[2], "")
  # dens_model_name1 <- paste0(dens_model_name0, "split-", sysdate[1], "-", sysdate[2], "")
  # dens_model_name2 <- paste0(dens_model_name0, "only-sampled-", sysdate[1], "-", sysdate[2], "")
  model_date <- paste0(sysdate[1], "-", sysdate[2])
} #else {
  dens_model_total <- paste0(dens_model_name0, model_date)
  dens_model_name1 <- paste0(dens_model_name0, "split-", model_date)
  dens_model_name2 <- paste0(dens_model_name0, "only-sampled-", model_date)
#}

FRENCH <- FALSE

source("stock-specific/01-prep-data.R")

### get density estimates and/or update biomass maps
source("stock-specific/02-sdm.R")

### get condition factors (figures not translated/used in MS)
source("stock-specific/03-condition-calc.R")

### condition models (figures not translated/used in MS)
##needed to restart before running due to m object getting retained in global environ
source("stock-specific/04-condition-models.R")


## FOR FIGURE GENERATION -----------

### rerun to update biomass maps
source("stock-specific/02-sdm.R")

### plot condition indices
source("stock-specific/05-combine-condition-indices.R")

### plot condition maps
source("stock-specific/06-plot-condition-maps.R")

### maturity, le crens and density index figures
### could produce complete flow chart figures with a little updating
source("stock-specific/07-figs-from-flow-chart.R")

source("stock-specific/08-survey-date-hist.R")

FRENCH <- TRUE

source("stock-specific/02-sdm.R")
### condition indices
source("stock-specific/05-combine-condition-indices.R")

### condition maps
source("stock-specific/06-plot-condition-maps.R")

### maturity, le crens and density index figures
source("stock-specific/07-figs-from-flow-chart.R")

source("stock-specific/08-survey-date-hist.R")


## options for reloading packages without restarting R
# detach("package:gfcondition", unload=TRUE)
# detach("package:gfplot", unload=TRUE)
# detach("package:rosettafish", unload=TRUE)

