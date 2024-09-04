remotes::install_github("pbs-assess/gfdata", ref = "trials")
library(gfdata)

# # load overall species list
source("analysis/00-species-list.R")

## all canadian waters
major_areas <- c("01", "03", "04", "05", "06", "07", "08", "09", "11",
                 "71","72","73","74","75","76","77","99")


# dd <- get_all_survey_sets(species_list[1:15],
#                           ssid = NULL,
#                           major = major_areas,
#                           remove_duplicates = TRUE,
#                           usability = NULL)
#
# saveRDS(dd, "data-raw/survey-sets-all-1.rds")

dd2 <- get_all_survey_sets(species_list[16:length(species_list)],
                           ssid = NULL,
                           major = major_areas,
                           remove_duplicates = TRUE,
                           usability = NULL)

saveRDS(dd2, "data-raw/survey-sets-all-2.rds")
