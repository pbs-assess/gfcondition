# data checks and summaries
library(tidyverse)

dsamp <- readRDS("data-generated/all-samples-used.rds")
dset <- readRDS("data-generated/all-sets-used.rds")


check_for_duplicates <- dsamp[duplicated(dsamp$specimen_id), ]
filter(dsamp, specimen_id %in% check_for_duplicates$specimen_id) |> View()
unique(check_for_duplicates$species_common_name)
unique(check_for_duplicates$survey_series_desc)

dd <- filter(dsamp, specimen_id == 8382976)


dsamp |>
  group_by(species_common_name, survey_series_id) |>
  summarise(
    n = n(),
    specimens = length(unique(paste0(specimen_id)))
  ) |>
  View()

dset |>
  group_by(species_common_name, survey_series_id) |>
  summarise(
    n = n(),
    events = length(unique(paste0(fishing_event_id, skate_id)))
  ) |>
  View()

filter(dset, species_common_name == "pacific cod") |>
  group_by(survey_abbrev) |>
  summarise(
    n = n(),
    min_year = min(year, na.rm = TRUE),
    max_year = max(year, na.rm = TRUE),
    min_depth = min(depth_m, na.rm = TRUE),
    max_depth = max(depth_m, na.rm = TRUE),
    events = length(unique(paste0(fishing_event_id, skate_id)))
  ) |>
  View()

# check for spawning

d <- dsamp |>
  filter(!maturity_code %in% c(1,2,3,7,0,6,4, 9:99)& !is.na(maturity_code))

d |> select(maturity_code, maturity_desc) |> distinct() |> View()

ripe <- d |>
  group_by(species_common_name, month, sex) |>
  summarise(
    n_ripe = n()
  )

tsamp <- dsamp |>
  filter(!maturity_code %in% c(0)& !is.na(maturity_code)) |>
  group_by(species_common_name, month,sex) |>
  summarise(
    n = n()
  )

ripe <- left_join(ripe, tsamp) |> mutate(percent_ripe = (n_ripe/n)*100)

ripe |> filter(sex == 2
               & month %in% c(6,7)
) |> View()

ripe |> filter(sex == 1
               & month %in% c(6,7)
) |> View()


### check which species are measured by IPHC
### just figured out how to add halibut data direct from IPHC ... halibut should be rerun
d <- dsamp |> filter(!is.na(length), survey_abbrev == "IPHC FISS") |>
  # filter(species_common_name == "pacific halibut") |>
  mutate(DOY = as.numeric(strftime(time_deployed, format = "%j")))
d |>
  group_by(species_common_name) |>
  summarise(min = min(year), max = max(year))

d2 <- filter(d, !is.na(weight))
d2 |>
  group_by(species_common_name) |>
  summarise(min = min(year), max = max(year))
d2 <- filter(d, maturity_code > 0)
d2 |>
  group_by(species_common_name) |>
  summarise(min = min(year), max = max(year))


### check which species are measured by HBLL
d <- dsamp |>
  filter(survey_abbrev %in% c("HBLL OUT N", "HBLL OUT S"), !is.na(length)) |>
  mutate(DOY = as.numeric(strftime(time_deployed, format = "%j")))

d |>
  group_by(species_common_name) |>
  summarise(min = min(year), max = max(year)) |>
  View()

d2 <- filter(d, !is.na(weight))
d2 |>
  group_by(species_common_name) |>
  summarise(min = min(year), max = max(year))

d2 <- filter(d, maturity_code > 0)
d2 |>
  group_by(species_common_name) |>
  summarise(min = min(year), max = max(year))

### check which species are measured by SABLE
d <- dsamp |>
  filter(survey_abbrev %in% c(
    "SABLE", "SABLE INLET",
    "SABLE OFF", "SABLE RAND"
  ), !is.na(length)) |>
  mutate(DOY = as.numeric(strftime(time_deployed, format = "%j")))

d |>
  group_by(species_common_name) |>
  summarise(min = min(year), max = max(year)) |>
  View()

d2 <- filter(d, maturity_code > 0)
d2 |>
  group_by(species_common_name) |>
  summarise(min = min(year, na.rm = TRUE), max = max(year, na.rm = TRUE), n = n()) #|> View()

d2 <- filter(d, !is.na(weight), !is.na(length))
d2 |>
  group_by(species_common_name) |>
  summarise(min = min(year), max = max(year), n = n()) #|> View()


### check which species are measured by HAKE
d <- dsamp |>
  filter(survey_abbrev %in% c("HAKE"), !is.na(length)) |>
  mutate(DOY = as.numeric(strftime(time_deployed, format = "%j")))

# d |> group_by(species_common_name) |> summarise(min = min(year),max = max(year)) |> View()

d2 <- filter(d, maturity_code > 0)
d2 |>
  group_by(species_common_name) |>
  summarise(min = min(year), max = max(year), n = n()) #|> View()

d2 <- filter(d, !is.na(weight), !is.na(length))
d2 |>
  group_by(species_common_name) |>
  summarise(min = min(year), max = max(year), n = n()) |>
  View()

### check which species are measured by HS PCOD
d <- dsamp |>
  filter( # !is.na(length),
    survey_abbrev %in% c("HS PCOD")
  ) |>
  mutate(DOY = as.numeric(strftime(time_deployed, format = "%j")))

# d |> group_by(species_common_name) |> summarise(min = min(year),max = max(year)) |> View()

d2 <- filter(d, maturity_code > 0)
d2 |>
  group_by(species_common_name) |>
  summarise(min = min(year), max = max(year), n = n()) #|> View()

d2 <- filter(d, !is.na(weight), !is.na(length))
d2 |>
  group_by(species_common_name) |>
  summarise(min = min(year), max = max(year), n = n()) #|> View()

d <- dset |> filter(survey_abbrev %in% c("HS PCOD"))
d |>
  group_by(species_common_name) |>
  summarise(sum = sum(catch_weight, na.rm = TRUE), n = n()) |>
  View()

d |> ggplot(aes(longitude, latitude,
                colour = as.factor(year),
                # size = log(catch_weight),
                alpha = log(catch_weight)
)) +
  geom_point() +
  facet_wrap(~species_common_name)

