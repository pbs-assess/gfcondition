
library(tidyverse)

f1 <- list.files(paste0("data-generated/condition-data-may-2024/"), pattern = ".rds", full.names = TRUE)

d1 <- purrr::map_dfr(f1, readRDS)


f2 <- list.files(paste0("data-generated/condition-data-black-swan/"), pattern = ".rds", full.names = TRUE)

d2 <- purrr::map_dfr(f2, readRDS)


d1b <- d1 |> group_by(species_common_name) |> filter(year < 2024) |>
  # filter(!is.na(age)) |>
  summarize(n_original = n())

d2b <- d2 |> group_by(species_common_name) |> filter(year < 2024) |>
  # filter(!is.na(age)) |>
  summarize(n_now = n())

d3 <- left_join(d1b, d2b) |>
  mutate(perc_gained = round(((n_now - n_original)/n_original)*100))


# also check set data
f1 <- list.files(paste0("data-generated/density-data-may/"), pattern = ".rds", full.names = TRUE)

s1 <- purrr::map_dfr(f1, readRDS)
s1b <- s1 |> group_by(species_common_name) |> filter(year < 2024) |> summarize(n_original = n())

f2 <- list.files(paste0("data-generated/density-data/"), pattern = ".rds", full.names = TRUE)
s2 <- purrr::map_dfr(f2, readRDS)
s2b <- s2 |> group_by(species_common_name) |> filter(year < 2024) |> summarize(n_now = n())

s3 <- left_join(s1b, s2b) |>  mutate(perc_gained = round(((n_now - n_original)/n_original)*100))

s1c <- select(s1, survey_abbrev, survey_series_id, year, fishing_event_id) |> filter(year < 2024) |> distinct()
s2c <- select(s2, survey_abbrev, survey_series_og, year, fishing_event_id) |> filter(year < 2024) |> distinct()
s4 <- anti_join(s2c,s1c, by = "fishing_event_id") |> group_by(survey_abbrev, year) |> summarise(n = n())
