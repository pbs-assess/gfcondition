library(tidyverse)
library(kableExtra)
library(stringr)

# for log(a) for weights in kg
# f2 <- list.files(paste0("data-generated/condition-data-black-swan-kg/"), pattern = ".rds", full.names = TRUE)

# for log(a) for weights in grams
f2 <- list.files(paste0("data-generated/condition-data-black-swan/"), pattern = ".rds", full.names = TRUE)

d2 <- purrr::map_dfr(f2, readRDS)


dp <- d2 |> ungroup() |>
  filter(sex %in% c(1,2),
         !is.na(species_science_name)) |>
  mutate(Species = str_to_title(species_common_name),
         `latin_name` = paste0("",str_to_sentence(species_science_name),""),
         threshold = round(threshold, 1),
         lw_m_a = round(exp(lw_m_log_a), 5),
         lw_f_a = round(exp(lw_f_log_a), 5),
         lw_m_log_a = round(lw_m_log_a, 2),
         lw_f_log_a = round(lw_f_log_a, 2),
         lw_m_b = round(lw_m_b, 2),
         lw_f_b = round(lw_f_b, 2)) |>
  select(Species, latin_name, sex, threshold,
         lw_m_log_a, lw_f_log_a,
         # lw_m_a, lw_f_a,
         lw_m_b, lw_f_b,) |>
  filter( !(Species %in% species_to_remove)) |>
  distinct() |>
  pivot_wider(names_from = sex, values_from = threshold) |>
  relocate(Species, latin_name,
           lw_m_log_a, lw_f_log_a,
           # lw_m_a, lw_f_a,
           lw_m_b, lw_f_b, `1`, `2`)

# library(formattable)
dp |>
  knitr::kable(format = "latex",
               col.names = c("Common name", "Scientific name","Male", "Female", "Male", "Female", "Male", "Female"),
               booktabs = TRUE, align = "llrrrrrr", caption = "TODO", label = "bio-params") |>
  kableExtra::column_spec(1, width = "3.4cm") |>
  kableExtra::column_spec(2, width = "3.75cm", italic = TRUE) |>
  kableExtra::column_spec(3, width = "0.9cm") |>
  kableExtra::column_spec(5, width = "0.4cm") |>
  kableExtra::column_spec(7, width = "0.4cm") |>
  add_header_above(c("Species" = 2, "ln(a)" = 2, "b" = 2, "50% maturity" = 2))
