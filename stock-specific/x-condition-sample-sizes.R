# getting summary of condition sample sizes
library(tidyverse)
source("stock-specific/00-set-options.R")
# spp <- gsub(" ", "-", gsub("\\/", "-", tolower(species)))
spp <- gsub(" ", "-", gsub("\\/", "-", tolower(stock_name)))

f <- list.files(paste0("stock-specific/", spp, "/output/specimen-counts/"), pattern = ".rds", full.names = TRUE)

d <- purrr::map_dfr(f, readRDS)

d <- pivot_wider(d, names_from = group, values_from = n, values_fill = 0)

d <- d |> relocate(species, survey_group) |>
  mutate(total = immatures + `mature females` + `mature males`) |>
  arrange(species, -total)


d |>
  knitr::kable(format = "latex", col.names = c("Species", "Survey", "Immatures", "Mature females", "Mature males", "Total"), booktabs = TRUE, align = "lrrrrr", caption = "TODO", label = "model-configs") |>
  kableExtra::column_spec(1, width = "5.5cm") |>
  kableExtra::column_spec(2, width = "2.2cm")|>
  kableExtra::column_spec(6, width = "1.5cm")
