
## get model structures
library(tidyverse)
library(sdmTMB)
devtools::load_all(".")
theme_set(ggsidekick:::theme_sleek())
source("analysis/00-species-list.R")

# biomass models
knot_distance <- 20

# just plot the better model
best_model <- readRDS("data-generated/all-models-compared.rds") %>%
  # filter(prop_ci_error < 0.05) %>%
  group_by(species) %>% mutate(
    min_diff = min(total_diff, na.rm = TRUE)
  ) %>% filter(total_diff == min_diff) %>%
  select(species, model_string, model_total) |>
  mutate(species2 = ifelse(species == "North Pacific Spiny Dogfish",
                           "Pacific Spiny Dogfish", species
  )) |> arrange(species2)

best_model <- filter(best_model, !(tolower(species) %in% tolower(c(species_to_remove))))

mdat <- list()

for (i in seq_along(best_model$species2)){
# for (i in 1:2){
  species <- best_model$species2[i]
  spp <- gsub(" ", "-", gsub("\\/", "-", tolower(best_model$species[i])))

  pd <- list()

  for(j in 1:4) {

    m <- NULL
    if(j == 1)  {
      group_tag <- "imm"
      group_label <-  "Immature"
    }
    if(j == 2)  {
      group_tag <- "mat-m"
      group_label <- "Mature male"
    }
    if(j == 3)  {
      group_tag <- "mat-fem"
      group_label <- "Mature female"
    }
    if(j == 4)  {
      group_tag <- "total"
      group_label <- "Total"
    }

    if(group_tag == "total") {
      model_name <- best_model$model_total[i]
      model_type <- "Total"
    } else {
      model_name <- best_model$model_string[i]
      model_type <- "Split"
    }

    model_file_name <- paste0("data-generated/density-models/",
                              model_name  , "/", group_tag, "/",
                              spp, "-", group_tag, "-", model_name, "-",
                              knot_distance,
                              "-km.rds")

    if(file.exists(model_file_name)) {
        m <- readRDS(model_file_name)
        pd[[j]] <- get_model_structure(m)
        pd[[j]]$species <- species
        pd[[j]]$group <- group_label
        pd[[j]]$model_name <- model_name
        pd[[j]]$model <- model_type
  }
  }
  mdat[[i]] <- do.call(bind_rows, pd)
}

allmods <- do.call(bind_rows, mdat)

saveRDS(allmods, "data-generated/biomass-model-structures.rds")

# Shared ranges helped in 16% of cases
check_range <- filter(allmods, `Spatial field on 1st component`)
sum(check_range$`Range shared`)/nrow(allmods)

# Proportion dropping spatial field on second component = 41%
1-(sum(check_range$`Spatial field on 2nd component`, na.rm = TRUE)/nrow(allmods))


# Proportion dropping spatial field on both components = 6%
no_sp <- filter(allmods, !(`Spatial field on 1st component`), !(`Spatial field on 2nd component`))
length(no_sp)/nrow(allmods)

# none
sp1_st2 <- filter(allmods, `Spatial field on 1st component`, !(`Spatiotemporal field on 1st component`), !(`Spatial field on 2nd component`), `Spatiotemporal field on 2nd component`)


# # Proportion dropping spatiotemporal field = 4%
# 1-(sum(allmods$`Spatiotemporal field on 1st component`)/length(allmods$`Spatiotemporal field on 1st component`))





## condition models
model_name <- "2024-09-doy-ld0c"

group_tag <-  c("imm", "mat-fem", "mat-m")
mcond <- list()
# for (j in seq_along(group_tag)){
  for (j in 2:3){
f <- list.files(paste0("data-generated/condition-models-", group_tag[[j]], "/", model_name, "/"),
                pattern = ".rds", full.names = TRUE)

m <- purrr::map(f, readRDS)

pd <- list()
if(group_tag[[j]] == "mat-m") {group_label <- "Mature male"}
if(group_tag[[j]] == "mat-fem") {group_label <- "Mature female"}
if(group_tag[[j]] == "imm") {group_label <- "Immature"}

for (i in seq_along(m)){
  pd[[i]] <- get_model_structure(m[[i]])
  pd[[i]]$species <- paste0(stringr::str_to_title(m[[i]]$data$species_common_name[1]))
  pd[[i]]$group <- group_label
  pd[[i]]$model_name <- model_name
}
mcond[[j]] <- do.call(bind_rows, pd)
}

allcond <- do.call(bind_rows, mcond)

saveRDS(allcond, "data-generated/condition-model-structures.rds")

# Anisotropy dropped
1-(sum(allcond$Anisotropy, na.rm = TRUE)/nrow(allcond))

# Shared ranges helped in 1% of cases
check_range <- filter(allcond, `Spatial field on 1st component` & `Spatiotemporal field on 1st component`)
(sum(check_range$`Range shared`)/nrow(allcond))

# Proportion dropping spatial field = 61%
1-(sum(allcond$`Spatial field on 1st component`)/nrow(allcond))

# Proportion dropping spatiotemporal field = 4%
1-(sum(allcond$`Spatiotemporal field on 1st component`)/nrow(allcond))

