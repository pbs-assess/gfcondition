# plots combining species
library(tidyverse)
library(sdmTMB)
library(ggsidekick)
devtools::load_all(".")
theme_set(ggsidekick:::theme_sleek())

fig_height <- 4 * 2
fig_width <- 5 * 2

source("analysis/00-species-list.R")


### for combining biomass indices

index_list <- expand.grid(species = unlist(species_list),
                          maturity = c("mat", "imm", "all"),
                          males = c(TRUE, FALSE)) %>%
  mutate(
    females = ifelse(males == FALSE & maturity == "mat", TRUE, FALSE),
    males = ifelse(maturity == "imm", FALSE, males)
  ) %>%
  distinct()


# these files are built while running the condition models
# only species used there will be included here
f1 <- list.files(paste0("data-generated/compare-models/"),
                 pattern = ".rds", full.names = TRUE)
d <- purrr::map_dfr(f1, readRDS)

saveRDS(d, "data-generated/all-models-compared.rds")

# d <- filter(d, !(species %in% species_to_remove))

models <- readRDS("data-generated/all-models-compared.rds") %>%
  filter(prop_ci_error < 0.1) %>%
  group_by(species) %>% mutate(
    min_diff = min(total_diff, na.rm = TRUE)
  ) %>% filter(total_diff == min_diff) %>%
  select(species, model_string)

index_list <- left_join(index_list, models)
index_list2 <- index_list %>% as.list()

combine_indices <- function(species, maturity, males, females, model_string,
                            model_type = "density",
                            file_prefix = "data-generated/density-index/i-"
                            ) {

spp <- gsub(" ", "-", gsub("\\/", "-", tolower(species)))

mat_class <- maturity
just_males <- males
just_females <- females


if (mat_class == "mat") {
  if (just_males) {
      group_tag <- "mat-m"
      group_label <- "mature males"
  } else {
    if (just_females) {
        group_tag <- "mat-fem"
        group_label <- "mature females"
    } else {
        group_tag <- "mat"
        group_label <- "mature (females and males)"
    }
  }
} else {
  if (mat_class == "imm") {
      group_tag <- "imm"
      group_label <- "immatures"
  } else {
    group_label <- group_tag <- "total"
  }
}

# browser()

if(model_type == "density") {
  f <- paste0(file_prefix, "/", model_string, "/", group_tag,
              "/i-", spp, "-", group_tag, "-", model_string, "-20-km.rds")
# f <- paste0(file_prefix, spp, "-", group_tag, "-", model_string, ".rds")
} else {
f <- paste0(file_prefix, group_tag, "-", spp, "-", model_string, ".rds")
}

if(file.exists(f)) {
  i <- readRDS(f)
i$species <- species
i$group <- group_label
i$model_string <- model_string
return(i)
}
return(NULL)
}


# density indices
d <- purrr::pmap_dfr(
  index_list, #best model
  combine_indices,
  model_type = "density",
  file_prefix = "data-generated/density-index/",
  .id = "model")

saveRDS(d, "data-generated/density-indices-best.rds")

# get totals
# model_string_fixed <- "dln-all-2024-09"
model_string_fixed <- "dln-ss5-2024-09" # this is for total


index_list2 <- index_list %>% mutate(model_string = model_string_fixed) %>% as.list()
d0 <- purrr::pmap_dfr(
  index_list2, # specific model chosen above
  combine_indices,
  model_type = "density",
  file_prefix = "data-generated/density-index/",
  .id = "model") %>% filter(group == "total")

saveRDS(d0, paste0("data-generated/density-indices-total-", model_string_fixed, ".rds"))


model_string_fixed <- "dln-only-sampled-2024-09"
index_list2 <- index_list %>% mutate(model_string = model_string_fixed) %>% as.list()
d1 <- purrr::pmap_dfr(
  index_list2, # specific model chosen above
  combine_indices,
  model_type = "density",
  file_prefix = "data-generated/density-index/",
  .id = "model")

saveRDS(d1, paste0("data-generated/density-indices-", model_string_fixed, ".rds"))

# model_string_fixed <- "dln-all-split-2024-09" # these are `all catches' models
model_string_fixed <- "dln-ss5-split-2024-09" # these are `all catches' models

index_list2 <- index_list %>% mutate(model_string = model_string_fixed) %>% as.list()
d2 <- purrr::pmap_dfr(
  index_list2, # specific model chosen above
  combine_indices,
  model_type = "density",
  file_prefix = "data-generated/density-index/",
  .id = "model")

saveRDS(d2, paste0("data-generated/density-indices-", model_string_fixed, ".rds"))




d0 <- filter(d0, !(species %in% species_to_remove)) %>% mutate(
  species = ifelse(species == "Rougheye/Blackspotted Rockfish Complex",
                   "Rougheye/Blackspotted", species
  ))
d <- filter(d, !(species %in% species_to_remove))

d |> mutate(
  # group = forcats::fct_relevel(group, "immatures", "mature males", "mature females"),
species = ifelse(species == "Rougheye/Blackspotted Rockfish Complex",
                            "Rougheye/Blackspotted", species
  ),
  group = factor(group, levels = c( "mature females","mature males", "immatures"),
                 labels = c("Mature females", "Mature males", "Immatures"))
  ) |>
ggplot(aes(year, est/1000*4,
           ymin = lwr/1000*4,
           ymax = upr/1000*4)) +#convert to tons, multiply by grid cell area
  geom_ribbon(data = d0, alpha = 0.08) +
  geom_line(aes(colour = group, linetype = "Split")) +
  geom_ribbon(aes(alpha = group, fill = group)) +
  geom_line(data = d0, colour = "black", aes(linetype = "Total")) + #linetype = "dotted"
  facet_wrap(~species, scales = "free_y", ncol = 5) +
  # scale_linetype(limits = c("Total")) +
  scale_alpha_discrete(range = c(0.45, 0.25)) +
  scale_color_viridis_d(
    # end = 0.99,
    direction =-1,
    option = "D") +
  scale_fill_viridis_d(# end = 0.99,
                       direction =-1,
                       option = "D") +
  # scale_color_viridis_d(option = "C", direction =-1, end = 0.9) +
  # scale_fill_viridis_d(option = "C", direction =-1, end = 0.9) +
  # guides(linetype = guide_legend(order = 1),colour = guide_legend(order = 2),fill = guide_legend(order = 2),fill = guide_legend(alpha = 2)) +
  # theme(legend.position = c(0.7, 0.04) )+
  theme(legend.position = c(0.6,0.05),
        legend.direction = "vertical", legend.box = "horizontal") +

  labs(
    fill = NULL,
    alpha = NULL,
    colour = NULL,
    linetype = NULL,
    x="Year", y="Relative biomass")


ggsave(paste0("figs/all-density-indices",
              # "-fixed",
              # "-not-flat-",
              # "-flatfish-",
              "-with-best-model-",
              # "2024-09-filtered",
              model_string_fixed,
              ".png"),
       # height = fig_height*.5, width = fig_width*1.1
       # height = fig_height*.65, width = fig_width*1.1
       height = fig_height*1.5, width = fig_width*1.0
)


set_legend_position <- c(0.7, 0.05)

d1 |> bind_rows(d2) |>
  filter(!(species %in% species_to_remove)) |>
  # bind_rows(d0) |>
  mutate(
  # group = forcats::fct_relevel(group, "immatures", "mature males", "mature females"),
  species = ifelse(species == "Rougheye/Blackspotted Rockfish Complex",
                   "Rougheye/Blackspotted", species
  ),
  group = factor(group, levels = c("immatures", "mature males", "mature females", "total"),
                        labels = c("Immatures", "Mature males", "Mature females", "Total")),
  # group = factor(group, levels = c( "mature females","mature males", "immatures", "total"),
  #                labels = c("Mature females", "Mature males", "Immatures", "Total"))
) |>
  select(-group, -model, -type) |>
  group_by(model_string, year, species) |>
  summarise_all("sum") |>
  ggplot(aes(year, est/1000*4,
             linetype = model_string
             )) +#convert to tons, multiply by grid cell area
  geom_line(
    # aes(colour = model_string),
            linewidth = 0.7) +
  geom_line(data = d0, aes(year, est/1000*4), colour = "black",
            linetype = "solid", alpha = 0.3, linewidth = 0.7) +
  geom_ribbon(data = d0, aes(
    ymin = lwr/1000*4,
    ymax = upr/1000*4
    ),alpha = 0.1) +
  # geom_line(aes(colour = group, linetype = model_string)) +
  # geom_ribbon(alpha = 0.1) +
  facet_wrap(~species, scales = "free_y", ncol = 5) +
  # scale_linetype(limits = c("Total")) +
  scale_linetype_manual(values=c(1,2,3),
      labels = c("Total density (all data)","Spilt (all data)","Split (measured survey-years)")
) +
  scale_alpha_discrete(range = c(0.1, 0.2)) +
  # scale_color_discrete(
  #   labels = c("Split (measured survey-years)","Total density (all data)","Spilt (all data)")) +
  # scale_fill_viridis_d(option = "C", direction =-1, end = 0.9) +
  # guides(linetype = guide_legend(order = 1),colour = guide_legend(order = 2),fill = guide_legend(order = 2),fill = guide_legend(alpha = 2)) +
  theme(legend.position = set_legend_position)+
  labs(
    fill = NULL,
    alpha = NULL,
    colour = NULL,
    linetype = NULL,
    x="Year", y="Relative biomass")

ggsave(paste0("figs/all-density-indices",
              "-summed-comparison-2024-09",
              "-w-pcod",
              "-BW.png"),
       height = fig_height*1.4, width = fig_width*1
)


hex <- scales::hue_pal()(2)

d1 |> bind_rows(d2) |>
  bind_rows(d0) |>
  filter(!(species %in% species_to_remove)) |>
  mutate(
    species = ifelse(species == "Rougheye/Blackspotted Rockfish Complex",
                     "Rougheye/Blackspotted", species
    ),
    group = factor(group, levels = c( "mature females","mature males", "immatures", "total"),
                   labels = c("Mature females", "Mature males", "Immatures", "Total"))
  ) |>
  select(-group, -model, -type) |>
  distinct() |>
  group_by(model_string, year, species) |>
  summarise_all("sum") |>
  ggplot(aes(year, est/1000*4,
             linetype = model_string
  )) +#convert to tons, multiply by grid cell area
  # geom_line(data = d0, aes(year, est/1000*4), colour = "black",
  #           linetype = "solid", alpha = 0.3, linewidth = 0.7) +
  geom_line(
    aes(colour = model_string),
    linewidth = 0.7) +
  geom_ribbon(data = d0, aes(
    ymin = lwr/1000*4,
    ymax = upr/1000*4
  ),alpha = 0.1) +
  # geom_line(aes(colour = group, linetype = model_string)) +
  # geom_ribbon(alpha = 0.1) +
  facet_wrap(~species, scales = "free_y", ncol = 5) +
  # scale_linetype(limits = c("Total")) +
  scale_linetype_manual(values=c(1,2,3),
                        labels = c("Total density (all data)","Spilt (all data)", "Split (measured survey-years)")
  ) +
  scale_alpha_discrete(range = c(0.1, 0.2)) +
  scale_color_manual(values= c("#A9A9A9", hex[2],hex[1]),
    labels = c("Total density (all data)","Spilt (all data)","Split (measured survey-years)")) +
  scale_fill_viridis_d(option = "C", direction =-1, end = 0.9) +
  labs(
    fill = NULL,
    alpha = NULL,
    colour = NULL,
    linetype = NULL,
    x="Year", y="Relative biomass") +
  # guides(linetype = guide_legend(order = 1),colour = guide_legend(order = 2),fill = guide_legend(order = 2),fill = guide_legend(alpha = 2)) +
  theme(legend.position = set_legend_position )

ggsave(paste0("figs/all-density-indices",
              "-summed-comparison-2024-09",
              "-w-pcod",
              ".png"),
       height = fig_height*1.4, width = fig_width*1
)


# condition model ----
# model_name <- "all-st2002-doy-d0c"
# model_name <- "all-st2002-doy"

# d2 <- purrr::pmap_dfr(index_list, combine_indices,
#                      model_type = "condition",
#                      file_prefix = paste0("data-generated/cond-index/",
#                                           model_name,"/cond-index-"),
#                      model_string = paste0(model_name,"-15-km"),
#                      .id = "model")
#

