# plots for flow chart

# total biomass sdm
devtools::load_all()
library(tidyverse)
library(gfplot)
library(ggsidekick)
library(patchwork)

source("stock-specific/00-set-options.R")

spp <- gsub(" ", "-", gsub("\\/", "-", tolower(species)))

dir.create(paste0("stock-specific/", spp, "/figs/flow-chart/"), showWarnings = FALSE)
# dir.create(paste0("stock-specific/", spp, "/output/", "density-predictions/"), showWarnings = FALSE)
# dir.create(paste0("stock-specific/", spp, "/output/", "density-index/"), showWarnings = FALSE)

## use definitions from the set-options file, for example:
# dens_model_total <- "dln-2024-11" # this is for total
# dens_model_name1 <- "dln-split-2024-11" # these are `all catches' models
# dens_model_name2 <- "dln-only-sampled-2024-11" # these are `sampled catches' models

dens_model_total
dens_model_name1



m0 <- paste0(spp, "-total-", dens_model_total, "-", knot_distance, "-km")
m1 <- paste0(spp, "-mat-fem-", dens_model_name1, "-", knot_distance, "-km")
m2 <- paste0(spp, "-mat-m-", dens_model_name1, "-", knot_distance, "-km")
m3 <- paste0(spp, "-imm-", dens_model_name1, "-", knot_distance, "-km")

pfn <- paste0("stock-specific/", spp, "/output/", "density-predictions/p-", m0, ".rds")
pmfn <- paste0("stock-specific/", spp, "/output/", "density-predictions/p-", m1, ".rds")
pmfn2 <- paste0("stock-specific/", spp, "/output/", "density-predictions/p-", m2, ".rds")
pifn <- paste0("stock-specific/", spp, "/output/", "density-predictions/p-", m3, ".rds")

# 1. total biomass maps----

p0 <- readRDS(pfn) #|> filter(year %in% c(2000, 2001, 2002, 2003, 2004,
                                         # 2020, 2021, 2022, 2023, 2024))
m0 <- readRDS(paste0("stock-specific/", spp, "/output/density-models/", dens_model_total, "/total/", m0, ".rds"))

m0$data$lat <- m0$data$latitude
m0$data$lon <- m0$data$longitude


p0f <- p0 |> filter(
                    # year %in% c(2000, 2001, 2002, 2003, 2004, 2020, 2021, 2022, 2023, 2024)
                    year %in% c(2000, 2001, 2002, 2003, 2004, 2020, 2021, 2022, 2023, 2024)
                    )
g <- plot_predictions(p0f, m0$data,
                      fill_column = "density_trimmed",
                      fill_label = "Total biomass",
                      fill_scale =
                        ggplot2::scale_fill_viridis_c(trans=fourth_root_power_trans()),
                      bounds = NULL,
                      # rotation_angle = 10,
                      show_raw_data = FALSE
)

g <- g + facet_wrap(~year, ncol = 5) + theme(
  legend.position = "none",
  panel.spacing.y = unit(0, "lines"),
  panel.spacing.x = unit(0.1, "lines"))
# g

ggsave(paste0("stock-specific/", spp, "/figs/flow-chart/01-total-dens.png"),
       height = 3, width = 6
)

# 2. maturity ----
mat <- readRDS(paste0("stock-specific/", spp, "/output/split-catch-data-", spp, ".rds"))
mat$maturity_plot$layers[[4]] <- NULL

mat$maturity_plot + theme(
  legend.position = "inside", legend.position.inside = c(0.2, 0.7),
  plot.title = element_blank())

ggsave(paste0("stock-specific/", spp, "/figs/flow-chart/02-maturity.png"),
       height = 2, width = 3.5
)

# 3. Le Crens----

dat <- readRDS(paste0("stock-specific/", spp, "/data/tidy-survey-samples-", spp, ".rds"))
## remove the sablefish survey because this survey is at different time of year than all the others
dat <- filter(dat, !(survey_abbrev %in% c("SABLE")))
## temporary fix for IPHC because of sample size imbalance over time and
## uncertainty about how weight was measured
if(species == "Pacific Halibut"){
  dat <- filter(dat, survey_abbrev != "IPHC FISS")
}

ds <- readRDS(paste0("stock-specific/", spp, "/output/condition-data-", spp, "-mat-", mat_threshold, ".rds"))

ggplot(dat |> mutate(weight = weight/1000) |> filter(
  sex %in% c(1,2)
  # fishing_event_id %in% fishing_event_id
), aes(length, weight, shape = as.factor(sex))) +
  geom_point(colour = "red") +
  geom_point(colour = "white", data = ds) +
  geom_point(aes(colour = cond_fac), data = ds, alpha = 0.4) +
  geom_vline(xintercept = mat$m$mat_perc$f.p0.5, col = "#fde725") +
  geom_vline(xintercept = mat$m$mat_perc$m.p0.5, col = "#21908CFF") +
  labs(
    colour = "Le Cren's", shape = "Sex",
    x = "Length (cm)", y = "Weight (kg)") +
  scale_colour_viridis_c() +
  scale_shape_discrete(guide = NULL) + # circles = M, triangles = F
  ggsidekick::theme_sleek() + theme(legend.position = c(0.2,0.7))

ggsave(paste0("stock-specific/", spp, "/figs/flow-chart/03-le-crens.png"),
       height = 3., width = 4
)


# 4. split biomass maps----
p1 <- readRDS(pmfn) |> filter(year %in% c(2024)) |> mutate(year = "Mature female\n2024")
p2 <- readRDS(pmfn2) |> filter(year %in% c(2024))|> mutate(year = "Mature male\n2024")
p3 <- readRDS(pifn) |> filter(year %in% c(2024)) |> mutate(year = "Immature\n2024")

g <- plot_predictions(bind_rows(p1,p2,p3), m0$data,
                      fill_column = "density_trimmed",
                      fill_label = "Biomass\ndensity",
                      fill_scale =
                        ggplot2::scale_fill_viridis_c(trans=fourth_root_power_trans()),
                      bounds = NULL,
                      # rotation_angle = 30,
                      show_raw_data = FALSE
)
g <- g + facet_wrap(~year, ncol = 3) + theme(
  panel.spacing = unit(0.2, "lines"),
  legend.position = "none")
# g

ggsave(paste0("stock-specific/", spp, "/figs/flow-chart/04-split-dens.png"),
       height = 2, width = 4
)


# 5. biomass index----

i0 <- paste0("stock-specific/", spp, "/output/", "density-index/", dens_model_total, "/total/i-", m0, ".rds")
i1 <- paste0("stock-specific/", spp, "/output/", "density-index/", dens_model_name1, "/mat-fem/i-", m1, ".rds")
i2 <- paste0("stock-specific/", spp, "/output/", "density-index/", dens_model_name1, "/mat-m/i-", m2, ".rds")
i3 <- paste0("stock-specific/", spp, "/output/", "density-index/", dens_model_name1, "/imm/i-", m3, ".rds")


# get totals
model_string_fixed <- "dln-1998-new"
index_list2 <- index_list %>% mutate(model_string = model_string_fixed) %>% as.list()
d0 <- readRDS(i0)
d <- readRDS(i1) |> bind_rows(readRDS(i2)) |> bind_rows(readRDS(i3))

ds <- d |>
  select(-group, -type) |>
  group_by(model_string, year, species) |>
  summarise_all("sum")

ds$group <- "Sum of split indices"

(g <- d |> #mutate(
#   group = factor(group, levels = c( "mature females","mature males", "immatures"),
#                  labels = c("Mature females", "Mature males", "Immatures"))
# ) |>
    ggplot(aes(year, est/1000*4,
               ymin = lwr/1000*4,
               ymax = upr/1000*4)) +#convert to tons, multiply by grid cell area
    geom_ribbon(data = d0, alpha = 0.08) +
    geom_line(aes(colour = group, linetype = "Split by sex and maturity")) +
    geom_ribbon(aes(alpha = group, fill = group)) +
    geom_line(data = ds, colour = "black", aes(linetype = "Sum of split indices")) +
    geom_line(data = d0, colour = "black", aes(linetype = "Index of total biomass")) + #linetype = "dotted"
    # scale_linetype(limits = c("Total")) +
    scale_alpha_discrete(range = c(0.45, 0.25)) +
    scale_color_viridis_d(
      # end = 0.99,
      direction =-1,
      option = "D") +
    scale_linetype_manual(
      values=c(1,2,3)
    ) +
    scale_fill_viridis_d(# end = 0.99,
      direction =-1,
      option = "D") +
    theme(legend.position = "inside", legend.position.inside = c(0.3, 0.75),
          legend.direction = "vertical", legend.box = "horizontal")+
    labs(
      fill = NULL,
      alpha = NULL,
      colour = NULL,
      linetype = NULL,
      x="Year", y="Relative biomass"))


ggsave(paste0("stock-specific/", spp, "/figs/flow-chart/05-density-indices.png"),
       height = 3, width = 7
)

# 6. condition maps----
cond_model_names <- list.files(paste0("stock-specific/", spp, "/output/cond-pred"),
                          pattern = "", full.names = FALSE)

# for now just plotting first model type
# in this case the density agnositic version
cond_model_name <- cond_model_names[1]


p1 <- readRDS(paste0("stock-specific/", spp, "/output/cond-pred/", cond_model_name))
m <- readRDS(paste0("stock-specific/", spp, "/output/condition-models/", cond_model_name))


    # filter to plot only cells representing 99% of mean predicted biomass
    # cells must be defined by "X", "Y", time by "year", and biomass/abundance stored as "density"
    p2 <- trim_predictions_by_year(p1, 0.001)

    # Map model predictions ----

    # # just sampled sets
    # set_list <- m$data %>% select(fishing_event_id, longitude, latitude, X, Y) %>% distinct() %>%
    #   mutate(lon = longitude, lat = latitude)

    model_dat <- m$data %>%
      group_by(fishing_event_id) %>%
      mutate(
        fishing_event_id = as.factor(fishing_event_id),
        count = n()
      )

    model_dat <- left_join(set_list, model_dat, multiple = "all") %>%
      filter(year >= min(p2$year), year <= max(p2$year)) %>%
      mutate(
        density = group_catch_weight,
        caught = ifelse(catch_count > 0 | catch_weight > 0, 1, 0),
        count = ifelse(is.na(count), 0, count),
        present = ifelse(count > 0, 1, ifelse(caught == 1, 0, NA_integer_))
      )

    # model_dat %>% group_by(present, caught) %>% summarise(n = n()) %>% View()
    p2$log_cond <- log(p2$cond)
    p2 <- p2 %>% mutate(cond_trim = ifelse(cond > quantile(p2$cond, 0.99),
                                           quantile(p2$cond, 0.99), cond
    ))

    g <- plot_predictions(p2, model_dat,
                          fill_column = "cond_trim",
                          fill_label = "Condition \nfactor",
                          fill_scale =
                            ggplot2::scale_fill_viridis_c(),
                          bounds = p1,
                          rotation_angle = 30,
                          show_raw_data = FALSE
    )

    g <- g + facet_wrap(~year, ncol = 8) +
      ggtitle(paste0(species, ": ", unique(m$data$group_name), " ", model_name[i]))






# density indices

ind0 <- readRDS(i0) %>% mutate(index = "Total")
ind1 <- readRDS(i1) %>% mutate(index = "Mature female")
ind2 <- readRDS(i2) %>% mutate(index = "Mature male")
try(ind3 <- readRDS(i3) %>% mutate(index = "Immature"))

bc_inds <- bind_rows(ind0, ind1, ind2)
try(bc_inds <- bind_rows(ind0, ind1, ind2, ind3))

## Plot coastwide indices ----
m <- readRDS(fm)

(p1 <- bc_inds %>%
    mutate(index = fct_relevel(index, rev)) %>%
    ggplot(aes(year, est/1000)) +
    geom_ribbon(aes(ymin = lwr/1000, ymax = upr/1000, fill = index), alpha = 0.3) +
    geom_line(aes(colour = index), linewidth = 0.7) +
    scale_colour_viridis_d(direction = 1, end = 0.8, option = "A") +
    scale_fill_viridis_d(direction = 1, end = 0.8, option = "A") +
    labs(colour = "Biomass Index", fill = "Biomass Index") +
    xlab("Year") +
    ylab("Biomass estimate (kg)") +
    ggtitle(paste0(species), subtitle = paste0(
      "Model: ",
      ifelse(isTRUE(m$family$delta), m$family$clean_name, paste0(m$family[1], "(link = 'log')")),
      ", spatial (", m[["spatial"]][1], ", ", m[["spatial"]][2],
      ") with st RW and ", dens_model_name_long,
      " (", paste(unique(m$data$survey_type), collapse = ", "), ")"
    )))
