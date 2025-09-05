### maps
library(tidyverse)

dsamp <- readRDS("data-generated/all-samples-used.rds")
dset <- readRDS("data-generated/all-sets-used.rds")


filter(
  dset, species_common_name == "pacific cod", year >= 2000,
  # year <2006,
  survey_type != "HBLL",
  survey_type != "SABLE",
  survey_type != "IPHC FISS"
) |>
  mutate(
    survey_type = ifelse(survey_abbrev %in% c("MSSM WCVI", "MSSM QCS"), "MSSM", as.character(survey_type)),
    survey_type = ifelse(survey_type %in% c("SYN"), "SYNOPTIC", as.character(survey_type))
  ) |>
  ggplot() +
  scale_colour_brewer(palette = "Paired") +
  labs(
    colour = "Survey",
    shape = "Survey",
    x = "Longitude",
    y = "Latitude"
  ) +
  geom_point(aes(longitude, latitude,
                 shape = survey_type,
                 colour = survey_type
  ), alpha = 0.95, size = 1) +
  facet_wrap(~year) +
  geom_sf(data = pacea::bc_coast, fill = "grey85", colour = NA) +
  coord_sf(xlim = range(dset$longitude, na.rm = TRUE), ylim = range(dset$latitude, na.rm = TRUE), expand = FALSE) +
  ggsidekick::theme_sleek()

ggsave("figs/man/supp-map-surveys-trawl.pdf", width = 10, height = 10)


filter(
  dset, species_common_name == "pacific cod", year >= 2000,
  survey_type %in% c("HBLL",
                     # "SABLE",
                     "IPHC FISS")
) |>
  ggplot() +
  scale_colour_brewer(palette = "Paired") +
  labs(
    colour = "Survey",
    shape = "Survey",
    x = "Longitude",
    y = "Latitude"
  ) +
  geom_point(
    aes(longitude, latitude,
        shape = survey_type,
        colour = survey_type
    ),
    alpha = 0.95, size = 1
  ) +
  facet_wrap(~year) +
  geom_sf(data = pacea::bc_coast, fill = "grey85", colour = NA) +
  coord_sf(
    xlim = range(dset$longitude, na.rm = TRUE),
    ylim = range(dset$latitude, na.rm = TRUE), expand = FALSE
  ) +
  ggsidekick::theme_sleek()

ggsave("figs/man/supp-map-surveys-ll.pdf", width = 10, height = 10)



area <- list(matrix(c(-134.5, -131, -127, -128.25, -124, -125.5, -132, -134.5,
                      54.4, 54.4,   51,   50.75, 48.5, 47.75, 52, 54.4),
                    ncol = 2))


coast <- load_coast(range(area[[1]][,1]) + c(-0.2, 0.2),
                    range(area[[1]][,2]) + c(-0.2, 0.2)
)

area_xy <- as.data.frame(area)
area_xy$X <- area_xy$X1
area_xy$Y <- area_xy$X2

ggplot(area_xy, aes(X, Y)) +
  geom_polygon(alpha =0.3, fill = "skyblue") +
  geom_polygon(
    data = coast, aes_string(x = "X", y = "Y", group = "PID"),
    fill = "grey87", col = "grey70", lwd = 0.2
  ) +
  xlab("Longitude") + ylab("Latitude") +
  ggsidekick::theme_sleek()

ggsave("figs/bccm-area.png", width = 3, height = 3)


area <- list(matrix(c(-134.5, -131, -127, -128.25, -124, -125.5, -132, -134.5,
                      54.4, 54.4,   51,   50.75, 48.5, 47.75, 52, 54.4),
                    ncol = 2))


coast <- load_coast(range(area[[1]][,1]) + c(-0.2, 0.2),
                    range(area[[1]][,2]) + c(-0.2, 0.2)
)

area_xy <- as.data.frame(area)
area_xy$X <- area_xy$X1
area_xy$Y <- area_xy$X2


filter(
  dset, species_common_name == "pacific cod", year >= 2000,
  # year <2006,
  # survey_type != "HBLL",
  survey_type != "SABLE"
  # survey_type != "IPHC FISS"
) |> mutate(
  survey_group = as.factor(
    case_when(
      survey_abbrev %in% c("HBLL OUT N", "HBLL OUT S")~"HBLL",
      survey_abbrev %in% c("MSSM QCS", "MSSM WCVI")~"MSSM",
      survey_abbrev %in% c("HS PCOD", "THORNYHEAD", "HS MSA", "SYN HS", "SYN QCS", "SYN WCHG", "SYN WCVI")~"TRAWL",
      survey_series_id == 68~"HAKE",
      TRUE~survey_abbrev
    ))) |>
  mutate(
    survey_type = ifelse(survey_abbrev %in% c("MSSM WCVI", "MSSM QCS"), "MSSM", as.character(survey_type)),
    survey_type = ifelse(survey_type %in% c("SYN"), "SYNOPTIC", as.character(survey_type))
  ) |>
  ggplot() +
  # geom_polygon(data = area_xy, aes(X, Y), alpha =0.3, fill = "grey90") +
  scale_colour_brewer(palette = "Paired") +
  labs(
    colour = "Gear type",
    shape = "Gear type",
    x = "Longitude",
    y = "Latitude"
  ) +
  geom_point(aes(longitude, latitude,
                 # shape = survey_type,
                 # colour = survey_type
                 colour = survey_group
  ), alpha = 0.4,
  shape = 16,
  size = 0.5) +
  # facet_wrap(~year) +
  # scale_colour_viridis_d() +
  scale_colour_viridis_d(option = "G", direction = -1, end = 0.85, begin = 0.2) +
  geom_sf(data = pacea::bc_coast, fill = "grey15", colour = NA) +
  coord_sf(xlim = range(dset$longitude, na.rm = TRUE), ylim = range(dset$latitude, na.rm = TRUE), expand = FALSE) +
  geom_text(x = -126, y = 54, label = "British Columbia,\nCanada", colour = "white", size = 5) +
  geom_text(x = -132, y = 51, label = "Pacific\n      Ocean", colour = "grey30", size = 5) +
  # geom_text(x = -129.5, y = 49.3, label = "Pacific\n       Ocean", colour = "grey20") +
  guides(colour = guide_legend(override.aes = list(size = 1,
                                                   alpha = 0.6))) +
  ggsidekick::theme_sleek() + theme(legend.position = "inside",
                                    legend.position.inside = c(.15,.15))



ggsave("figs/man/map-surveys-all.pdf", width = 5, height = 5)



