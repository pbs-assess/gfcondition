## maturity plots
library(tidyverse)
# library(sdmTMB)
library(ggsidekick)
library(aplot)
library(patchwork)
# devtools::load_all(".")
theme_set(ggsidekick:::theme_sleek())

source("analysis/00-species-list.R")

included_only <- TRUE

## while the maturity data is loaded, might as well produce all the Le Cren's plots together
dat <- readRDS("data-generated/all-samples-used.rds") %>%
  filter(## remove the combined version of the sablefish survey (only version retained currently)
    ## because this survey is at different time of year than all the others
    !(survey_abbrev %in% c("SABLE")),
    !is.na(longitude), !is.na(latitude)
  )

# temporary fix for IPHC because of sample size imbalance over time and
# uncertainty about how weight was measured
  dat <- filter(dat, !(survey_abbrev == "IPHC FISS" & species_common_name == tolower("Pacific Halibut")))



dat <- dat %>%
  filter(!(weight > 900 & species_common_name == tolower("Pacific Sanddab")))  %>%
  filter(!(weight > 3500 & species_common_name %in% tolower(c("Yellowmouth Rockfish",
                                                              # "Widow Rockfish",
                                                              "Quillback Rockfish"))))

mat_threshold <- 0.5
f1 <- list.files(paste0("data-generated/condition-data-black-swan/"), pattern = ".rds", full.names = TRUE)

d1 <- purrr::map_dfr(f1, readRDS)
spp <- unique(d1$species_common_name)

p2 <- list()

for (i in seq_along(spp)){
  # if(is.null(m[[i]])) {
  #   p2[[i]] <- NULL
  # } else {
  min_LC <- min(d1$cond_fac, na.rm = TRUE)
  max_LC <- max(d1$cond_fac, na.rm = TRUE)

  .ds <- d1 |> filter(species_common_name == spp[i])|>
    mutate(sex_label = as.factor(ifelse(sex == 1, "Male", ifelse(sex == 2, "Female", "Unknown"))))

  # browser()

  p2[[i]] <- dat %>% filter(species_common_name == spp[i]) |>
    mutate(weight = weight/1000,
           sex_label = as.factor(ifelse(sex == 1, "Male", ifelse(sex == 2, "Female", "Unknown")))
    ) |>
    filter(
      sex %in% c(1,2,3), # are ones that couldn't tell--hopefully only tiny ones?
      weight < 50) |>
    ggplot(aes(length, weight, shape = sex_label)) +
    geom_point(colour = "red") +
    geom_point(colour = "white", data = .ds) +
    geom_point(aes(colour = cond_fac), data = .ds, alpha = 0.4) +
    geom_vline(xintercept = unique(.ds[.ds$sex == 2,]$threshold), col = "#fde725") +
    # geom_vline(xintercept = m[[i]]$mat_perc$mean$f.mean.p0.5, col = "#fde725") +
    geom_vline(xintercept = unique(.ds[.ds$sex == 1,]$threshold), col = "#21908CFF", alpha = 0.5) +
    # geom_vline(xintercept = m[[i]]$mat_perc$mean$m.mean.p0.5, col = "#440154", alpha = 0.5) +
    labs(
      colour = "Le Cren's",
      shape = "Sex",
      x = "Length (cm)", y = "Weight (kg)") +
    scale_colour_viridis_c(limits= c(min_LC, max_LC)) +
    ggtitle(paste0(toupper(spp[i]))) +
    ggsidekick::theme_sleek() + theme(legend.position = c(0.2,0.8))
  # }

  if(included_only){
    if(spp[i] %in% tolower(c(species_to_remove))) { p2[[i]] <- NULL }
  }
}

p2 <- p2 %>% discard(is.null)

y_lab_big <- ggplot() +
  annotate(geom = "text", x = 1, y = 1, size = 5,
           label = "Weight (kg)", angle = 90) +
  coord_cartesian(clip = "off")+
  theme_void()

x_lab_big <- ggplot() +
  annotate(geom = "text", x = 1, y = 1, size = 5,
           label = "Length (cm)") +
  coord_cartesian(clip = "off")+
  theme_void()


design = "
AAAAAA
#BBBBB
"

(g <- ((y_lab_big |
          wrap_plots(gglist = p2, ncol = 5) &
          theme(text = element_text(size = 9),
                # plot.title = element_blank(),
                axis.title = element_blank())) +
         plot_layout(widths = c(0.05, 1)))
  /x_lab_big + plot_layout(heights = c(1,0.05), design = design, guides = "collect")
)


# (g <- plot_list(gglist = p2, ncol = 3)+ patchwork::plot_layout())
if(included_only){
  ggsave("figs/man/all-Le-Crens-filtered3.png", height = 16, width = 12)
} else{
  ggsave("figs/all-Le-Crens-all-20-trim.png", height = 16, width = 16)
}
