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
first_split <- TRUE

# included_only <- FALSE
# first_split <- FALSE

if(first_split) {
  f <- list.files(paste0("data-generated/split-catch-data/"), pattern = ".rds", full.names = TRUE)
} else {
  f <- list.files(paste0("data-generated/maturity-ogives-02/"), pattern = ".rds", full.names = TRUE)
}

d <- purrr::map(f, readRDS)

p <- list()
m <- list()

for (i in seq_along(d)){
# browser()

if(is.null(d[[i]]$model) & is.null(d[[i]]$m)) { p[[i]] <- NULL } else {

  if(first_split) {
    m[[i]] <- d[[i]]$m

  } else {
    m[[i]] <- d[[i]]
  }

  # browser()
  # m[[i]]$data$species_common_name <- ifelse(
  #   m[[i]]$data$species_common_name == "rougheye/blackspotted rockfish complex", "Rougheye/Blackspotted", m[[i]]$data$species_common_name)

  p[[i]] <- gfplot::plot_mat_ogive(m[[i]]) +
    ggtitle(paste0(toupper(
      ifelse(m[[i]]$data$species_common_name == "north pacific spiny dogfish",
           "pacific spiny dogfish", m[[i]]$data$species_common_name)), " (",
                length(unique(m[[i]]$data$specimen_id)),
                " fish, ", length(unique(m[[i]]$data$sample_id)),
      # "/", length(unique(d[[i]]$data$sample_id)),
                " sets)"
                )) +
    theme(axis.title = element_blank())

  # browser()
  if(included_only){
    if(m[[i]]$data$species_common_name[1] %in% tolower(c(species_to_remove))) { p[[i]] <- NULL }
  }
}
}

# move pacific spiny dogfish
p[[20]] <- p[[14]]
p[[14]] <- NULL

p <- p %>% discard(is.null)

# (g <- plot_list(gglist = p, ncol = 5)+ patchwork::plot_layout(guides = "collect"))

y_lab_big <- ggplot() +
  annotate(geom = "text", x = 1, y = 1, size = 5,
           label = "Probability Mature", angle = 90) +
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
          wrap_plots(gglist = p, ncol = 4) &
          theme(text = element_text(size = 9),
                # plot.title = element_blank(),
                axis.title = element_blank())) +
         plot_layout(widths = c(0.05, 1)))
  /x_lab_big + plot_layout(heights = c(1,0.05), design = design,guides = "collect")
)

if(first_split) {
  if(included_only){
    ggsave("figs/man/all-maturities-density-split-filtered2.png", height = 18, width = 15)
  }else{
    ggsave("figs/all-maturities-density-split-all-20.png", height = 18, width = 15)
  }
} else {
  ggsave("figs/all-maturities-condition-split-all.png", height = 18, width = 15)
}


## landscape version
# (g <- ((y_lab_big |
#           wrap_plots(gglist = p, ncol = 5) &
#           theme(text = element_text(size = 9),
#                 # plot.title = element_blank(),
#                 axis.title = element_blank())) +
#          plot_layout(widths = c(0.05, 1)))
#   /x_lab_big + plot_layout(heights = c(1,0.05), design = design,guides = "collect")
# )
#
# ggsave("figs/all-maturities.png", height = 12, width = 18)

# gridExtra::grid.arrange(g, left = "Probablity Mature", bottom = "Length (cm)")

