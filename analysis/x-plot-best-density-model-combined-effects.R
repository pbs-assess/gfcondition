
## effect plots
library(tidyverse)
library(sdmTMB)
library(ggsidekick)
library(aplot)
library(patchwork)
library(gridGraphics)

# devtools::load_all(".")
theme_set(ggsidekick:::theme_sleek())

dir.create(paste0("data-generated/density-effects/"))
source("analysis/00-species-list.R")


# plot_split_density_effects
knot_distance <- 20
# variable <- "days_to_solstice"
# variable <- "log_depth_c"

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


plot_split_density_effects <- function(best_model, variable, knot_distance = 20) {

m <- list()
p <- list()


for (i in seq_along(best_model$species2)){
# for (i in 25:35){
  species <- best_model$species2[i]
  spp <- gsub(" ", "-", gsub("\\/", "-", tolower(best_model$species[i])))

  pd <- list()
  # labels <- list()

  m[[i]] <- NULL

  for(j in 1:4) {
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
  filename <- paste0("data-generated/density-effects/",
                     model_name, "-", group_tag, "-",
                     spp, "-", variable,".rds")

  if(file.exists(filename)) {
    pd[[j]] <- readRDS(filename)
  } else {

    m[[i]] <- readRDS(model_file_name)

    # m[[i]] <- sdmTMB:::update_version(m[[i]])

    m[[i]]$data$depth <- m[[i]]$data$depth_m

  if (variable == "log_depth_c"|variable == "depth") {
    nd <- data.frame(
      days_to_solstice = 0,
      survey_type = as.factor("SYN"),
      log_depth_c = seq(quantile(m[[i]]$data$log_depth_c, 0.025),
                          quantile(m[[i]]$data$log_depth_c, 0.975),
                          length.out = 50
      ),
      year = max(m[[i]]$data$year) # a chosen year
    )
    xlabel <- "Log depth (centered on mean)"
  }

  if(variable == "days_to_solstice") {
    nd <- data.frame(
      days_to_solstice = seq(quantile(m[[i]]$data$days_to_solstice, 0.025),
                             quantile(m[[i]]$data$days_to_solstice, 0.975),
                             length.out = 50
      ),
      survey_type = as.factor("SYN"),
      log_depth_c = 0,
      year = max(m[[i]]$data$year) # a chosen year
    )
    xlabel <- "Days from solstice"
  }

  pd[[j]] <- predict(m[[i]], newdata = nd, se_fit = TRUE, re_form = NA)
  pd[[j]]$depth <- exp(pd[[j]]$log_depth_c + 5)

  saveRDS(pd[[j]], filename)
  }
  } else {
    # for missing models, fill in empty data
    nd <- data.frame(
      days_to_solstice = 0,
      survey_type = as.factor("SYN"),
      log_depth_c = 0,
      year = 2020L
    )
    pd[[j]] <- nd
    pd[[j]]$est <- NA
    pd[[j]]$est_se <- NA
    pd[[j]]$depth <- exp(pd[[j]]$log_depth_c + 5)
  }
  pd[[j]]$group <- group_label
  pd[[j]]$model_name <- model_name
  pd[[j]]$model <- model_type
  pd[[j]]$est1 <- NA
  pd[[j]]$est2 <- NA

  }

  dd <- do.call(rbind, pd)


# set_alpha <- 0.8

  m[[i]] <- readRDS(paste0("data-generated/density-models/",
                           model_name  , "/total/",
                           spp, "-total-", best_model$model_total[i], "-",
                           knot_distance,
                           "-km.rds"))

  # m[[i]] <- sdmTMB:::update_version(m[[i]])

  m[[i]]$data$depth <- m[[i]]$data$depth_m

  # browser()

if (variable == "log_depth_c") {
  variable <- "depth"
  xlabel <- "Depth (m)"
}

  if (variable == "days_to_solstice") {
    variable <- "days_to_solstice"
    xlabel <- "Days from solstice"
  }

p[[i]] <- ggplot(dd, aes(.data[[variable]], exp(est)
)) +
  geom_rug(data = m[[i]]$data, aes(.data[[variable]], y = 0.9),
             sides = "b", alpha = 0.1, inherit.aes = FALSE) +
  geom_line(data = filter(dd, group == "Total"), colour = "black", aes(linetype = "Total")) +
  geom_line(data = filter(dd, group != "Total"), aes(colour = group, linetype = "Split"), alpha = 0.8) +
  geom_ribbon(data = filter(dd, group != "Total"),
              aes(ymin = exp(est - 1.96 * est_se),
                  ymax = exp(est + 1.96 * est_se),
                  alpha = group,
                  fill = group)) +
  scale_x_continuous() +
  scale_linetype_manual(values=c(1,2)) +
  scale_alpha_discrete(range = c(0.05, 0.1)) +
  scale_color_viridis_d(option = "D", direction =1) +
  scale_fill_viridis_d(option = "D", direction =1) +
  coord_cartesian(#expand = F,
                  xlim = c(min(dd[[variable]]), max(dd[[variable]])),
                  ylim = c(NA,
                           max(exp(dd$est), na.rm = TRUE)+
                           max(exp(dd$est)*0.9, na.rm = TRUE))) +
  # coord_cartesian(expand = F, ylim = c(NA, NA)) +
  labs(x = xlabel,
       y = "Effect on biomass") +
theme(axis.title = element_blank())

p[[i]] <- p[[i]] + ggtitle(paste0(species))
}

saveRDS(p, paste0("data-generated/density-effects/", "best-models-",
                  # group_tag, "-",
                  variable, ".rds"))

# if (variable == "log_depth_c") {
#   variable <- "depth"
#   xlabel <- "Depth (m)"
# }

y_lab_big <- ggplot() +
  annotate(geom = "text", x = 1, y = 1, size = 5,
           label = paste0("Conditional effect on biomass"), angle = 90) +
  coord_cartesian(clip = "off")+
  theme_void()

x_lab_big <- ggplot() +
  annotate(geom = "text", x = 1, y = 1, size = 5,
           label = paste("", xlabel)) +
  coord_cartesian(clip = "off")+
  theme_void()

# length(p)/3

design = "
AAAAAA
#BBBBB
"

# if(variable == "log_depth_c") {set_font <- 9} else {
  set_font <- 12
# }


(g2 <- ((y_lab_big |
          wrap_plots(gglist = p, ncol = 5) &
          # xlim(0, 820) & # if plotting depth
          theme(text = element_text(size = set_font),
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank())) +
         plot_layout(widths = c(0.05, 1), guides = "collect")&labs(
           colour = "", fill = "", alpha = "", linetype = ""
         ))
  /x_lab_big + plot_layout(heights = c(1,0.05), design = design)
)

ggsave(paste0("figs/man/dens-effects-", variable, "-trans-",
              best_model$model_total[1], "-combined-for-best.png"),
       height = 18, width = 14)


(g2 <- ((y_lab_big |
           wrap_plots(gglist = p, ncol = 7) &
           # xlim(0, 820) & # if plotting depth
           theme(text = element_text(size = set_font),
                 axis.text.y = element_blank(),
                 axis.ticks.y = element_blank(),
                 legend.position.inside = c(0.55,0.08))) +
          plot_layout(widths = c(0.05, 1), guides = "collect")&labs(
            colour = "", fill = "", alpha = "", linetype = ""
          ))
  /x_lab_big + plot_layout(heights = c(1,0.05), design = design)
)

ggsave(paste0("figs/dens-effects-", variable, "-trans-",
              best_model$model_total[1], "-combined-for-best-wide.png"),
       height = 8.5, width = 16.5)

}


plot_split_density_effects(best_model, variable = "days_to_solstice")
plot_split_density_effects(best_model, variable = "log_depth_c")


## would work if layout stayed the same
# p_axis <- ggplot() + labs(x = xlabel, y = "Condition factor")
# x_lab_big <- cowplot::get_plot_component(p_axis, "xlab-b")
# y_lab_big <- cowplot::get_plot_component(p_axis, "ylab-l")
# design = "
# FAB
# FCD
# #EE
# "
#
# c(gglist = p, list(x_lab_big, y_lab_big)) |>
#   wrap_plots() +
#   plot_layout(heights = c(20, 20, 1), widths = c(1, 25, 25), design = design)

# (g <- (y_lab_big | wrap_plots(gglist = p, ncol = 2)/x_lab_big ) +
#     plot_layout(widths = c(0.1, 1), heights = c(1,0.1)))

