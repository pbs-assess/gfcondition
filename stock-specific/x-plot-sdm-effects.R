## SDM effect plots
library(tidyverse)
library(sdmTMB)
library(ggsidekick)
library(aplot)
library(patchwork)
library(gridGraphics)

devtools::load_all(".")

source("stock-specific/00-set-options.R")

spp <- gsub(" ", "-", gsub("\\/", "-", tolower(species)))

dir.create(paste0("stock-specific/", spp, "/output/density-effects/"))


model_names <- list.files(paste0("stock-specific/", spp, "/output/density-models"),
                pattern = "", full.names = FALSE)


# variable <- "days_to_solstice"
# variable <- "log_depth_c"

plot_split_density_effects <- function(model_names, variable, knot_distance) {

m <- list()
p <- list()

for (i in seq_along(model_names)){
  spp <- gsub(" ", "-", gsub("\\/", "-", tolower(species)))

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
    model_name <- model_names[i]
    model_type <- "Total"
  } else {
    model_name <- model_names[i]
    model_type <- "Split"
  }

  model_file_name <- list.files(paste0("stock-specific/", spp, "/output/density-models/",
                                       model_names[i], "/", group_tag, ""),
                          pattern = "", full.names = FALSE)

  if(file.exists(paste0("stock-specific/", spp, "/output/density-models/",
                        model_names[i], "/", group_tag, "/", model_file_name))) {

  filename <- paste0("stock-specific/", spp, "/output/density-effects/dens-",
                     variable, "-", model_file_name)

  if(file.exists(filename)) {
    pd[[j]] <- readRDS(filename)
  } else {

    m[[i]] <- readRDS(paste0("stock-specific/", spp, "/output/density-models/",
                             model_names[i], "/", group_tag, "/", model_file_name))

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
  pd[[j]]$group_tag <- group_tag
  pd[[j]]$model_name <- model_names[i]
  pd[[j]]$model <- model_type
  # pd[[j]]$model_full_name <- model_file_name
  pd[[j]]$est1 <- NA
  pd[[j]]$est2 <- NA

  }

  dd <- do.call(rbind, pd)

# browser()
# set_alpha <- 0.8
 if(is.na(dd$est[1])){k <- nrow(dd)} else {k <- 1}

  m[[i]] <- readRDS(paste0("stock-specific/", spp, "/output/density-models/",
                           dd$model_name[k], "/", dd$group_tag[k], "/",
                           spp, "-", dd$group_tag[k], "-", dd$model_name[k], "-",
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
  geom_ribbon(data = filter(dd, group == "Total"),
              aes(ymin = exp(est - 1.96 * est_se),
                  ymax = exp(est + 1.96 * est_se)),
                  alpha = 0.1,
                  fill = "black") +
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

p[[i]] <- p[[i]] + ggtitle(paste0(model_names[i]))
}

saveRDS(p, paste0("data-generated/density-effects/", "all-models-",
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
          wrap_plots(gglist = p, ncol = round(length(model_names)/2 + 0.1)) &
          # xlim(0, 820) & # if plotting depth
          theme(text = element_text(size = set_font),
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank())) +
         plot_layout(widths = c(0.05, 1), guides = "collect")&labs(
           colour = "", fill = "", alpha = "", linetype = ""
         ))
  /x_lab_big + plot_layout(heights = c(1,0.05), design = design)
)

ggsave(paste0("stock-specific/", spp, "/figs/dens-effects-", variable, ".png"),
       height = if(length(model_names)>3){round(length(model_names)/2 + 1)*1.5}else{4},
       width = round(length(model_names)/2 + 0.1)*3)
}


plot_split_density_effects(model_names, variable = "days_to_solstice", knot_distance)
plot_split_density_effects(model_names, variable = "log_depth_c", knot_distance)

