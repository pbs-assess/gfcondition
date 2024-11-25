## condition effect plots for all model structures tried for a single species
## also displays AIC and "FALSE" after the model name when any model failed the sanity() check

library(tidyverse)
library(sdmTMB)
library(ggsidekick)
library(aplot)
library(patchwork)
library(gridGraphics)
devtools::load_all(".")

source("stock-specific/00-set-options.R")

spp <- gsub(" ", "-", gsub("\\/", "-", tolower(species)))

dir.create(paste0("stock-specific/", spp, "/output/condition-effects/"), showWarnings = FALSE)

model_names <- list.files(paste0("stock-specific/", spp, "/output/condition-models"),
  pattern = "", full.names = FALSE
)

plot_split_condition_effects <- function(model_names, variable, knot_distance) {
  m <- list()
  p <- list()

  for (i in seq_along(model_names)) {
    spp <- gsub(" ", "-", gsub("\\/", "-", tolower(species)))

    pd <- list()
    # labels <- list()

    m[[i]] <- NULL

    for (j in 1:3) {
      if (j == 1) {
        group_tag <- "imm"
        group_label <- "Immature"
      }
      if (j == 2) {
        group_tag <- "mat-fem"
        group_label <- "Mature female"
      }
      if (j == 3) {
        group_tag <- "mat-m"
        group_label <- "Mature male"
      }


      model_file_name <- list.files(
        paste0(
          "stock-specific/", spp, "/output/condition-models/",
          model_names[i]
        ),
        pattern = "", full.names = FALSE
      )
      # browser()
      full_model_path <- paste0(
        "stock-specific/", spp, "/output/condition-models/",
        model_names[i], "/", model_file_name[j]
      )

      if (file.exists(full_model_path)) {
        filename <- paste0(
          "stock-specific/", spp, "/output/condition-effects/",
          variable, "-", model_file_name[j]
        )

        if (file.exists(filename)) {
          pd[[j]] <- readRDS(filename)

          if (!("sanity" %in% names(pd[[j]]))) {
            m[[i]] <- readRDS(full_model_path)
            pd[[j]]$AIC <- AIC(m[[i]])
            pd[[j]]$sanity <- isTRUE(all(sdmTMB::sanity(m[[i]])))
            saveRDS(pd[[j]], filename)
          }
        } else {
          m[[i]] <- readRDS(full_model_path)

          # m[[i]] <- sdmTMB:::update_version(m[[i]])

          m[[i]]$data$depth <- m[[i]]$data$depth_m

          t <- tidy(m[[i]], conf.int = TRUE)


          if (variable == "log_depth_c" | variable == "depth") {
            nd <- data.frame(
              days_to_solstice = 0,
              log_depth_c = seq(quantile(m[[i]]$data$log_depth_c, 0.025),
                quantile(m[[i]]$data$log_depth_c, 0.975),
                length.out = 50
              ),
              survey_group = "TRAWL",
              log_density_c = 0,
              ann_log_density_c = 0,
              dens_dev = 0,
              log_depth_c = 0,
              year = max(m[[i]]$data$year) # a chosen year
            )
            xlabel <- "Log depth (centered on mean)"
          }


          if (variable == "log_density_c") {
            nd <- data.frame(
              days_to_solstice = 0,
              survey_group = "TRAWL",
              log_density_c = seq(min(m[[i]]$data$log_density_c),
                max(m[[i]]$data$log_density_c),
                length.out = 50
              ),
              ann_log_density_c = 0,
              dens_dev = 0,
              log_depth_c = 0,
              year = max(m[[i]]$data$year) # a chosen year
            )
            xlabel <- "Local log density of maturity class (centered on mean)"
          }


          if (variable == "days_to_solstice") {
            nd <- data.frame(
              # days_to_solstice = seq(min(m[[i]]$data$days_to_solstice),
              #                        max(m[[i]]$data$days_to_solstice),
              #                        length.out = 50
              # ),
              days_to_solstice = seq(quantile(m[[i]]$data$days_to_solstice, 0.025),
                quantile(m[[i]]$data$days_to_solstice, 0.975),
                length.out = 50
              ),
              survey_group = "TRAWL",
              log_density_c = 0,
              ann_log_density_c = 0,
              dens_dev = 0,
              log_depth_c = 0,
              year = max(m[[i]]$data$year) # a chosen year
            )
            xlabel <- "Days from solstice"
          }

          pd[[j]] <- predict(m[[i]], newdata = nd, se_fit = TRUE, re_form = NA)
          pd[[j]]$density <- exp(pd[[j]]$log_density_c + mean(m[[i]]$data$log_density, na.rm = TRUE))
          pd[[j]]$depth <- exp(pd[[j]]$log_depth_c + 5)
          pd[[j]]$AIC <- AIC(m[[i]])
          pd[[j]]$sanity <- isTRUE(all(sdmTMB::sanity(m[[i]])))
          pd[[j]]$sig <- 1

          if (variable == "log_density_c") {
            if (t$estimate[t$term == "log_density_c"] < -t$std.error[t$term == "log_density_c"]) {
              # if(t$conf.high[t$term == "log_density_c"] < 0){ # too strict?
              pd[[j]]$sig <- 1
            } else {
              pd[[j]]$sig <- 0
            }
          }
          saveRDS(pd[[j]], filename)
        }
      } else {
        # for missing models, fill in empty data
        nd <- data.frame(
          days_to_solstice = 0,
          survey_group = "TRAWL",
          log_density_c = 0,
          ann_log_density_c = 0,
          dens_dev = 0,
          log_depth_c = 0,
          year = 2020L
        )
        pd[[j]] <- nd
        pd[[j]]$est <- NA
        pd[[j]]$est_se <- NA
        pd[[j]]$depth <- exp(pd[[j]]$log_depth_c + 5)
        pd[[j]]$density <- NA
        pd[[j]]$AIC <- NA
        pd[[j]]$sanity <- NA
        pd[[j]]$sig <- 1
      }
      pd[[j]]$group <- group_label
      pd[[j]]$group_tag <- group_tag
      pd[[j]]$model_name <- model_names[i]
      # pd[[j]]$model <- model_type
      # pd[[j]]$model_full_name <- model_file_name
      pd[[j]]$est1 <- NA
      pd[[j]]$est2 <- NA
    }

    dd <- do.call(rbind, pd)

    # browser()
    # set_alpha <- 0.8
    # if(is.na(dd$est[1])){k <- nrow(dd)} else {k <- 1}

    # m[[i]] <- readRDS(paste0("stock-specific/", spp, "/output/density-models/",
    #                          dd$model_name[k], "/", dd$group_tag[k], "/",
    #                          spp, "-", dd$group_tag[k], "-", dd$model_name[k], "-",
    #                          knot_distance,
    #                          "-km.rds"))
    m[[i]] <- readRDS(full_model_path)

    # m[[i]] <- sdmTMB:::update_version(m[[i]])

    m[[i]]$data$depth <- m[[i]]$data$depth_m



    set_alpha <- 0.8

    # browser()

    if (variable == "log_depth_c") {
      variable <- "depth"
      xlabel <- "Depth (m)"
    }

    if (variable == "days_to_solstice") {
      variable <- "days_to_solstice"
      xlabel <- "Days from solstice"
    }

    if (variable == "log_density_c") {
      variable2 <- "density"
      # xlabel <- "Local log density of maturity class (centered on mean)"
      xlabel <- "Local biomass density of maturity class (slopes are in log space)"

      pd$density <- exp(pd$log_density_c + mean(m[[i]]$data$log_density, na.rm = TRUE))

      p[[i]] <- ggplot(dd, aes(density, exp(est))) +
        geom_rug(
          data = m[[i]]$data, aes(density, y = 0.95),
          sides = "b", alpha = 0.1, inherit.aes = FALSE
        ) +
        geom_line(data = dd, aes(colour = group, alpha = as.factor(sig))) +
        scale_alpha_discrete(label = c("No", "Yes")) +
        geom_ribbon(
          data = dd,
          aes(
            ymin = exp(est - 1.96 * est_se),
            ymax = exp(est + 1.96 * est_se),
            fill = group
          ),
          alpha = 0.1
        )

      # p[[i]] <- ggplot(dd, aes(.data[[variable]], exp(est)
      # )) +
      #   geom_rug(data = m[[i]]$data, aes(.data[[variable]], y = 0.9),
      #            sides = "b", alpha = 0.1, inherit.aes = FALSE) +
      #   geom_line(data = filter(dd, group == "Total"), colour = "black", aes(linetype = "Total")) +
      #   geom_ribbon(data = filter(dd, group == "Total"),
      #               aes(ymin = exp(est - 1.96 * est_se),
      #                   ymax = exp(est + 1.96 * est_se)),
      #               alpha = 0.1,
      #               fill = "black") +
      #   geom_line(data = filter(dd, group != "Total"), aes(colour = group, linetype = "Split"), alpha = 0.8) +
      #   geom_ribbon(data = filter(dd, group != "Total"),
      #               aes(ymin = exp(est - 1.96 * est_se),
      #                   ymax = exp(est + 1.96 * est_se),
      #                   alpha = group,
      #                   fill = group)) +
      #   scale_x_continuous() +
      #   scale_linetype_manual(values=c(1,2)) +
      #   scale_alpha_discrete(range = c(0.05, 0.1)) +
      #   scale_color_viridis_d(option = "D", direction =1) +
      #   scale_fill_viridis_d(option = "D", direction =1) +
      #   coord_cartesian(#expand = F,
      #     xlim = c(min(dd[[variable]]), max(dd[[variable]])),
      #     ylim = c(NA,
      #              max(exp(dd$est), na.rm = TRUE)+
      #                max(exp(dd$est)*0.9, na.rm = TRUE))) +
      #   # coord_cartesian(expand = F, ylim = c(NA, NA)) +
      #   labs(x = xlabel,
      #        y = "Effect on biomass") +
      #   theme(axis.title = element_blank())

      # p[[i]] <- p[[i]] + ggtitle(paste0(model_names[i], " ", if(!all(unique(na.omit(dd$sanity)))){"FALSE"}),
      #                            subtitle = paste0("AIC: ", paste(round(unique(na.omit(dd$AIC))), collapse = ", ")))


      xlabel <- "Local biomass density of maturity class (slopes are in log space)"

      p[[i]] <- p[[i]] + ggtitle(
        paste0(model_names[i], " ", if (!all(unique(na.omit(dd$sanity)))) {
          "FALSE"
        }),
        subtitle = paste0("AIC: ", paste(round(unique(na.omit(dd$AIC))), collapse = ", "))
      )
      # p[[i]] <- p[[i]] + ggtitle(paste0(stringr::str_to_title(m[[i]]$data$species_common_name)),
      #                          subtitle = paste0(
      # "slope: ", round(t$estimate[t$term == variable], 3),
      # ", SE: ", round(t$std.error[t$term == variable], 3), ""
      # ))
    } else {
      p[[i]] <- ggplot(dd, aes(.data[[variable]], exp(est))) +
        geom_rug(
          data = m[[i]]$data, aes(.data[[variable]], y = 0.95),
          sides = "b", alpha = 0.1, inherit.aes = FALSE
        ) +
        geom_line(data = dd, aes(colour = group)) +
        geom_ribbon(
          data = dd,
          aes(
            ymin = exp(est - 1.96 * est_se),
            ymax = exp(est + 1.96 * est_se),
            fill = group
          ),
          alpha = 0.1
        ) +
        scale_x_continuous()

      p[[i]] <- p[[i]] +
        ggtitle(
          paste0(
            model_names[i], " ",
            if (!all(unique(na.omit(dd$sanity)))) {
              "FALSE"
            }
          ),
          subtitle = paste0("AIC: ", paste(round(unique(na.omit(dd$AIC))), collapse = ", "))
        )
    }

    p[[i]] <- p[[i]] +
      scale_color_viridis_d(option = "D", direction = 1) +
      scale_fill_viridis_d(option = "D", direction = 1) +
      facet_wrap(~group) +
      scale_x_continuous() +
      coord_cartesian(expand = F) +
      labs(
        alpha = "Negative \neffect",
        x = xlabel,
        y = "Effect on condition"
      ) +
      theme(axis.title = element_blank())
  }

  # if (variable == "log_depth_c") {
  #   variable <- "depth"
  #   xlabel <- "Depth (m)"
  # }

  y_lab_big <- ggplot() +
    annotate(
      geom = "text", x = 1, y = 1, size = 5,
      label = paste0("Conditional effect on body condition"), angle = 90
    ) +
    coord_cartesian(clip = "off") +
    theme_void()

  x_lab_big <- ggplot() +
    annotate(
      geom = "text", x = 1, y = 1, size = 5,
      label = paste("", xlabel)
    ) +
    coord_cartesian(clip = "off") +
    theme_void()

  design <- "
AAAAAA
#BBBBB
"

  # if(variable == "log_depth_c") {set_font <- 9} else {
  set_font <- 12
  # }

  (g2 <- ((y_lab_big |
    wrap_plots(
      gglist = p,
      # ncol = round(length(model_names) + 0.1)
      ncol = 1
    ) &
      # xlim(0, 820) & # if plotting depth
      theme( # axis.text.y = element_blank(),
        # axis.ticks.y = element_blank(),
        text = element_text(size = set_font)
      )) +
    plot_layout(widths = c(0.05, 1), guides = "collect") & labs(
    colour = "", fill = "", linetype = ""
  ))
  / x_lab_big + plot_layout(heights = c(1, 0.05), design = design)
  )

  ggsave(paste0("stock-specific/", spp, "/figs/condition-effects-", variable, ".png"),
    height = if (length(model_names) > 1) {
      round(length(model_names) / 2 + 1) * 4
    } else {
      4
    },
    # width = round(length(model_names)/2 + 0.1)*3
    width = 10
  )
}


plot_split_condition_effects(model_names, variable = "days_to_solstice", knot_distance)
plot_split_condition_effects(model_names[2], variable = "log_density_c", knot_distance)


#
# #-------
# variable <- "log_density_c"
# xlabel <- "Local log density of maturity class (centered on mean)"
#
# # model_name <- "apr-2024" # might not need this because density removed from ones lacking a negative effect.
# # model_name <- "apr-2024-density"
# # model_name <- "apr-2024-not-total-density"
# model_name <- "2024-09-doy-ld0c"
#
# # group_tag <- "mat-m"
# group_tag <- "mat-fem"
# # group_tag <- "imm"
#
# f <- list.files(paste0("data-generated/condition-models-", group_tag, "/", model_name, "/"),
#                 pattern = ".rds", full.names = TRUE)
#
# m <- purrr::map(f, readRDS)
#
#
#
# p <- list()
# p2 <- list() #for filtered version
#
# for (i in seq_along(m)){
# # for (i in 11:14){
#   s <- sanity(m[[i]], gradient_thresh = 0.005)
#
# if(all(s)){
#   if (variable == "log_density_c") {
#     nd <- data.frame(
#       days_to_solstice = 0,
#       survey_group = "TRAWL",
#       log_density_c = seq(min(m[[i]]$data$log_density_c),
#                           max(m[[i]]$data$log_density_c),
#                           length.out = 50
#       ),
#       ann_log_density_c = 0,
#       dens_dev = 0,
#       year = max(m[[i]]$data$year) # a chosen year
#     )
#     xlabel <- "Local log density of maturity class (centered on mean)"
#   }
#
#   if(variable == "days_to_solstice") {
#     nd <- data.frame(
#       days_to_solstice = seq(min(m[[i]]$data$days_to_solstice),
#                              max(m[[i]]$data$days_to_solstice),
#                              length.out = 50
#       ),
#       survey_group = "TRAWL",
#       log_density_c = 0,
#       ann_log_density_c = 0,
#       dens_dev = 0,
#       year = max(m[[i]]$data$year) # a chosen year
#     )
#     xlabel <- "Days from solstice"
#   }
#
# pd <- predict(m[[i]], newdata = nd, se_fit = TRUE, re_form = NA)
#
# t <- tidy(m[[i]], conf.int = TRUE)
#
# set_alpha <- 0.8
#
# p[[i]] <- ggplot(pd, aes(.data[[variable]], exp(est),
#                ymin = exp(est - 1.96 * est_se),
#                ymax = exp(est + 1.96 * est_se)
# )) +
#   geom_rug(data = m[[i]]$data, aes(.data[[variable]], y = 0.9),
#              sides = "b", alpha = 0.1, inherit.aes = FALSE) +
#   geom_line(alpha = set_alpha) +
#   geom_ribbon(alpha = set_alpha/2) +
#   scale_x_continuous() +
#   # coord_cartesian(expand = F, ylim = c(NA, max(exp(pd$est))+0.01)) +
#   coord_cartesian(expand = F, ylim = c(NA, 1.2)) +
#   labs(x = xlabel,
#        y = "condition") +
# theme(axis.title = element_blank())
#
# if (variable == "log_density_c") {
#
#   if(t$estimate[t$term == "log_density_c"] < -t$std.error[t$term == "log_density_c"]){
#   # if(t$conf.high[t$term == "log_density_c"] < 0){ # too strict?
#     set_alpha <- 1
#   } else {
#     set_alpha <- 0.4
#   }
#
#   pd$density <- exp(pd$log_density_c + mean(m[[i]]$data$log_density, na.rm = TRUE))
#
#   # p[[i]] <- ggplot(pd, aes(log_density_c, exp(est))) +
#   #   geom_rug(data = m[[i]]$data, aes(log_density_c, y = 0.95),
#   #          sides = "b", alpha = 0.1, inherit.aes = FALSE) +
#   p[[i]] <- ggplot(pd, aes(density, exp(est))) +
#     geom_rug(data = m[[i]]$data, aes(density, y = 0.95),
#              sides = "b", alpha = 0.1, inherit.aes = FALSE) +
#     geom_line(alpha = set_alpha) +
#     geom_ribbon(aes(ymin = exp(est - 1.96 * est_se),
#                     ymax = exp(est + 1.96 * est_se)
#     ), alpha = set_alpha/2) +
#     scale_x_continuous() +
#     # geom_vline(xintercept = exp(mean(m[[i]]$data$log_density_c, na.rm = TRUE)), linetype = "dashed") +
#     # ggridges::geom_density_ridges(data = m[[i]]$data,
#     #    aes(log_density_c, y = 0.95
#     #    height = after_stat(count)),
#     #    rel_min_height = 0.01, scale = 0.25, alpha = 0) +
#     # coord_cartesian(ylim = c(0.95, 1.15),  xlim = c(0, NA), expand = F) +
#     coord_cartesian(expand = F) +
#     labs(x = xlabel,
#          y = "condition") +
#     theme(axis.title = element_blank())
#
#
#
#   xlabel <- "Local biomass density of maturity class (slopes are in log space)"
#
# p[[i]] <- p[[i]] + ggtitle(paste0(stringr::str_to_title(m[[i]]$data$species_common_name)), subtitle = paste0(
#                "slope: ", round(t$estimate[t$term == variable], 3),
#                ", SE: ", round(t$std.error[t$term == variable], 3), ""
# ))
#
#
# } else {
#   p[[i]] <- p[[i]] + ggtitle(paste0(stringr::str_to_title(m[[i]]$data$species_common_name)))
# }
#
# p2[[i]] <- p[[i]]
# if(m[[i]]$data$species_common_name[1] %in% tolower(c(species_to_remove))) { p2[[i]] <- NULL }
# } else{
#     p[[i]] <- NULL
#     p2[[i]] <- NULL
# }
# }
#
# p2 <- p2 %>% discard(is.null)
#
#
# if(group_tag == "mat-m") {group_label <- "Mature male"}
# if(group_tag == "mat-fem") {group_label <- "Mature female"}
# if(group_tag == "imm") {group_label <- "Immature"}
#
# y_lab_big <- ggplot() +
#   annotate(geom = "text", x = 1, y = 1, size = 5,
#            label = paste0(group_label, " condition factor"), angle = 90) +
#   coord_cartesian(clip = "off")+
#   theme_void()
#
# x_lab_big <- ggplot() +
#   annotate(geom = "text", x = 1, y = 1, size = 5,
#            label = paste("", xlabel)) +
#   coord_cartesian(clip = "off")+
#   theme_void()
#
# # length(p)/3
#
# design = "
# AAAAAA
# #BBBBB
# "
#
# if(variable == "log_density_c") {set_font <- 9} else{
#   set_font <- 12
# }
#
#
# (g <- ((y_lab_big |
#           wrap_plots(gglist = p2, ncol = 9) &
#           theme(text = element_text(size = set_font))) +
#           plot_layout(widths = c(0.05, 1)))
#   /x_lab_big + plot_layout(heights = c(1,0.05), design = design)
#   )
#
# ggsave(paste0("figs/cond-effects-", variable, "-trans-",
#               model_name, "-", group_tag, "-s.png"),
#        height = 5.5, width = 15)
#
# (g2 <- ((y_lab_big |
#           wrap_plots(gglist = p2, ncol = 4) &
#           theme(text = element_text(size = set_font))) +
#          plot_layout(widths = c(0.05, 1)))
#   /x_lab_big + plot_layout(heights = c(1,0.05), design = design)
# )
#
#
# ggsave(paste0("figs/cond-effects-", variable, "-trans-",
#               model_name, "-", group_tag, "-filtered-s.png"),
#        height = 18, width = 14)
#
#
#
# ## would work if layout stayed the same
# # p_axis <- ggplot() + labs(x = xlabel, y = "Condition factor")
# # x_lab_big <- cowplot::get_plot_component(p_axis, "xlab-b")
# # y_lab_big <- cowplot::get_plot_component(p_axis, "ylab-l")
# # design = "
# # FAB
# # FCD
# # #EE
# # "
# #
# # c(gglist = p, list(x_lab_big, y_lab_big)) |>
# #   wrap_plots() +
# #   plot_layout(heights = c(20, 20, 1), widths = c(1, 25, 25), design = design)
#
# # (g <- (y_lab_big | wrap_plots(gglist = p, ncol = 2)/x_lab_big ) +
# #     plot_layout(widths = c(0.1, 1), heights = c(1,0.1)))
#
