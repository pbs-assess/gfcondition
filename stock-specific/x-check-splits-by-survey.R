# check which surveys to include for which species
library(tidyverse)
source("stock-specific/00-set-options.R")
spp <- gsub(" ", "-", gsub("\\/", "-", tolower(species)))

d1 <- readRDS(paste0("stock-specific/", spp, "/output/surv-summary-",spp, ".rds"))

d2 <- filter(d1,
                # round(prop_pos, 2) >= 0.01 &
                prop_pos >= 0.01 &
                # (max_pos_by_year < 5 & change_prop > -0.75) &
                # pos_n >= 9 &
                max_pos_by_year >= 3 &
                !(prop_years_w_0 > 0.5 & max_pos_by_year < 5) &
                round(max_prop_pos, 2) >= 0.05
             )
d2 <- d2 %>% mutate(group_name = case_when(group_name=="Males"~"Mature males",
                                           group_name=="Females"~"Mature females",
                                           TRUE ~ group_name))

# d2 %>% ggplot() + geom_point(aes(mean_ratio_w_0, mean_ratio_filled, size = total_samples))
d2 %>% ggplot() + geom_point(aes(mean_ratio_true,
                                 mean_ratio_filled,
                                 shape = group_name,
                                 alpha = factor(min_samples),
                                 size = max_samples,
                                 # alpha = max_samples,
                                 # size = min_samples,
                                 colour = survey_type)) +
  scale_alpha_discrete(range = c(0.2,1)) +
  facet_wrap(~species) +
  coord_fixed(xlim= c(0,1), ylim = c(0,1))

ggsave("figs/ratios-true-vs-assumed.png")

