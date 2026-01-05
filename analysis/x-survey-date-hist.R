# sampling date stats and figure?
library(tidyverse)
theme_set(ggsidekick::theme_sleek())

FRENCH <- FALSE
options(scipen=999)

# # # Load data ----
dat <- readRDS("data-generated/all-samples-used.rds") %>%
  filter(!is.na(longitude), !is.na(latitude)
  )

# if(species != "Sablefish"){
## remove the sablefish survey
## because this survey is at different time of year than all the others
dat <- filter(dat, !(survey_abbrev %in% c("SABLE")), year >= 2002)

d2 <- dat |> mutate(DOY = as.numeric(strftime(time_deployed, format = "%j")))

hist(d2$DOY)
median(d2$DOY) # june 6
mean(d2$DOY) # june 14
range(d2$DOY) # April 25 - Sept 22nd

p1 <- ggplot(d2) +
  geom_vline(xintercept = 172, colour = "darkgrey", linetype = "dashed") +
  geom_histogram(aes(DOY, fill = survey_abbrev), binwidth = 5) +
  coord_cartesian(expand = FALSE) +
  scale_fill_viridis_d(option = "G", direction = -1, end = 0.95, begin = 0.15) +
  labs(x = rosettafish::en2fr("Day of year", FRENCH),
       y = rosettafish::en2fr("Specimens sampled", FRENCH),
       fill = rosettafish::en2fr("Survey", FRENCH)
       )

ggsave(paste0("figs/sample-date-hist.png"), width = 6, height = 3)

p2 <- ggplot(d2) +
  geom_histogram(aes(year, fill = survey_abbrev), binwidth = 1) +
  coord_cartesian(expand = FALSE) +
  scale_fill_viridis_d(option = "G", direction = -1, end = 0.95, begin = 0.15) +
  labs(x = rosettafish::en2fr("Year", FRENCH),
       y = rosettafish::en2fr("Specimens sampled", FRENCH),
       fill = rosettafish::en2fr("Survey", FRENCH)
       )

ggsave(paste0("figs/sample-year-hist.png"), width = 6, height = 3)

p2 + p1 + patchwork::plot_layout(nrow = 2,guides = "collect")

ggsave(paste0("figs/sample-hists.png"), width = 6, height = 6)
