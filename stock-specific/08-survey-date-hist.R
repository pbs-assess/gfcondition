# sampling date stats and figure?
library(tidyverse)

# spp <- gsub(" ", "-", gsub("\\/", "-", tolower(species)))
spp <- gsub(" ", "-", gsub("\\/", "-", tolower(stock_name)))

# ds <- readRDS(paste0("stock-specific/", spp, "/output/condition-data-", spp, "-mat-", mat_threshold, ".rds"))

# sg <- readRDS(paste0("stock-specific/", spp, "/output/specimen-counts/", spp, "-", group_tag, ".rds"))
mc <- readRDS(paste0("stock-specific/", spp, "/output/condition-models/", model_date, "/", spp, "-c-mat-fem-", model_date, "-", knot_distance, "-km.rds"))
mc2 <- readRDS(paste0("stock-specific/", spp, "/output/condition-models/", model_date, "/", spp, "-c-mat-m-", model_date, "-", knot_distance, "-km.rds"))
mc3 <- readRDS(paste0("stock-specific/", spp, "/output/condition-models/", model_date, "/", spp, "-c-imm-", model_date, "-", knot_distance, "-km.rds"))

ds <- bind_rows(mc$data, mc2$data, mc3$data)


d2 <- ds |> mutate(DOY = as.numeric(strftime(time_deployed, format = "%j")))

hist(d2$DOY)
median(d2$DOY) # june 6
mean(d2$DOY) # june 14
range(d2$DOY) # April 25 - Sept 22nd

ggplot(d2) +
  geom_vline(xintercept = 172, colour = "darkgrey", linetype = "dashed") +
  geom_histogram(aes(DOY, fill = survey_abbrev), binwidth = 5) +
  coord_cartesian(expand = FALSE) +
  scale_fill_viridis_d(option = "G", direction = -1, end = 0.95, begin = 0.15) +
  labs(x = rosettafish::en2fr("Day of year", FRENCH),
       y = rosettafish::en2fr("Specimens sampled", FRENCH),
       fill = rosettafish::en2fr("Survey", FRENCH)
       )

ggsave(paste0("stock-specific/", spp, "/figs", if(FRENCH){"-french"},
              "/E-sample-date-hist.png"), width = 6, height = 3)

ggplot(d2) +
  geom_histogram(aes(year, fill = survey_abbrev), binwidth = 1) +
  coord_cartesian(expand = FALSE) +
  scale_fill_viridis_d(option = "G", direction = -1, end = 0.95, begin = 0.15) +
  labs(x = rosettafish::en2fr("Year", FRENCH),
       y = rosettafish::en2fr("Specimens sampled", FRENCH),
       fill = rosettafish::en2fr("Survey", FRENCH)
       )

ggsave(paste0("stock-specific/", spp, "/figs", if(FRENCH){"-french"},
              "/C-sample-year-hist.png"), width = 6, height = 3)
