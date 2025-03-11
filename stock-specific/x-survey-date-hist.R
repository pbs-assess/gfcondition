# sampling date stats and figure?

spp <- gsub(" ", "-", gsub("\\/", "-", tolower(species)))
ds <- readRDS(paste0("stock-specific/", spp, "/output/condition-data-", spp, "-mat-", mat_threshold, ".rds"))

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
  labs(y = "Specimens sampled", fill = "Survey")

ggsave(paste0("stock-specific/", spp, "/figs/sample-date-hist.png"), width = 5, height = 3)


ggplot(d2) +
  geom_histogram(aes(year, fill = survey_abbrev), binwidth = 1) +
  coord_cartesian(expand = FALSE) +
  scale_fill_viridis_d(option = "G", direction = -1, end = 0.95, begin = 0.15) +
  labs(X = "Year", y = "Specimens sampled", fill = "Survey")

ggsave(paste0("stock-specific/", spp, "/figs/sample-year-hist.png"), width = 5, height = 3)
