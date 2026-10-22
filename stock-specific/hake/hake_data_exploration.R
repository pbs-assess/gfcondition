


#####
#
# Hake exploration
#
# Conducting an analysis with Hake data from the Hake survey GitHub repository.
# First using length-weight data to define a relationship for males and females across all the data
# Then using the observed over expected weight to create a body condition index. This can then be used to
#  evaluate the effect of envrionmental covariates on body condition
# Hopefully these results shed light on the correlation between environmental indicators
#  and mean age - the plots that Rebecca Thomas showed that initiated this project.

# load packages
library(devtools)
load_all()

library(tidyverse)
library(glmmTMB)
library(broom.mixed)
library(ggeffects)
library(performance)
library(patchwork)
library(pacea)
library(lubridate)
library(slider)
library(GGally)


# Read in data

# US Hake weight-at-age data
# only has US at sea and US shore Source data
# has data from 1965 to 2025
us_dat <- read.csv("https://raw.githubusercontent.com/pacific-hake/hake-assessment/refs/heads/master/data-tables/us-weight-at-age.csv", header = TRUE)

str(us_dat)
head(us_dat)
unique(us_dat$Source)
unique(us_dat$Year)

# length weight age data
# Has numerous sources
# has data from 1975 to 2021
lwa_dat <- read.csv("https://raw.githubusercontent.com/pacific-hake/hake-assessment/refs/heads/master/data-tables/length-weight-age/LWAdata_1975to2021.csv", header = TRUE)

str(lwa_dat)
head(lwa_dat)
unique(lwa_dat$Source)
unique(lwa_dat$Year)

# acoustic survey weight at age
# US and Can Acoustic sources
# 1995 to 2025, every 2-3 years
ac_dat <- read.csv("https://raw.githubusercontent.com/pacific-hake/hake-assessment/refs/heads/master/data-tables/survey-weight-at-age.csv", header = TRUE)

str(ac_dat)
head(ac_dat)
unique(ac_dat$Source)
unique(ac_dat$Year)


# Biomass at age
#  Age is in columns, and the values represent the total biomass at each age
# github directory
git_dir <- "C:/github/hakeanalysis/data/"

# canada survey data
can.dat <- read.csv(paste0(git_dir, "Survey_Biomass_at_age_Canada.csv"), header = TRUE)

# california survey data
cal.dat <- read.csv(paste0(git_dir, "Survey_Biomass_at_age_California.csv"), header = TRUE)

# oregon survey data
ore.dat <- read.csv(paste0(git_dir, "Survey_Biomass_at_age_Oregon.csv"), header = TRUE)

# total survey data
tot.dat <- read.csv(paste0(git_dir, "Survey_Biomass_at_Age_Total.csv"), header = TRUE)

str(can.dat)
str(cal.dat)
str(ore.dat)
str(tot.dat)



##### -------------------------------------------------------------------- #####
##### -------------------------------------------------------------------- #####
##### -------------------------------------------------------------------- #####
##### -------------------------------------------------------------------- #####
##### -------------------------------------------------------------------- #####

##### Calculate le cren's body condition

# Length-weight mixed effects analysis + Le Cren condition + pacea covariates

# -----------------------------
# 1.1 Clean data
# -----------------------------

# currently using the length weight data that seems to have the most data (a combination of the other datasets
lwa <- lwa_dat %>%
  filter(
    !outlierL,
    !is.na(Weight_kg),
    !is.na(Length_cm),
    Weight_kg > 0,
    Length_cm > 0,
    Sex %in% c("F","M")
  ) %>%
  mutate(
    Sex = factor(Sex),
    Age_yrs = factor(Age_yrs),
    Source = factor(Source),
    Year = factor(Year),
    Month = factor(Month),
    year_month = interaction(Year, Month, drop = TRUE),
    log_weight = log(Weight_kg),
    log_length = log(Length_cm)
  )

# -----------------------------
# 1.2 Exploratory plots
# -----------------------------

# length weight relationship by sex
p_lw <- ggplot(lwa, aes(Length_cm, Weight_kg)) +
  geom_point(alpha = 0.08, size = 0.5) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE) +
  scale_x_log10() +
  scale_y_log10() +
  facet_wrap(~ Sex) +
  labs(
    x = "Length (cm)",
    y = "Weight (kg)",
    title = "Length-weight relationship by sex"
  ) +
  theme_bw()
p_lw

# length weight by age
p_age <- ggplot(lwa, aes(Length_cm, Weight_kg, colour = Age_yrs)) +
  geom_point(alpha = 0.08, size = 0.5) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    x = "Length (cm)",
    y = "Weight (kg)",
    colour = "Age",
    title = "Length-weight relationship by age"
  ) +
  theme_bw()
p_age

# both in same plot
p_lw / p_age

rm(p_lw,p_age)

# -----------------------------
# 1.3 Mixed effects length-weight model
# -----------------------------
# log(W) = log(a) + b log(L)

##
# interaction of sex and age
# Month as a factor
m_lw1.1 <- glmmTMB(
  log_weight ~ log_length * Sex + log_length * Age_yrs +
    factor(Month) +
    (1 | Source) + (1 | Year),
  data = lwa,
  family = gaussian(),
  control = glmmTMBControl(
    optCtrl = list(iter.max = 1e4, eval.max = 1e4)
  )
)

# interaction of sex and age
# month as random intercept
m_lw1.2 <- glmmTMB(
  log_weight ~ log_length * Sex + log_length * Age_yrs +
    (1 | Month) +
    (1 | Source) + (1 | Year),
  data = lwa,
  family = gaussian(),
  control = glmmTMBControl(
    optCtrl = list(iter.max = 1e4, eval.max = 1e4)
  )
)

##
# interaction of sex
# month as factor
m_lw2.1 <- glmmTMB(
  log_weight ~
    log_length * Sex + Age_yrs +
    factor(Month) +
    (1 | Source) + (1 | Year),
  data = lwa,
  family = gaussian(),
  control = glmmTMBControl(
    optCtrl = list(iter.max = 1e4, eval.max = 1e4)
  )
)


# interaction of sex
# month random
m_lw2.2 <- glmmTMB(
  log_weight ~
    log_length * Sex + Age_yrs +
    (1 | Month) +
    (1 | Source) + (1 | Year),
  data = lwa,
  family = gaussian(),
  control = glmmTMBControl(
    optCtrl = list(iter.max = 1e4, eval.max = 1e4)
  )
)

##
# interaction of sex
# no age
# month as a factor
m_lw3.1 <- glmmTMB(
  log_weight ~ log_length * Sex +
    factor(Month) +
    (1 | Source) + (1 | Year),
  data = lwa,
  family = gaussian(),
  control = glmmTMBControl(
    optCtrl = list(iter.max = 1e4, eval.max = 1e4)
  )
)

# interaction of sex
# no age
# month as a random
m_lw3.2 <- glmmTMB(
  log_weight ~ log_length * Sex +
    (1 | Month) +
    (1 | Source) + (1 | Year),
  data = lwa,
  family = gaussian(),
  control = glmmTMBControl(
    optCtrl = list(iter.max = 1e4, eval.max = 1e4)
  )
)

##
# No interaction
# month as factor
m_lw4.1 <- glmmTMB(
  log_weight ~ log_length + Sex + Age_yrs +
    factor(Month) +
    (1 | Source) + (1 | Year),
  data = lwa,
  family = gaussian(),
  control = glmmTMBControl(
    optCtrl = list(iter.max = 1e4, eval.max = 1e4)
  )
)

# No interaction
# month as random
m_lw4.2 <- glmmTMB(
  log_weight ~ log_length + Sex + Age_yrs +
    (1 | Month) +
    (1 | Source) + (1 | Year),
  data = lwa,
  family = gaussian(),
  control = glmmTMBControl(
    optCtrl = list(iter.max = 1e4, eval.max = 1e4)
  )
)


##
# No interaction, no age
# month as factor
m_lw5.1 <- glmmTMB(
  log_weight ~ log_length + Sex +
    factor(Month) +
    (1 | Source) + (1 | Year),
  data = lwa,
  family = gaussian(),
  control = glmmTMBControl(
    optCtrl = list(iter.max = 1e4, eval.max = 1e4)
  )
)

# No interaction, no age
# month as random
m_lw5.2 <- glmmTMB(
  log_weight ~ log_length + Sex +
    (1 | Month) +
    (1 | Source) + (1 | Year),
  data = lwa,
  family = gaussian(),
  control = glmmTMBControl(
    optCtrl = list(iter.max = 1e4, eval.max = 1e4)
  )
)

##
# age as numeric
m_lw6.1 <- glmmTMB(
  log_weight ~ log_length * Sex + scale(as.numeric(Age_yrs)) +
    factor(Month) +
    (1 | Source) + (1 | Year)
  # + (1 | Month)
  ,
  data = lwa,
  family = gaussian(),
  control = glmmTMBControl(
    optCtrl = list(iter.max = 1e4, eval.max = 1e4)
  )
)

## USE THIS
# simplest
m_lw7.1 <- glmmTMB(
  log_weight ~ log_length * Sex
  ,
  data = lwa,
  family = gaussian(),
  control = glmmTMBControl(
    optCtrl = list(iter.max = 1e4, eval.max = 1e4)
  )
)

# comparing models
AIC(m_lw1.1, m_lw1.2, m_lw2.1, m_lw2.2,
    m_lw3.1, m_lw3.2, m_lw4.1, m_lw4.2,
    m_lw5.1, m_lw5.2, m_lw6.1, m_lw7.1
    )

anova(m_lw1.1, m_lw2.1, m_lw3.1, m_lw5.1, m_lw7.1)

rm(m_lw1.1,m_lw1.2,m_lw2.1,m_lw2.2,m_lw3.1,m_lw3.2,m_lw4.1,m_lw4.2,m_lw5.1,m_lw5.2,m_lw6.1)
gc()

# Even though we get a better fit with more complex models, we just want the length-weight model parameters
#  so we stick with the simple model, with sex as the only factor.

# selected model - simple one for lw model
m_lw_selected <- m_lw7.1


# -----------------------------
# 1.4 Predict expected weight and calculate Le Cren's condition
# -----------------------------
# Le Cren's relative condition factor:
# Kn = observed weight / expected weight

lwa_cond <- lwa %>%
  mutate(
    log_weight_hat = predict(m_lw_selected, type = "response"),
    weight_hat_kg = exp(log_weight_hat),
    Kn = Weight_kg / weight_hat_kg,
    log_Kn = log(Kn)
  )

summary(lwa_cond$Kn)

p_kn_hist <- ggplot(lwa_cond, aes(Kn)) +
  geom_histogram(bins = 80) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  coord_cartesian(xlim = quantile(lwa_cond$Kn, c(0.01, 0.99), na.rm = TRUE)) +
  labs(
    x = "Le Cren's relative condition factor, Kn",
    y = "Count",
    title = "Distribution of body condition"
  ) +
  theme_bw()

p_kn_time <- lwa_cond %>%
  mutate(
    Year_num = as.numeric(as.character(Year)),
    Month_num = as.numeric(as.character(Month))
  ) %>%
  group_by(Year_num, Month_num) %>%
  summarise(
    mean_Kn = mean(Kn, na.rm = TRUE),
    median_Kn = median(Kn, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  ggplot(aes(Year_num, mean_Kn)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_point(aes(size = n), alpha = 0.5) +
  geom_smooth(se = TRUE) +
  facet_wrap(~ Month_num, scales = "free_y") +
  labs(
    x = "Year",
    y = "Mean Kn",
    size = "n",
    title = "Mean body condition through time"
  ) +
  theme_bw()

p_kn_time_med <- lwa_cond %>%
  mutate(
    Year_num = as.numeric(as.character(Year)),
    Month_num = as.numeric(as.character(Month))
  ) %>%
  group_by(Year_num,Month_num) %>%
  summarise(
    mean_Kn = mean(Kn, na.rm = TRUE),
    median_Kn = median(Kn, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  ggplot(aes(Year_num, median_Kn)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_point(aes(size = n), alpha = 0.5) +
  geom_smooth(se = TRUE) +
  facet_wrap(~ Month_num, scales = "free_y") +
  labs(
    x = "Year",
    y = "Median Kn",
    size = "n",
    title = "Median body condition through time"
  ) +
  theme_bw()

# distribution historgram of body condition
p_kn_hist

# mean body condition over time by month
p_kn_time
# p_kn_time_med # median condition over time by month

rm(p_kn_hist, p_kn_time, p_kn_time_med)
gc()

# -----------------------------
# 1.5 Prepare monthly condition index
# -----------------------------

# predict the expected weight and estimate condition
lwa_cond <- lwa %>%
  mutate(
    log_weight_hat = predict(m_lw_selected, type = "response"),
    weight_hat_kg = exp(log_weight_hat),
    Kn = Weight_kg / weight_hat_kg,
    log_Kn = log(Kn),
    year = as.integer(as.character(Year)),
    month = as.integer(as.character(Month))
  )

# summarize condition by month
cond_monthly <- lwa_cond %>%
  group_by(year, month) %>%
  summarise(
    mean_Kn = mean(Kn, na.rm = TRUE),
    median_Kn = median(Kn, na.rm = TRUE),
    mean_log_Kn = mean(log_Kn, na.rm = TRUE),
    sd_log_Kn = sd(log_Kn, na.rm = TRUE),
    se_log_Kn = sd_log_Kn / sqrt(n()),
    n_fish = n(),
    .groups = "drop"
  )

# summarize condition by year
cond_annual <- lwa_cond %>%
  group_by(year) %>%
  summarise(
    mean_age = mean(as.numeric(as.character(Age_yrs)), na.rm = TRUE),
    mean_Kn = mean(Kn, na.rm = TRUE),
    median_Kn = median(Kn, na.rm = TRUE),
    mean_log_Kn = mean(log_Kn, na.rm = TRUE),
    sd_log_Kn = sd(log_Kn, na.rm = TRUE),
    se_log_Kn = sd_log_Kn / sqrt(n()),
    n_fish = n(),
    .groups = "drop"
  )


# -----------------------------
# 1.6 Add environmental covariates from pacea
# -----------------------------

# list of pacea indices
pacea_indices

# function for formatting covariates from pacea
format_covariate_month <- function(dat, value.col, name) {
  dat %>%
    as_tibble()  %>%
    group_by(year, month) %>%
    summarize(value = mean(.data[[value.col]])) %>%
    mutate(covariate = name)
}

# Get environmental covariates
env_pdo <- format_covariate_month(pdo, "anomaly", "pdo_m_prev1")
env_npgo <- format_covariate_month(npgo, "anomaly", "npgo_m_prev1")
env_oni <- format_covariate_month(oni, "anomaly", "oni_m_prev1")
env_npi <- format_covariate_month(npi_monthly , "value", "npi_m_prev1")
env_mei <- format_covariate_month(mei , "anomaly", "mei_m_prev1")

# combine variables
env_long <- bind_rows(env_pdo, env_npgo, env_oni, env_npi, env_mei)

##
# seasonal covariates of previous 1, 3, and 6 months - combined into one data set

# estimate prev 1 variables first
lag_months <- 1
env_long <- env_long %>%
  mutate(month = month + lag_months) %>%
  mutate(year = case_when(month > 12 ~ year + 1,
                          .default = year),
         month = case_when(month > 12 ~ (month - 12),
                           .default = month))

# make data wide
env_wide <- env_long %>%
  pivot_wider(names_from = covariate, values_from = value)


# use lag 1 variable names to then find the average across 3 and 6 months
lag1_vars <- c(
  "pdo_m_prev1",
  "npgo_m_prev1",
  "oni_m_prev1",
  "npi_m_prev1",
  "mei_m_prev1"
)

env_wide_rolling <- env_wide %>%
  ungroup() %>%

  # Create a proper monthly date
  mutate(
    month_date = make_date(
      year = year,
      month = month,
      day = 1
    )
  ) %>%
  arrange(month_date) %>%

  # Insert missing calendar months, if any
  complete(
    month_date = seq(
      min(month_date, na.rm = TRUE),
      max(month_date, na.rm = TRUE),
      by = "month"
    )
  ) %>%
  mutate(
    year = lubridate::year(month_date),
    month = lubridate::month(month_date)
  ) %>%
  arrange(month_date) %>%

  # Calculate rolling means from the lag-1 variables
  mutate(
    across(
      all_of(lag1_vars),
      list(
        prev3 = ~ slide_dbl(
          .x,
          mean,
          na.rm = FALSE,
          .before = 2,
          .complete = TRUE
        ),
        prev6 = ~ slide_dbl(
          .x,
          mean,
          na.rm = FALSE,
          .before = 5,
          .complete = TRUE
        )
      ),
      .names = "{sub('_m_prev1$', '', .col)}_m_{.fn}"
    )
  )

dat_env <- cond_monthly %>%
  left_join(env_wide_rolling, by = c("year", "month"))

# -----------------------------
# 1.7 Correlations with environmental covariates
# -----------------------------

# Names of dat_env environmental data columns:
env_vars <- c(
  "pdo_m_prev1",
  "npgo_m_prev1",
  "oni_m_prev1",
  "npi_m_prev1",
  "mei_m_prev1",
  "pdo_m_prev3",
  "npgo_m_prev3",
  "oni_m_prev3",
  "npi_m_prev3",
  "mei_m_prev3",
  "pdo_m_prev6",
  "npgo_m_prev6",
  "oni_m_prev6",
  "npi_m_prev6",
  "mei_m_prev6"
)

# correlations of environmental variables with mean body condition
cor_results <- map_dfr(env_vars, function(v) {
  tmp <- dat_env %>%
    select(mean_log_Kn, all_of(v)) %>%
    drop_na()

  ct <- cor.test(tmp$mean_log_Kn, tmp[[v]], method = "pearson")

  tibble(
    covariate = v,
    n = nrow(tmp),
    correlation = unname(ct$estimate),
    p_value = ct$p.value,
    conf_low = ct$conf.int[1],
    conf_high = ct$conf.int[2]
  )
}) %>%
  mutate(p_adj = p.adjust(p_value, method = "BH"))

# print correlation results
cor_results

# correlation plot of mean condition for previous 3 months only
ggpairs(dat_env %>% select(mean_Kn, contains("prev3")))


# -----------------------------
# 1.8 Mixed effects models for environmental effects
# -----------------------------

# filter only canada or us data
filter_index <- unique(lwa_cond$Source)
filter_index_can <- c("CAN_acoustic", "CAN_shoreside", "CAN_JV","CAN_polish", "Acoustic Canada")
filter_index_us <- c("US_foreign", "US_JV", "US_shore", "US_acoustic", "US_atsea", "Acoustic U.S.")

# selected data
selected <- "all"
selected <- "us"
selected <- "can"

if(selected == "all"){
  lwa_env <- lwa_cond %>%
    filter(Source %in% filter_index)

} else {
  if(selected == "us"){
    lwa_env <- lwa_cond %>%
      filter(Source %in% filter_index_us)

  } else {
    if(selected == "can"){
      lwa_env <- lwa_cond %>%
        filter(Source %in% filter_index_can)

    } else {
      rm(lwa_env)
    }
  }
}

# Fish-level model: body condition as response.
# Environmental covariates joined to each fish by Year-Month.
lwa_env <- lwa_env %>%
  mutate(
    year = as.integer(as.character(year)),
    month = as.integer(as.character(month))
  ) %>%
  left_join(env_wide_rolling, by = c("year", "month")) %>%
  mutate(
    year_f = factor(year),
    month_f = factor(month)
  )

# Scale environmental covariates
lwa_env <- lwa_env %>%
  mutate(across(all_of(env_vars), scale))

# Fit one model per environmental covariate - potentially need to change formula
env_models <- map(env_vars, function(v) {
  form <- as.formula(
    paste0(
      "log_Kn ~ ", v,
      " + Sex + Age_yrs + ",
      "(1 | Source) + (1 | year_f) + (1 | month_f)"
    )
  )

  glmmTMB(
    form,
    data = lwa_env,
    family = gaussian(),
    control = glmmTMBControl(
      optCtrl = list(iter.max = 1e4, eval.max = 1e4)
    )
  )
})

# naming each object in list with variable name
names(env_models) <- env_vars

# extract model results output - coefficient estimate with error and p value
env_model_results <- map_dfr(names(env_models), function(v) {
  tidy(env_models[[v]], effects = "fixed") %>%
    filter(term == v) %>%
    mutate(covariate = v)
}) %>%
  mutate(p_adj = p.adjust(p.value, method = "BH"))

env_model_results


# -----------------------------
# 1.9 Plot environmental relationships
# -----------------------------

# filter out Source
cond_monthly <- lwa_cond %>%
  filter(Source %in% filter_index) %>%
  group_by(year, month) %>%
  summarise(
    mean_Kn = mean(Kn, na.rm = TRUE),
    median_Kn = median(Kn, na.rm = TRUE),
    mean_log_Kn = mean(log_Kn, na.rm = TRUE),
    sd_log_Kn = sd(log_Kn, na.rm = TRUE),
    se_log_Kn = sd_log_Kn / sqrt(n()),
    n_fish = n(),
    .groups = "drop"
  )

dat_env <- cond_monthly %>%
  left_join(env_wide_rolling, by = c("year", "month"))


plot_env_relationship <- function(v) {
  ggplot(dat_env, aes(x = .data[[v]], y = mean_Kn)) +
    geom_hline(yintercept = 1, linetype = "dashed") +
    geom_point(aes(size = n_fish), alpha = 0.6) +
    geom_smooth(method = "lm", se = TRUE) +
    labs(
      x = v,
      y = "Monthly mean Kn",
      size = "n fish",
      title = paste("Body condition vs", v, "-", selected, "data")
    ) +
    theme_bw()
}

env_plots <- map(env_vars, plot_env_relationship)

wrap_plots(env_plots)

rm(env_plots)


# -----------------------------
# 1.10 Model-based predicted plots
# -----------------------------

pred_lw <- ggpredict(
  m_lw_selected,
  terms = c("log_length [all]", "Sex")
) %>%
  as_tibble() %>%
  mutate(
    Length_cm = exp(x),
    Weight_kg = exp(predicted),
    conf.low = exp(conf.low),
    conf.high = exp(conf.high)
  )

# model fitted lines for male and female length-weight relationship
ggplot() +
  geom_point(
    data = lwa,
    aes(Length_cm, Weight_kg),
    alpha = 0.03,
    size = 0.4
  ) +
  geom_ribbon(
    data = pred_lw,
    aes(Length_cm, ymin = conf.low, ymax = conf.high, fill = group),
    alpha = 0.25
  ) +
  geom_line(
    data = pred_lw,
    aes(Length_cm, Weight_kg, colour = group),
    linewidth = 1
  ) +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    x = "Length (cm)",
    y = "Weight (kg)",
    colour = "Sex",
    fill = "Sex",
    title = "Mixed-effects length-weight model predictions"
  ) +
  theme_bw()


##### -------------------------------------------------------------------- #####
##### -------------------------------------------------------------------- #####
##### -------------------------------------------------------------------- #####
##### -------------------------------------------------------------------- #####
##### -------------------------------------------------------------------- #####

##### Bayesian analysis for length-weight relationship


# -----------------------------
# 2.1 Bayesian analysis packages
# -----------------------------

library(brms)
library(cmdstanr)
library(posterior)
library(tidybayes)
library(tidyverse)

options(mc.cores = parallel::detectCores())

# Optional: check that cmdstan is available
cmdstanr::cmdstan_version()


# -----------------------------
# 2.2 Prepare data for Bayesian length-weight model
# -----------------------------

lwa_bayes <- lwa %>%
  mutate(
    year = as.integer(as.character(Year)),
    month = as.integer(as.character(Month)),

    # Explicit factor levels help when predicting
    Sex = droplevels(factor(Sex)),
    Source = droplevels(factor(Source)),
    Month_f = factor(
      month,
      levels = 1:12,
      labels = month.abb
    ),

    # Center log length to improve MCMC sampling.
    # Predictions are still returned on the original log-weight scale.
    log_length_mean = mean(log_length),
    log_length_c = log_length - log_length_mean,

    # Unique observation ID for joining predictions back to fish
    fish_id = row_number()
  )

log_length_center <- unique(lwa_bayes$log_length_mean)

stopifnot(length(log_length_center) == 1)


# -----------------------------
# 2.3 Priors
# -----------------------------

prior_intercept_mean <- median(lwa_bayes$log_weight)

priors_lw <- c(
  # Expected log weight at the centered length
  set_prior(
    paste0(
      "normal(",
      format(
        prior_intercept_mean,
        scientific = FALSE,
        digits = 15
      ),
      ", 1)"
    ),
    class = "Intercept"
  ),

  # Fixed effects, including length slope, sex effect,
  # month effects and length-by-sex interaction
  prior(
    normal(0, 1),
    class = "b"
  ),

  # Among-source variability - not needed for simpler LW model
  # prior(
  #   student_t(3, 0, 0.5),
  #   class = "sd"
  # ),

  # Individual residual variability in log weight
  prior(
    student_t(3, 0, 0.5),
    class = "sigma"
  )
)

priors_lw

get_prior(
  formula =
    log_weight ~ log_length_c * Sex,
  data = lwa_bayes,
  family = gaussian()
)


# -----------------------------
# 2.4 Bayesian length-weight model
# -----------------------------
#
# Deliberately excludes a Year random intercept so that annual
# weight-at-length variation remains in the condition index.
#
# Month_f controls for the average seasonal pattern.
# Source controls for persistent source-specific differences.

form_lw_bayes <- bf(
  log_weight ~
    log_length_c * Sex
)

m_lw_bayes <- brm(
  formula = form_lw_bayes,
  data = lwa_bayes,
  family = gaussian(),
  prior = priors_lw,

  backend = "cmdstanr",
  chains = 4,
  iter = 3000,
  warmup = 1500,
  seed = 2,

  control = list(
    adapt_delta = 0.95,
    max_treedepth = 12
  ),

  save_pars = save_pars(all = TRUE),
  file = "stock-specific/hake/model-outputs/m_lw_bayes",
  file_refit = "on_change"
)


# -----------------------------
# 2.5 Model diagnostics
# -----------------------------

# show r-hats, ESS and coefficient estimates distributions
summary(m_lw_bayes)

plot(m_lw_bayes)

# These take a while to run so skipping these

# show density distribution of posterior predictions with observed data
# pp_check(
#   m_lw_bayes,
#   ndraws = 100,
#   type = "dens_overlay"
# )

# compare observed vs predicted as scatter plot
# pp_check(
#   m_lw_bayes,
#   ndraws = 100,
#   type = "scatter_avg"
# )

# variation explained by model
r2_lw <- bayes_R2(
  m_lw_bayes,
  ndraws = 500
)

r2_lw

# Fixed-effect posterior summaries
fixef(m_lw_bayes, probs = c(0.025, 0.5, 0.975))

# -----------------------------
# Sex-specific length slopes
# -----------------------------

lw_coef_draws <- as_draws_df(m_lw_bayes)

sex_levels <- levels(lwa_bayes$Sex)

# female are the reference
sex_levels

variables(lw_coef_draws)[
  stringr::str_detect(
    variables(lw_coef_draws),
    "log_length"
  )
]

# getting slope estimates (and distirbution) for each sex from bayesian draws
slope_draws <- lw_coef_draws %>%
  transmute(
    .draw,
    slope_F = b_log_length_c,
    slope_M = b_log_length_c + `b_log_length_c:SexM`
  )

# summarizing them
slope_summary <- slope_draws %>%
  pivot_longer(
    cols = starts_with("slope_"),
    names_to = "Sex",
    values_to = "b"
  ) %>%
  group_by(Sex) %>%
  summarise(
    median = median(b),
    lower = quantile(b, 0.025),
    upper = quantile(b, 0.975),
    probability_b_gt_3 = mean(b > 3),
    probability_b_lt_3 = mean(b < 3),
    .groups = "drop"
  )

# print slope summary
slope_summary

# -----------------------------
# 2.6 Construct posterior draws of monthly condition
# -----------------------------

# function to extract posterior distribution of monthly condition
make_monthly_condition_draws <- function(
    model,
    data,
    ndraws = 500,
    chunk_size = 10000,
    seed = 1837,
    include_source_effect = TRUE
) {

  set.seed(seed)

  # Select posterior draws once and use the same draw IDs
  # for every data chunk.
  total_draws <- posterior::ndraws(model)

  selected_draw_ids <- sort(
    sample(
      seq_len(total_draws),
      size = min(ndraws, total_draws),
      replace = FALSE
    )
  )

  # Integer index for year-month groups
  group_lookup <- data %>%
    distinct(year, month) %>%
    arrange(year, month) %>%
    mutate(group_id = row_number())

  data_indexed <- data %>%
    left_join(
      group_lookup,
      by = c("year", "month")
    )

  n_groups <- nrow(group_lookup)
  n_selected <- length(selected_draw_ids)

  # Accumulate:
  # 1. sum of observed log weights
  # 2. sum of posterior expected log weights
  # 3. number of fish
  observed_sum <- numeric(n_groups)
  expected_sum <- matrix(
    0,
    nrow = n_selected,
    ncol = n_groups
  )
  group_n <- integer(n_groups)

  chunk_starts <- seq(
    from = 1,
    to = nrow(data_indexed),
    by = chunk_size
  )

  for (chunk_number in seq_along(chunk_starts)) {

    row_start <- chunk_starts[chunk_number]
    row_end <- min(
      row_start + chunk_size - 1,
      nrow(data_indexed)
    )

    rows <- row_start:row_end
    chunk <- data_indexed[rows, ]

    message(
      "Processing chunk ",
      chunk_number,
      " of ",
      length(chunk_starts),
      ": rows ",
      row_start,
      "-",
      row_end
    )

    # Posterior draws of the expected log weight.
    #
    # re_formula = NULL:
    # include estimated Source random effects.
    #
    # re_formula = NA:
    # exclude all random effects and estimate condition relative
    # to the population-average source.
    mu_chunk <- posterior_linpred(
      model,
      newdata = chunk,
      transform = FALSE,
      re_formula = if (include_source_effect) NULL else NA,
      draw_ids = selected_draw_ids
    )

    group_id <- chunk$group_id

    # Add observed log weights and sample sizes by group
    observed_chunk <- rowsum(
      chunk$log_weight,
      group = group_id,
      reorder = FALSE
    )

    n_chunk <- rowsum(
      rep(1L, nrow(chunk)),
      group = group_id,
      reorder = FALSE
    )

    group_present <- as.integer(rownames(observed_chunk))

    observed_sum[group_present] <-
      observed_sum[group_present] +
      observed_chunk[, 1]

    group_n[group_present] <-
      group_n[group_present] +
      n_chunk[, 1]

    # Aggregate expected log weights within this chunk.
    #
    # rowsum() operates on rows, so transpose first:
    # fish x posterior draws.
    expected_chunk <- rowsum(
      t(mu_chunk),
      group = group_id,
      reorder = FALSE
    )

    expected_sum[, group_present] <-
      expected_sum[, group_present] +
      t(expected_chunk)
  }

  # Monthly mean log(Kn):
  #
  # mean(log(W observed) - log(W expected))
  #
  # = mean(log observed weight) -
  #   mean(expected log weight)
  mean_observed_log_weight <- observed_sum / group_n

  mean_expected_log_weight <- sweep(
    expected_sum,
    MARGIN = 2,
    STATS = group_n,
    FUN = "/"
  )

  mean_log_kn <- sweep(
    mean_expected_log_weight,
    MARGIN = 2,
    STATS = mean_observed_log_weight,
    FUN = function(expected, observed) observed - expected
  )

  # Convert posterior matrix to tidy format
  condition_draws <- as_tibble(mean_log_kn) %>%
    mutate(
      .draw_index = seq_len(n_selected),
      .draw = selected_draw_ids
    ) %>%
    pivot_longer(
      cols = starts_with("V"),
      names_to = "group_column",
      values_to = "mean_log_Kn"
    ) %>%
    mutate(
      group_id = as.integer(
        stringr::str_remove(group_column, "^V")
      )
    ) %>%
    select(
      .draw,
      .draw_index,
      group_id,
      mean_log_Kn
    ) %>%
    left_join(
      group_lookup,
      by = "group_id"
    ) %>%
    mutate(
      # Geometric-mean Kn for each posterior draw
      geometric_mean_Kn = exp(mean_log_Kn)
    ) %>%
    arrange(.draw_index, year, month)

  condition_draws
}


# Separate All, US, and Can data here for monthly draws

# filter only canada or us data
filter_index <- unique(lwa_cond$Source)
filter_index_can <- c("CAN_acoustic", "CAN_shoreside", "CAN_JV","CAN_polish", "Acoustic Canada")
filter_index_us <- c("US_foreign", "US_JV", "US_shore", "US_acoustic", "US_atsea", "Acoustic U.S.")

lwa_bayes_can <- lwa_bayes %>%
  filter(Source %in% filter_index_can)

lwa_bayes_us <- lwa_bayes %>%
  filter(Source %in% filter_index_us)

cond_monthly_draws <- make_monthly_condition_draws(
  model = m_lw_bayes,
  data = lwa_bayes,
  ndraws = 500,
  chunk_size = 10000,
  seed = 1837,

  # TRUE adjusts expected weight for systematic source differences.
  include_source_effect = TRUE
)

cond_monthly_draws_can<- make_monthly_condition_draws(
  model = m_lw_bayes,
  data = lwa_bayes_can,
  ndraws = 500,
  chunk_size = 10000,
  seed = 1837,

  # TRUE adjusts expected weight for systematic source differences.
  include_source_effect = TRUE
)

cond_monthly_draws_us <- make_monthly_condition_draws(
  model = m_lw_bayes,
  data = lwa_bayes_us,
  ndraws = 500,
  chunk_size = 10000,
  seed = 1837,

  # TRUE adjusts expected weight for systematic source differences.
  include_source_effect = TRUE
)

glimpse(cond_monthly_draws)
glimpse(cond_monthly_draws_can)
glimpse(cond_monthly_draws_us)


# -----------------------------
# 2.7 Posterior summaries by year and month
# -----------------------------

cond_monthly_summary <- cond_monthly_draws %>%
  group_by(year, month) %>%
  summarise(
    median_log_Kn = median(mean_log_Kn),
    lower_log_Kn = quantile(mean_log_Kn, 0.025),
    upper_log_Kn = quantile(mean_log_Kn, 0.975),

    median_Kn = median(geometric_mean_Kn),
    lower_Kn = quantile(geometric_mean_Kn, 0.025),
    upper_Kn = quantile(geometric_mean_Kn, 0.975),

    probability_Kn_gt_1 = mean(geometric_mean_Kn > 1),

    .groups = "drop"
  ) %>%
  left_join(
    lwa_bayes %>%
      count(year, month, name = "n_fish"),
    by = c("year", "month")
  )

cond_monthly_summary


# plot monthly posterior condition index
ggplot(
  cond_monthly_summary,
  aes(
    x = as.Date(sprintf("%04d-%02d-15", year, month)),
    y = median_Kn
  )
) +
  geom_hline(
    yintercept = 1,
    linetype = "dashed"
  ) +
  geom_ribbon(
    aes(
      ymin = lower_Kn,
      ymax = upper_Kn
    ),
    alpha = 0.25
  ) +
  geom_line() +
  geom_point(
    aes(size = n_fish),
    alpha = 0.6
  ) +
  labs(
    x = NULL,
    y = "Posterior geometric mean Kn",
    size = "Number of fish",
    title = "Monthly posterior body-condition index",
    subtitle = "Ribbon shows the 95% credible interval"
  ) +
  theme_bw()

# seasonal patterns
ggplot(
  cond_monthly_summary,
  aes(
    x = year,
    y = median_Kn
  )
) +
  geom_hline(
    yintercept = 1,
    linetype = "dashed"
  ) +
  geom_ribbon(
    aes(
      ymin = lower_Kn,
      ymax = upper_Kn
    ),
    alpha = 0.25
  ) +
  geom_line() +
  facet_wrap(
    ~ month,
    ncol = 4
  ) +
  labs(
    x = "Year",
    y = "Posterior geometric mean Kn",
    title = "Body-condition anomalies by calendar month"
  ) +
  theme_bw()


# -----------------------------
# 2.8 Fish-level condition draws for validation
# -----------------------------

set.seed(22)

# sample from observed data used for bayes analysis
validation_fish <- lwa_bayes %>%
  slice_sample(n = min(2000, nrow(lwa_bayes)))

# posterior draws of expected log_weight
mu_validation <- posterior_linpred(
  m_lw_bayes,
  newdata = validation_fish,
  transform = FALSE,
  re_formula = NULL,
  ndraws = 500
)

# mutate the posterior draws into a dataframe with original observed data. calculated distribution of expected condition for each fish
fish_condition_draws <- as_tibble(mu_validation) %>%
  mutate(.draw = row_number()) %>%
  pivot_longer(
    cols = starts_with("V"),
    names_to = "fish_column",
    values_to = "expected_log_weight"
  ) %>%
  mutate(
    fish_row = as.integer(
      stringr::str_remove(fish_column, "^V")
    )
  ) %>%
  left_join(
    validation_fish %>%
      mutate(fish_row = row_number()) %>%
      select(
        fish_row,
        fish_id,
        year,
        month,
        Sex,
        Source,
        Length_cm,
        Weight_kg,
        log_weight
      ),
    by = "fish_row"
  ) %>%
  mutate(
    log_Kn = log_weight - expected_log_weight,
    Kn = exp(log_Kn)
  )

fish_condition_draws

# summarize each fish's posterior condition
fish_condition_summary <- fish_condition_draws %>%
  group_by(
    fish_row,
    fish_id,
    year,
    month,
    Sex,
    Source,
    Length_cm,
    Weight_kg
  ) %>%
  summarise(
    Kn = median(Kn),
    Kn_lower = quantile(Kn, 0.025),
    Kn_upper = quantile(Kn, 0.975),
    log_Kn = median(log_Kn),
    .groups = "drop"
  ) %>%
  arrange(year, month) %>%
  mutate(date = lubridate::make_date(year = year, month = month, day = 1))

# plot fish size and median body condition from posterior distribution
ggplot(
  fish_condition_summary,
  aes(x = Length_cm, y = Kn)
) +
  geom_point(alpha = 0.2) +
  geom_smooth() +
  geom_hline(yintercept = 1, linetype = 2)

# plot fish size and median body condition from posterior distribution - separated by sex
ggplot(
  fish_condition_summary,
  aes(x = Length_cm, y = Kn, colour = Sex)
) +
  geom_point(alpha = 0.15) +
  geom_smooth(
    method = "gam",
    formula = y ~ s(x),
    se = TRUE
  ) +
  geom_hline(
    yintercept = 1,
    linetype = "dashed"
  ) +
  labs(
    x = "Fork length (cm)",
    y = "Relative condition (Kn)",
    colour = "Sex"
  ) +
  theme_classic()

# log condition
ggplot(
  fish_condition_summary,
  aes(x = Length_cm, y = log_Kn, colour = Sex)
) +
  geom_point(alpha = 0.15) +
  geom_smooth(
    method = "gam",
    formula = y ~ s(x),
    se = TRUE
  ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_classic()

# check effects of sex on overall body condition
ggplot(
  fish_condition_summary,
  aes(x = Sex, y = Kn)
) +
  geom_boxplot() +
  geom_hline(yintercept = 1, linetype = 2)

# check effects of month
ggplot(
  fish_condition_summary,
  aes(x = factor(month), y = Kn)
) +
  geom_boxplot() +
  geom_hline(yintercept = 1, linetype = 2)

ggplot(
  fish_condition_summary,
  aes(x = Length_cm, y = Kn)
) +
  geom_point(alpha = 0.1) +
  geom_smooth(
    method = "gam",
    formula = y ~ s(x),
    se = FALSE
  ) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  facet_wrap(~ month) +
  labs(
    x = "Fork length (cm)",
    y = "Relative condition (Kn)"
  ) +
  theme_classic()

# check effects of sex and condition over time
ggplot(
  fish_condition_summary,
  aes(x = date, y = Kn, col = Sex)
) +
  geom_point(alpha = 0.15) +
  geom_smooth(
    method = "gam",
    formula = y ~ s(x),
    se = TRUE
  )

# check effects of source
ggplot(
  fish_condition_summary,
  aes(x = Source, y = Kn)
) +
  geom_boxplot() +
  geom_hline(yintercept = 1, linetype = 2) +
  coord_flip()

ggplot(
  fish_condition_summary,
  aes(x = Length_cm, y = Kn)
) +
  geom_point(alpha = 0.1) +
  geom_smooth(
    method = "gam",
    formula = y ~ s(x),
    se = FALSE
  ) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  facet_wrap(~ Source) +
  labs(
    x = "Fork length (cm)",
    y = "Relative condition (Kn)"
  ) +
  theme_classic()

##### -------------------------------------------------------------------- #####
##### -------------------------------------------------------------------- #####
##### -------------------------------------------------------------------- #####
##### -------------------------------------------------------------------- #####
##### -------------------------------------------------------------------- #####

# Using posteriors of body condition for bayesian analysis with env covariates

# -----------------------------
# 3.1 Join monthly environmental covariates
# -----------------------------

# take the environmental rolling data from secion 1.6
env_wide_clean <- env_wide_rolling %>%
  mutate(
    year = as.integer(year),
    month = as.integer(month)
  ) %>%
  arrange(year, month)


selected <- "all"
selected <- "us"
selected <- "can"

if(selected == "all"){
  cond_monthly_draws_used <- cond_monthly_draws

} else {
  if(selected == "us"){
    cond_monthly_draws_used <- cond_monthly_draws_us

  } else {
    if(selected == "can"){
      cond_monthly_draws_used <- cond_monthly_draws_can

    } else {
      rm(cond_monthly_draws_used)
    }
  }
}

cond_env_draws <- cond_monthly_draws_used %>%
  left_join(
    env_wide_clean,
    by = c("year", "month")
  ) %>%
  arrange(.draw_index, year, month)

glimpse(cond_env_draws)


# List env variables to use in analysis
env_vars <- c(
  "pdo_m_prev1",
  "npgo_m_prev1",
  "oni_m_prev1",
  "npi_m_prev1",
  "mei_m_prev1",
  "pdo_m_prev3",
  "npgo_m_prev3",
  "oni_m_prev3",
  "npi_m_prev3",
  "mei_m_prev3",
  "pdo_m_prev6",
  "npgo_m_prev6",
  "oni_m_prev6",
  "npi_m_prev6",
  "mei_m_prev6"
)

# scale environmental variables
env_scaling <- env_wide_clean %>%
  ungroup() %>%
  summarise(
    across(
      all_of(env_vars),
      list(
        mean = ~ mean(.x, na.rm = TRUE),
        sd = ~ sd(.x, na.rm = TRUE)
      )
    )
  )

scale_env_variable <- function(x, variable, scaling_table) {

  variable_mean <- scaling_table[[paste0(variable, "_mean")]]

  variable_sd <- scaling_table[[paste0(variable, "_sd")]]

  as.numeric(
    (x - variable_mean) / variable_sd
  )
}

for (v in env_vars) {
  cond_env_draws[[paste0(v, "_z")]] <-
    scale_env_variable(
      x = cond_env_draws[[v]],
      variable = v,
      scaling_table = env_scaling
    )
}

# -----------------------------
# 3.2 Create posterior-imputed datasets
# -----------------------------

set.seed(22)

# number of posterior draws to pull from condition model output
n_condition_datasets <- 50

available_condition_draws <- cond_env_draws %>%
  distinct(.draw_index, .draw)

selected_condition_draws <- available_condition_draws %>%
  slice_sample(
    n = min(
      n_condition_datasets,
      nrow(available_condition_draws)
    )
  )

# create list of 50 dataset (from posterior draws) that are matched to environmental variables
environment_data_list <- selected_condition_draws$.draw_index %>%
  map(function(draw_number) {

    cond_env_draws %>%
      filter(.draw_index == draw_number) %>%
      mutate(
        month_f = factor(
          month,
          levels = 1:12,
          labels = month.abb
        ),

        year_f = factor(year),

        # Consecutive monthly time index
        time = year * 12L + month
      ) %>%
      arrange(time) %>%
      select(
        mean_log_Kn,
        year,
        year_f,
        month,
        month_f,
        time,
        all_of(env_vars),
        ends_with("_z")
      ) %>%
      drop_na(
        mean_log_Kn
      )
  })

length(environment_data_list)


# -----------------------------
# 3.3 Environmental analysis using brm_multiple
# -----------------------------
#
# Month controls for any remaining seasonal structure.
#
# A year random intercept is optional here. It accounts for unexplained
# annual variation, but it can compete with annual environmental
# covariates. For monthly covariates, it is generally more defensible.
#
# The environmental coefficient represents the effect of a
# one-standard-deviation change in PDO.

# # modelling one env variable - pdo lag 1 month
# form_env_pdo <- bf(
#   mean_log_Kn ~
#     pdo_m_lag1_z +
#     month_f +
#     (1 | year_f)
# )
#
# # priors
# priors_env <- c(
#   prior(
#     normal(0, 0.1),
#     class = "Intercept"
#   ),
#   prior(
#     normal(0, 0.1),
#     class = "b"
#   ),
#   prior(
#     student_t(3, 0, 0.1),
#     class = "sd"
#   ),
#   prior(
#     student_t(3, 0, 0.1),
#     class = "sigma"
#   )
# )
#
# # run model
# m_env_pdo <- brm_multiple(
#   formula = form_env_pdo,
#   data = environment_data_list,
#   family = gaussian(),
#   prior = priors_env,
#
#   backend = "cmdstanr",
#   chains = 4,
#   iter = 2500,
#   warmup = 1250,
#   seed = 22,
#
#   control = list(
#     adapt_delta = 0.95,
#     max_treedepth = 12
#   ),
#
#   combine = TRUE
# )
#
#
# # inspect results
# summary(m_env_pdo)
#
# plot(m_env_pdo)
#
# pp_check(
#   m_env_pdo,
#   ndraws = 100,
#   type = "dens_overlay"
# )
#
# fixef(
#   m_env_pdo,
#   probs = c(0.025, 0.5, 0.975)
# )
#
#
# # extract environmental-effect posterior
# pdo_effect <- as_draws_df(m_env_pdo) %>%
#   transmute(
#     .draw,
#     beta_pdo = b_pdo_m_lag1_z,
#
#     # Multiplicative change in Kn for a one-SD increase in PDO
#     Kn_ratio = exp(beta_pdo)
#   )
#
# pdo_effect_summary <- pdo_effect %>%
#   summarise(
#     median_beta = median(beta_pdo),
#     lower_beta = quantile(beta_pdo, 0.025),
#     upper_beta = quantile(beta_pdo, 0.975),
#
#     median_Kn_ratio = median(Kn_ratio),
#     lower_Kn_ratio = quantile(Kn_ratio, 0.025),
#     upper_Kn_ratio = quantile(Kn_ratio, 0.975),
#
#     probability_positive = mean(beta_pdo > 0),
#     probability_negative = mean(beta_pdo < 0)
#   )
#
# pdo_effect_summary



# -----------------------------
# 3.4 General function for environmental models
# -----------------------------

# function for running all models with each environmental variable
fit_environment_model <- function(
    variable,
    data_list,
    model_file = NULL,
    seed = 22
) {

  variable_z <- paste0(variable, "_z")

  formula_text <- paste0(
    "mean_log_Kn ~ ",
    variable_z,
    " + month_f + (1 | year_f)"
  )

  model_formula <- bf(
    as.formula(formula_text)
  )

  brm_multiple(
    formula = model_formula,
    data = map(
      data_list,
      ~ .x %>%
        drop_na(
          mean_log_Kn,
          all_of(variable_z)
        )
    ),
    family = gaussian(),
    prior = priors_env,

    backend = "cmdstanr",
    chains = 4,
    iter = 1000,
    warmup = 500,
    seed = seed,

    control = list(
      adapt_delta = 0.95,
      max_treedepth = 12
    ),

    combine = TRUE,

    file = model_file,
    file_refit = "always"
    # file_refit = if (is.null(model_file)) NULL else "on_change"
  )
}

# priors
priors_env <- c(
  prior(
    normal(0, 0.1),
    class = "Intercept"
  ),
  prior(
    normal(0, 0.1),
    class = "b"
  ),
  prior(
    student_t(3, 0, 0.1),
    class = "sd"
  ),
  prior(
    student_t(3, 0, 0.1),
    class = "sigma"
  )
)


# fit models
env_models_bayes <- list(
  pdo_m_prev1 = fit_environment_model(
    variable = "pdo_m_prev1",
    data_list = environment_data_list,
    model_file = paste0("stock-specific/hake/model-outputs/m_env_pdo_m_prev1_", selected)
  ),

  npgo_m_prev1 = fit_environment_model(
    variable = "npgo_m_prev1",
    data_list = environment_data_list,
    model_file = paste0("stock-specific/hake/model-outputs/m_env_npgo_m_prev1_", selected)
  ),

  oni_m_prev1 = fit_environment_model(
    variable = "oni_m_prev1",
    data_list = environment_data_list,
    model_file = paste0("stock-specific/hake/model-outputs/m_env_oni_m_prev1_", selected)
  ),

  npi_m_prev1 = fit_environment_model(
    variable = "npi_m_prev1",
    data_list = environment_data_list,
    model_file = paste0("stock-specific/hake/model-outputs/m_env_npi_m_prev1_", selected)
  ),

  pdo_m_prev3 = fit_environment_model(
    variable = "pdo_m_prev3",
    data_list = environment_data_list,
    model_file = paste0("stock-specific/hake/model-outputs/m_env_pdo_m_prev3_", selected)
  ),

  npgo_m_prev3 = fit_environment_model(
    variable = "npgo_m_prev3",
    data_list = environment_data_list,
    model_file = paste0("stock-specific/hake/model-outputs/m_env_npgo_m_prev3_", selected)
  ),

  oni_m_prev3 = fit_environment_model(
    variable = "oni_m_prev3",
    data_list = environment_data_list,
    model_file = paste0("stock-specific/hake/model-outputs/m_env_oni_m_prev3_", selected)
  ),

  npi_m_prev3 = fit_environment_model(
    variable = "npi_m_prev3",
    data_list = environment_data_list,
    model_file = paste0("stock-specific/hake/model-outputs/m_env_npi_m_prev3_", selected)
  ),

  pdo_m_prev6 = fit_environment_model(
    variable = "pdo_m_prev6",
    data_list = environment_data_list,
    model_file = paste0("stock-specific/hake/model-outputs/m_env_pdo_m_prev6_", selected)
  ),

  npgo_m_prev6 = fit_environment_model(
    variable = "npgo_m_prev6",
    data_list = environment_data_list,
    model_file = paste0("stock-specific/hake/model-outputs/m_env_npgo_m_prev6_", selected)
  ),

  oni_m_prev6 = fit_environment_model(
    variable = "oni_m_prev6",
    data_list = environment_data_list,
    model_file = paste0("stock-specific/hake/model-outputs/m_env_oni_m_prev6_", selected)
  ),

  npi_m_prev6 = fit_environment_model(
    variable = "npi_m_prev6",
    data_list = environment_data_list,
    model_file = paste0("stock-specific/hake/model-outputs/m_env_npi_m_prev6_", selected)
  )
)


env_models_bayes_sub <- list(
  mei_m_prev1 = fit_environment_model(
    variable = "mei_m_prev1",
    data_list = environment_data_list,
    model_file = paste0("stock-specific/hake/model-outputs/m_env_mei_m_prev1_", selected)
  ),

  mei_m_prev3 = fit_environment_model(
    variable = "mei_m_prev3",
    data_list = environment_data_list,
    model_file = paste0("stock-specific/hake/model-outputs/m_env_mei_m_prev3_", selected)
  ),

  mei_m_prev6 = fit_environment_model(
    variable = "mei_m_prev6",
    data_list = environment_data_list,
    model_file = paste0("stock-specific/hake/model-outputs/m_env_mei_m_prev6_", selected)
  )
)

length(env_models_bayes)
length(env_models_bayes_sub)

# combine initial and extra analysis
# env_models_bayes <- c(env_models_bayes, env_models_bayes_sub)
# rm(env_models_bayes_sub)

# summarize

# slow try below before deleting
# env_effect_summary <- imap_dfr(
#   env_models_bayes,
#   function(model, variable) {
#
#     coefficient_name <- paste0(
#       "b_",
#       variable,
#       "_z"
#     )
#
#     model_draws <- as_draws_df(model)
#
#     beta <- model_draws[[coefficient_name]]
#
#     tibble(
#       covariate = variable,
#
#       median_beta = median(beta),
#       lower_beta = quantile(beta, 0.025),
#       upper_beta = quantile(beta, 0.975),
#
#       median_Kn_ratio = median(exp(beta)),
#       lower_Kn_ratio = quantile(exp(beta), 0.025),
#       upper_Kn_ratio = quantile(exp(beta), 0.975),
#
#       probability_positive = mean(beta > 0),
#       probability_negative = mean(beta < 0)
#     )
#   }
# )


env_effect_summary <- imap_dfr(
  env_models_bayes,
  function(model, variable) {

    coefficient_name <- paste0(
      "b_",
      variable,
      "_z"
    )

    beta <- as_draws_matrix(
      model,
      variable = coefficient_name
    )[, coefficient_name]

    beta_exp <- exp(beta)

    tibble(
      covariate = variable,

      median_beta = median(beta),
      lower_beta = quantile(beta, 0.025),
      upper_beta = quantile(beta, 0.975),

      median_Kn_ratio = median(beta_exp),
      lower_Kn_ratio = quantile(beta_exp, 0.025),
      upper_Kn_ratio = quantile(beta_exp, 0.975),

      probability_positive = mean(beta > 0),
      probability_negative = mean(beta < 0)
    )
  }
)

env_effect_summary


# -----------------------------
# 3.5 Save posterior outputs
# -----------------------------

dir.create(
  "stock-specific/hake/posterior-outputs",
  showWarnings = FALSE,
  recursive = TRUE
)

saveRDS(
  cond_monthly_draws,
  file = paste0("stock-specific/hake/posterior-outputs/condition_monthly_posterior_draws_", selected, ".rds")
)

saveRDS(
  cond_monthly_summary,
  file = paste0("stock-specific/hake/posterior-outputs/condition_monthly_posterior_summary_", selected, ".rds")
)

saveRDS(
  env_scaling,
  file = paste0("stock-specific/hake/posterior-outputs/environment_covariate_scaling_", selected, ".rds")
)

write.csv(
  cond_monthly_summary,
  file = paste0("stock-specific/hake/posterior-outputs/condition_monthly_posterior_summary_", selected, ".csv"),
  row.names = FALSE
)


# -----------------------------
# 3.6 Plot posterior outputs
# -----------------------------

library(brms)
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(ggdist)
library(tibble)

# plot individual env variable posterior
plot_env_posterior <- function(model, variable) {

  coefficient_name <- paste0(
    "b_",
    variable,
    "_z"
  )

  beta <- posterior::as_draws_matrix(
    model,
    variable = coefficient_name
  )[, coefficient_name]

  beta <- as.numeric(beta)

  posterior_data <- tibble::tibble(
    beta = beta
  )

  variable_label <- variable |>
    gsub("_m_prev1$", ": previous month", x = _) |>
    gsub("_m_prev3$", ": previous 3 months", x = _) |>
    gsub("_m_prev6$", ": previous 6 months", x = _) |>
    toupper()

  ggplot2::ggplot(
    posterior_data,
    ggplot2::aes(
      x = beta,
      y = variable_label
    )
  ) +
    ggdist::stat_halfeye(
      .width = c(0.50, 0.80, 0.95),
      point_interval = ggdist::median_qi,
      normalize = "groups"
    ) +
    ggplot2::geom_vline(
      xintercept = 0,
      linetype = "dashed",
      linewidth = 0.6
    ) +
    ggplot2::labs(
      title = paste(
        "Posterior effect of",
        variable_label
      ),
      subtitle = paste(
        "Coefficient:",
        coefficient_name
      ),
      x = expression(
        beta ~ "(change in log condition per 1 SD)"
      ),
      y = NULL
    ) +
    ggplot2::theme_classic(
      base_size = 12
    )
}

plot_env_posterior(
  model = env_models_bayes[["pdo_m_prev1"]],
  variable = "pdo_m_prev1"
)

# all plots in facet
extract_env_posterior <- function(model, variable) {

  coefficient_name <- paste0(
    "b_",
    variable,
    "_z"
  )

  draws <- posterior::as_draws_matrix(
    model,
    variable = coefficient_name
  )

  if (ncol(draws) != 1L) {
    stop(
      paste0(
        "Expected coefficient '",
        coefficient_name,
        "' in model '",
        variable,
        "', but it was not found uniquely."
      ),
      call. = FALSE
    )
  }

  tibble::tibble(
    model = variable,
    coefficient = coefficient_name,
    beta = as.numeric(draws[, 1])
  )
}

env_posterior_draws <- imap_dfr(
  env_models_bayes,
  \(model, variable) {
    extract_env_posterior(
      model = model,
      variable = variable
    )
  }
)

env_posterior_draws <- env_posterior_draws %>%
  mutate(
    index = case_when(
      grepl("^pdo", model)  ~ "PDO",
      grepl("^npgo", model) ~ "NPGO",
      grepl("^oni", model)  ~ "ONI",
      grepl("^npi", model)  ~ "NPI",
      grepl("^mei", model)  ~ "MEI",
      TRUE                  ~ model
    ),

    exposure_window = case_when(
      grepl("_prev1$", model) ~ "Previous month",
      grepl("_prev3$", model) ~ "Previous 3 months",
      grepl("_prev6$", model) ~ "Previous 6 months",
      TRUE                    ~ "Other"
    ),

    exposure_window = factor(
      exposure_window,
      levels = c(
        "Previous month",
        "Previous 3 months",
        "Previous 6 months"
      )
    ),

    index = factor(
      index,
      levels = c("PDO", "NPGO", "ONI", "NPI", "MEI")
    )
  )

env_posterior_combined_plot <- ggplot(
  env_posterior_draws,
  aes(
    x = beta,
    y = exposure_window
  )
) +
  stat_halfeye(
    .width = c(0.50, 0.80, 0.95),
    point_interval = median_qi,
    normalize = "groups"
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    linewidth = 0.6
  ) +
  facet_wrap(
    vars(index),
    ncol = 2,
    scales = "free_x"
  ) +
  labs(
    title = "Posterior environmental effects on body condition",
    subtitle = paste(
      "Points show posterior medians;",
      "intervals show 50%, 80%, and 95% credible intervals"
    ),
    x = expression(beta ~ "(change in log condition per 1 SD)"),
    y = "Environmental exposure window"
  ) +
  theme_classic(base_size = 12) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    axis.text.y = element_text(size = 10)
  )

env_posterior_combined_plot


# percent change in body condition
env_posterior_draws <- env_posterior_draws %>%
  mutate(
    percent_change = 100 * (exp(beta) - 1)
  )

env_percent_change_plot <- ggplot(
  env_posterior_draws,
  aes(
    x = percent_change,
    y = exposure_window
  )
) +
  stat_halfeye(
    .width = c(0.50, 0.80, 0.95),
    point_interval = median_qi,
    normalize = "groups"
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    linewidth = 0.6
  ) +
  facet_wrap(
    vars(index),
    ncol = 2,
    scales = "free_x"
  ) +
  labs(
    title = "Estimated changes in body condition",
    subtitle = "Change associated with a 1-SD increase in each index",
    x = "Change in body condition (%)",
    y = "Environmental exposure window"
  ) +
  theme_classic(base_size = 12) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    axis.text.y = element_text(size = 10)
  )

env_percent_change_plot

# funtion to plot linear relationship
plot_env_linear <- function(
    variable,
    data = dat_env,
    posterior_draws = env_posterior_draws,
    response = "mean_log_Kn",
    n_grid = 100
) {

  # Extract existing posterior slopes
  beta_z <- posterior_draws %>%
    filter(model == variable) %>%
    pull(beta)

  if (length(beta_z) == 0) {
    stop("No posterior draws found for ", variable)
  }

  # Observed data
  observed <- data %>%
    transmute(
      x = .data[[variable]],
      y = .data[[response]]
    ) %>%
    filter(
      is.finite(x),
      is.finite(y)
    )

  # IMPORTANT:
  # These must match the scaling used to fit the model.
  mean_x <- mean(observed$x)
  sd_x <- sd(observed$x)

  # Convert standardized posterior slopes to original units
  beta_original <- beta_z / sd_x

  # Anchor the regression lines at the observed means
  mean_y <- mean(observed$y)

  # Grid of environmental values
  x_grid <- seq(
    min(observed$x),
    max(observed$x),
    length.out = n_grid
  )

  # Compute posterior expected relationships
  # Each row corresponds to an environmental value
  prediction_summary <- lapply(
    x_grid,
    function(x) {

      predicted_y <- mean_y +
        beta_original * (x - mean_x)

      tibble(
        x = x,
        median = median(predicted_y),
        lower = quantile(predicted_y, 0.025),
        upper = quantile(predicted_y, 0.975)
      )
    }
  ) %>%
    bind_rows()

  # Plot
  ggplot() +
    geom_point(
      data = observed,
      aes(x = x, y = y),
      alpha = 0.4,
      size = 1.5
    ) +
    geom_ribbon(
      data = prediction_summary,
      aes(
        x = x,
        ymin = lower,
        ymax = upper
      ),
      alpha = 0.2
    ) +
    geom_line(
      data = prediction_summary,
      aes(
        x = x,
        y = median
      ),
      linewidth = 0.9
    ) +
    labs(
      title = toupper(variable),
      subtitle = "Posterior median slope and 95% credible interval",
      x = "Environmental index (original units)",
      y = "Mean log body condition"
    ) +
    theme_classic(base_size = 12)
}

env_linear_plots <- map(
  unique(env_posterior_draws$model),
  # unique(env_posterior_draws$model)[order(unique(env_posterior_draws$model))],
  plot_env_linear
)

env_linear_combined <- wrap_plots(
  env_linear_plots,
  ncol = 3
)

env_linear_combined

##### -------------------------------------------------------------------- #####
##### -------------------------------------------------------------------- #####
##### -------------------------------------------------------------------- #####
##### -------------------------------------------------------------------- #####
##### -------------------------------------------------------------------- #####

# Mean age with environmental variables individually by month

# -----------------------------
# 4.1 Define the environmental variables
# -----------------------------

# list of environmental variables to use
env_variables <- c(
  "pdo_m_prev1",
  "npgo_m_prev1",
  "oni_m_prev1",
  "npi_m_prev1",
  "mei_m_prev1",
  "pdo_m_prev3",
  "npgo_m_prev3",
  "oni_m_prev3",
  "npi_m_prev3",
  "mei_m_prev3",
  "pdo_m_prev6",
  "npgo_m_prev6",
  "oni_m_prev6",
  "npi_m_prev6",
  "mei_m_prev6"
)

# check if there are missing environmental variables in the data to us
missing_env_variables <- setdiff(
  env_variables,
  names(lwa_env)
)

missing_env_variables


# -----------------------------
# 4.2 Prepare the individual-level source data
# -----------------------------

age_source_data <- lwa_env %>%
  filter(
    !is.na(Age_yrs),
    !is.na(Year),
    !is.na(Month)
  ) %>%
  mutate(
    Year = as.integer(Year),
    Month = as.integer(Month),

    date = make_date(
      year = Year,
      month = Month,
      day = 1
    ),

    year_f = factor(Year),
    month_f = factor(
      Month,
      levels = 1:12,
      labels = month.abb
    ),

    Source = factor(Source),
    Sex = factor(Sex)
  ) %>%
  arrange(date)


age_source_data %>%
  mutate(Age_yrs = as.numeric(as.character(Age_yrs))) %>%
  summarise(
    n_fish = n(),
    minimum_age = min(Age_yrs),
    median_age = median(Age_yrs),
    mean_age = mean(Age_yrs),
    maximum_age = max(Age_yrs),
    n_years = n_distinct(Year),
    n_months = n_distinct(date)
  )


# -----------------------------
# 4.3 Check whether environmental values are unique within each month
# -----------------------------

env_uniqueness_check <- age_source_data %>%
  group_by(Year, Month) %>%
  summarise(
    across(
      all_of(env_variables),
      \(x) n_distinct(x[!is.na(x)]),
      .names = "n_{.col}"
    ),
    .groups = "drop"
  )

# each year month combination should have just one unique env variable value. max value should be 1.
env_uniqueness_check %>%
  summarise(
    across(
      starts_with("n_"),
      \(x) max(x, na.rm = TRUE)
    )
  ) %>% as.data.frame()

# -----------------------------
# 4.4 Create monthly mean-age dataset
# -----------------------------

# summarize age by month
monthly_mean_age <- age_source_data %>%
  mutate(Age_yrs = as.numeric(as.character(Age_yrs))) %>%
  group_by(
    Year,
    Month,
    date,
    year_f,
    month_f
  ) %>%
  summarise(
    median_age = median(
      Age_yrs,
      na.rm = TRUE
    ),

    mean_age = mean(
      Age_yrs,
      na.rm = TRUE
    ),

    age_sd = sd(
      Age_yrs,
      na.rm = TRUE
    ),

    age_q25 = quantile(
      Age_yrs,
      probs = 0.25,
      na.rm = TRUE
    ),

    age_q75 = quantile(
      Age_yrs,
      probs = 0.75,
      na.rm = TRUE
    ),

    age_iqr = IQR(
      Age_yrs,
      na.rm = TRUE
    ),

    n_fish = sum(
      !is.na(Age_yrs)
    ),

    n_sources = n_distinct(
      Source[!is.na(Source)]
    ),

    prop_female = mean(
      Sex == "F",
      na.rm = TRUE
    ),

    across(
      all_of(env_variables),
      \(x) {
        values <- unique(x[!is.na(x)])

        if (length(values) == 0L) {
          NA_real_
        } else if (length(values) == 1L) {
          values
        } else {
          stop(
            "More than one environmental value occurred within a year-month."
          )
        }
      }
    ),

    .groups = "drop"
  ) %>%
  arrange(date)

# inspect monthly sample sizes
summary(monthly_mean_age$n_fish)

# determine minimum sample size before modelling
minimum_monthly_sample <- 20

# remove months with less than the minimum_monthly_sample of age estimates
monthly_mean_age <- monthly_mean_age %>%
  filter(n_fish >= minimum_monthly_sample)


# -----------------------------
# 4.5 Standardize the environmental predictors
# -----------------------------

# scale variables and log number of fish
monthly_mean_age <- monthly_mean_age %>%
  mutate(
    across(
      all_of(env_variables),
      \(x) as.numeric(scale(x)),
      .names = "{.col}_z"
    ),
    log_n_fish = log(n_fish),
    time_index = row_number()
  )

# view data
monthly_mean_age %>%
  select(
    date,
    mean_age,
    n_fish,
    all_of(paste0(env_variables, "_z"))
  ) %>%
  glimpse()


# -----------------------------
# 4.6 Plot median and mean sampled age through time
# -----------------------------

# median line plot
ggplot(
  monthly_mean_age,
  aes(
    x = date,
    y = median_age
  )
) +
  geom_line(
    linewidth = 0.5
  ) +
  geom_point(
    aes(size = n_fish),
    alpha = 0.5
  ) +
  scale_size_continuous(
    trans = "sqrt"
  ) +
  labs(
    title = "Monthly median age of sampled fish",
    x = NULL,
    y = "Median age (years)",
    size = "Fish sampled"
  ) +
  theme_classic()

# mean line plot
ggplot(
  monthly_mean_age,
  aes(
    x = date,
    y = mean_age
  )
) +
  geom_line(
    linewidth = 0.5
  ) +
  geom_point(
    aes(size = n_fish),
    alpha = 0.5
  ) +
  scale_size_continuous(
    trans = "sqrt"
  ) +
  labs(
    title = "Monthly mean age of sampled fish",
    x = NULL,
    y = "Mean age (years)",
    size = "Fish sampled"
  ) +
  theme_classic()

# mean box plot
ggplot(
  monthly_mean_age,
  aes(
    x = month_f,
    y = mean_age
  )
) +
  geom_boxplot(
    outlier.alpha = 0.3
  ) +
  labs(
    title = "Seasonal variation in sampled age composition",
    x = "Month",
    y = "Mean age (years)"
  ) +
  theme_classic()

# mean age accross sample size
ggplot(
  monthly_mean_age,
  aes(
    x = log_n_fish,
    y = mean_age
  )
) +
  geom_point(
    alpha = 0.5
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE
  ) +
  labs(
    x = "Log monthly sample size",
    y = "Mean age (years)"
  ) +
  theme_classic()

# mean age by source
ggplot(
  monthly_mean_age,
  aes(
    x = n_sources,
    y = mean_age
  )
) +
  geom_point(
    alpha = 0.5
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE
  ) +
  labs(
    x = "Number of sources represented",
    y = "Mean age (years)"
  ) +
  theme_classic()


# -----------------------------
# 4.7 Fit a monthly null model
# -----------------------------

# monthly mean age
monthly_mean_age_null_model <- brm(
  formula = bf(
    mean_age ~
      month_f +
      s(time_index, k = 10) +
      log_n_fish +
      n_sources +
      (1 | year_f)
  ),

  data = monthly_mean_age,

  family = student(),

  prior = c(
    prior(
      normal(0, 1),
      class = "b"
    ),

    prior(
      normal(8, 3),
      class = "Intercept"
    ),

    prior(
      exponential(1),
      class = "sd"
    ),

    prior(
      exponential(1),
      class = "sigma"
    ),

    prior(
      gamma(2, 0.1),
      class = "nu"
    )
  ),

  chains = 4,
  cores = 4,
  iter = 4000,
  warmup = 2000,

  seed = 123,
  backend = "cmdstanr",

  control = list(
    adapt_delta = 0.97,
    max_treedepth = 13
  ),

  save_pars = save_pars(
    all = TRUE
  ),

  file = "stock-specific/hake/model-outputs/monthly_mean_age_null_model",
  file_refit = "on_change"
)


# --------------------------------------------------
# 4.8 Basic model summaries
# --------------------------------------------------

summary(monthly_mean_age_null_model)

# Useful compact fixed-effect tables
fixef(monthly_mean_age_null_model)

# Random-effect SDs and other variance parameters
VarCorr(monthly_mean_age_null_model)


# --------------------------------------------------
# 4.9 model output diagnostics
# --------------------------------------------------

library(brms)
library(bayesplot)
library(posterior)
library(dplyr)
library(ggplot2)
library(tidyr)

check_nuts <- function(model) {

  np <- nuts_params(model)

  cat("\nDIVERGENCES\n")

  print(
    np %>%
      filter(Parameter == "divergent__") %>%
      count(Value)
  )

  cat("\nTREE DEPTH\n")

  print(
    np %>%
      filter(Parameter == "treedepth__") %>%
      count(Value)
  )

  cat("\nLEAPFROG STEPS\n")

  print(
    np %>%
      filter(Parameter == "n_leapfrog__") %>%
      summarise(
        min = min(Value),
        median = median(Value),
        mean = mean(Value),
        max = max(Value)
      )
  )

  invisible(np)
}

# diagnostics - want zero divergences, and minimal iterations at max tree depth
nuts_mean <- check_nuts(
  monthly_mean_age_null_model
)

# check how many iterations were at max tree depth
nuts_mean %>%
  filter(
    Parameter == "treedepth__",
    Value == 13
  )

# proportion of iterations at max tree depth
treedepth_rate <- function(model, max_depth = 13) {

  nuts_params(model) %>%
    filter(Parameter == "treedepth__") %>%
    summarise(
      n = n(),
      n_at_max = sum(Value >= max_depth),
      proportion_at_max = mean(Value >= max_depth)
    )
}

treedepth_rate(
  monthly_mean_age_null_model
)

# chheck E-BFMI - plots compare distribution
bayesplot::mcmc_nuts_energy(
  nuts_params(monthly_mean_age_null_model)
)

# traceplots
plot(monthly_mean_age_null_model)

draws_mean <- as_draws_df(
  monthly_mean_age_null_model
)

mcmc_trace(
  draws_mean,
  pars = c(
    "b_Intercept",
    "sigma",
    "nu"
  )
)

# -----------------------------
# 4.10 posterior predictive checks
# -----------------------------

pp_check(
  monthly_mean_age_null_model,
  ndraws = 100,
  type = "dens_overlay"
)

pp_check(
  monthly_mean_age_null_model,
  type = "scatter_avg",
  ndraws = 100
)

pp_check(
  monthly_mean_age_null_model,
  type = "stat",
  stat = "max",
  ndraws = 100
)

pp_check(
  monthly_mean_age_null_model,
  type = "stat",
  stat = "sd",
  ndraws = 100
)

# -----------------------------
# 4.11 Smooth temporal trend
# -----------------------------

conditional_smooths(
  monthly_mean_age_null_model
)

plot(
  conditional_smooths(
    monthly_mean_age_null_model
  )
)

smooth_mean <- conditional_smooths(
  monthly_mean_age_null_model
)

smooth_mean

# inspect month effects
conditional_effects(
  monthly_mean_age_null_model,
  effects = "month_f"
)

# sample size effects
plot(
  conditional_effects(
    monthly_mean_age_null_model,
    effects = "log_n_fish"
  )
)

plot(
  conditional_effects(
    monthly_mean_age_null_model,
    effects = "n_sources"
  )
)


# -----------------------------
# 4.12 fitted versus observed value
# -----------------------------
pred_mean <- fitted(
  monthly_mean_age_null_model,
  summary = TRUE
)

mean_check <- monthly_mean_age %>%
  mutate(
    fitted = pred_mean[, "Estimate"],
    fitted_lower = pred_mean[, "Q2.5"],
    fitted_upper = pred_mean[, "Q97.5"]
  )

ggplot(
  mean_check,
  aes(
    x = fitted,
    y = mean_age
  )
) +
  geom_point(alpha = 0.5) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed"
  ) +
  labs(
    x = "Fitted mean age",
    y = "Observed mean age"
  ) +
  theme_classic()


# -----------------------------
# 4.13 residual structure
# -----------------------------

mean_check <- mean_check %>%
  mutate(
    residual = mean_age - fitted
  )

ggplot(
  mean_check,
  aes(
    x = time_index,
    y = residual
  )
) +
  geom_point(alpha = 0.5) +
  geom_smooth(
    method = "gam",
    formula = y ~ s(x)
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  labs(
    x = "Time",
    y = "Observed - fitted mean age"
  ) +
  theme_classic()

# residuals by month
ggplot(
  mean_check,
  aes(
    x = month_f,
    y = residual
  )
) +
  geom_boxplot() +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  theme_classic()




##### -------------------------------------------------------------------- #####
##### -------------------------------------------------------------------- #####
##### -------------------------------------------------------------------- #####
##### -------------------------------------------------------------------- #####
##### -------------------------------------------------------------------- #####

# # Annual mean age with which env variables? Might have to remove this section
#
# # -----------------------------
# # 5.1 Fit an annual mean age model
# # -----------------------------
#
# annual_mean_age <- age_source_data %>%
#   mutate(Age_yrs = as.numeric(as.character(Age_yrs))) %>%
#   group_by(
#     Year,
#     year_f
#   ) %>%
#   summarise(
#     median_age = median(
#       Age_yrs,
#       na.rm = TRUE
#     ),
#
#     mean_age = mean(
#       Age_yrs,
#       na.rm = TRUE
#     ),
#
#     age_sd = sd(
#       Age_yrs,
#       na.rm = TRUE
#     ),
#
#     age_iqr = IQR(
#       Age_yrs,
#       na.rm = TRUE
#     ),
#
#     n_fish = sum(
#       !is.na(Age_yrs)
#     ),
#
#     n_months = n_distinct(
#       Month
#     ),
#
#     n_sources = n_distinct(
#       Source[!is.na(Source)]
#     ),
#
#     across(
#       all_of(env_variables),
#       \(x) mean(
#         x,
#         na.rm = TRUE
#       ),
#       .names = "{.col}_annual"
#     ),
#
#     .groups = "drop"
#   ) %>%
#   arrange(Year) %>%
#   mutate(
#     across(
#       ends_with("_annual"),
#       \(x) as.numeric(scale(x)),
#       .names = "{.col}_z"
#     ),
#
#     year_z = as.numeric(
#       scale(Year)
#     ),
#
#     log_n_fish = log(n_fish)
#   )
#
# # plot annual age over time
# ggplot(
#   annual_mean_age,
#   aes(
#     x = Year,
#     y = mean_age
#   )
# ) +
#   geom_line() +
#   geom_point(
#     aes(size = n_fish),
#     alpha = 0.6
#   ) +
#   labs(
#     title = "Annual mean age of sampled fish",
#     x = "Year",
#     y = "Mean age (years)",
#     size = "Fish sampled"
#   ) +
#   theme_classic()
#
# # select annual env variables to use
# annual_env_variables <- c(
#   "pdo_m_lag1_annual_z",
#   "npgo_m_lag1_annual_z",
#   "oni_m_lag1_annual_z",
#   "npi_m_lag1_annual_z",
#
#   "pdo_m_prev3_annual_z",
#   "npgo_m_prev3_annual_z",
#   "oni_m_prev3_annual_z",
#   "npi_m_prev3_annual_z",
#
#   "pdo_m_prev6_annual_z",
#   "npgo_m_prev6_annual_z",
#   "oni_m_prev6_annual_z",
#   "npi_m_prev6_annual_z"
# )
#
# setdiff(
#   annual_env_variables,
#   names(annual_median_age)
# )
#
# annual_age_all_env <- annual_median_age %>%
#   mutate(
#     Year = as.numeric(Year),
#
#     year_z = as.numeric(
#       scale(Year)
#     ),
#
#     log_n_fish = log(n_fish)
#   ) %>%
#   filter(
#     if_all(
#       all_of(
#         c(
#           "median_age",
#           "mean_age",
#           "year_z",
#           "log_n_fish",
#           "n_months",
#           "n_sources",
#           annual_env_variables
#         )
#       ),
#       ~ !is.na(.x)
#     )
#   ) %>%
#   arrange(Year)
#
#
#
# # check usable years
# annual_age_all_env %>%
#   summarise(
#     n_years = n(),
#     first_year = min(Year),
#     last_year = max(Year),
#     median_age_min = min(median_age),
#     median_age_max = max(median_age),
#     mean_age_min = min(mean_age),
#     mean_age_max = max(mean_age)
#   )
#
# cat(
#   "Annual observations:",
#   nrow(annual_age_all_env),
#   "\nEnvironmental predictors:",
#   length(annual_env_variables),
#   "\n"
# )
#
# # correlations among env predictors
# annual_env_correlation <- annual_age_all_env %>%
#   select(
#     all_of(annual_env_variables)
#   ) %>%
#   cor(
#     use = "pairwise.complete.obs"
#   )
#
# round(
#   annual_env_correlation,
#   2
# )
#
# ## annual pdo age model
# annual_pdo_age_model <- brm(
#   formula = bf(
#     median_age ~
#       pdo_m_prev6_annual_z +
#       s(Year, k = 8) +
#       log_n_fish +
#       n_months +
#       n_sources
#   ),
#
#   data = annual_median_age,
#
#   family = student(),
#
#   prior = c(
#     prior(
#       normal(0, 1),
#       class = "b"
#     ),
#
#     prior(
#       normal(8, 3),
#       class = "Intercept"
#     ),
#
#     prior(
#       exponential(1),
#       class = "sigma"
#     ),
#
#     prior(
#       gamma(2, 0.1),
#       class = "nu"
#     )
#   ),
#
#   chains = 4,
#   cores = 4,
#   iter = 4000,
#   warmup = 2000,
#
#   backend = "cmdstanr",
#   seed = 123,
#
#   control = list(
#     adapt_delta = 0.97,
#     max_treedepth = 13
#   )
# )
#
#
# grep(
#   "^b_",
#   variables(annual_pdo_age_model),
#   value = TRUE
# )
#
# pdo_coefficient <- "b_pdo_m_prev6_annual_z"
#
# annual_pdo_draws <- as_draws_df(
#   annual_pdo_age_model
# ) %>%
#   transmute(
#     beta_pdo = .data[[pdo_coefficient]]
#   )
#
#
# annual_pdo_summary <- annual_pdo_draws %>%
#   summarise(
#     median_beta = median(beta_pdo),
#     lower_95 = quantile(beta_pdo, 0.025),
#     upper_95 = quantile(beta_pdo, 0.975),
#     probability_positive = mean(beta_pdo > 0),
#     probability_negative = mean(beta_pdo < 0)
#   )
#
# annual_pdo_summary
#
#
# annual_pdo_posterior_plot <- ggplot(
#   annual_pdo_draws,
#   aes(
#     x = beta_pdo,
#     y = "PDO"
#   )
# ) +
#   stat_halfeye(
#     .width = c(0.50, 0.80, 0.95),
#     point_interval = median_qi,
#     normalize = "groups"
#   ) +
#   geom_vline(
#     xintercept = 0,
#     linetype = "dashed",
#     linewidth = 0.6
#   ) +
#   labs(
#     title = "Posterior effect of PDO on annual median age",
#     subtitle = paste(
#       "Effect associated with a 1-SD increase in annual PDO;",
#       "intervals show 50%, 80%, and 95% credible intervals"
#     ),
#     x = "Change in annual median age (years)",
#     y = NULL
#   ) +
#   theme_classic(base_size = 12)
#
# annual_pdo_posterior_plot



##### -------------------------------------------------------------------- #####
##### -------------------------------------------------------------------- #####
##### -------------------------------------------------------------------- #####
##### -------------------------------------------------------------------- #####
##### -------------------------------------------------------------------- #####

# Fit an annual median age age model for each env variable

# -----------------------------
# 5.1 Prep data
# -----------------------------


# Will need to do this for seasonal covariates (as opposed to annual below)
# annual_env_variables <- c(
#   "pdo_m_lag1_annual_z",
#   "npgo_m_lag1_annual_z",
#   "oni_m_lag1_annual_z",
#   "npi_m_lag1_annual_z",
#
#   "pdo_m_prev3_annual_z",
#   "npgo_m_prev3_annual_z",
#   "oni_m_prev3_annual_z",
#   "npi_m_prev3_annual_z",
#
#   "pdo_m_prev6_annual_z",
#   "npgo_m_prev6_annual_z",
#   "oni_m_prev6_annual_z",
#   "npi_m_prev6_annual_z"
# )
#
# setdiff(
#   annual_env_variables,
#   names(annual_median_age)
# )


# function to summarize covariates annually - currently doing the three month season Apr-Jun
format_covariate <- function(dat, value.col, name) {
  dat %>%
    as_tibble()  %>%
    filter(month %in% c(4,5,6)) %>% # only spring months
    group_by(year) %>%
    summarize(value = mean(.data[[value.col]])) %>%
    mutate(covariate = name)
}

env_vars <- c("pdo", "npgo", "oni", "bi", "npi", "mei")


# Get environmental covariates
# annual for prior year and current year
env_pdo <- format_covariate(pdo, "anomaly", "pdo")
env_npgo <- format_covariate(npgo, "anomaly", "npgo")
env_oni <- format_covariate(oni, "anomaly", "oni")
env_mei <- format_covariate(mei, "anomaly", "mei")
env_npi <- format_covariate(npi_monthly, "value", "npi")

env_bi <- pacea::bi %>%
  mutate(value = anomaly,
         covariate = "bi")  %>%
  select(year, value, covariate)
# env_npi <- pacea::npi_annual %>% # annual npi covariate
#   mutate(value = anomaly,
#          covariate = "npi")  %>%
#   select(year, value, covariate)

env_long <- bind_rows(env_pdo, env_npgo, env_oni, env_bi, env_npi, env_mei)

env_wide <- env_long %>%
  pivot_wider(names_from = covariate, values_from = value)


# filter only canada or us data
filter_index <- unique(lwa_cond$Source)
filter_index_can <- c("CAN_acoustic", "CAN_shoreside", "CAN_JV","CAN_polish", "Acoustic Canada")
filter_index_us <- c("US_foreign", "US_JV", "US_shore", "US_acoustic", "US_atsea", "Acoustic U.S.")

lwa_cond_can <- lwa_cond %>%
  filter(Source %in% filter_index_can)

lwa_cond_us <- lwa_cond %>%
  filter(Source %in% filter_index_us)


## select which option
selected <- "all"
# selected <- "us"
# selected <- "can"

if(selected == "all"){
  lwa_env <- lwa_cond

} else {
  if(selected == "us"){
    lwa_env <- lwa_cond_us

  } else {
    if(selected == "can"){
      lwa_env <- lwa_cond_can

    } else {
      rm(lwa_env)
    }
  }
}

# join env data with condition data
lwa_env <- lwa_env %>%
  mutate(
    year = as.integer(as.character(year)),
    month = as.integer(as.character(month))
  ) %>%
  left_join(env_wide, by = c("year")) %>%
  mutate(
    year_f = factor(year)
  )

# Scale environmental covariates
lwa_env <- lwa_env %>%
  mutate(across(all_of(env_vars), scale))

# copy to original
lwa_env_original <- lwa_env

annual_env_column_names <- c(
  pdo = "pdo",
  npgo = "npgo",
  oni = "oni",
  bi = "bi",
  npi = "npi",
  mei = "mei"
)

# should be nothing
setdiff(
  unname(annual_env_column_names),
  names(lwa_env)
)

# create new dataframe of annual env variables - will be similar to env_wide dataframe
annual_environment <- lwa_env %>%
  transmute(
    Year = as.integer(as.character(Year)),

    pdo_annual_same =
      .data[[annual_env_column_names[["pdo"]]]],

    npgo_annual_same =
      .data[[annual_env_column_names[["npgo"]]]],

    oni_annual_same =
      .data[[annual_env_column_names[["oni"]]]],

    bi_annual_same =
      .data[[annual_env_column_names[["bi"]]]],

    npi_annual_same =
      .data[[annual_env_column_names[["npi"]]]],

    mei_annual_same =
      .data[[annual_env_column_names[["mei"]]]]
  ) %>%
  group_by(Year) %>%
  summarise(
    across(
      ends_with("_same"),
      \(x) {
        values <- unique(x[!is.na(x)])

        if (length(values) == 0L) {
          return(NA_real_)
        }

        if (length(values) > 1L) {
          stop(
            paste0(
              "More than one annual environmental value was found in ",
              "Year ",
              first(Year),
              "."
            ),
            call. = FALSE
          )
        }

        values
      }
    ),

    .groups = "drop"
  ) %>%
  arrange(Year)

# create lag of 1 year for env variables
annual_environment_prior <- annual_environment %>%
  transmute(
    Year = Year + 1L,

    pdo_annual_prior = pdo_annual_same,
    npgo_annual_prior = npgo_annual_same,
    oni_annual_prior = oni_annual_same,
    bi_annual_prior = bi_annual_same,
    npi_annual_prior = npi_annual_same,
    mei_annual_prior = mei_annual_same
  )

# join lagged and current anv variables
annual_environment_paired <- annual_environment %>%
  left_join(
    annual_environment_prior,
    by = "Year"
  ) %>%
  arrange(Year)

# names of env variables
annual_env_variables <- c(
  "pdo_annual_same",
  "pdo_annual_prior",
  "npgo_annual_same",
  "npgo_annual_prior",
  "oni_annual_same",
  "oni_annual_prior",
  "bi_annual_same",
  "bi_annual_prior",
  "npi_annual_same",
  "npi_annual_prior",
  "mei_annual_same",
  "mei_annual_prior"
)

# bothe same year and prior year env variables
env_variables <- annual_env_variables

# new lwa_env dataframe with env variables (prior and current year)
lwa_env <- lwa_env %>%
  mutate(
    Year = as.integer(as.character(Year))
  ) %>%
  select(
    -any_of(annual_env_variables)
  ) %>%
  left_join(
    annual_environment_paired,
    by = "Year"
  )

# scale all env variables
lwa_env <- lwa_env %>%
  mutate(
    across(
      all_of(env_variables),
      \(x) as.numeric(scale(x)),
      .names = "{.col}_z"
    )
  )

# summarise mean ages by year
# maybe should exclude certain months (e.g. winter months)
annual_mean_age <- lwa_env %>%
  filter(Month %in% c(4:10)) %>%
  group_by(Year) %>%
  mutate(Age_yrs = as.numeric(as.character(Age_yrs))) %>%
  summarise(
    median_age = median(
      Age_yrs,
      na.rm = TRUE
    ),

    mean_age = mean(
      Age_yrs,
      na.rm = TRUE
    ),

    age_sd = sd(
      Age_yrs,
      na.rm = TRUE
    ),

    age_iqr = IQR(
      Age_yrs,
      na.rm = TRUE
    ),

    n_fish = sum(
      !is.na(Age_yrs)
    ),

    n_months = n_distinct(
      Month[!is.na(Month)]
    ),

    n_sources = n_distinct(
      Source[!is.na(Source)]
    ),

    across(
      all_of(env_variables),
      \(x) {
        values <- unique(x[!is.na(x)])

        if (length(values) == 0L) {
          NA_real_
        } else if (length(values) == 1L) {
          values
        } else {
          stop(
            paste0(
              "More than one annual value occurred in Year ",
              first(Year),
              "."
            ),
            call. = FALSE
          )
        }
      }
    ),

    .groups = "drop"
  ) %>%
  arrange(Year) %>%
  mutate(
    across(
      all_of(env_variables),
      \(x) as.numeric(scale(x)),
      .names = "{.col}_z"
    ),

    year_z = as.numeric(
      scale(Year)
    ),

    log_n_fish = log(n_fish)
  )



# -----------------------------
# 5.2 prepare dataset
# -----------------------------

annual_age_env <- annual_mean_age %>%
  mutate(
    Year = as.numeric(Year),

    year_z = as.numeric(
      scale(Year)
    ),

    log_n_fish = log(n_fish)
  ) %>%
  arrange(Year)


# correlation plots
annual_mean_age %>%
  select(mean_age, pdo_annual_same, npgo_annual_same, oni_annual_same, bi_annual_same, npi_annual_same, mei_annual_same) %>%
  ggpairs()

annual_mean_age %>%
  select(mean_age, pdo_annual_prior, npgo_annual_prior, oni_annual_prior, bi_annual_prior, npi_annual_prior, mei_annual_prior) %>%
  ggpairs()

# -----------------------------
# 5.3 function to fit annual environmental model
# -----------------------------

fit_annual_age_env_model <- function(
    data,
    env_variable,
    output_directory = "stock-specific/hake/output-models/annual_age_env",
    seed = 123
) {

  required_variables <- c(
    "mean_age",
    "year_z",
    "log_n_fish",
    "n_months",
    "n_sources",
    env_variable
  )

  missing_variables <- setdiff(
    required_variables,
    names(data)
  )

  if (length(missing_variables) > 0L) {
    stop(
      "Missing variables: ",
      paste(missing_variables, collapse = ", "),
      call. = FALSE
    )
  }

  model_data <- data %>%
    filter(
      if_all(
        all_of(required_variables),
        ~ !is.na(.x)
      )
    )

  model_formula <- bf(
    as.formula(
      paste(
        # "median_age ~",
        "mean_age ~",

        env_variable,
        "+ year_z",
        "+ log_n_fish",
        "+ n_months",
        "+ n_sources"
      )
    )
  )

  dir.create(
    output_directory,
    recursive = TRUE,
    showWarnings = FALSE
  )

  brm(
    formula = model_formula,

    data = model_data,

    family = student(),

    prior = c(
      prior(
        normal(0, 0.5),
        class = "b"
      ),

      prior(
        normal(8, 3),
        class = "Intercept"
      ),

      prior(
        exponential(1),
        class = "sigma"
      ),

      prior(
        gamma(2, 0.1),
        class = "nu"
      )
    ),

    chains = 4,
    cores = 4,
    iter = 4000,
    warmup = 2000,

    backend = "cmdstanr",
    seed = seed,

    control = list(
      adapt_delta = 0.97,
      max_treedepth = 13
    ),

    save_pars = save_pars(
      all = TRUE
    ),

    file = file.path(
      output_directory,
      paste0(
        "annual_mean_age_",

        env_variable
      )
    ),

    file_refit = "on_change",

    refresh = 500
  )
}


# -----------------------------
# 5.4 fit each env variable separately
# -----------------------------

# run model
annual_age_env_models <- annual_env_variables %>%
  set_names() %>%
  map(
    \(env_variable) {
      fit_annual_age_env_model(
        data = annual_age_env,
        env_variable = env_variable
      )
    }
  )


# check model diagnostics
annual_age_env_diagnostics <- imap_dfr(
  annual_age_env_models,
  \(model, variable) {

    # Posterior parameter draws
    draws <- posterior::as_draws_array(model)

    # Parameter-level convergence diagnostics
    diagnostics <- posterior::summarise_draws(
      draws,
      "rhat",
      "ess_bulk",
      "ess_tail"
    )

    # NUTS sampler diagnostics
    nuts <- brms::nuts_params(model)

    tibble(
      covariate = variable,

      maximum_rhat = max(
        diagnostics$rhat,
        na.rm = TRUE
      ),

      minimum_bulk_ess = min(
        diagnostics$ess_bulk,
        na.rm = TRUE
      ),

      minimum_tail_ess = min(
        diagnostics$ess_tail,
        na.rm = TRUE
      ),

      divergences = nuts %>%
        filter(Parameter == "divergent__") %>%
        summarise(
          n = sum(Value)
        ) %>%
        pull(n)
    )
  }
)

annual_age_env_diagnostics

# -----------------------------
# 5.5 extract possterior from each model
# -----------------------------

# function to extract across all models
extract_annual_env_draws <- function(
    model,
    env_variable
) {

  coefficient_name <- paste0(
    "b_",
    env_variable
  )

  model_draws <- as_draws_df(model)

  if (!coefficient_name %in% names(model_draws)) {
    stop(
      "Coefficient not found: ",
      coefficient_name,
      call. = FALSE
    )
  }

  tibble(
    covariate = env_variable,
    beta = model_draws[[coefficient_name]]
  )
}

# run function
annual_age_env_draws <- imap_dfr(
  annual_age_env_models,
  \(model, variable) {
    extract_annual_env_draws(
      model = model,
      env_variable = variable
    )
  }
)

# add labels for plotting
annual_age_env_draws <- annual_age_env_draws %>%
  mutate(
    index = case_when(
      grepl("^pdo", covariate)  ~ "PDO",
      grepl("^npgo", covariate) ~ "NPGO",
      grepl("^oni", covariate)  ~ "ONI",
      grepl("^bi", covariate)  ~ "BI",
      grepl("^npi", covariate)  ~ "NPI",
      grepl("^mei", covariate)  ~ "MEI",
      TRUE                      ~ covariate
    ),

    exposure_window = case_when(
      grepl("_same", covariate) ~
        "Same year",

      grepl("_prior", covariate) ~
        "Previous year",

      TRUE ~ "Other"
    ),

    index = factor(
      index,
      levels = c(
        "PDO",
        "NPGO",
        "ONI",
        "BI",
        "NPI",
        "MEI"
      )
    ),

    exposure_window = factor(
      exposure_window,
      levels = c(
        "Same year",
        "Previous year"
      )
    )
  )

# -----------------------------
# 5.6 summarise posterior coefficints
# -----------------------------

annual_age_env_summary <- annual_age_env_draws %>%
  group_by(
    covariate,
    index,
    exposure_window
  ) %>%
  summarise(
    median_beta = median(beta),

    mean_beta = mean(beta),

    lower_beta = quantile(
      beta,
      0.025
    ),

    upper_beta = quantile(
      beta,
      0.975
    ),

    probability_positive = mean(
      beta > 0
    ),

    probability_negative = mean(
      beta < 0
    ),

    probability_abs_gt_0_25 = mean(
      abs(beta) > 0.25
    ),

    probability_abs_gt_0_50 = mean(
      abs(beta) > 0.50
    ),

    .groups = "drop"
  )

# table of posterior coefficient estimates for each env variables
annual_age_env_summary

# -----------------------------
# 5.7 plot all posterior coefficient distributions
# -----------------------------

# plot and save in object
annual_age_env_posterior_plot <- ggplot(
  annual_age_env_draws,
  aes(
    x = beta,
    y = exposure_window
  )
) +
  stat_halfeye(
    .width = c(
      0.50,
      0.80,
      0.95
    ),
    point_interval = "mean_qi",
    normalize = "groups"
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    linewidth = 0.6
  ) +
  facet_wrap(
    vars(index),
    ncol = 2,
    scales = "free_x"
  ) +
  labs(
    title = "Environmental associations with annual mean age",
    subtitle = paste(
      "Each environmental variable was assessed",
      "in a separate model"
    ),
    x = "Change in annual mean age per 1-SD increase",
    y = "Environmental exposure window"
  ) +
  theme_classic(base_size = 12) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(
      face = "bold"
    )
  )

annual_age_env_posterior_plot

# -----------------------------
# 5.8 forest plot of all posteriors
# -----------------------------

annual_age_env_forest_data <- annual_age_env_summary %>%
  mutate(
    covariate_label = paste(
      index,
      exposure_window,
      sep = ": "
    ),

    covariate_label = factor(
      covariate_label,
      levels = rev(
        unique(covariate_label)
      )
    )
  )


annual_age_env_forest_plot <- ggplot(
  annual_age_env_forest_data,
  aes(
    x = mean_beta,
    y = covariate_label
  )
) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    linewidth = 0.6
  ) +
  geom_errorbarh(
    aes(
      xmin = lower_beta,
      xmax = upper_beta
    ),
    height = 0
  ) +
  geom_point(
    size = 2.5
  ) +
  labs(
    title = "Posterior environmental effects on annual mean age",
    subtitle = "Points are means; lines are 95% credible intervals",
    x = "Change in annual mean age (years)",
    y = NULL
  ) +
  theme_classic(base_size = 12)

annual_age_env_forest_plot


# -----------------------------
# 5.9 environmental effects plot - data organizing
# -----------------------------

get_model_data <- function(
    model,
    env_variable,
    source_data = annual_age_env
) {

  required_variables <- c(
    "Year",
    "mean_age",
    "n_fish",
    "year_z",
    "log_n_fish",
    "n_months",
    "n_sources",
    env_variable
  )

  missing_variables <- setdiff(
    required_variables,
    names(source_data)
  )

  if (length(missing_variables) > 0L) {
    stop(
      "Missing variables in source_data: ",
      paste(missing_variables, collapse = ", "),
      call. = FALSE
    )
  }

  source_data %>%
    filter(
      if_all(
        all_of(required_variables),
        ~ !is.na(.x)
      )
    ) %>%
    arrange(Year)
}

get_annual_env_effect <- function(
    model,
    env_variable,
    source_data = annual_age_env,
    prediction_length = 100
) {

  model_data <- get_model_data(
    model = model,
    env_variable = env_variable,
    source_data = source_data
  )

  prediction_data <- tibble(
    env_value = seq(
      min(
        model_data[[env_variable]],
        na.rm = TRUE
      ),
      max(
        model_data[[env_variable]],
        na.rm = TRUE
      ),
      length.out = prediction_length
    ),

    year_z = 0,

    log_n_fish = median(
      model_data$log_n_fish,
      na.rm = TRUE
    ),

    n_months = median(
      model_data$n_months,
      na.rm = TRUE
    ),

    n_sources = median(
      model_data$n_sources,
      na.rm = TRUE
    )
  )

  prediction_data[[env_variable]] <-
    prediction_data$env_value

  fitted_summary <- fitted(
    model,
    newdata = prediction_data,
    re_formula = NA,
    probs = c(0.025, 0.975)
  ) %>%
    as_tibble()

  bind_cols(
    prediction_data,
    fitted_summary
  ) %>%
    mutate(
      covariate = env_variable
    )
}

# -----------------------------
# 5.10 function to plot one env relationship
# -----------------------------

plot_annual_age_env_effect <- function(
    model,
    env_variable,
    source_data = annual_age_env
) {

  model_data <- get_model_data(
    model = model,
    env_variable = env_variable,
    source_data = source_data
  )

  effect_data <- get_annual_env_effect(
    model = model,
    env_variable = env_variable,
    source_data = source_data
  )

  index_label <- case_when(
    grepl("^pdo", env_variable)  ~ "PDO",
    grepl("^npgo", env_variable) ~ "NPGO",
    grepl("^oni", env_variable)  ~ "ONI",
    grepl("^npi", env_variable)  ~ "NPI",
    TRUE                         ~ env_variable
  )

  timing_label <- case_when(
    grepl("_same", env_variable)  ~ "Same year",
    grepl("_prior", env_variable) ~ "Previous year",
    TRUE                          ~ ""
  )

  ggplot(
    effect_data,
    aes(
      x = env_value,
      y = Estimate
    )
  ) +
    geom_ribbon(
      aes(
        ymin = Q2.5,
        ymax = Q97.5
      ),
      alpha = 0.20,
      linewidth = 0
    ) +
    geom_line(
      linewidth = 0.9
    ) +
    geom_point(
      data = model_data,
      aes(
        x = .data[[env_variable]],
        y = mean_age,
        size = n_fish
      ),
      inherit.aes = FALSE,
      alpha = 0.50
    ) +
    geom_rug(
      data = model_data,
      aes(
        x = .data[[env_variable]]
      ),
      inherit.aes = FALSE,
      sides = "b",
      alpha = 0.4
    ) +
    scale_size_continuous(
      trans = "sqrt"
    ) +
    labs(
      title = paste(
        "Annual mean age and",
        index_label
      ),
      subtitle = timing_label,
      x = index_label,
      y = "Predicted annual mean age (years)",
      size = "Fish sampled"
    ) +
    theme_classic(base_size = 12)
}


# -----------------------------
# 5.11 effects plot for each env variable
# -----------------------------

annual_age_env_effect_plots <- imap(
  annual_age_env_models,
  \(model, variable) {
    plot_annual_age_env_effect(
      model = model,
      env_variable = variable,
      source_data = annual_age_env
    )
  }
)

# display all variables in separate plots
walk(
  annual_age_env_effect_plots,
  print
)

# all variables in one plot
annual_age_env_effect_combined <-
  wrap_plots(
    annual_age_env_effect_plots,
    ncol = 2,
    guides = "collect"
  ) &
  theme(
    legend.position = "bottom"
  )

# show single plot
annual_age_env_effect_combined


# -----------------------------
# 5.12 posterior predictions and model evaluation
# -----------------------------

get_annual_fitted_values <- function(
    model,
    env_variable
) {

  model_data <- get_model_data(model = model,
                               env_variable = env_variable)

  fitted_values <- fitted(
    model,
    newdata = model_data,
    re_formula = NA,
    probs = c(
      0.025,
      0.975
    )
  ) %>%
    as_tibble()

  bind_cols(
    model_data %>%
      select(
        Year,
        mean_age,
        n_fish
      ),
    fitted_values
  ) %>%
    mutate(
      covariate = env_variable
    )
}

# table of posterior values with error
annual_age_env_fitted <- imap_dfr(
  annual_age_env_models,
  \(model, variable) {
    get_annual_fitted_values(
      model = model,
      env_variable = variable
    )
  }
)

# calculate residuals
annual_age_env_fitted <- annual_age_env_fitted %>%
  mutate(
    residual = mean_age - Estimate
  )

# plots of observed vs fitted through time
ggplot(
  annual_age_env_fitted,
  aes(x = Year)
) +
  geom_ribbon(
    aes(
      ymin = Q2.5,
      ymax = Q97.5
    ),
    alpha = 0.2
  ) +
  geom_line(
    aes(y = Estimate),
    linewidth = 0.8
  ) +
  geom_point(
    aes(y = mean_age),
    size = 1.5
  ) +
  facet_wrap(
    ~ covariate,
    ncol = 2
  ) +
  labs(
    x = "Year",
    y = "Mean age",
    title = "Observed and fitted annual mean age"
  ) +
  theme_classic()

# plot residuals through time
ggplot(
  annual_age_env_fitted,
  aes(
    x = Year,
    y = residual
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  geom_point() +
  geom_smooth(
    method = "loess",
    se = FALSE
  ) +
  facet_wrap(
    ~ covariate,
    ncol = 2
  ) +
  labs(
    x = "Year",
    y = "Observed - fitted mean age",
    title = "Residual patterns through time"
  ) +
  theme_classic()


# observed vs fitted
ggplot(
  annual_age_env_fitted,
  aes(
    x = Estimate,
    y = mean_age
  )
) +
  geom_point() +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed"
  ) +
  facet_wrap(
    ~ covariate,
    ncol = 2
  ) +
  labs(
    x = "Fitted mean age",
    y = "Observed mean age",
    title = "Observed versus fitted annual mean age"
  ) +
  theme_classic()

# rmse and mean absolute error
annual_age_env_fit_summary <- annual_age_env_fitted %>%
  mutate(
    residual = mean_age - Estimate
  ) %>%
  group_by(covariate) %>%
  summarise(
    n = n(),

    RMSE = sqrt(
      mean(residual^2, na.rm = TRUE)
    ),

    MAE = mean(
      abs(residual),
      na.rm = TRUE
    ),

    bias = mean(
      residual,
      na.rm = TRUE
    ),

    .groups = "drop"
  ) %>%
  arrange(RMSE)

annual_age_env_fit_summary

# -----------------------------
# 5.13 function to plot observed vs fitted ages over time
# -----------------------------


plot_annual_fitted <- function(
    model,
    env_variable
) {

  fitted_data <- get_annual_fitted_values(
    model = model,
    env_variable = env_variable
  )

  plot_title <- gsub(
    "_annual_z$",
    "",
    env_variable
  )

  ggplot(
    fitted_data,
    aes(x = Year)
  ) +
    geom_ribbon(
      aes(
        ymin = Q2.5,
        ymax = Q97.5
      ),
      alpha = 0.20
    ) +
    geom_line(
      aes(
        y = Estimate
      ),
      linewidth = 0.9
    ) +
    geom_point(
      aes(
        y = mean_age,
        size = n_fish
      ),
      alpha = 0.65
    ) +
    scale_size_continuous(
      trans = "sqrt"
    ) +
    labs(
      title = paste(
        "Observed and fitted annual mean age:",
        plot_title
      ),
      subtitle = paste(
        "Line is the posterior expected value;",
        "ribbon is the 95% credible interval"
      ),
      x = "Year",
      y = "Annual mean age (years)",
      size = "Fish sampled"
    ) +
    theme_classic(base_size = 12)
}

annual_age_env_fitted_plots <- imap(
  annual_age_env_models,
  \(model, variable) {
    plot_annual_fitted(
      model = model,
      env_variable = variable
    )
  }
)

# only need one plot to see, but worth it to view other if needed
annual_age_env_fitted_plots[[1]]

# -----------------------------
# 5.14 posterior predictive intervals
# -----------------------------

# posterior predictive intervals
get_annual_predictive_summary <- function(
    model,
    env_variable,
    ndraws = 2000
) {

  model_data <- get_model_data(
    model = model,
    env_variable = env_variable
  )

  # Posterior predictive draws
  predictive_draws <- posterior_predict(
    model,
    newdata = model_data,
    re_formula = NA,
    ndraws = ndraws
  )

  # Summarize posterior predictions
  predictive_summary <- apply(
    predictive_draws,
    MARGIN = 2,
    FUN = function(x) {

      c(
        mean = mean(x),

        lower_50 = unname(quantile(x, 0.25)),
        upper_50 = unname(quantile(x, 0.75)),

        lower_95 = unname(quantile(x, 0.025)),
        upper_95 = unname(quantile(x, 0.975))
      )
    }
  ) %>%
    t() %>%
    as_tibble()

  bind_cols(
    model_data %>%
      select(
        Year,
        mean_age,
        n_fish
      ),
    predictive_summary
  ) %>%
    mutate(
      covariate = env_variable
    )
}

set.seed(123)

annual_age_env_predictive <- imap_dfr(
  annual_age_env_models,
  \(model, variable) {
    get_annual_predictive_summary(
      model = model,
      env_variable = variable,
      ndraws = 2000
    )
  }
)

annual_age_env_predictive

# posterior predictions with observations
plot_annual_predictive <- function(
    predictive_data,
    env_variable
) {

  plot_title <- gsub(
    "_annual_z$",
    "",
    env_variable
  )

  ggplot(
    predictive_data,
    aes(x = Year)
  ) +
    geom_ribbon(
      aes(
        ymin = lower_95,
        ymax = upper_95
      ),
      alpha = 0.15
    ) +
    geom_ribbon(
      aes(
        ymin = lower_50,
        ymax = upper_50
      ),
      alpha = 0.30
    ) +
    geom_line(
      aes(y = mean),
      linewidth = 0.9
    ) +
    geom_point(
      aes(
        y = mean_age,
        size = n_fish
      ),
      alpha = 0.70
    ) +
    scale_size_continuous(
      trans = "sqrt"
    ) +
    labs(
      title = paste(
        "Posterior predictions:",
        plot_title
      ),
      subtitle = paste(
        "Dark ribbon is the 50% predictive interval;",
        "light ribbon is the 95% predictive interval"
      ),
      x = "Year",
      y = "Annual mean age (years)",
      size = "Fish sampled"
    ) +
    theme_classic(base_size = 12)
}

annual_age_env_predictive_plots <- annual_age_env_predictive %>%
  split(.$covariate) %>%
  imap(
    \(data, variable) {
      plot_annual_predictive(
        predictive_data = data,
        env_variable = variable
      )
    }
  )

walk(
  annual_age_env_predictive_plots,
  print
)

# combine plots
annual_age_env_predictive_combined <- wrap_plots(
  annual_age_env_predictive_plots,
  ncol = 3
)

annual_age_env_predictive_combined


# -----------------------------
# 5.15 compare each model to a matched null model
# -----------------------------

# function to create null model

fit_annual_age_null_model <- function(
    data,
    env_variable,
    output_directory = "models/annual_age_env_null",
    seed = 123
) {

  required_variables <- c(
    "mean_age",
    "year_z",
    "log_n_fish",
    "n_months",
    "n_sources",
    env_variable
  )

  model_data <- data %>%
    filter(
      if_all(
        all_of(required_variables),
        ~ !is.na(.x)
      )
    )

  dir.create(
    output_directory,
    recursive = TRUE,
    showWarnings = FALSE
  )

  brm(
    formula = bf(
      median_age ~
        year_z +
        log_n_fish +
        n_months +
        n_sources
    ),

    data = model_data,

    family = student(),

    prior = c(
      prior(
        normal(0, 0.5),
        class = "b"
      ),

      prior(
        normal(8, 3),
        class = "Intercept"
      ),

      prior(
        exponential(1),
        class = "sigma"
      ),

      prior(
        gamma(2, 0.1),
        class = "nu"
      )
    ),

    chains = 4,
    cores = 4,
    iter = 4000,
    warmup = 2000,

    backend = "cmdstanr",
    seed = seed,

    control = list(
      adapt_delta = 0.97,
      max_treedepth = 13
    ),

    save_pars = save_pars(
      all = TRUE
    ),

    file = file.path(
      output_directory,
      paste0(
        "null_",
        env_variable
      )
    ),

    file_refit = "on_change"
  )
}

annual_age_null_models <- annual_env_variables %>%
  set_names() %>%
  map(
    \(env_variable) {
      fit_annual_age_null_model(
        data = annual_age_env,
        env_variable = env_variable
      )
    }
  )


# add LOO to all models
annual_age_env_models <- annual_age_env_models %>%
  map(
    \(model) {
      add_criterion(
        model,
        criterion = "loo",
        moment_match = FALSE
      )
    }
  )

loo_results <- map(
  annual_age_env_models,
  \(model) model$criteria$loo
)

walk(loo_results, print)


# pareto-k diagnostics
loo::pareto_k_table(
  loo_results[["pdo_annual_same"]]
)

imap(
  loo_results,
  \(loo_result, model_name) {
    cat("\nModel:", model_name, "\n")
    print(loo::pareto_k_table(loo_result))
  }
)



### HERE NEW STUfF BELOW

##### -------------------------------------------------------------------- #####
##### -------------------------------------------------------------------- #####
##### -------------------------------------------------------------------- #####
##### -------------------------------------------------------------------- #####
##### -------------------------------------------------------------------- #####

# -----------------------------
# 6.1 Annual condition and environment
# -----------------------------

# April-June condition, giving each month equal weight
annual_condition <- cond_monthly %>%
  filter(month %in% 4:6) %>%
  group_by(year) %>%
  summarise(
    mean_log_Kn = mean(mean_log_Kn, na.rm = TRUE),
    n_months = sum(!is.na(month)),
    n_fish = sum(n_fish),
    .groups = "drop"
  ) #%>%
  # filter(n_months == 3)

# April-June environmental conditions
# Use existing lagged indices
annual_environment <- env_wide_rolling %>%
  filter(month %in% (4:6 + 1)) %>% # add one as we have a lagfor each month
  group_by(year) %>%
  summarise(
    across(
      matches("^(pdo|npgo|oni|npi|mei)_m_prev[1]$"),
      ~ if (sum(!is.na(.x)) == 3) {
        mean(.x)
      } else {
        NA_real_
      }
    ),
    .groups = "drop"
  ) %>%
  rename_with(~ str_replace(.x, "_m_prev1$", "_annual_same"))

# Join condition, environment, and age
annual_joint <- annual_condition %>%
  left_join(annual_environment, by = "year") %>%
  inner_join(
    annual_age_env %>%
      transmute(
        year = as.integer(Year),
        mean_age = mean_age
      ),
    by = "year"
  ) %>%
  arrange(year) %>%
  mutate(
    year_z = as.numeric(scale(year)),
    condition_z = as.numeric(scale(mean_log_Kn))
  )

glimpse(annual_joint)


# -----------------------------
# 6.2 plot age and condition together
# -----------------------------

annual_joint %>%
  select(year, mean_age, mean_log_Kn) %>%
  pivot_longer(
    cols = c(mean_age, mean_log_Kn),
    names_to = "response",
    values_to = "value"
  ) %>%
  group_by(response) %>%
  mutate(value_z = as.numeric(scale(value))) %>%
  ungroup() %>%
  ggplot(aes(year, value_z, colour = response)) +
  geom_line(alpha = 0.6) +
  geom_point(size = 1.5) +
  geom_smooth(
    method = "loess",
    se = FALSE,
    linewidth = 1
  ) +
  labs(
    x = "Year",
    y = "Standardized value",
    colour = NULL,
    title = "Temporal patterns in fish age and body condition"
  ) +
  theme_classic()




# -----------------------------
# 6.3 Model annual condition relate to annual age
# -----------------------------

# Simple association
m_age_condition <- brm(
  mean_age ~ condition_z,
  data = annual_joint,
  family = student(),
  backend = "cmdstanr",
  chains = 4,
  iter = 3000,
  warmup = 1500,
  seed = 22
)

# Adjust for linear time trend
m_age_condition_year <- brm(
  mean_age ~ condition_z + year_z,
  data = annual_joint,
  family = student(),
  backend = "cmdstanr",
  chains = 4,
  iter = 3000,
  warmup = 1500,
  seed = 22
)

fixef(m_age_condition)
fixef(m_age_condition_year)


m_age_condition_smooth <- brm(
  mean_age ~ condition_z + s(year, k = 8),
  data = annual_joint,
  family = student(),
  backend = "cmdstanr",
  chains = 4,
  iter = 3000,
  warmup = 1500,
  seed = 22
)



# -----------------------------
# 6.4 Environment effects on body condition
# -----------------------------

# Select environmental indices
annual_env_vars <- c(
  "pdo_annual_same",
  # "pdo_annual_prior",
  "npgo_annual_same",
  # "npgo_annual_prior",
  "oni_annual_same",
  # "oni_annual_prior",
  "npi_annual_same",
  # "npi_annual_prior",
  "mei_annual_same"
  # "mei_annual_prior"
)

# Fit one model per environmental variable
annual_condition_models <- set_names(
  map(annual_env_vars, function(v) {

    dat <- annual_joint %>%
      select(year, year_z, mean_log_Kn, all_of(v)) %>%
      drop_na() %>%
      mutate(env_z = as.numeric(scale(.data[[v]])))

    brm(
      mean_log_Kn ~ env_z + s(year, k = 8),
      data = dat,
      family = student(),
      backend = "cmdstanr",
      chains = 4,
      iter = 3000,
      warmup = 1500,
      seed = 22
    )

  }),
  annual_env_vars
)

# Summarize environmental slopes
annual_condition_results <- imap_dfr(
  annual_condition_models,
  function(model, variable) {

    x <- fixef(model)["env_z", ]

    tibble(
      covariate = variable,
      estimate = x["Estimate"],
      lower = x["Q2.5"],
      upper = x["Q97.5"]
    )
  }
)

annual_condition_results


# -----------------------------
# 6.4.1 Extract posterior slopes
# -----------------------------

library(posterior)
library(ggdist)
library(patchwork)

annual_condition_draws <- purrr::imap_dfr(
  annual_condition_models,
  function(model, variable) {

    draws <- posterior::as_draws_matrix(
      model,
      variable = "b_env_z"
    )

    tibble(
      covariate = variable,
      beta = as.numeric(draws[, 1])
    )
  }
) %>%
  mutate(
    index = toupper(
      sub("_.*", "", covariate)
    )
  )

# Summarize posterior effects
annual_condition_posterior_summary <- annual_condition_draws %>%
  group_by(covariate, index) %>%
  summarise(
    median_beta = median(beta),
    lower_95 = quantile(beta, 0.025),
    upper_95 = quantile(beta, 0.975),
    prob_positive = mean(beta > 0),
    prob_negative = mean(beta < 0),

    # Convert log-condition effects to percentage change
    median_pct = 100 * (exp(median_beta) - 1),
    lower_pct = 100 * (exp(lower_95) - 1),
    upper_pct = 100 * (exp(upper_95) - 1),

    .groups = "drop"
  )

annual_condition_posterior_summary

# -----------------------------
# 6.4.2 Posterior effect size plot
# -----------------------------

annual_condition_draws <- annual_condition_draws %>%
  mutate(
    pct_change = 100 * (exp(beta) - 1),
    index = factor(
      index,
      levels = c("PDO", "NPGO", "ONI", "NPI", "MEI")
    )
  )

p_annual_condition_posterior <- ggplot(
  annual_condition_draws,
  aes(
    x = pct_change,
    y = index
  )
) +
  stat_halfeye(
    .width = c(0.50, 0.80, 0.95),
    point_interval = median_qi,
    normalize = "groups",
    slab_colour = "black",
    slab_linewidth = 0.3
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    linewidth = 0.6
  ) +
  labs(
    title = "Environmental effects on annual body condition",
    subtitle = paste(
      "April-June environmental conditions;",
      "adjusted for temporal trends"
    ),
    x = "Change in body condition (%) per 1-SD increase in index",
    y = NULL
  ) +
  theme_classic(base_size = 12)

p_annual_condition_posterior



# -----------------------------
# 6.4.3 Bayesian environmental relationships
# -----------------------------

get_annual_condition_relationship <- function(
    model,
    variable,
    data = annual_joint,
    ndraws = 1000
) {

  # Original data used for this predictor
  dat <- data %>%
    select(
      year,
      mean_log_Kn,
      all_of(variable)
    ) %>%
    drop_na() %>%
    mutate(
      env_z = as.numeric(
        scale(.data[[variable]])
      )
    )

  # Environmental grid
  env_grid <- seq(
    min(dat$env_z),
    max(dat$env_z),
    length.out = 100
  )

  # Hold year constant to isolate environmental effect
  newdata <- tibble(
    env_z = env_grid,
    year = median(dat$year)
  )

  # Posterior expected values
  epred <- posterior_epred(
    model,
    newdata = newdata,
    ndraws = ndraws,
    re_formula = NA
  )

  # Summarize expected values
  pred_summary <- tibble(
    env_z = env_grid,
    median = apply(epred, 2, median),
    lower_95 = apply(
      epred, 2,
      \(x) unname(quantile(x, 0.025))
    ),
    upper_95 = apply(
      epred, 2,
      \(x) unname(quantile(x, 0.975))
    )
  )

  # Return both observed data and predictions
  list(
    observed = dat %>%
      mutate(covariate = variable),

    predicted = pred_summary %>%
      mutate(covariate = variable)
  )
}

# Run for all environmental variables
annual_condition_relationships <- imap(
  annual_condition_models,
  ~ get_annual_condition_relationship(
    model = .x,
    variable = .y
  )
)

# Combine observations
annual_condition_observed <- map_dfr(
  annual_condition_relationships,
  "observed"
)

# Combine predictions
annual_condition_predicted <- map_dfr(
  annual_condition_relationships,
  "predicted"
)


# -----------------------------
# 6.4.4 Plot all environmental relationships
# -----------------------------

p_annual_condition_relationships <- ggplot() +

  # 95% credible interval
  geom_ribbon(
    data = annual_condition_predicted,
    aes(
      x = env_z,
      ymin = lower_95,
      ymax = upper_95
    ),
    alpha = 0.2
  ) +

  # Median fitted relationship
  geom_line(
    data = annual_condition_predicted,
    aes(
      x = env_z,
      y = median
    ),
    linewidth = 1
  ) +

  # Observed annual values
  geom_point(
    data = annual_condition_observed,
    aes(
      x = env_z,
      y = mean_log_Kn
    ),
    alpha = 0.5,
    size = 1.8
  ) +

  facet_wrap(
    ~ covariate,
    scales = "free_x",
    ncol = 3
  ) +

  labs(
    title = "Annual body condition and environmental indices",
    subtitle = paste(
      "Bayesian fitted relationships adjusted",
      "for nonlinear temporal trends"
    ),
    x = "Standardized environmental index",
    y = "Annual mean log condition"
  ) +

  theme_classic(base_size = 12)

p_annual_condition_relationships






















# -----------------------------
# 6.5 Joint model comparison - just PDO
# -----------------------------

dat_joint_pdo <- annual_joint %>%
  select(
    year,
    mean_age,
    condition_z,
    pdo_m_prev3
  ) %>%
  drop_na() %>%
  mutate(
    pdo_z = as.numeric(scale(pdo_m_prev3))
  )

# Null
m0 <- brm(
  mean_age ~ s(year, k = 8),
  data = dat_joint_pdo,
  family = student(),
  backend = "cmdstanr"
)

# Environment
m1 <- brm(
  mean_age ~ s(year, k = 8) + pdo_z,
  data = dat_joint_pdo,
  family = student(),
  backend = "cmdstanr"
)

# Condition
m2 <- brm(
  mean_age ~ s(year, k = 8) + condition_z,
  data = dat_joint_pdo,
  family = student(),
  backend = "cmdstanr"
)

# Combined
m3 <- brm(
  mean_age ~ s(year, k = 8) +
    pdo_z + condition_z,
  data = dat_joint_pdo,
  family = student(),
  backend = "cmdstanr"
)

# LOO comparison
loo_comparison <- loo_compare(
  loo(m0),
  loo(m1),
  loo(m2),
  loo(m3)
)

loo_comparison

# Examine posterior effects
fixef(m3)


### STEPPING THROUGH ALL VARIABLES
# -----------------------------
# 6.6.1 Environmental variables
# -----------------------------

library(dplyr)
library(purrr)
library(tidyr)
library(tibble)
library(brms)
library(posterior)
library(ggplot2)

annual_env_vars <- c(
  # "pdo_m_prev1",
  # "npgo_m_prev1",
  # "oni_m_prev1",
  # "npi_m_prev1",
  # "mei_m_prev1",
  #
  # "pdo_m_prev3",
  # "npgo_m_prev3",
  # "oni_m_prev3",
  # "npi_m_prev3",
  # "mei_m_prev3",

  "pdo_m_prev6",
  "npgo_m_prev6",
  "oni_m_prev6",
  "npi_m_prev6",
  "mei_m_prev6"
)

# Check availability
setdiff(annual_env_vars, names(annual_joint))




# -----------------------------
# 6.6.2 Model fitting function
# -----------------------------

fit_annual_joint_models <- function(
    variable,
    data = annual_joint
) {

  # Prepare data
  dat <- data %>%
    select(
      year,
      mean_age,
      condition_z,
      all_of(variable)
    ) %>%
    drop_na() %>%
    mutate(
      env_z = as.numeric(
        scale(.data[[variable]])
      )
    )

  if (nrow(dat) < 15) {
    stop("Too few observations for: ", variable)
  }

  # Define formulas
  formulas <- list(
    null = bf(
      mean_age ~ s(year, k = 8)
    ),

    environment = bf(
      mean_age ~ s(year, k = 8) + env_z
    ),

    condition = bf(
      mean_age ~ s(year, k = 8) + condition_z
    ),

    combined = bf(
      mean_age ~ s(year, k = 8) +
        env_z + condition_z
    )
  )

  # Fit models
  models <- map(
    formulas,
    function(form) {

      brm(
        formula = form,
        data = dat,
        family = student(),

        backend = "cmdstanr",

        chains = 4,
        cores = 4,
        iter = 3000,
        warmup = 1500,

        seed = 22,
        refresh = 0
      )
    }
  )

  list(
    variable = variable,
    data = dat,
    models = models
  )
}


# -----------------------------
# 6.6.3 Fit all environmental models
# -----------------------------

annual_joint_models <- annual_env_vars %>%
  set_names() %>%
  map(
    ~ fit_annual_joint_models(
      variable = .x,
      data = annual_joint
    )
  )

# Save models to avoid refitting
saveRDS(
  annual_joint_models,
  "annual_joint_models.rds"
)




# -----------------------------
# 6.6.4 Extract posterior effects
# -----------------------------

annual_joint_posterior <- imap_dfr(
  annual_joint_models,
  function(result, variable) {

    model <- result$models$combined

    draws <- as_draws_df(model)

    tibble(
      covariate = variable,

      env_beta = draws$b_env_z,

      condition_beta = draws$b_condition_z
    )
  }
)

# Summarize posterior distributions
annual_joint_summary <- annual_joint_posterior %>%
  group_by(covariate) %>%
  summarise(
    env_mean = mean(env_beta),
    env_lower = quantile(env_beta, 0.025),
    env_upper = quantile(env_beta, 0.975),
    prob_env_positive = mean(env_beta > 0),

    condition_mean = mean(condition_beta),
    condition_lower = quantile(condition_beta, 0.025),
    condition_upper = quantile(condition_beta, 0.975),
    prob_condition_positive = mean(condition_beta > 0),

    .groups = "drop"
  )

annual_joint_summary


# -----------------------------
# 6.6.5 Posterior coefficient plot
# -----------------------------

annual_joint_plot_data <- annual_joint_posterior %>%
  pivot_longer(
    cols = c(env_beta, condition_beta),
    names_to = "predictor",
    values_to = "beta"
  ) %>%
  mutate(
    predictor = recode(
      predictor,
      env_beta = "Environment",
      condition_beta = "Body condition"
    ),

    index = toupper(
      sub("_.*", "", covariate)
    ),

    exposure_window = case_when(
      grepl("prev1$", covariate) ~ "Previous month",
      grepl("prev3$", covariate) ~ "Previous 3 months",
      grepl("prev6$", covariate) ~ "Previous 6 months"
    )
  )

ggplot(
  annual_joint_plot_data,
  aes(
    x = beta,
    y = exposure_window
  )
) +
  ggdist::stat_halfeye(
    .width = c(0.50, 0.80, 0.95),
    point_interval = "median_qi"
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed"
  ) +
  facet_grid(
    predictor ~ index,
    scales = "free_x"
  ) +
  labs(
    title = "Environmental and condition associations with annual mean age",
    subtitle = "Posterior coefficients from combined models",
    x = "Change in annual mean age (years)",
    y = "Environmental exposure window"
  ) +
  theme_classic()



# -----------------------------
# 6.6.6 Calculate LOO
# -----------------------------

annual_joint_loo <- imap(
  annual_joint_models,
  function(result, variable) {

    loo_results <- map(
      result$models,
      ~ loo(
        .x,
        moment_match = FALSE
      )
    )

    comparison <- do.call(
      loo_compare,
      loo_results
    )

    list(
      variable = variable,
      loo = loo_results,
      comparison = comparison
    )
  }
)


condition_added_value <- imap_dfr(
  annual_joint_loo,
  function(result, variable) {

    comparison <- loo_compare(
      result$loo$combined,
      result$loo$environment
    )

    comparison <- as.data.frame(comparison)

    # Difference in ELPD: combined minus environment
    elpd_combined <- result$loo$combined$estimates[
      "elpd_loo", "Estimate"
    ]

    elpd_environment <- result$loo$environment$estimates[
      "elpd_loo", "Estimate"
    ]

    tibble(
      covariate = variable,
      elpd_gain_condition =
        elpd_combined - elpd_environment,

      se_diff = comparison[
        "environment",
        "se_diff"
      ]
    )
  }
)

condition_added_value

# -----------------------------
# 6.6.7 Summarize model comparison
# -----------------------------
annual_joint_loo_summary <- imap_dfr(
  annual_joint_loo,
  function(result, variable) {

    comparison <- as.data.frame(
      result$comparison
    )

    comparison %>%
      rownames_to_column("model") %>%
      mutate(
        covariate = variable
      ) %>%
      select(
        covariate,
        model,
        elpd_diff,
        se_diff
      )
  }
)

annual_joint_loo_summary


# plot LOO comparisons
ggplot(
  annual_joint_loo_summary,
  aes(
    x = elpd_diff,
    y = reorder(covariate, elpd_diff),
    colour = model
  )
) +
  geom_point(
    position = position_dodge(width = 0.6),
    size = 2
  ) +
  geom_errorbarh(
    aes(
      xmin = elpd_diff - se_diff,
      xmax = elpd_diff + se_diff
    ),
    position = position_dodge(width = 0.6),
    height = 0
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed"
  ) +
  labs(
    title = "Comparison of annual mean age models",
    subtitle = "ELPD differences relative to the best model for each index",
    x = "ELPD difference",
    y = "Environmental variable",
    colour = "Model"
  ) +
  theme_classic()


# -----------------------------
# 6.6.8 Condition add value to analysis
# -----------------------------

condition_added_value <- imap_dfr(
  annual_joint_loo,
  function(result, variable) {

    comparison <- loo_compare(
      result$loo$combined,
      result$loo$environment
    )

    comparison <- as.data.frame(comparison)

    # Difference in ELPD: combined minus environment
    elpd_combined <- result$loo$combined$estimates[
      "elpd_loo", "Estimate"
    ]

    elpd_environment <- result$loo$environment$estimates[
      "elpd_loo", "Estimate"
    ]

    tibble(
      covariate = variable,
      elpd_gain_condition =
        elpd_combined - elpd_environment,

      se_diff = comparison[
        "environment",
        "se_diff"
      ]
    )
  }
)

condition_added_value












