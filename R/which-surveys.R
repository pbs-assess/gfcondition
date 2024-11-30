#' quantify positive catch and frequency of samples
#'
#' for groups and survey types
#' @export
#'
which_surveys <- function(data){
  .data <- data %>%
    group_by(survey_type, group_name) %>%
    mutate(year_range = length(unique(year)),
           total_fish = sum(n_fish_sampled, na.rm = TRUE),
           total_n = n()) %>%
    ungroup() %>%
    filter(catch_weight > 0) %>%
    mutate(group_name = ifelse(is.na(group_catch_est), "Unsampled", group_name),
           group_catch_est = ifelse(group_name == "Unsampled", catch_weight, group_catch_est),
           true_ratio = ifelse(n_fish_sampled > 0, proportion, NA_real_)
    ) %>%
    group_by(survey_abbrev, group_name, year) %>%
    mutate(any_pos_by_year = n(),
           n_fish_by_yr = sum(n_fish_sampled, na.rm = TRUE),
           # n_samples = length(!is.na(median_prop_ann)),
           n_samples = sum(n_fish_sampled!=0),
           mean_ratio_true = ifelse(all(is.na(true_ratio)), NA_real_,
                                    mean(true_ratio, na.rm = TRUE)),
           mean_ratio_filled = ifelse(all(is.na(proportion)), NA_real_,
                                      mean(proportion, na.rm = TRUE))
    ) %>% ungroup() %>%
    # group_by(survey_abbrev, group_name) %>% summarise(any_samples = paste(range(any_samples)[1],"-",range(any_samples)[2]))
    filter(group_catch_est > 0) %>%
    group_by(survey_type, group_name, year) %>%
    mutate(pos_n_by_year = n(),
    ) %>% ungroup() %>%
    group_by(survey_type, group_name) %>%
    summarise(
      any_2003_MSA = any(year == 2003),
      any_2004_MSSM = any(year == 2004),
      years = mean(year_range, na.rm = TRUE),
      n_pos_years = length(unique(year)),
      prop_years_w_0 = mean(year_range - n_pos_years, na.rm = TRUE)/years,
      max_pos_by_year = max(pos_n_by_year, na.rm = TRUE),
      pos_n = n(),
      total_n = mean(total_n, na.rm = TRUE),
      mean_pos_n = mean(pos_n/years, na.rm = TRUE),
      prop_pos = mean(pos_n/total_n, na.rm = TRUE),
      # total_samples = length(!is.na(median_prop_ann)),
      total_samples = sum(n_fish_sampled!=0),
      min_samples = min(n_samples),
      max_samples = max(n_samples),
      n_fish_by_yr = mean(n_fish_by_yr, na.rm = TRUE),
      mean_ratio_true = mean(mean_ratio_true, na.rm = TRUE),
      min_ratio_true = min(mean_ratio_true, na.rm = TRUE),
      max_ratio_true = max(mean_ratio_true, na.rm = TRUE),
      mean_ratio_filled = mean(mean_ratio_filled, na.rm = TRUE),
      min_ratio_filled = min(mean_ratio_filled, na.rm = TRUE),
      max_ratio_filled = max(mean_ratio_filled, na.rm = TRUE)
    ) %>% ungroup() %>%
    group_by(group_name) %>%
    mutate(
      max_prop_pos = max(prop_pos, na.rm = TRUE),
      # diff_prop = prop_pos - max_prop_pos,
      change_prop = (prop_pos - max_prop_pos)/max_prop_pos
    )

  .data$species <- data$species_common_name[1]
  .data
}
