# dfa helper functions

format_process_cov <- function(data, trend_title = "Trend 1") {
  data |>
    mutate(
      trend_number = trend_title,
      Variable = type
    ) |>
    select(-year, -type) |>
    as.data.frame(stringsAsFactors = FALSE)
}

flip_trend <- function(rotated_modelfit, trend = 1L) {
  rflip <- rotated_modelfit
  rflip$trends_mean[trend,] <- -1 * rotated_modelfit$trends_mean[trend,]
  rflip$trends_lower[trend,] <- -1 * rotated_modelfit$trends_lower[trend,]
  rflip$trends_upper[trend,] <- -1 * rotated_modelfit$trends_upper[trend,]
  for (i in seq_len(dim(rotated_modelfit$Z_rot)[1])) {
    rflip$Z_rot[i,,trend] <- -1 * rotated_modelfit$Z_rot[i,,trend]
    rflip$trends[i,,trend] <- -1 * rotated_modelfit$trends[i,,trend]
  }
  rflip
}
