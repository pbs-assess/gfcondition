#' refine delta model
#' @export
refine_model <- function(m, alternate_family = set_family2, use_priors = sdmTMBpriors()){
  # browser()
  reverted_mesh <- update_mesh(m)

  if(isTRUE(m$family$delta)) {
    s <- sanity(m)
    # s$sigmas_ok <- TRUE
    if (!s$range_ok) {
      m <- update(m, share_range = TRUE,
                  spatial = as.list(m[["spatial"]]),
                  spatiotemporal = as.list(m[["spatiotemporal"]]),
                  extra_time = m$extra_time,
                  priors = use_priors,
                  data = m$data, family = m$family, mesh = reverted_mesh)
      s <- sanity(m)
    }
    if (!s$gradients_ok) {
      m <- run_extra_optimization(m)
      s <- sanity(m)
    }
    if (!s$hessian_ok | !s$nlminb_ok) {
      m <- update(m,
                  # family = m$family,
                  family = delta_lognormal(), # used if using generalized gamma?
                  ## should probably be updated to this
                  spatial = "on",
                  spatiotemporal = "rw",
                  # spatial = as.list(m[["spatial"]]),
                  # spatiotemporal = as.list(m[["spatiotemporal"]]),
                  extra_time = m$extra_time,
                  priors = use_priors,
                  data = m$data, mesh = reverted_mesh)
      s <- sanity(m)
    }
    if (!s$se_magnitude_ok|!s$se_na_ok|!s$sigmas_ok) {
      m <- update(m, spatial = list("on", "off"),
                  spatiotemporal = "rw",
                  extra_time = m$extra_time,
                  priors = use_priors,
                  data = m$data, family = m$family, mesh = reverted_mesh)
      s <- sanity(m)
    }
    if (!s$hessian_ok | !s$nlminb_ok) {
      m <- update(m, family = alternate_family,
                  ## should probably be updated to this
                  spatial = "on",
                  spatiotemporal = "rw",
                  # spatial = as.list(m[["spatial"]]),
                  # spatiotemporal = as.list(m[["spatiotemporal"]]),
                  extra_time = m$extra_time,
                  priors = use_priors,
                  data = m$data, mesh = reverted_mesh)
      s <- sanity(m)
    } else {
      if (!s$se_magnitude_ok|!s$se_na_ok|!s$sigmas_ok) {
        m <- update(m, spatial = list("off", "off"),
                    spatiotemporal = "rw",
                    extra_time = m$extra_time,
                    priors = use_priors,
                    data = m$data, family = m$family, mesh = reverted_mesh)
        s <- sanity(m)
      }

      if (!s$se_magnitude_ok|!s$se_na_ok|!s$sigmas_ok) {
        m <- update(m, spatial = list("on", "off"),
                    spatiotemporal = list("off", "rw"),
                    extra_time = m$extra_time,
                    priors = use_priors,
                    data = m$data, family = m$family, mesh = reverted_mesh)
        s <- sanity(m)
      }

      if (!s$se_magnitude_ok|!s$se_na_ok|!s$sigmas_ok) {
        m <- update(m,
                    spatial = "on",
                    spatiotemporal = "rw",
                    family = alternate_family,
                    share_range = FALSE,
                    extra_time = m$extra_time,
                    priors = use_priors,
                    data = m$data, mesh = reverted_mesh)
        s <- sanity(m)
      }
    }

    if(!all(s)){
      m <- update(m,
                  share_range = FALSE,
                  spatial = "on",
                  spatiotemporal = "rw",
                  family = alternate_family,
                  extra_time = m$extra_time,
                  priors = use_priors,
                  data = m$data, mesh = reverted_mesh)
      s <- sanity(m)
    }
    if(!all(s)){
      m <- update(m,
                  share_range = TRUE,
                  spatial = "on",
                  spatiotemporal = "rw",
                  family = alternate_family,
                  extra_time = m$extra_time,
                  priors = use_priors,
                  data = m$data, mesh = reverted_mesh)
      s <- sanity(m)
    }
    if (!all(s)) {
      m <- update(m, spatial = "off",
                  spatiotemporal = "rw",
                  family = alternate_family,
                  extra_time = m$extra_time,
                  priors = use_priors,
                  data = m$data, mesh = reverted_mesh)
      s <- sanity(m)
    }
    if(!s$gradients_ok){
      m <- run_extra_optimization(m)
      s <- sanity(m)
    }
    sanity(m)
    return(m)
  } else {
    # if not a delta model
    s <- sanity(m)
    # browser()
    # if(!s$gradients_ok){
    #   m <- run_extra_optimization(m)
    #   s <- sanity(m)
    # }
    if(!s$se_magnitude_ok|!s$se_na_ok){
      m <- update(m, share_range = TRUE,
                  spatial = as.list(m[["spatial"]]),
                  spatiotemporal = as.list(m[["spatiotemporal"]]),
                  extra_time = m$extra_time,
                  priors = use_priors,
                  data = m$data, family = m$family, mesh = reverted_mesh)
      s <- sanity(m)
    }
    if (!s$se_magnitude_ok|!s$se_na_ok) {
      m <- update(m, spatial = "off",
                  spatiotemporal = as.list(m[["spatiotemporal"]]),
                  extra_time = m$extra_time,
                  priors = use_priors,
                  data = m$data, family = m$family, mesh = reverted_mesh)
      s <- sanity(m)
    }
    if(!s$gradients_ok){
      m <- run_extra_optimization(m)
      s <- sanity(m)
    }
    sanity(m)
    return(m)
  }
}

#' refine non-delta model; particularly one with anisotropy and/or without shared ranges
#' @export
refine_cond_model <- function(m, set_formula = cond_formula, dist = knot_distance) {
  s <- sanity(m, gradient_thresh = 0.005)
  t <- tidy(m, "ran_pars", conf.int = TRUE)

  if (!all(s) & length(t$estimate[t$term == "range"]) > 1) {
    if (abs(diff(t$estimate[t$term == "range"])) < dist | !s$range_ok | !s$hessian_ok | !s$nlminb_ok) {
      # try shared range but still allowing anisotropy
      try(m <- update(m,
                      formula = set_formula,
                      # formula = as.list(m[["formula"]]),
                      share_range = TRUE,
                      weights = m$data$sample_multiplier,
                      # # spatial = as.list(m[["spatial"]]),
                      # # spatiotemporal = as.list(m[["spatiotemporal"]]),
                      # # extra_time = m$extra_time,
                      # # family = m$family,
                      # priors = sdmTMBpriors(
                      #   matern_s = pc_matern(range_gt = dist,
                      #                        sigma_lt = 2),
                      #   matern_st = pc_matern(range_gt = dist,
                      #                         sigma_lt = 2)
                      # ),
                      data = m$data, mesh = m$spde
      ))
      s <- sanity(m)
      t <- tidy(m, "ran_pars", conf.int = TRUE)
    }
  }

  if (!s$range_ok | !s$sigmas_ok| !s$se_magnitude_ok) {
    # drop anisotropy and add spatial priors
    try(m <- update(m,
                    formula = set_formula,
                    weights = m$data$sample_multiplier,
                    priors = sdmTMBpriors(
                      matern_s = pc_matern(range_gt = dist,
                                           sigma_lt = 2),
                      matern_st = pc_matern(range_gt = dist,
                                            sigma_lt = 2)
                    ),
                    data = m$data, mesh = m$spde
    ))
    s <- sanity(m)
    t <- tidy(m, "ran_pars", conf.int = TRUE)
  }

  if (nrow(t)>3){
    if (t$estimate[t$term == "sigma_O"] < 0.005 | !all(s)) {
      # drop spatial field
      try(m <- update(m,
                      formula = set_formula,
                      weights = m$data$sample_multiplier,
                      spatial = "off",
                      spatiotemporal = "rw",
                      priors = sdmTMBpriors(
                        matern_st = pc_matern(range_gt = dist,
                                              sigma_lt = 2)
                      ),
                      data = m$data, mesh = m$spde
      ))
      s <- sanity(m)
      t <- tidy(m, "ran_pars", conf.int = TRUE)
    }
  }

  if (!all(s)) {
    # strengthen prior
    try(m <- update(m,
                    formula = set_formula,
                    weights = m$data$sample_multiplier,
                    spatial = "off",
                    spatiotemporal = "rw",
                    priors = sdmTMBpriors(
                      matern_st = pc_matern(range_gt = dist*2,
                                            sigma_lt = 2)
                    ),
                    data = m$data, mesh = m$spde
    ))
    s <- sanity(m)
    t <- tidy(m, "ran_pars", conf.int = TRUE)
  }

  if (!all(s)){
    # drop spatiotemporal field instead but add ar1 on time intercepts
    try(m <- update(m,
                    formula = set_formula,
                    # formula = update(set_formula,    ~ . + as.factor(year)),
                    time_varying = ~ 1,
                    time_varying_type = "ar1",
                    weights = m$data$sample_multiplier,
                    spatial = "on",
                    spatiotemporal = "off",
                    priors = sdmTMBpriors(
                      matern_s = pc_matern(range_gt = dist,
                                           sigma_lt = 2)
                    ),
                    # time_varying_type = "rw0",
                    # control = sdmTMBcontrol(map = list(ln_tau_V = factor(NA)),
                    #                         start = list(ln_tau_V = matrix(log(0.1), ncol = 1, nrow = 1))
                    #                         ),
                    # silent = TRUE,
                    data = m$data, mesh = m$spde
    ))
  }

  sanity(m)
  return(m)
}
