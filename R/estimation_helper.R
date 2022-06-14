estimation_helper <- function(data, utility_script, nDraws, ...)
{

  n_available_choices <- length(unique(data$CHOICE))

  model_spec <- specify_model(utility_script, data)

  n_params <- length(model_spec$beta_names)
  est <- stats::setNames(rep(0, n_params), model_spec$beta_names)

  availabilities <- mixl::generate_default_availabilities(data, n_available_choices)

  model <- mixl::estimate(model_spec, est, data, availabilities, nDraws = nDraws, ...)

  return(model)

}
