#' Check stability of model estimates
#'
#' Randomly creates starting values
#'
#' @param data
#' @param utility_script
#' @param nDraws
#' @param mean
#' @param sd
#'
#' @return
#' @export
stability_check <- function(data, utility_script, nDraws, mean = 1, sd = 1)
{
  model_spec <- mixl::specify_model(utility_script, data)
  est <- stats::setNames(rnorm(length(model_spec$beta_names), mean = mean, sd = sd), model_spec$beta_names)
  availabilities <- mixl::generate_default_availabilities(data, length(unique(data$CHOICE)))
  model <- mixl::estimate(model_spec, est, data, availabilities, nDraws = nDraws)

  return(model)
}
