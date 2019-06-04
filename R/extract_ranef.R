#' Random effects for gam
#'
#' @description Extract what would be the random effects from a mixed model from
#'   a gam object.  Assumes an mgcv model of the form `gam(... + s(g, bs='re'))`.
#'
#' @param model The mgcv model
#' @param re Which specific coefficients to extract. Currently not
#'   implemented.
#' @param tibble Return a tibble or standard default data.frame.  Default is
#'   `TRUE`.
#'
#' @details Returns a data frame of the the `component` type, the estimated random effect `re`, the estimated `se`, and approximate `lower` and `upper` bounds assuming  `+- 1.96*se`.  Note that the standard errors are Bayesian estimates (see \code{\link{gamObject}}, specifically type `Vp`).
#'
#' @return A `tibble` (if `tibble = TRUE`) or data frame with the random effect, its
#'   standard errror, and its lower and upper bounds. The bounds are based on a
#'   simple normal approximation using the standard error.
#'
#' @seealso \code{\link{ranef}}
#'
#' @examples
#' library(mgcv); library(lme4)

#' lmer_model = lmer(Reaction ~  Days + (Days || Subject), data=sleepstudy)
#' ga_model = gam(Reaction ~  Days + s(Subject, bs='re') + s(Days, Subject, bs='re'),
#' data=sleepstudy,
#' method = 'REML')
#'
#' ranef(lmer_model)
#' extract_ranef(ga_model)
#'
#' @importFrom tibble as_tibble
#' @importFrom dplyr n_distinct
#' @importFrom stringr str_detect
#'
#' @export
extract_ranef <- function(model, re = NULL, tibble = TRUE) {

  gam_coef = stats::coef(model)

  # for later
  # if (!rlang::is_null(re)) {
  #   stopifnot(rlang::is_character(re))
  #   # ...
  # }

  re0 = gam_coef[stringr::str_detect(names(gam_coef), pattern = '^s\\(')]

  gam_se = sqrt(diag(model$Vp))  # no names
  gam_se = gam_se[names(gam_coef) %in% names(re0)]

  # clean up names
  names(re0) = stringr::str_remove(names(re0), pattern = 's\\(|\\)')  # for some reason this won't do the latter
  names(re0) = stringr::str_remove(names(re0), pattern = '\\)')
  names(re0) = stringr::str_replace(names(re0), pattern = ',', replacement = '|')
  names(re0) = stringr::str_remove(names(re0), pattern = '\\.[0-9]+')

  re_n = dplyr::n_distinct(names(re0))  # possible use later
  re_names = names(re0)

  re = data.frame(
    component = re_names,
    re = re0,
    se = gam_se,
    lower = re0 - 1.96*gam_se,
    upper = re0 + 1.96*gam_se
  )

  if (tibble) tibble::as_tibble(re)
  else re
}
