#' Extract the fixed effects from a GAMM
#'
#' When using a GAM for mixed models, we may have specific interest in the fixed
#' effect parameters.
#'
#' @param model The mgcv model
#' @param tibble Return a tibble or standard default data.frame.  Default is
#' `TRUE`.
#'
#'
#' @details This essentially reproduces the 'parametric' output from
#'   \code{\link[mgcv]{summary.gam}}.
#'
#' @return A \code{data.frame} with the coefficients, standard error, and upper
#'   and lower bounds based on the standard error.
#'
#' @seealso \code{\link[lme4]{fixef}}
#' @importFrom dplyr as_tibble
#' @examples
#'
#' library(mgcv)
#' library(lme4)
#'
#' lmer_model <- lmer(Reaction ~ Days + (Days || Subject), data = sleepstudy)
#' ga_model <- gam(Reaction ~ Days + s(Subject, bs = "re") + s(Days, Subject, bs = "re"),
#'   data = sleepstudy,
#'   method = "REML"
#' )
#'
#' fixef(lmer_model)
#' extract_fixed(ga_model)
#'
#' @importFrom stats coef vcov
#' @export
extract_fixed <- function(model, tibble = TRUE) {
  if (!inherits(model, "gam")) stop("Need a gam object.")

  coefs_all = stats::coef(model)
  nams_all  = names(coefs_all)
  coefs_fe  = coefs_all[!grepl(nams_all, pattern = '^s\\(')]
  nams_fe   = names(coefs_fe)
  coefs_se  = sqrt(diag(stats::vcov(model)[nams_fe, nams_fe]))

  # create table
  out = data.frame(
    Term = nams_fe,
    Estimate = coefs_fe,
    SE = coefs_se,
    LL = coefs_fe - 1.96*coefs_se,
    UL = coefs_fe + 1.96*coefs_se
  )

  if (tibble) dplyr::as_tibble(out)
  else out

}
