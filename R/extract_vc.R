#' Extract variance components
#'
#' @description This function extracts the variance components from a gam or bam
#'   object, where one is using the associated functions from the mgcv package
#'   for mixed models.
#'
#' @param model A gam or bam model
#' @param ci_level Level for the confidence interval. Must be between 0 and 1.
#' @param ci_scale Whether the confidence interval should be on the variance or standard deviation scale.
#' @param digits Rounding for the output.
#'
#' @details This is essentially a pretty way to print \code{\link{gam.vcomp}}.
#'   Note that if you do something like a random slope, the gam approach does
#'   not estimate the intercept-slope correlation, so none will be printed.
#'   Should work fine on standard smooth terms also, but the model \emph{must be
#'   estimated with} `method = REML` or `method = ML`. This is the default for
#'   non-exponential families, but otherwise you'll have to change the value or
#'   will receive an error.  Have not tested much with interactions, using `by`,
#'   etc., but it has worked on some default \code{\link{mgcv}} examples.
#'
#' @return A tibble or data frame with the standard deviation, its lower and
#'   upper bounds, the variance, and the relative proportion of total variance
#'   for each variance component.
#'
#' @examples
#' library(mgcv)
#' library(lme4)
#'
#' lmer_model <- lmer(Reaction ~ Days + (Days || Subject), data = sleepstudy)
#' ga_model <- gam(Reaction ~ Days + s(Subject, bs = "re") + s(Days, Subject, bs = "re"),
#'   data = sleepstudy,
#'   method = "REML"
#' )
#'
#' VarCorr(lmer_model)
#' extract_vc(ga_model)
#'
#' @importFrom stringr str_replace
#' @importFrom mgcv gam.vcomp
#' @export
extract_vc <- function(
  model,
  ci_level = .95,
  ci_scale = 'sd',
  digits = 3
) {

  if (!inherits(model, "gam")) stop("Need a gam object.")

  if (!grepl(model$method, pattern = "REML")) {
    stop("REML required. Rerun model with method = 'REML' for appropriate results.")
  }

  # keep from printing result
  invisible(
    utils::capture.output(
      vc <- mgcv::gam.vcomp(model, conf.lev = ci_level)
    )
  )

  vc <- data.frame(vc)

  # clean up names
  vc <- vc %>%
    dplyr::mutate(
      effect  = rownames(vc),
      effect  = gsub(effect, pattern = "s\\(|ti\\(|te\\(|\\)", replacement = '')
    )

  # if more two after split, suggests random slope
  vc <- vc %>%
    dplyr::mutate(
      group = split_group_effect(effect, which = 2),
      group = ifelse(group == 'scale', 'Residual', group),
      effect = split_group_effect(effect, which = 1),
      effect = ifelse(effect == 'scale', '', effect),
      effect = ifelse(effect ==  group, 'Intercept', effect)
    )

  # calc variance and scale
  vc <- vc %>%
    dplyr::mutate(
      variance = std.dev^2,
      var_prop = variance / sum(variance)
    ) %>%
    dplyr::rename(sd = std.dev)

  lower <- (1 - ci_level) / 2
  upper <- 1 - lower

  if (ci_scale == 'var') {
    vc <- vc %>%
      dplyr::mutate(lower = lower^2, upper = upper^2)

    colnames(vc)[colnames(vc) %in% c('lower', 'upper')] <-
      c(paste0('var_', 100 * lower), paste0('var_', 100 * upper))
  }
  else {
    colnames(vc)[colnames(vc) %in% c('lower', 'upper')] <-
      c(paste0('sd_', 100 * lower), paste0('sd_', 100 * upper))
  }

  vc %>%
    dplyr::select(group, effect, variance, dplyr::everything())  %>%
    dplyr::mutate_if(is.numeric, round, digits = digits)
}
