#' Extract the fixed effects from a GAMM
#'
#' When using a GAM for mixed models, we may have specific interest in the fixed
#' effect parameters.
#'
#' @inheritParams extract_vc
#'
#'
#' @details This essentially reproduces the 'parametric' output from
#'   \code{\link[mgcv]{summary.gam}}.
#'
#' @return A \code{data.frame} with the coefficients, standard error, and upper
#'   and lower bounds.
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
extract_fixed <- function(
  model,
  ci_level = .95,
  digits = 3
) {
  fe <- data.frame(summary(model, re.test = FALSE)$p.table)

  colnames(fe) =  c('value', 'se', 't', 'p')

  # no confint.gam
  if (ci_level > 0) {

    lower <- (1 - ci_level)/2
    upper <- 1 - lower
    nu <- model$df.residual
    mult <- stats::qt(upper, nu)

    ci <- data.frame(
      lower = fe$value - mult * fe$se,
      upper = fe$value + mult * fe$se
    )

    colnames(ci) <- paste0(c('lower_', 'upper_'), c(lower, upper) * 100)

    fe <- data.frame(fe, ci)
  }

  fe <- fe %>%
    dplyr::mutate_all(round, digits = digits) %>%
    dplyr::mutate(term = gsub(rownames(fe),
                              pattern = '[\\(,\\)]',
                              replacement = '')) %>%
    dplyr::select(term, dplyr::everything()) %>%
    dplyr::as_tibble()

  fe

}
