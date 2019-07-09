#' Extract variance components
#'
#' @description This function extracts the variance components from a gam or bam
#'   object, where one is using the associated functions from the mgcv package
#'   for mixed models.
#'
#' @param model A gam or bam model
#' @param tibble Return a tibble or standard default data.frame.  Default is
#'   `TRUE`.
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
#' @return A tibble (if `tibble = TRUE`) or data frame with the standard
#'   deviation, its lower and upper bounds, the variance, and the relative
#'   proportion of total variance for each variance component.
#'
#' @examples
#' library(mgcv); library(lme4)

#' lmer_model = lmer(Reaction ~  Days + (Days || Subject), data=sleepstudy)
#' ga_model = gam(Reaction ~  Days + s(Subject, bs='re') + s(Days, Subject, bs='re'),
#' data=sleepstudy,
#' method = 'REML')
#'
#' VarCorr(lmer_model)
#' extract_vc(ga_model)
#'
#'
#' @importFrom tibble rownames_to_column
#' @importFrom stringr str_replace
#' @importFrom mgcv gam.vcomp
#' @export
extract_vc <- function(model, tibble = TRUE) {

  if ( !inherits(model, 'gam') ) stop("Need a gam object.")
  if ( model$method != 'REML' )
    stop("REML required. Rerun model with method = 'REML' for appropriate results.")

  invisible(utils::capture.output(vc <- mgcv::gam.vcomp(model)))  # keep from printing result

  vc = data.frame(vc)

  vc = tibble::rownames_to_column(vc, var = 'component')
  vc$component = stringr::str_remove_all(vc$component, pattern = 's\\(|ti\\(|te\\(|\\)')
  vc$component = stringr::str_replace(vc$component, pattern = ',', '|')


  vc$variance = vc$std.dev^2
  vc$proportion = vc$var/sum(vc$var)

  if (tibble) vc = tibble::as_tibble(vc)

  vc
}
