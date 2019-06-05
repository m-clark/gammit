#' Summarize a GAMM
#' @description Summarize a gam model in a clean mixed effects style.
#' @param model The mgcv model
#' @param digits number of digits to display
#'
#' @details This returns the variance components and fixed effects from a gam
#'   model. Assumes an mgcv model of the form `gam(... + s(g, bs='re'))`,
#'   but should work with just about any gam with a smooth term.
#'
#' @return Invisibly returns a list with the variance componets via
#'   `extract_vc` and fixed effects parameter
#'
#' @seealso \code{\link{extract_vc}}
#'
#' @examples
#' library(mgcv); library(lme4)

#' lmer_model = lmer(Reaction ~  Days + (Days || Subject), data=sleepstudy)
#' ga_model = gam(Reaction ~  Days + s(Subject, bs='re') + s(Days, Subject, bs='re'),
#' data=sleepstudy,
#' method = 'REML')
#'
#' summary(lmer_model)
#' summary_gamm(ga_model)
#'
#'
#' @export
summary_gamm = function(model, digits=3) {
  if ( !inherits(model, 'gam') ) stop("Need a gam object.")

  summary_table = mgcv::summary.gam(model)$p.table

  vc = extract_vc(model, tibble = FALSE)
  fe = data.frame(summary_table)
  names(fe) = colnames(summary_table)

  # re part
  message("\nVariance components:\n")
  # for more consistency with fe table do digits - 1; Fix later to not use round
  # or signif digits
  print(dplyr::mutate_if(data.frame(vc), is.numeric, round, digits = digits-1))

  # fe part
  message("\n\nFixed Effects:\n")
  stats::printCoefmat(summary_table,
                      digits = digits,
                      signif.stars = FALSE,
                      na.print = "NA")

  invisible(list(vc = vc,
                 fe = fe))
}

