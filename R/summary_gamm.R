#' Summarize a GAMM
#'
#' @description Summarize a gam model in a clean mixed effects style.
#' @param model The mgcv model
#' @param digits number of digits to display
#'
#' @details This displays the variance components and fixed effects from a gam
#'   model. Assumes an mgcv model of the form `gam(... + s(g, bs='re'))`,
#'   but should work with just about any gam with a smooth term.
#'
#' @return Invisibly returns a list with the variance components via
#'   `extract_vc` and fixed effects parameter, labeled `vc` and `fe`
#'   respectively.
#'
#' @seealso \code{\link{extract_vc}}
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
#' summary(lmer_model)
#' summary_gamm(ga_model)
#' @export
summary_gamm <- function(model, digits = 3) {
  if (!inherits(model, "gam")) stop("Need a gam object.")

  summary_table <- mgcv::summary.gam(model)$p.table

  vc <- extract_vc(model)

  fe <- data.frame(round(summary_table, digits = digits))
  names(fe) <- colnames(summary_table)
  fe$Term <- rownames(summary_table)
  fe <- dplyr::select(fe, Term, dplyr::everything())
  rownames(fe) <- NULL

  # re part
  message("\nVariance components:\n")

  print(
    format(
      dplyr::mutate_if(vc,
        is.numeric,
        round,
        digits = digits
      ),
      nsmall = digits
    ),
    row.names = FALSE
  )

  # fe part
  message("\n\nFixed Effects:\n")

  print(format(fe, nsmall = digits), row.names = FALSE)


  invisible(list(vc = vc, fe = fe))
}
