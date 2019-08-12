#' Extract random effects from a gam
#'
#' @description Extract what would be the random effects from a mixed model from
#'   a gam object.  Assumes an mgcv model of the form `gam(... + s(g,
#'   bs='re'))`.
#'
#' @param model The mgcv model
#' @param re Which specific coefficients to extract. Currently not
#'   implemented.
#' @param tibble Return a tibble or standard default data.frame.  Default is
#'   `TRUE`.
#'
#' @details Returns a data frame of the the `component` type, the estimated
#'   random effect `re`, the estimated `se`, and approximate `lower` and `upper`
#'   bounds assuming  `+- 1.96*se`.  Note that the standard errors are Bayesian
#'   estimates (see \code{\link{gamObject}}, specifically type `Vp`).  The `re`
#'   will only reflect smooth terms whose basis function is 're', i.e. of class
#'   `random.effect`.  Others will be ignored.
#'
#' @return A `tibble` (if `tibble = TRUE`) or data frame with the random effect,
#'   its standard error, and its lower and upper bounds. The bounds are based
#'   on a simple normal approximation using the standard error.
#'
#' @note `mgcv` strips the level names for 're' smooth terms, so this attempts
#'   to get them back. This may not work under every circumstance, but the
#'   attempt is made to extract the names of random effect groups based on how
#'   they are ordered in the data (which is how the model matrix would be
#'   constructed), and in the case of random slopes, detect that second variable
#'   in the 're' specification would be the grouping variable. This will not
#'   work for continuous x continuous smooths of type 're', but I can't think of
#'   a reason why you'd use that given your other options with `mgcv`.
#'
#' @seealso \code{\link{ranef}}
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
#' ranef(lmer_model)
#' extract_ranef(ga_model)
#' @importFrom tibble as_tibble
#' @importFrom dplyr n_distinct
#' @importFrom stringr str_detect
#'
#' @export
extract_ranef <- function(model, re = NULL, tibble = TRUE) {
  if (!inherits(model, "gam")) stop("Need a gam object.")

  # get the re variables and their levels
  re_terms <- sapply(model$smooth, function(x) inherits(x, "random.effect"))
  re_var_names <- sapply(
    model$smooth[re_terms],
    function(x) ifelse(length(x$vn) == 1,
        x$vn,
        x$vn[length(x$vn)]
      )
  )
  re_labels <- sapply(model$smooth[re_terms], function(x) x$label)

  re_levels <- vector("list", length(re_var_names))

  # to do: check that re is factor; tried unique but won't hold ordering as mgcv
  # uses levels to order coefficients
  for (i in seq_along(re_var_names)) {
    if (!inherits(model$model[, re_var_names[i]], "factor")) {
      stop("Specified random effect is not a factor, aborting.
           You are on your own.")
    }
    re_levels[[i]] <- levels(model$model[, re_var_names[i]])
  }

  gam_coef <- stats::coef(model)

  # for later
  # if (!rlang::is_null(re)) {
  #   stopifnot(rlang::is_character(re))
  #   # ...
  # }

  # issue, parenthesis in the names means problematic regex matching so remove
  # all but key part of pattern
  re_label_base <- stringr::str_remove(re_labels, "s") # remove first s
  re_label_base <- stringr::str_remove_all(re_label_base, "\\(|\\)") # remove parenthesis

  re_coef <- stringr::str_detect(names(gam_coef),
    pattern = paste0(re_label_base, collapse = "|")
  )

  re0 <- gam_coef[re_coef]

  gam_se <- sqrt(diag(model$Vp)) # no names
  gam_se <- gam_se[names(gam_coef) %in% names(re0)]

  # clean up names
  names(re0) <- stringr::str_remove(names(re0), pattern = "s\\(|\\)") # for some reason this won't do the latter
  names(re0) <- stringr::str_remove(names(re0), pattern = "\\)")
  names(re0) <- stringr::str_replace(names(re0), pattern = ",", replacement = "|")
  names(re0) <- stringr::str_remove(names(re0), pattern = "\\.[0-9]+")

  re_n <- dplyr::n_distinct(names(re0)) # possible use later
  re_names <- names(re0)

  re <- data.frame(
    component = re_names,
    group = unlist(re_levels),
    re = re0,
    se = gam_se,
    lower = re0 - 1.96 * gam_se,
    upper = re0 + 1.96 * gam_se
  )

  if (tibble) {
    tibble::as_tibble(re)
  } else {
    re
  }
}
