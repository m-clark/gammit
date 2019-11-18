#' Extract random effects from a gam
#'
#' @description Extract what would be the random effects from a mixed model from
#'   a gam object.  Assumes an mgcv model of the form `gam(... + s(g,
#'   bs='re'))`.
#'
#' @inheritParams extract_vc
#' @param re Filter results to a specific random effect/grouping variable.
#'
#' @details Returns a data frame of the the `component` type, the estimated
#'   random effect `re`, the estimated `se`, and approximate `lower` and `upper`
#'   bounds.  Note that the standard errors are Bayesian
#'   estimates (see \code{\link{gamObject}}, specifically type `Vp`).  The `re`
#'   will only reflect smooth terms whose basis function is 're', i.e. of class
#'   `random.effect`.  Others will be ignored.
#'
#' @return A `tibble` or data frame with the random effect,
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
#' @importFrom purrr map_lgl map_chr is_empty map
#' @importFrom dplyr n_distinct tibble
#' @importFrom stringr str_detect
#'
#' @export
extract_ranef <- function(
  model,
  re = NULL,
  ci_level = .95,
  digits = 3
) {

  if (!inherits(model, "gam")) stop("Need a gam object.")

  # get the re variables and their levels
  re_terms <- purrr::map_lgl(model$smooth,
                             function(x)
                               inherits(x, "random.effect"))

  re_names <- purrr::map_chr(model$smooth[re_terms],
                             function(x)
                               ifelse(length(x$vn) == 1,
                                      x$vn,
                                      x$vn[length(x$vn)]))

  re_levels <- vector("list", length(re_names))

  # add check on re name/type
  # check that re is factor as re smooth can be applied to continuous
  for (i in seq_along(re_names)) {
    if (!inherits(model$model[, re_names[i]], "factor")) {
      warning(
        paste0(re_names[i], ' is not a factor. No results provided for it.')
      )
      re_levels[[i]] <- NULL
    }
    else {
      re_levels[[i]] <- levels(model$model[, re_names[i]])
    }
  }


  if (purrr::is_empty(re_levels) | all(purrr::map_lgl(re_levels, is.null))) {
    stop('No factor random effects.')
  }

  non_factors <- purrr::map_lgl(re_levels, is.null)

  # this test is covered but covr ignores for some reason
  if (any(non_factors)) {
    re_terms[non_factors] <- FALSE
  }

  if (!is.null(re) && !re %in% re_names)
    stop(
      paste0('re is not among the names of the random effects: ',
             paste0(re_names, collapse = ' ')
      )
    )

  re_labels <- purrr::map(model$smooth[re_terms], function(x) x$label)

  gam_coef <- stats::coef(model)

  # issue, parenthesis in the names means problematic regex matching so remove
  # all but key part of pattern
  re_label_base <- gsub(re_labels, pattern = "s\\(", replacement = '') # remove first s
  re_label_base <- gsub(re_label_base, pattern = "\\(|\\)", replacement = '') # remove parenthesis

  re_coef <- grepl(names(gam_coef), pattern = paste0('^s\\(', re_label_base, collapse = "|"))

  re0 <- gam_coef[re_coef]

  gam_se <- sqrt(diag(model$Vp)) # no names
  gam_se <- gam_se[names(gam_coef) %in% names(re0)]

  # clean up names
  names(re0) <- gsub(names(re0), pattern = "s\\(|\\)", replacement = '')
  names(re0) <- gsub(names(re0), pattern = "\\.[0-9]+", replacement = '')

  re_n <- dplyr::n_distinct(names(re0)) # possible use later
  re_names <- names(re0)

  random_effects <- dplyr::tibble(effect = re_names) %>%
    dplyr::mutate(
      group_var = split_group_effect(effect, which = 2),
      effect = split_group_effect(effect, which = 1),
      effect = ifelse(effect ==  group_var, 'Intercept', effect),
      group = unlist(re_levels),
      value = re0,
      se = gam_se
    )

  if (ci_level > 0) {

    lower = (1 - ci_level)/2
    upper = 1 - lower
    mult <- stats::qnorm(upper)

    random_effects <- random_effects %>%
      dplyr::mutate(
        lower = value - mult * se,
        upper = value + mult * se
      )

    colnames(random_effects)[colnames(random_effects) %in% c('lower', 'upper')] <-
      paste0(c('lower_', 'upper_'), c(lower, upper) * 100)
  }

  if (!is.null(re)) {
    random_effects <- random_effects %>%
      dplyr::filter(group_var == re)
  }

  random_effects %>%
    dplyr::select(group_var, effect, dplyr::everything()) %>%
    dplyr::mutate_if(is.numeric, round, digits = digits)
}


remove_parens <- function(x) {
  colnames(x) <- gsub(colnames(x), pattern = '[\\(,\\)]', replacement = '')
  rownames(x) <- colnames(x)
  x
}


split_group_effect <- function(x, which = 1) {
  init = strsplit(x, split = ",")
  purrr::map_chr(init, function(x) x[min(which, length(x))])
}
