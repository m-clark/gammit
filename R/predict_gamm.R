#' Make predictions with or without random effects
#'
#' @description Use predict in an `lme4` style on gam/bam objects from \code{\link{mgcv}}.
#' @param model A gam class model from the \code{\link{mgcv}} package.
#' @param newdata Data on which to predict on. Empty by default.
#' @param re_form `NULL`, `NA`, or character string. If `NULL` (default), all
#'   random effects will be used.  If NA, no random effects will be used. If
#'   character, must be of the form `"s(varname)"`, where `varname` would be the
#'   name of the grouping variable pertaining to the random effect.  Appended to
#'   `include`.
#' @param se Logical.  Include standard errors or not. Default is FALSE.
#' @param include Which random effects to include in prediction. See
#'   \code{\link{predict.gam}} for details.
#' @param exclude Which random effects to exclude in prediction. See
#'   \code{\link{predict.gam}} for details.
#' @param keep_prediction_data  Keep the model frame or newdata that was used in
#'   the prediction in final output? Default is FALSE.
#' @param ... Other arguments for \code{\link{predict.gam}}.
#'
#' @details This is a wrapper for \code{\link{predict.gam}}.  The goal is to
#'   have similar functionality with predict function in `lme4`, which makes it
#'   easy to drop all random effects or include specific ones.  Some of this
#'   functionality is not yet available for class `bam`.
#'
#' @return A data frame of predictions and possibly standard errors.
#'
#' @seealso \code{\link{predict.gam}}
#'
#' @importFrom stats predict
#' @examples
#' library(lme4)
#' library(mgcv)
#' lmer_model <- lmer(Reaction ~ Days + (Days || Subject), data = sleepstudy)
#' ga_model <- gam(Reaction ~ Days + s(Subject, bs = "re") + s(Days, Subject, bs = "re"),
#'   data = sleepstudy,
#'   method = "REML"
#' )
#'
#' head(
#'   data.frame(
#'     lmer = predict(lmer_model),
#'     gam = predict_gamm(ga_model)
#'   )
#' )
#'
#' head(
#'   cbind(
#'     lmer = predict(lmer_model, re.form = NA),
#'     gam1 = predict_gamm(ga_model, re_form = NA),
#'     gam2 = predict_gamm(ga_model,
#'       exclude = c("s(Subject)", "s(Days,Subject)")
#'     )
#'   )
#' )
#'
#' head(predict_gamm(ga_model, se = TRUE))
#' @export
predict_gamm <- function(
                         model,
                         newdata,
                         re_form = NULL,
                         se = FALSE,
                         include = NULL,
                         exclude = NULL,
                         keep_prediction_data = FALSE,
                         ...) {

  # Note because predict doesn't use NULL, can't use NULL for new_data arg or
  # even a differently named arg, and I'm not going into the weeds of rlang to
  # find a hack.

  # basic checks
  if (!inherits(model, "gam")) stop("Need a gam object.")

  if (!rlang::is_null(include) && !rlang::is_character(include)) {
    stop("include must be NULL or character.")
  }

  if (!rlang::is_null(exclude) && !rlang::is_character(exclude)) {
    stop("exclude must be NULL or character.")
  }

  if (!rlang::is_null(re_form) &&
    !rlang::is_na(re_form) &
    !rlang::is_character(re_form)) {
    stop("re_form must be NULL, NA, or character.")
  }

  if (any(include %in% exclude)) {
    stop("You can't include and exclude the same thing.")
  }

  if (!rlang::is_logical(se)) {
    stop("se must be TRUE or FALSE")
  }

  if (!rlang::is_logical(keep_prediction_data)) {
    stop("keep_prediction_data must be TRUE or FALSE")
  }

  # standard prediction would simply call predict.gam
  if (rlang::is_null(re_form) | rlang::is_character(re_form)) {
    if (rlang::is_null(re_form)) {
      preds <- predict(model,
        newdata,
        se = se,
        terms = include,
        exclude = exclude,
        ...
      )
    } else {
      preds <- predict(model,
        newdata,
        se = se,
        terms = c(include, re_form),
        exclude = exclude,
        ...
      )
    }
  } else if (rlang::is_na(re_form)) {

    # FE only
    re_terms <- sapply(model$smooth, function(x) inherits(x, "random.effect"))
    re_terms <- sapply(model$smooth[re_terms], function(x) x$label)

    preds <- predict(model,
      newdata,
      se = se,
      terms = include,
      exclude = c(re_terms, exclude),
      ...
    )
  }

  if (se) {
    preds <- data.frame(prediction = preds$fit, se = preds$se)
  } else {
    preds <- data.frame(prediction = preds)
  }

  if (keep_prediction_data) {
    if (missing(newdata)) {
      base <- model$model
    } else {
      base <- newdata
    }
    preds <- data.frame(base, preds)
  }

  preds
}
