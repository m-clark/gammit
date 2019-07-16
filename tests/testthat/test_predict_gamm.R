context('test predict_gamm')

ga_model = mgcv::gam(Reaction ~  Days
                     + s(Subject, bs='re')
                     + s(Days, Subject, bs='re'),
                     data = lme4::sleepstudy,
                     method = 'REML')

sleepstudy = lme4::sleepstudy
nd = sleepstudy[sample(1:180, 10),]

# Basic checks
test_that('Works on single lme model', {
  expect_s3_class(predict_gamm(ga_model), 'data.frame')
})

test_that('Fails with non-gam', {
  expect_error(predict_gamm(lm(mpg ~ wt, mtcars)))
})

test_that('Fails with non-character for include', {
  expect_error(predict_gamm(ga_model, include = TRUE))
})

test_that('Fails with non-character for exclude', {
  expect_error(predict_gamm(ga_model, exclude = TRUE))
})

test_that('Fails include in exclude', {
  expect_error(predict_gamm(ga_model, include = 's(Subject)', exclude = 's(Subject)'))
})

test_that('Fails with non-character for re_form', {
  expect_error(predict_gamm(ga_model, re_form = TRUE))
})

test_that('Fails with non-logical for se', {
  expect_error(predict_gamm(ga_model, se = 'sure!'))
})

test_that('Fails with non-logical for keep_prediction_data', {
  expect_error(predict_gamm(ga_model, keep_prediction_data = 'sure!'))
})


# Other predictions -------------------------------------------------------

# newdata
test_that('Ok for new data', {
  expect_s3_class(predict_gamm(ga_model, newdata = nd), 'data.frame')
})


# random/fixed effects
test_that('Can take specific re_form', {
  expect_s3_class(predict_gamm(ga_model, newdata = nd, re_form = 's(Subject)'), 'data.frame')
})

test_that('Can take NA for re_form', {
  expect_s3_class(predict_gamm(ga_model, newdata = nd, re_form = NA), 'data.frame')
})

# keeps data

test_that('Can take arg for keep_prediction_data', {
  expect_equal(
    colnames(predict_gamm(
      ga_model,
      newdata = nd,
      keep_prediction_data = TRUE
    )),
    c(colnames(nd), 'prediction')
  )
})

test_that('Can take arg for keep_prediction_data', {
  expect_equal(
    colnames(predict_gamm(
      ga_model,
      keep_prediction_data = TRUE
    )),
    c(colnames(ga_model$model), 'prediction')
  )
})

# Other misc --------------------------------------------------------------

# can do se

test_that('Test se', {
  expect_s3_class(predict_gamm(ga_model, se = TRUE), 'data.frame')
})

# compare to lme4

library(lme4); library(mgcv)
lmer_model = lmer(Reaction ~  Days + (Days || Subject), data=sleepstudy)
ga_model = gam(Reaction ~  Days + s(Subject, bs='re')
               + s(Days, Subject, bs='re'),
               data=sleepstudy,
               method = 'REML')

cor_pred1 = cor(predict(lmer_model), predict_gamm(ga_model))


fe_only = cbind(
  lmer = predict(lmer_model, re.form = NA),
  gam1 = predict_gamm(ga_model, re_form = NA),
  gam2 = predict_gamm(ga_model,
                      exclude = c('s(Subject)', "s(Days,Subject)"))
)

#
# head(
#   cbind(predict_gamm(ga_model, include = 'Days'),
#         predict(lm(Reaction ~ Days, sleepstudy))
#   ))

test_that('Can take NA for re_form', {
  expect_s3_class(predict_gamm(ga_model, newdata = nd, re_form = NA), 'data.frame')
})


test_that('Predictions reflect lme4', {
  expect_gt(cor_pred1, .99)
})

test_that('Predictions reflect lme4', {
  expect_equal(fe_only[,1], c(fe_only[,2]))
})

test_that('exclude can equal re_form = NA', {
  expect_equal(fe_only[,2], fe_only[,3])
})
