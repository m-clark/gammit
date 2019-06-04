context('summary_gamm')

ga_model = mgcv::gam(Reaction ~  Days + s(Subject, bs='re') + s(Days, Subject, bs='re'),
                     data = lme4::sleepstudy,
                     method = 'REML')

ga_model_bin = mgcv::gam(Reaction > median(Reaction) ~  Days + s(Subject, bs='re') + s(Days, Subject, bs='re'),
                         data = lme4::sleepstudy,
                         family = binomial,
                         method = 'REML')

test_that('Works on single cluster lme model', {
  expect_type(summary_gamm(ga_model), 'list')
})

test_that('Returns df for vc', {
  expect_s3_class(summary_gamm(ga_model)$vc, 'data.frame')
})

test_that('Returns df for fe', {
  expect_s3_class(summary_gamm(ga_model)$fe, 'data.frame')
})

test_that('Works on single cluster lme model', {
  expect_type(summary_gamm(ga_model_bin), 'list')
})

test_that('Returns invisible', {
  expect_invisible(summary_gamm(ga_model))
})
