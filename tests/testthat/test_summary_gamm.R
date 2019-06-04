context('summary_gamm')

ga_model = mgcv::gam(Reaction ~  Days + s(Subject, bs='re') + s(Days, Subject, bs='re'),
                     data = lme4::sleepstudy,
                     method = 'REML')





# test standard lme with single cluster var -------------------------------

test_that('Works on single cluster lme model', {
  expect_type(summary_gamm(ga_model), 'list')
})

test_that('Fails with non-gam', {
  expect_error(summary_gamm(lm(mpg ~ wt, mtcars)))
})

test_that('Returns df for vc', {
  expect_s3_class(summary_gamm(ga_model)$vc, 'data.frame')
})

test_that('Returns df for fe', {
  expect_s3_class(summary_gamm(ga_model)$fe, 'data.frame')
})


test_that('Returns invisible', {
  expect_invisible(summary_gamm(ga_model))
})




# Test alt dist -----------------------------------------------------------

ga_model_bin = mgcv::gam(Reaction > median(Reaction) ~  Days + s(Subject, bs='re') + s(Days, Subject, bs='re'),
                         data = lme4::sleepstudy,
                         family = binomial,
                         method = 'REML')

test_that('Works on single cluster glme model', {
  expect_type(summary_gamm(ga_model_bin), 'list')
})



# Test bam ----------------------------------------------------------------

ba_model = mgcv::bam(Reaction ~  Days + s(Subject, bs='re') + s(Days, Subject, bs='re'),
                         data = lme4::sleepstudy,
                         method = 'REML')

ba_model_bin = mgcv::bam(Reaction > median(Reaction) ~  Days + s(Subject, bs='re') + s(Days, Subject, bs='re'),
                         data = lme4::sleepstudy,
                         family = binomial,
                         method = 'REML')


test_that('Bam works on single cluster lme model', {
  expect_type(summary_gamm(ba_model), 'list')
})

test_that('Works on single cluster glme model', {
  expect_type(summary_gamm(ba_model_bin), 'list')
})
