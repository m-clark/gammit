context('test extract variance components')


ga_model = mgcv::gam(Reaction ~  Days + s(Subject, bs='re') + s(Days, Subject, bs='re'),
                     data = lme4::sleepstudy,
                     method = 'REML')

test_that('Works on single lme model', {
  expect_s3_class(extract_vc(ga_model), 'data.frame')
})

test_that('Fails with non-gam', {
  expect_error(extract_vc(lm(mpg ~ wt, mtcars)))
})

test_that('Fails with non-REML', {
  ga_model_noREML = mgcv::gam(Reaction ~  Days + s(Subject, bs='re') + s(Days, Subject, bs='re'),
                              data = lme4::sleepstudy)
  expect_error(extract_vc(ga_model_noREML))
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
  expect_s3_class(extract_vc(ba_model), 'data.frame')
})

test_that('Works on single cluster glme model', {
  expect_s3_class(extract_vc(ba_model_bin), 'data.frame')
})
