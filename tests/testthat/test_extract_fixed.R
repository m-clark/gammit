context('test extract fixed effects')

ga_model = mgcv::gam(Reaction ~  Days + s(Subject, bs='re') + s(Days, Subject, bs='re'),
                     data = lme4::sleepstudy,
                     method = 'REML')

test_that('Works on single lme model', {
  expect_s3_class(extract_fixed(ga_model), 'data.frame')
})

test_that('Fails with non-gam', {
  expect_error(extract_fixed(lm(mpg ~ wt, mtcars)))
})


test_that('extract_fixed_effects.gam handles no ci', {
  expect_s3_class(extract_fixed(ga_model, ci_level = 0), 'data.frame')
})


test_that('extract_fixed_effects.gam handles digits', {
  expect_s3_class(extract_fixed(ga_model, digits = 2), 'data.frame')
})
