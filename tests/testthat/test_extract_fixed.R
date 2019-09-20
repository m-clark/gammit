context('test extract fixed effects')

ga_model = mgcv::gam(Reaction ~  Days + s(Subject, bs='re') + s(Days, Subject, bs='re'),
                     data = lme4::sleepstudy,
                     method = 'REML')

test_that('Works on single lme model', {
  expect_s3_class(extract_fixed(ga_model, tibble = FALSE), 'data.frame')
})

test_that('Fails with non-gam', {
  expect_error(extract_fixed(lm(mpg ~ wt, mtcars)))
})

test_that('Takes tibble arg', {
  expect_failure(
    expect_s3_class(extract_fixed(ga_model, tibble = TRUE), 'tibble')
  )
})

