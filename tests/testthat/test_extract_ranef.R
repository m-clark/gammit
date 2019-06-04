context('test extract random effects')

ga_model = mgcv::gam(Reaction ~  Days + s(Subject, bs='re') + s(Days, Subject, bs='re'),
                     data = lme4::sleepstudy,
                     method = 'REML')

test_that('Works on single lme model', {
  expect_s3_class(extract_ranef(ga_model), 'data.frame')
})

test_that('Takes tibble arg', {
  expect_failure(
    expect_s3_class(extract_ranef(ga_model, tibble = FALSE), 'tibble')
  )
})
