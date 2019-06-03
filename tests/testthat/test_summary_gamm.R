context('summary_gamm')

ga_model = mgcv::gam(Reaction ~  Days + s(Subject, bs='re') + s(Days, Subject, bs='re'),
                     data = lme4::sleepstudy,
                     method = 'REML')

test_that('Works on single lme model', {
  expect_type(summary_gamm(ga_model), 'list')
})

test_that('Returns invisible', {
  expect_invisible(summary_gamm(ga_model))
})
