context('test extract random effects')

ga_model = mgcv::gam(Reaction ~  Days + s(Subject, bs='re') + s(Days, Subject, bs='re'),
                     data = lme4::sleepstudy,
                     method = 'REML')

test_that('Works on single lme model', {
  expect_s3_class(extract_ranef(ga_model), 'data.frame')
})

test_that('Fails with non-gam', {
  expect_error(extract_ranef(lm(mpg ~ wt, mtcars)))
})



ss2 = lme4::sleepstudy %>%
  dplyr::mutate(id = factor(rep(rev(letters[1:10]), e=18)))

ga_model_2gr = mgcv::gam(Reaction ~  Days +
                           s(Subject, bs='re') +
                           s(Days, Subject, bs='re') +
                           s(id, bs = 're'),
                         data = ss2,
                         method = 'REML')

test_that('Works on multiple re lme model', {
  expect_s3_class(extract_ranef(ga_model_2gr), 'data.frame')
})




test_that('Ranef reflect lme4', {
  lmer_re = unlist(lme4::ranef(lme4::lmer(Reaction ~  Days
                                          + (Days || Subject)
                                          + (1|id),
                              data=ss2)))

  cor_re = cor(lmer_re, extract_ranef(ga_model_2gr)$value)
  expect_gt(cor_re, .99)
})

test_that('Ranef do not include other smooths (for now)', {

  ga_model_sm = mgcv::gam(Reaction ~  s(Days) + s(Subject, bs='re') + s(Days, Subject, bs='re'),
                          data = lme4::sleepstudy,
                          method = 'REML')

  expect_s3_class(extract_ranef(ga_model_sm), 'data.frame')
  expect_equal(nrow(extract_ranef(ga_model_sm)),  36)

})

test_that('Fails if RE is no factors', {

  ga_model_num_re = mgcv::gam(Reaction ~  s(Days) + s(Subject, bs='re') + s(Days, Subject, bs='re'),
                              data = within(lme4::sleepstudy, {Subject = as.integer(Subject)}),
                              method = 'REML')

  expect_error(suppressWarnings(extract_ranef(ga_model_num_re)))

})
