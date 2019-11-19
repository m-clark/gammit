context('test extract_ranef')

test_that('Fails with non-gam', {
  expect_error(extract_ranef(lm(mpg ~ wt, mtcars)))
})

test_that('extract_ranef.gam basic functionality', {
  expect_s3_class(extract_ranef(gam_1), 'data.frame')
})

test_that('extract_ranef.gam basic functionality', {
  expect_s3_class(extract_ranef(gam_2), 'data.frame')
})

test_that('extract_vc.gam basic functionality: bam', {
  expect_s3_class(extract_ranef(bam_1), 'data.frame')
})

test_that('extract_ranef.gam works with multiple re', {
  expect_equal(
    nrow(extract_ranef(gam_3, re = 's')),
    nlevels(gam_3$model$s)
  )
})

test_that('extract_ranef.gam errors with bad re name', {
  expect_error(extract_ranef(gam_2, re = 'subject'))
})

test_that('extract_ranef.gam errors with only non-factor random effects',
          {
            d = mgcv::gamSim(n = 100, verbose = FALSE)
            m = mgcv::gam(y ~ s(x1, bs = 're'), data = d)
            expect_error(suppressWarnings(extract_ranef(m)))
          })

test_that('extract_ranef.gam warns with non-factor random effects', {
  set.seed(4)
  d = mgcv::gamSim(n = 100, verbose = FALSE)
  nb <- 10; n <- 100
  b <- rnorm(nb) * 2 ## random effect
  r <- sample(1:nb, n, replace = TRUE) ## r.e. levels
  y <- 2 + b[r] + rnorm(n)
  r <- factor(r)
  m = mgcv::gam(y ~ s(x1, bs = 're') + s(r, bs = 're'),
                data = cbind(d, r),
                method = 'REML')
  expect_warning(extract_ranef(m))
})

test_that('Fails if no factors', {

  ga_model_num_re = mgcv::gam(Reaction ~  s(Days) + s(Subject, bs='re') + s(Days, Subject, bs='re'),
                              data = within(lme4::sleepstudy, {Subject = as.integer(Subject)}),
                              method = 'REML')

  expect_error(suppressWarnings(extract_ranef(ga_model_num_re)))

})

test_that('extract_ranef.brmsfit correct output', {
  expect_equal(
    nrow(extract_ranef(gam_2, re = 'Subject')),
    nlevels(lme4::sleepstudy$Subject)*2
  )
})


test_that('Ranef reflect lme4', {
  lmer_re = unlist(lme4::ranef(lme4::lmer(Reaction ~  Days
                                          + (Days || Subject),
                              data=lme4::sleepstudy)))

  cor_re = cor(lmer_re, extract_ranef(gam_2)$value)
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

