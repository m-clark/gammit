context('test extract variance components')


ga_model = mgcv::gam(Reaction ~  Days + s(Subject, bs='re') + s(Days, Subject, bs='re'),
                     data = lme4::sleepstudy,
                     method = 'REML')



# Test gam ----------------------------------------------------------------



test_that('Fails with non-gam', {
  expect_error(extract_vc(lm(mpg ~ wt, mtcars)))
})

test_that('Fails with non-REML', {
  ga_model_noREML = mgcv::gam(Reaction ~  Days + s(Subject, bs='re') + s(Days, Subject, bs='re'),
                              data = lme4::sleepstudy)
  expect_error(extract_vc(ga_model_noREML))
})


test_that('Works on single lme model', {
  expect_s3_class(extract_vc(gam_1), 'data.frame')
})


test_that('extract_vc.gam basic functionality: random slopes', {
  expect_s3_class(extract_vc(gam_2), 'data.frame')
})

test_that('extract_vc.gam basic functionality: multiple grouping factors', {
  expect_s3_class(extract_vc(gam_3), 'data.frame')
})

test_that('extract_vc.gam basic functionality: bam', {
  expect_s3_class(extract_vc(bam_1), 'data.frame')
})

test_that('extract_vc.gam basic functionality: correct results', {
  invisible(
    utils::capture.output(
      raw_output <- mgcv::gam.vcomp(gam_1)[,'std.dev']
    )
  )
  names(raw_output) = NULL
  expect_equal(extract_vc(gam_1, digits = 10)$sd, raw_output)
})

test_that('extract_vc.gam basic functionality: correct results', {
  invisible(
    utils::capture.output(
      raw_output <- mgcv::gam.vcomp(gam_2)[,'std.dev']
    )
  )
  names(raw_output) = NULL
  expect_equal(extract_vc(gam_2, digits = 10)$sd, raw_output)
})

test_that('extract_vc.gam works with ci_scale = var and alternate ci_level', {
  invisible(
    utils::capture.output(
      raw_output <- mgcv::gam.vcomp(gam_2, conf.lev = .9)[,'upper']
    )
  )
  names(raw_output) = NULL
  expect_equal(extract_vc(gam_2, ci_level = .9, digits = 10)$sd_95, raw_output)
})


test_that('extract_vc.gam works with ci_scale = var', {
  expect_type(extract_vc(gam_1, ci_scale = 'var')$var_2.5, 'double')
  expect_type(extract_vc(gam_1, ci_scale = 'sd')$sd_2.5, 'double')
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
