# Run mgcv models ---------------------------------------------------------

load('mgcv_results.RData')

# library(mgcv)
#
# gam_1 = gam(Reaction ~  Days + s(Subject, bs = 're'),
#                   data = lme4::sleepstudy,
#                   method = 'REML')
#
# gam_2 = gam(
#   Reaction ~  Days +
#     s(Subject, bs = 're') +
#     s(Days, Subject, bs = 're'),
#   data = lme4::sleepstudy,
#   method = 'REML'
# )
#
# gam_3 <- gam(
#   y ~ service +
#     s(s, bs = 're') +
#     s(dept, bs = 're'),
#   method = 'REML',
#   data = lme4::InstEval[1:1000, ]
# )
#
# # bam objects are very large to save even for small models
# bam_1 = bam(
#   y ~ service +
#     s(d, bs = 're') +
#     s(dept, bs = 're'),
#   data = lme4::InstEval[1:1000, ],
#   nthreads = 10
#   )
#
# ## TODO: glm
#
# save(
#   gam_1,
#   gam_2,
#   gam_3,
#   bam_1,
#   file = 'tests/testthat/mgcv_results.RData'
# )
