
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gammit

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/m-clark/gammit.svg?branch=master)](https://travis-ci.org/m-clark/gammit)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/m-clark/gammit?branch=master&svg=true)](https://ci.appveyor.com/project/m-clark/gammit)
[![Codecov test
coverage](https://codecov.io/gh/m-clark/gammit/branch/master/graph/badge.svg)](https://codecov.io/gh/m-clark/gammit?branch=master)
<!-- badges: end -->

The goal of gammit is to provide a set of functions to aid using `mgcv`
(possibly solely) for mixed models. I’ve been using it in lieu of
`lme4`, especially the `bam` function, for GLMM with millions of
observations and multiple random effects. It’s turning out very useful
in this sense, but I’d like some more/different functionality with the
results. Furthermore, `mgcv` just has some nice things going on for such
models anyway, so I’m looking to make it easier for me to get some
things I want when I use it.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("m-clark/gammit")
```

## Example

This example demonstrates the summary function:

``` r
library(mgcv); library(lme4); library(gammit)
# Loading required package: nlme
# This is mgcv 1.8-28. For overview type 'help("mgcv-package")'.
# Loading required package: Matrix
# 
# Attaching package: 'lme4'
# The following object is masked from 'package:nlme':
# 
#     lmList

lmer_model = lmer(Reaction ~  Days + (1|Subject) + (0 + Days|Subject), data=sleepstudy)

ga_model = gam(Reaction ~  Days + s(Subject, bs='re') + s(Days, Subject, bs='re'),
               data=sleepstudy,
               method = 'REML')

summary(lmer_model)
# Linear mixed model fit by REML ['lmerMod']
# Formula: Reaction ~ Days + (1 | Subject) + (0 + Days | Subject)
#    Data: sleepstudy
# 
# REML criterion at convergence: 1743.7
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -3.9626 -0.4626  0.0204  0.4653  5.1860 
# 
# Random effects:
#  Groups    Name        Variance Std.Dev.
#  Subject   (Intercept) 627.50   25.050  
#  Subject.1 Days         35.86    5.989  
#  Residual              653.58   25.565  
# Number of obs: 180, groups:  Subject, 18
# 
# Fixed effects:
#             Estimate Std. Error t value
# (Intercept)  251.405      6.885  36.514
# Days          10.467      1.560   6.711
# 
# Correlation of Fixed Effects:
#      (Intr)
# Days -0.184


summary_gamm(ga_model)
# 
# Variance components:
#      component std.dev lower upper variance proportion
# 1      Subject   25.05 16.09 39.02   627.57       0.48
# 2 Days|Subject    5.99  4.03  8.91    35.86       0.03
# 3        scale   25.57 22.79 28.68   653.58       0.50
# 
# 
# Fixed Effects:
#             Estimate Std. Error t value Pr(>|t|)
# (Intercept)   251.41       6.89   36.51  < 2e-16
# Days           10.47       1.56    6.71  3.7e-10
```

Extract the variance components.

``` r
VarCorr(lmer_model)
#  Groups    Name        Std.Dev.
#  Subject   (Intercept) 25.0499 
#  Subject.1 Days         5.9887 
#  Residual              25.5652


extract_vc(ga_model)
# # A tibble: 3 x 6
#   component    std.dev lower upper variance proportion
#   <chr>          <dbl> <dbl> <dbl>    <dbl>      <dbl>
# 1 Subject        25.1  16.1  39.0     628.      0.477 
# 2 Days|Subject    5.99  4.03  8.91     35.9     0.0272
# 3 scale          25.6  22.8  28.7     654.      0.496
```

Extract the random effects.

``` r
ranef(lmer_model)
# $Subject
#     (Intercept)        Days
# 308   1.5116973   9.3237308
# 309 -40.3720066  -8.5995358
# 310 -39.1795117  -5.3880715
# 330  24.5188072  -4.9686809
# 331  22.9141930  -3.1939310
# 332   9.2217762  -0.3084673
# 333  17.1557243  -0.2871512
# 334  -7.4516633   1.1159907
# 335   0.5798353 -10.9062401
# 337  34.7661741   8.6279628
# 349 -25.7538155   1.2806250
# 350 -13.8653871   6.7565202
# 351   4.9161750  -3.0751926
# 352  20.9281601   3.5123758
# 369   3.2584763   0.8730848
# 370 -26.4756822   4.9838147
# 371   0.9057286  -1.0053150
# 372  12.4213189   1.2584805
# 
# with conditional variances for "Subject"


extract_ranef(ga_model)
# # A tibble: 36 x 6
#    component group      re    se  lower upper
#    <fct>     <fct>   <dbl> <dbl>  <dbl> <dbl>
#  1 Subject   308     1.51   13.3 -24.5   27.5
#  2 Subject   309   -40.4    13.3 -66.4  -14.3
#  3 Subject   310   -39.2    13.3 -65.2  -13.2
#  4 Subject   330    24.5    13.3  -1.51  50.5
#  5 Subject   331    22.9    13.3  -3.11  48.9
#  6 Subject   332     9.22   13.3 -16.8   35.2
#  7 Subject   333    17.2    13.3  -8.87  43.2
#  8 Subject   334    -7.45   13.3 -33.5   18.6
#  9 Subject   335     0.579  13.3 -25.4   26.6
# 10 Subject   337    34.8    13.3   8.74  60.8
# # ... with 26 more rows

data.frame(extract_ranef(ga_model))
#       component group          re       se      lower       upper
# 1       Subject   308   1.5127209 13.27913 -24.514366  27.5398077
# 2       Subject   309 -40.3739742 13.27913 -66.401061 -14.3468874
# 3       Subject   310 -39.1811090 13.27913 -65.208196 -13.1540223
# 4       Subject   330  24.5189267 13.27913  -1.508160  50.5460134
# 5       Subject   331  22.9144576 13.27913  -3.112629  48.9415443
# 6       Subject   332   9.2219858 13.27913 -16.805101  35.2490725
# 7       Subject   333  17.1561444 13.27913  -8.870942  43.1832312
# 8       Subject   334  -7.4517412 13.27913 -33.478828  18.5753455
# 9       Subject   335   0.5786996 13.27913 -25.448387  26.6057863
# 10      Subject   337  34.7679974 13.27913   8.740911  60.7950841
# 11      Subject   349 -25.7543565 13.27913 -51.781443   0.2727302
# 12      Subject   350 -13.8650381 13.27913 -39.892125  12.1620486
# 13      Subject   351   4.9159796 13.27913 -21.111107  30.9430663
# 14      Subject   352  20.9290802 13.27913  -5.098006  46.9561670
# 15      Subject   369   3.2586540 13.27913 -22.768433  29.2857407
# 16      Subject   370 -26.4758514 13.27913 -52.502938  -0.4487647
# 17      Subject   371   0.9056463 13.27913 -25.121440  26.9327331
# 18      Subject   372  12.4217779 13.27913 -13.605309  38.4488646
# 19 Days|Subject   308   9.3234834  2.67273   4.084934  14.5620333
# 20 Days|Subject   309  -8.5991559  2.67273 -13.837706  -3.3606060
# 21 Days|Subject   310  -5.3877793  2.67273 -10.626329  -0.1492295
# 22 Days|Subject   330  -4.9686478  2.67273 -10.207198   0.2699021
# 23 Days|Subject   331  -3.1939375  2.67273  -8.432487   2.0446123
# 24 Days|Subject   332  -0.3084952  2.67273  -5.547045   4.9300546
# 25 Days|Subject   333  -0.2872107  2.67273  -5.525761   4.9513392
# 26 Days|Subject   334   1.1159909  2.67273  -4.122559   6.3545407
# 27 Days|Subject   335 -10.9059600  2.67273 -16.144510  -5.6674101
# 28 Days|Subject   337   8.6276039  2.67273   3.389054  13.8661538
# 29 Days|Subject   349   1.2806922  2.67273  -3.957858   6.5192421
# 30 Days|Subject   350   6.7563993  2.67273   1.517849  11.9949492
# 31 Days|Subject   351  -3.0751321  2.67273  -8.313682   2.1634177
# 32 Days|Subject   352   3.5122033  2.67273  -1.726347   8.7507532
# 33 Days|Subject   369   0.8730495  2.67273  -4.365500   6.1115993
# 34 Days|Subject   370   4.9837889  2.67273  -0.254761  10.2223387
# 35 Days|Subject   371  -1.0052925  2.67273  -6.243842   4.2332574
# 36 Days|Subject   372   1.2583995  2.67273  -3.980150   6.4969494
```
