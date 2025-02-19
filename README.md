
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gammit

This package was specifically for mixed models using mgcv. At present it
has been almost entirely superseded by the
[mixedup](https://m-clark.github.io/mixedup/) package, which provides
all the same functionality and more, while the visualizations were
originally from the [visibly](https://m-clark.github.io/visibly/)
package anyway. That only leaves the prediction functionality as unique
to this package, so I leave it here as I may still use it for that, and
I’m contemplating moving the GAM specific visuals from visibly at some
point.

# Package Description

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/m-clark/gammit.svg?branch=master)](https://travis-ci.org/m-clark/gammit)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/m-clark/gammit?branch=master&svg=true)](https://ci.appveyor.com/project/m-clark/gammit)
[![Codecov test
coverage](https://codecov.io/gh/m-clark/gammit/branch/master/graph/badge.svg)](https://codecov.io/gh/m-clark/gammit?branch=master)
[![Lifecycle:
superseded](https://img.shields.io/badge/lifecycle-superseded-orange.svg)](https://www.tidyverse.org/lifecycle/#superseded)
<!-- badges: end -->

## Introduction

The goal of gammit is to provide a set of functions to aid using
<span class="pack" style="">mgcv</span> (possibly solely) for mixed
models. Lately I’ve been using it in lieu of
<span class="pack" style="">lme4</span>, especially the
<span class="func" style="">bam</span> function, for GLMM with millions
of observations and multiple random effects. It’s turning out very
useful in this sense (see [this
post](https://m-clark.github.io/posts/2019-10-20-big-mixed-models/) for
details), but I’d like some more/different functionality with the
results. Furthermore, <span class="pack" style="">mgcv</span> just has
some nice things going on for such models anyway, like the ability to
add other smooth terms, alternative distributions for the target
variable, etc., so I’m looking to make it easier for me to get some
things I want when I use it.

At present there are four functions:
<span class="func" style="">extract\_vc</span>,
<span class="func" style="">extract\_ranef</span>,
<span class="func" style="">extract\_fixed</span>,
<span class="func" style="">summary\_gamm</span>, and
<span class="func" style="">predict\_gamm.</span>

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("m-clark/gammit")
```

## Example

This example demonstrates the
<span class="func" style="">summary\_gamm</span> function with
comparison to the corresponding <span class="pack" style="">lme4</span>
model.

``` r
library(mgcv)
Loading required package: nlme
This is mgcv 1.8-31. For overview type 'help("mgcv-package")'.
library(lme4)
Loading required package: Matrix

Attaching package: 'lme4'
The following object is masked from 'package:nlme':

    lmList
library(gammit)

lmer_model = lmer(Reaction ~  Days + (Days || Subject), data=sleepstudy)

ga_model = gam(Reaction ~  Days + s(Subject, bs='re') + s(Days, Subject, bs='re'),
               data=sleepstudy,
               method = 'REML')

summary(lmer_model)
Linear mixed model fit by REML ['lmerMod']
Formula: Reaction ~ Days + ((1 | Subject) + (0 + Days | Subject))
   Data: sleepstudy

REML criterion at convergence: 1743.7

Scaled residuals: 
    Min      1Q  Median      3Q     Max 
-3.9626 -0.4625  0.0204  0.4653  5.1860 

Random effects:
 Groups    Name        Variance Std.Dev.
 Subject   (Intercept) 627.57   25.051  
 Subject.1 Days         35.86    5.988  
 Residual              653.58   25.565  
Number of obs: 180, groups:  Subject, 18

Fixed effects:
            Estimate Std. Error t value
(Intercept)  251.405      6.885  36.513
Days          10.467      1.560   6.712

Correlation of Fixed Effects:
     (Intr)
Days -0.184


summary_gamm(ga_model)

Variance components:
    group    effect variance     sd sd_2.5 sd_97.5 var_prop
  Subject Intercept  627.571 25.051 16.085  39.015    0.477
  Subject      Days   35.858  5.988  4.025   8.908    0.027
 Residual            653.582 25.565 22.792  28.676    0.496


Fixed Effects:
        Term Estimate Std. Error t value Pr(>|t|)
 (Intercept)  251.405      6.885  36.513    0.000
        Days   10.467      1.560   6.712    0.000
```

Extract the variance components with
<span class="func" style="">extract\_vc</span>.

``` r
data.frame(VarCorr(lmer_model))
        grp        var1 var2      vcov     sdcor
1   Subject (Intercept) <NA> 627.56905 25.051328
2 Subject.1        Days <NA>  35.85838  5.988187
3  Residual        <NA> <NA> 653.58350 25.565279


extract_vc(ga_model)
     group    effect variance     sd sd_2.5 sd_97.5 var_prop
1  Subject Intercept  627.571 25.051 16.085  39.015    0.477
2  Subject      Days   35.858  5.988  4.025   8.908    0.027
3 Residual            653.582 25.565 22.792  28.676    0.496
```

Extract the random effects with
<span class="func" style="">extract\_ranef</span>.

``` r
ranef(lmer_model)
$Subject
    (Intercept)        Days
308   1.5126648   9.3234970
309 -40.3738728  -8.5991757
310 -39.1810279  -5.3877944
330  24.5189244  -4.9686503
331  22.9144470  -3.1939378
332   9.2219759  -0.3084939
333  17.1561243  -0.2872078
334  -7.4517382   1.1159911
335   0.5787623 -10.9059754
337  34.7679030   8.6276228
349 -25.7543312   1.2806892
350 -13.8650598   6.7564064
351   4.9159912  -3.0751356
352  20.9290332   3.5122123
369   3.2586448   0.8730514
370 -26.4758468   4.9837910
371   0.9056510  -1.0052938
372  12.4217547   1.2584037

with conditional variances for "Subject" 


extract_ranef(ga_model)
# A tibble: 36 x 7
   group_var effect    group   value    se lower_2.5 upper_97.5
   <chr>     <chr>     <chr>   <dbl> <dbl>     <dbl>      <dbl>
 1 Subject   Intercept 308     1.51   13.3    -24.5        27.5
 2 Subject   Intercept 309   -40.4    13.3    -66.4       -14.3
 3 Subject   Intercept 310   -39.2    13.3    -65.2       -13.2
 4 Subject   Intercept 330    24.5    13.3     -1.51       50.5
 5 Subject   Intercept 331    22.9    13.3     -3.11       48.9
 6 Subject   Intercept 332     9.22   13.3    -16.8        35.2
 7 Subject   Intercept 333    17.2    13.3     -8.87       43.2
 8 Subject   Intercept 334    -7.45   13.3    -33.5        18.6
 9 Subject   Intercept 335     0.579  13.3    -25.4        26.6
10 Subject   Intercept 337    34.8    13.3      8.74       60.8
# … with 26 more rows
```

Extract the fixed effects
<span class="func" style="">extract\_fixef</span>.

``` r
fixef(lmer_model)
(Intercept)        Days 
  251.40510    10.46729 


extract_fixed(ga_model)
# A tibble: 2 x 7
  term      value    se     t     p lower_2.5 upper_97.5
  <chr>     <dbl> <dbl> <dbl> <dbl>     <dbl>      <dbl>
1 Intercept 251.   6.88 36.5      0    238.        265. 
2 Days       10.5  1.56  6.71     0      7.39       13.5
```

## Prediction

There are a couple of ways to do prediction, and the main goal for
gammit was to make it easy to use the
<span class="pack" style="">lme4</span> style to include random effects
or not. <span class="pack" style="">mgcv</span> already has this
functionality as well, so the functionality of
<span class="func" style="">predict\_gamm</span> is mostly cosmetic. One
benefit here is to provide standard errors for the prediction also.

``` r
head(predict_gamm(ga_model))
  prediction
1   252.9178
2   272.7086
3   292.4994
4   312.2901
5   332.0809
6   351.8717
```

Add standard errors.

``` r
head(data.frame(predict_gamm(ga_model, se=T)))
  prediction        se
1   252.9178 12.410220
2   272.7086 10.660891
3   292.4994  9.191224
4   312.2901  8.153871
5   332.0809  7.724998
6   351.8717  8.003034
```

``` r
compare = data.frame(
  gam_original  = predict_gamm(ga_model)$prediction,
  gam_fe_only   = predict_gamm(ga_model, re_form = NA)$prediction,
  gam_fe_only2  = predict_gamm(ga_model, 
                               exclude =  c('s(Subject)', "s(Days,Subject)"))$prediction,
  lme4_fe_only  = predict(lmer_model, re.form = NA))

head(compare)
  gam_original gam_fe_only gam_fe_only2 lme4_fe_only
1     252.9178    251.4051     251.4051     251.4051
2     272.7086    261.8724     261.8724     261.8724
3     292.4994    272.3397     272.3397     272.3397
4     312.2901    282.8070     282.8070     282.8070
5     332.0809    293.2742     293.2742     293.2742
6     351.8717    303.7415     303.7415     303.7415
```

Along with that, one can still use include/exclude for other smooth
terms as above. Unfortunately, some options do not yet work with
<span class="objclass" style="">bam</span> objects, but this is to due
to the functionality in <span class="func" style="">predict.gam</span>
from <span class="pack" style="">mgcv</span> and should change in the
near future.
