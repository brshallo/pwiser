
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pwiser

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of pwiser is to make applying arbitrary functions across
combinations of columns within `{dplyr}` easy. Currently, the only
function is `pairwise()`, which applies a function to all pairs of
columns.

`pairwise()` is an altered version of `dplyr::across()` and, similarly,
is meant to be used within `mutate()` / `transmute()` and `summarise()`
verbs. pwiser sprang from conversations on an [Rstudio Community
thread](https://community.rstudio.com/t/pairwise-function-for-use-within-dplyr-mutate-and-dplyr-summarise/94684)
and related conversations.

## Example within `summarise()`

``` r
library(dplyr)
library(pwiser)
library(palmerpenguins)

penguins <- na.omit(penguins)
```

`pairwise()` respects grouped dataframes:

``` r
# When using `pairwise()` within `summarise()` the function(s) applied should
# have an output length of 1 (for each group). (Though could wrap in `list()` to make a list column output.)
cor_p_value <- function(x, y){
  stats::cor.test(x, y)$p.value
}

penguins %>% 
  group_by(species) %>% 
  summarise(pairwise(contains("_mm"), 
                     cor_p_value, 
                     .is_commutative = TRUE),
            n = n())
#> # A tibble: 3 x 5
#>   species  bill_length_mm_bill~ bill_length_mm_flipp~ bill_depth_mm_flipp~     n
#>   <fct>                   <dbl>                 <dbl>                <dbl> <int>
#> 1 Adelie               1.51e- 6              4.18e- 5             1.34e- 4   146
#> 2 Chinstr~             1.53e- 9              4.92e- 5             2.16e- 7    68
#> 3 Gentoo               7.34e-16              1.80e-16             1.40e-19   119
```

Setting `.is_commutative = TRUE` can save time on redundant
calculations.

Equivalently, could have written with `.x` and `.y` in a lambda
function:

``` r
penguins %>% 
  group_by(species) %>% 
  summarise(pairwise(contains("_mm"), 
                     ~stats::cor.test(.x, .y)$p.value, 
                     .is_commutative = TRUE),
            n = n())
```

<!-- You'll still need to render `README.Rmd` regularly, to keep `README.md` up-to-date. `devtools::build_readme()` is handy for this. You could also use GitHub Actions to re-render `README.Rmd` every time you push. An example workflow can be found here: <https://github.com/r-lib/actions/tree/master/examples>. -->

## Example within `mutate()`

Can apply multiple functions via a named list:

``` r
penguins %>% 
  mutate(pairwise(contains("_mm"), 
                  list(ratio = `/`, difference = `-`),
                  .names = "features_{.fn}_{.col_x}_{.col_y}")) %>% 
  glimpse()
#> Rows: 333
#> Columns: 20
#> $ species                                              <fct> Adelie, Adelie, A~
#> $ island                                               <fct> Torgersen, Torger~
#> $ bill_length_mm                                       <dbl> 39.1, 39.5, 40.3,~
#> $ bill_depth_mm                                        <dbl> 18.7, 17.4, 18.0,~
#> $ flipper_length_mm                                    <int> 181, 186, 195, 19~
#> $ body_mass_g                                          <int> 3750, 3800, 3250,~
#> $ sex                                                  <fct> male, female, fem~
#> $ year                                                 <int> 2007, 2007, 2007,~
#> $ features_ratio_bill_length_mm_bill_depth_mm          <dbl> 2.090909, 2.27011~
#> $ features_difference_bill_length_mm_bill_depth_mm     <dbl> 20.4, 22.1, 22.3,~
#> $ features_ratio_bill_length_mm_flipper_length_mm      <dbl> 0.2160221, 0.2123~
#> $ features_difference_bill_length_mm_flipper_length_mm <dbl> -141.9, -146.5, -~
#> $ features_ratio_bill_depth_mm_bill_length_mm          <dbl> 0.4782609, 0.4405~
#> $ features_difference_bill_depth_mm_bill_length_mm     <dbl> -20.4, -22.1, -22~
#> $ features_ratio_bill_depth_mm_flipper_length_mm       <dbl> 0.10331492, 0.093~
#> $ features_difference_bill_depth_mm_flipper_length_mm  <dbl> -162.3, -168.6, -~
#> $ features_ratio_flipper_length_mm_bill_length_mm      <dbl> 4.629156, 4.70886~
#> $ features_difference_flipper_length_mm_bill_length_mm <dbl> 141.9, 146.5, 154~
#> $ features_ratio_flipper_length_mm_bill_depth_mm       <dbl> 9.679144, 10.6896~
#> $ features_difference_flipper_length_mm_bill_depth_mm  <dbl> 162.3, 168.6, 177~
```

Can use `.names` to customize outputted column names.

## Installation

Install from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("brshallo/pwiser")
```

## See Also

There are other tools in R for doing tidy pairwise operations.
[widyr](https://github.com/dgrtwo/widyr) (by David Robinson) and
[corrr](https://github.com/tidymodels/corrr) (in the `tidymodels` suite)
offer solutions (primarily) for summarising contexts
(`corrr::colpair_map()` is the closest comparison as it also supports
arbitrary functions). `recipes::step_ratio()` and
`recipes::step_interact()` can be used for making pairwise products or
ratios in mutating contexts. (See Appendix section of prior blog post on
[Tidy Pairwise
Operations](https://www.bryanshalloway.com/2020/06/03/tidy-2-way-column-combinations/#tweets)
for a few cataloged tweets on these approaches.)

The novelty of `pwiser::pairwise()` is its integration in both mutating
and summarising verbs in `{dplyr}`.

### `dplyover`

The [dplyover](https://github.com/TimTeaFan/dplyover) package offers a
wide range of extensions on `across()` for iteration problems (and was
identified after sharing the initial version of `{pwiser}`).
`dplyover::across2x()` can be used to do the same things as
`pwiser::pairwise()`. As `{dplyover}` continues to mature its interface
and improve its performance, we may eventually mark `{pwiser}` as
superseded.

## Computation Speed

*For problems with lots of data you should use more efficient
approaches.*

Matrix operations (compared to dataframes) are much more computationally
efficient for problems involving combinations (which can get big very
quickly). We’ve done nothing to optimize the computation of functions
run through pwiser.

For example, when calculating pearson correlations, `pairwise()`
calculates the correlation *separately* for each pair, whereas
`stats::cor()` (or `corrr::correlate()` which calls `cor()` under the
hood) uses R’s matrix operations to calculate all correlations
simultaneously.

``` r
library(modeldata)

data(cells)
cells_numeric <- select(cells, where(is.numeric))

dim(cells_numeric)
#> [1] 2019   56
```

Let’s do a speed test using the 56 numeric columns from the `cells`
dataset (which means 1540 pairwise combinations or 3080 permutations)
imported from `{modeltime}`.

``` r
library(corrr)
if (!requireNamespace("dplyover")) devtools::install_github('TimTeaFan/dplyover')
library(dplyover)

set.seed(123)

microbenchmark::microbenchmark(
  cor = cor(cells_numeric),
  correlate = correlate(cells_numeric),
  colpair_map = colpair_map(cells_numeric, cor),
  pairwise = summarise(cells_numeric, pairwise(where(is.numeric), cor, .is_commutative = TRUE)),
  dplyover = summarise(cells_numeric, across2x(where(is.numeric), where(is.numeric), cor, .comb = "minimal")),
  times = 10L,
  unit = "ms")
#> Unit: milliseconds
#>         expr       min        lq       mean     median        uq       max
#>          cor    5.0229    5.1667    5.38959    5.30345    5.5008    5.9832
#>    correlate   41.7989   44.2974   50.43496   45.64110   49.7036   78.9423
#>  colpair_map  629.2201  679.8760  722.77073  733.22545  762.8037  788.9171
#>     pairwise  228.8138  234.7653  287.11852  260.42045  326.8639  466.1481
#>     dplyover 1393.9595 1531.9376 1821.72660 1647.67720 2098.0986 2663.2241
#>  neval  cld
#>     10 a   
#>     10 ab  
#>     10   c 
#>     10  b  
#>     10    d
```

The `stats::cor()` and `corrr::correlate()` approaches are many times
faster than using `pairwise()`. However `pairwise()` still only takes
about one fifth of a second to calculate 1540 correlations in this case.
Hence on relatively constrained problems `pairwise()` is still quite
usable. (Though there are many cases where you should go for a matrix
based solution.)

`pairwise()` seems to be faster than `corrr::colpair_map()` (a more
apples-to-apples comparison as both can handle arbitrary functions),
though much of this speed difference goes away when
`.is_commutative = FALSE`.

`pairwise()` (at the moment) seems to also be faster than running the
equivalent operation with `dplyover::across2x()`.

<details style="margin-bottom:10px;">
<summary>
Session info
</summary>

``` r
sessioninfo::session_info()
#> - Session info ---------------------------------------------------------------
#>  setting  value                       
#>  version  R version 3.5.1 (2018-07-02)
#>  os       Windows 10 x64              
#>  system   x86_64, mingw32             
#>  ui       RTerm                       
#>  language (EN)                        
#>  collate  English_United States.1252  
#>  ctype    English_United States.1252  
#>  tz       America/New_York            
#>  date     2021-06-04                  
#> 
#> - Packages -------------------------------------------------------------------
#>  package        * version    date       lib source                             
#>  assertthat       0.2.1      2019-03-21 [1] CRAN (R 3.5.3)                     
#>  cli              2.5.0      2021-04-26 [1] CRAN (R 3.5.1)                     
#>  codetools        0.2-15     2016-10-05 [2] CRAN (R 3.5.1)                     
#>  colorspace       2.0-1      2021-05-04 [1] CRAN (R 3.5.1)                     
#>  corrr          * 0.4.3      2020-11-24 [1] CRAN (R 3.5.1)                     
#>  crayon           1.4.1      2021-02-08 [1] CRAN (R 3.5.1)                     
#>  DBI              1.1.1      2021-01-15 [1] CRAN (R 3.5.1)                     
#>  digest           0.6.27     2020-10-24 [1] CRAN (R 3.5.1)                     
#>  dplyover       * 0.0.8.9000 2021-06-05 [1] Github (TimTeaFan/dplyover@b6a48fb)
#>  dplyr          * 1.0.6      2021-05-05 [1] CRAN (R 3.5.1)                     
#>  ellipsis         0.3.2      2021-04-29 [1] CRAN (R 3.5.1)                     
#>  evaluate         0.14       2019-05-28 [1] CRAN (R 3.5.3)                     
#>  fansi            0.5.0      2021-05-25 [1] CRAN (R 3.5.1)                     
#>  generics         0.1.0      2020-10-31 [1] CRAN (R 3.5.1)                     
#>  ggplot2          3.3.3      2020-12-30 [1] CRAN (R 3.5.1)                     
#>  glue             1.4.2      2020-08-27 [1] CRAN (R 3.5.1)                     
#>  gtable           0.3.0      2019-03-25 [1] CRAN (R 3.5.3)                     
#>  htmltools        0.5.1.1    2021-01-22 [1] CRAN (R 3.5.1)                     
#>  knitr            1.33       2021-04-24 [1] CRAN (R 3.5.1)                     
#>  lattice          0.20-35    2017-03-25 [2] CRAN (R 3.5.1)                     
#>  lifecycle        1.0.0      2021-02-15 [1] CRAN (R 3.5.1)                     
#>  magrittr         2.0.1      2020-11-17 [1] CRAN (R 3.5.1)                     
#>  MASS             7.3-50     2018-04-30 [2] CRAN (R 3.5.1)                     
#>  Matrix           1.2-14     2018-04-13 [1] CRAN (R 3.5.1)                     
#>  microbenchmark   1.4-7      2019-09-24 [1] CRAN (R 3.5.3)                     
#>  modeldata      * 0.1.0      2020-10-22 [1] CRAN (R 3.5.1)                     
#>  multcomp         1.4-17     2021-04-29 [1] CRAN (R 3.5.1)                     
#>  munsell          0.5.0      2018-06-12 [1] CRAN (R 3.5.1)                     
#>  mvtnorm          1.1-1      2020-06-09 [1] CRAN (R 3.5.1)                     
#>  palmerpenguins * 0.1.0      2020-07-23 [1] CRAN (R 3.5.1)                     
#>  pillar           1.6.1      2021-05-16 [1] CRAN (R 3.5.1)                     
#>  pkgconfig        2.0.3      2019-09-22 [1] CRAN (R 3.5.3)                     
#>  purrr            0.3.4      2020-04-17 [1] CRAN (R 3.5.3)                     
#>  pwiser         * 0.0.1.9000 2021-05-19 [1] Github (brshallo/pwiser@3131adb)   
#>  R6               2.5.0      2020-10-28 [1] CRAN (R 3.5.1)                     
#>  rlang            0.4.11     2021-04-30 [1] CRAN (R 3.5.1)                     
#>  rmarkdown        2.8        2021-05-07 [1] CRAN (R 3.5.1)                     
#>  rstudioapi       0.13       2020-11-12 [1] CRAN (R 3.5.1)                     
#>  sandwich         3.0-1      2021-05-18 [1] CRAN (R 3.5.1)                     
#>  scales           1.1.1      2020-05-11 [1] CRAN (R 3.5.1)                     
#>  sessioninfo      1.1.1      2018-11-05 [1] CRAN (R 3.5.2)                     
#>  stringi          1.6.2      2021-05-17 [1] CRAN (R 3.5.1)                     
#>  stringr          1.4.0      2019-02-10 [1] CRAN (R 3.5.3)                     
#>  survival         3.1-12     2020-04-10 [1] CRAN (R 3.5.3)                     
#>  TH.data          1.0-10     2019-01-21 [1] CRAN (R 3.5.3)                     
#>  tibble           3.1.2      2021-05-16 [1] CRAN (R 3.5.1)                     
#>  tidyselect       1.1.1      2021-04-30 [1] CRAN (R 3.5.1)                     
#>  utf8             1.2.1      2021-03-12 [1] CRAN (R 3.5.1)                     
#>  vctrs            0.3.8      2021-04-29 [1] CRAN (R 3.5.1)                     
#>  withr            2.4.2      2021-04-18 [1] CRAN (R 3.5.1)                     
#>  xfun             0.23       2021-05-15 [1] CRAN (R 3.5.1)                     
#>  yaml             2.2.1      2020-02-01 [1] CRAN (R 3.5.3)                     
#>  zoo              1.8-9      2021-03-09 [1] CRAN (R 3.5.1)                     
#> 
#> [1] C:/Users/BSHALLOW/Documents/R/win-library/3.5
#> [2] C:/Program Files/R/R-3.5.1/library
```

</details>

# Limitations

See issue [\#1](https://github.com/brshallo/pwiser/issues/1) for notes
on limitations in current set-up.
