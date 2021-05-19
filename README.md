
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

**Identified after publishing {pwiser}:**

The [dplyover](https://github.com/TimTeaFan/dplyover) package is a more
mature package that also offers a wide range of extensions on `across()`
for iteration problems. `dplyover::over2x()` can be used to do
essentially the same thing as `pairwise()`. We are currently reviewing
whether to mark {pwiser} as superseded so we can point people to
{dplyover}.

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
set.seed(123)

microbenchmark::microbenchmark(
  cor = cor(cells_numeric),
  correlate = corrr::correlate(cells_numeric),
  colpair_map = corrr::colpair_map(cells_numeric, cor),
  pairwise = summarise(cells_numeric, pairwise(where(is.numeric), cor, .is_commutative = TRUE)),
  times = 10L,
  unit = "ms")
#> Unit: milliseconds
#>         expr      min       lq      mean    median       uq      max neval  cld
#>          cor   5.2296   5.5658   6.01588   5.85185   6.2553   7.2560    10 a   
#>    correlate  43.2178  45.2571  49.23176  47.46405  52.1339  62.1109    10  b  
#>  colpair_map 655.3740 695.4097 729.78595 722.32390 737.7867 882.0173    10    d
#>     pairwise 245.2783 264.9519 277.95506 271.93135 295.3673 319.9610    10   c
```

The `stats::cor()` and `corrr::correlate()` approaches are many times
faster than using `pairwise()`. However `pairwise()` still only takes
about one fifth of a second to calculate 1540 correlations in this case.
Hence on relatively constrained problems pairwise() is still quite
usable. (Though there are many cases where you should go for a matrix
based solution.)

`pairwise()` seems to be faster than `corrr::colpair_map()` (a more
apples-to-apples comparison as both can handle arbitrary functions),
though much of this speed difference goes away when
`.is_commutative = FALSE`.

# Limitations

See issue [\#1](https://github.com/brshallo/pwiser/issues/1) for a
little on limitations in current set-up.
