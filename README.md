
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
#>          cor    5.0872    5.2673    6.04834    5.84305    6.7173    7.5421
#>    correlate   40.2647   43.8159   47.96368   46.35755   50.4519   61.3870
#>  colpair_map  609.4319  621.8832  675.88510  658.82900  673.0988  885.6218
#>     pairwise  232.2557  239.8043  278.56514  256.20245  289.0009  439.8503
#>     dplyover 1393.0210 1411.3117 1719.47016 1722.58500 1849.7877 2223.6372
#>  neval  cld
#>     10 a   
#>     10 a   
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
sessionInfo()
#> R version 3.5.1 (2018-07-02)
#> Platform: x86_64-w64-mingw32/x64 (64-bit)
#> Running under: Windows 10 x64 (build 19042)
#> 
#> Matrix products: default
#> 
#> locale:
#> [1] LC_COLLATE=English_United States.1252 
#> [2] LC_CTYPE=English_United States.1252   
#> [3] LC_MONETARY=English_United States.1252
#> [4] LC_NUMERIC=C                          
#> [5] LC_TIME=English_United States.1252    
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> other attached packages:
#> [1] dplyover_0.0.8.9000  corrr_0.4.3          modeldata_0.1.0     
#> [4] palmerpenguins_0.1.0 pwiser_0.0.1.9000    dplyr_1.0.6         
#> 
#> loaded via a namespace (and not attached):
#>  [1] pillar_1.6.1         compiler_3.5.1       tools_3.5.1         
#>  [4] digest_0.6.27        lattice_0.20-35      evaluate_0.14       
#>  [7] lifecycle_1.0.0      tibble_3.1.2         gtable_0.3.0        
#> [10] pkgconfig_2.0.3      rlang_0.4.11         Matrix_1.2-14       
#> [13] DBI_1.1.1            cli_2.5.0            rstudioapi_0.13     
#> [16] microbenchmark_1.4-7 yaml_2.2.1           mvtnorm_1.1-1       
#> [19] xfun_0.23            stringr_1.4.0        knitr_1.33          
#> [22] generics_0.1.0       vctrs_0.3.8          grid_3.5.1          
#> [25] tidyselect_1.1.1     glue_1.4.2           R6_2.5.0            
#> [28] fansi_0.5.0          survival_3.1-12      rmarkdown_2.8       
#> [31] multcomp_1.4-17      TH.data_1.0-10       purrr_0.3.4         
#> [34] ggplot2_3.3.3        magrittr_2.0.1       codetools_0.2-15    
#> [37] MASS_7.3-50          splines_3.5.1        scales_1.1.1        
#> [40] ellipsis_0.3.2       htmltools_0.5.1.1    assertthat_0.2.1    
#> [43] colorspace_2.0-1     sandwich_3.0-1       utf8_1.2.1          
#> [46] stringi_1.6.2        munsell_0.5.0        crayon_1.4.1        
#> [49] zoo_1.8-9
```

</details>

# Limitations

See issue [\#1](https://github.com/brshallo/pwiser/issues/1) for notes
on limitations in current set-up.
