
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SASfromR

<!-- badges: start -->

<!-- badges: end -->

The goal of SASfromR is to allow you to execute SAS code from within R
and to seamlessly transfer files between SAS and R. The typical use-case
is when your project requires you to use both SAS and R, or when you
need to utilize procedures that are only available in SAS, but don’t
wish to leave the comfort of R.

It works by generating SAS scripts which are then sent to sas.exe to be
executed in batch-mode.

R data sets are made available to SAS by exporting them to SAS transport
files using the `haven` package and placed in a temporary directory
which SAS then reads. To simplify the usage, they are then placed in the
SAS library “work” for easy access.

Similarly, a temporary SAS library “out” is created to allow results
from SAS to be stored in, allowing SASfromR to import them using
`haven::read_sas()` and return as R data sets.

For convenience, the SAS output is printed to the R console by default,
and for debugging purposes, the log can also be displayed using
`display_log=TRUE`.

## Installation

You can install the development version of SASfromR like so:

``` r
devtools::install_local("path/to/package")
```

## Example

This is a basic example which shows you how to use SASfromR

``` r
library(SASfromR)
## basic example code
SASfromR(r"(
         proc means data=mtcars_fromR;
           var mpg;
           ods output Summary=out.means;
         run;
         )", indata=list("mtcars_fromR"=mtcars))
#> 
#> ── SAS Output ──────────────────────────────────────────────────────────────────
#> 
#> 
#> 
#>                                       The MEANS Procedure
#> 
#>                                    Analysis Variable : mpg 
#>  
#>                N            Mean         Std Dev         Minimum         Maximum
#>               ------------------------------------------------------------------
#>               32      20.0906250       6.0269481      10.4000000      33.9000000
#>               ------------------------------------------------------------------
#> ................................................................................
#> # A tibble: 1 × 5
#>   mpg_N mpg_Mean mpg_StdDev mpg_Min mpg_Max
#>   <dbl>    <dbl>      <dbl>   <dbl>   <dbl>
#> 1    32     20.1       6.03    10.4    33.9
```
