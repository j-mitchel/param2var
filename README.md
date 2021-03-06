
<!-- README.md is generated from README.Rmd. Please edit that file -->

# param2var

This package aims to make it easier to debug functions with many
parameters. To debug such functions, it is common to first assign the
broken function’s parameters as global variables. Then, one can run the
individual lines of the function to determine where it is broken. As it
can be a frustration to individually assign dozens of parameters to
variables, this package can be used to automatically assign a function’s
parameters to global variables (see example below).

## Installation

Install param2var with the following:

``` r
devtools::install_github("j-mitchel/param2var")
```

## Usage example

``` r
library(param2var)

# Say you have a function with many parameters that you want to debug, such as the following:
my_test_function <- function(p1, p2='foo', p3=10, p4=NULL, p5=12, p6='baz') {
  message('This function does nothing')
}

# Call param2var() to assign specified values to global variables with the same
# names as the parameters in the above function. In this example, we are going to
# set specific values for the parameters p1 and p2. By setting def_fn=my_test_function,
# you automatically assign the rest that function's default parameters to global variables.
param2var(def_fn=my_test_function, p1=99, p2='bar')
#> Assigning variables:
#> p1 <- 99
#> p2 <- 'bar'
#> p3 <- 10
#> p4 <- NULL
#> p5 <- 12
#> p6 <- 'baz'

# After debugging, all of the variables we just set can easily be removed by 
# setting rm_vars=TRUE
param2var(def_fn=my_test_function, p1=99, p2='bar', rm_vars=TRUE)
```
