writing\_functions
================
Dantong Zhu
2021/11/4

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.4     v dplyr   1.0.7
    ## v tidyr   1.1.3     v stringr 1.4.0
    ## v readr   2.0.1     v forcats 0.5.1

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
knitr::opts_chunk$set(
  fig.width = 6,
  fig.asp = .6,
  out.width = "90%"
)
theme_set(theme_minimal() + theme(legend.position = "bottom"))
options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)
scale_colour_discrete = scale_colour_viridis_d
scale_fill_discrete = scale_fill_viridis_d
```

## Z-score

``` r
x_vec = rnorm(25, mean = 5, sd = 4)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1]  0.9907508  0.9661249 -1.0225058 -0.3619385  0.4855215 -0.5282161
    ##  [7] -0.5910884 -0.2388467  0.3884259 -0.1979632  1.4908963  2.6180579
    ## [13] -1.0320839 -1.0533756 -0.1821166  1.6969719 -0.6250735 -1.2174773
    ## [19]  0.1438495 -1.0216128 -1.2252897  0.6122352 -0.5688642 -0.2009676
    ## [25]  0.6745863

``` r
z_scores = function(x){
  
  z = (x - mean(x)) / sd(x)
  
  return(z)
  
}

z_scores(x = x_vec)
```

    ##  [1]  0.9907508  0.9661249 -1.0225058 -0.3619385  0.4855215 -0.5282161
    ##  [7] -0.5910884 -0.2388467  0.3884259 -0.1979632  1.4908963  2.6180579
    ## [13] -1.0320839 -1.0533756 -0.1821166  1.6969719 -0.6250735 -1.2174773
    ## [19]  0.1438495 -1.0216128 -1.2252897  0.6122352 -0.5688642 -0.2009676
    ## [25]  0.6745863

``` r
y_vec = rnorm(40, mean = 12, sd = .3)

z_scores(y_vec) ##name input; also unneccessary
```

    ##  [1]  0.26872437 -1.63826539 -1.82661052  0.45692087  1.65667026  1.38920070
    ##  [7] -2.66788588 -0.80246870 -0.88806508  0.72961097  0.91968912 -0.06632997
    ## [13] -0.69559552 -0.28337761 -0.10143191  1.03769609  0.29839931 -0.01853257
    ## [19]  0.12792477 -0.76473091 -1.26334664  1.80012111 -0.02111081 -0.26463881
    ## [25]  0.01543352  0.88439412  0.97761904 -0.14801999 -0.64145220  0.17458338
    ## [31]  0.77266533  1.36864921  0.39202330  0.38544354 -0.88713844  0.24597120
    ## [37] -0.23380011  0.34444515 -1.98525939  0.95187507

how great is this?

``` r
#evaluate the type of x
z_scores = function(x){
  
  if(!is.numeric(x)) {
    stop("x needs to be numeric") 
  }
  
  if(length(x) < 3){
    stop("x should have at least 3 numbers")
  }
  
  z = (x - mean(x)) / sd(x)
  
  return(z)
  
}

z_scores(3)
```

    ## Error in z_scores(3): x should have at least 3 numbers

``` r
z_scores(c("my","name"))
```

    ## Error in z_scores(c("my", "name")): x needs to be numeric

``` r
z_scores(mtcars)
```

    ## Error in z_scores(mtcars): x needs to be numeric
