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

    ##  [1] -0.0131481994 -0.5690863668 -0.2838970671  0.2300791739 -0.1585789544
    ##  [6]  0.0855821937 -2.0337329451 -1.0863848095  1.5166762700  0.9687174412
    ## [11] -1.1564026256 -1.0566373413  0.6072713174  1.4974226651  1.8357452429
    ## [16]  0.0115017498 -0.9771607904 -1.5259328862  1.3390255384 -0.1433072041
    ## [21] -0.0001026048 -0.4857783267  1.0555197381  0.0864188411  0.2561899499

``` r
z_scores = function(x){
  
  z = (x - mean(x)) / sd(x)
  
  return(z)
  
}

z_scores(x = x_vec)
```

    ##  [1] -0.0131481994 -0.5690863668 -0.2838970671  0.2300791739 -0.1585789544
    ##  [6]  0.0855821937 -2.0337329451 -1.0863848095  1.5166762700  0.9687174412
    ## [11] -1.1564026256 -1.0566373413  0.6072713174  1.4974226651  1.8357452429
    ## [16]  0.0115017498 -0.9771607904 -1.5259328862  1.3390255384 -0.1433072041
    ## [21] -0.0001026048 -0.4857783267  1.0555197381  0.0864188411  0.2561899499

``` r
y_vec = rnorm(40, mean = 12, sd = .3)

z_scores(y_vec) ##name input; also unneccessary
```

    ##  [1]  1.78257770  0.38075497 -0.30739514 -0.17030414 -0.21980206  0.73500793
    ##  [7] -0.14489262 -0.46075312  0.33071855 -1.46302393 -0.10332186 -1.57957032
    ## [13] -0.14183260 -0.20610795  0.11670493 -2.07563042 -1.77286837  0.15112605
    ## [19]  0.99962414  0.93997590 -0.66675245  0.14473019  0.41965664 -0.91268803
    ## [25]  0.73490156  1.62083508 -0.54933840 -0.08062449  0.30834370  0.83718328
    ## [31] -0.92500163  0.44112426 -0.52780215  2.56944268  1.78265533 -0.63662980
    ## [37]  0.70972128 -0.56791422 -1.56583522  0.07300476

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

# Multiple outputs

``` r
mean_sd = function(x){
  
  if(!is.numeric(x)) {
    stop("x needs to be numeric") 
  }
  
  if(length(x) < 3){
    stop("x should have at least 3 numbers")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)
  
  output_df = 
    tibble(
      mean_x = mean(x),
      sd_x = sd(x)
    )
    
    return(output_df)

}
mean_sd(x_vec)
```

    ## # A tibble: 1 x 2
    ##   mean_x  sd_x
    ##    <dbl> <dbl>
    ## 1   4.70  4.71

``` r
mean_sd(y_vec)
```

    ## # A tibble: 1 x 2
    ##   mean_x  sd_x
    ##    <dbl> <dbl>
    ## 1   12.1 0.278

## Different sample sizes, means, sds

``` r
sim_data = 
  tibble(
    x = rnorm(30, mean = 2, sd =3)
  )

sim_data %>% 
  summarise(
    mean = mean(x),
    sd = sd(x)
  )
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.41  2.86

let’s write function simulating data, computing the mean and sd

``` r
sim_mean_sd = function(n, mu, sigma){
  
  # do checks on inputs
  
sim_data = 
tibble(
  x = rnorm(mu, mean = mu, sd = sigma)
)

sim_data %>% 
summarise(
  mean = mean(x),
  sd = sd(x)
)
}

sim_mean_sd(30,4,3)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.59  1.40

``` r
#position matching in（）
```
