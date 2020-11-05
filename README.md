
## Overview

Welcome to GDPVotes\!

``` r
library(GDPVoters)

States<-getGDP(filepath="./data/lagdp1219.xlsx")
print(States[["Louisiana"]])
```

    ## # A tibble: 64 x 10
    ##    county   `2015`   `2016`  `2017` `2018` rTotal c2016      c2017 c2018 rChange
    ##    <chr>    <chr>     <dbl>   <dbl>  <dbl> <chr>  <chr>      <dbl> <dbl> <chr>  
    ##  1 Acadia   1596865  1.59e6  1.59e6 1.60e6 30     -0.2         0     0.6 40     
    ##  2 Allen    591149   6.16e5  6.29e5 6.31e5 45     4.0999999~   2.2   0.2 43     
    ##  3 Ascensi~ 8200541  7.92e6  8.31e6 8.60e6 9      -3.4         4.9   3.5 17     
    ##  4 Assumpt~ 439467   4.07e5  4.39e5 4.54e5 53     -7.5         7.9   3.5 18     
    ##  5 Avoyell~ 828797   8.34e5  8.54e5 8.48e5 40     0.6          2.4  -0.7 54     
    ##  6 Beaureg~ 916684   9.08e5  9.02e5 9.22e5 37     -0.9        -0.6   2.2 26     
    ##  7 Bienvil~ 614943   6.13e5  7.06e5 7.44e5 43     -0.3        15     5.5 6      
    ##  8 Bossier  5515885  5.44e6  5.59e6 5.81e6 11     -1.4         2.9   3.9 13     
    ##  9 Caddo    129249~  1.29e7  1.31e7 1.33e7 6      -0.5         1.6   1.7 31     
    ## 10 Calcasi~ 126886~  1.32e7  1.39e7 1.44e7 4      3.8          5.3   3.8 15     
    ## # ... with 54 more rows

``` r
LA_GDP<-getStateGDP("Louisiana")
print(LA_GDP)
```

    ## # A tibble: 64 x 10
    ##    county   `2015`   `2016`  `2017` `2018` rTotal c2016      c2017 c2018 rChange
    ##    <chr>    <chr>     <dbl>   <dbl>  <dbl> <chr>  <chr>      <dbl> <dbl> <chr>  
    ##  1 Acadia   1596865  1.59e6  1.59e6 1.60e6 30     -0.2         0     0.6 40     
    ##  2 Allen    591149   6.16e5  6.29e5 6.31e5 45     4.0999999~   2.2   0.2 43     
    ##  3 Ascensi~ 8200541  7.92e6  8.31e6 8.60e6 9      -3.4         4.9   3.5 17     
    ##  4 Assumpt~ 439467   4.07e5  4.39e5 4.54e5 53     -7.5         7.9   3.5 18     
    ##  5 Avoyell~ 828797   8.34e5  8.54e5 8.48e5 40     0.6          2.4  -0.7 54     
    ##  6 Beaureg~ 916684   9.08e5  9.02e5 9.22e5 37     -0.9        -0.6   2.2 26     
    ##  7 Bienvil~ 614943   6.13e5  7.06e5 7.44e5 43     -0.3        15     5.5 6      
    ##  8 Bossier  5515885  5.44e6  5.59e6 5.81e6 11     -1.4         2.9   3.9 13     
    ##  9 Caddo    129249~  1.29e7  1.31e7 1.33e7 6      -0.5         1.6   1.7 31     
    ## 10 Calcasi~ 126886~  1.32e7  1.39e7 1.44e7 4      3.8          5.3   3.8 15     
    ## # ... with 54 more rows