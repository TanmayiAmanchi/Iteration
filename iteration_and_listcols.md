Iteration and Listcols
================

this is the repository for iteration part 2 of P8105

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'
    ## 
    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

``` r
set.seed(1)
```

## Lists

``` r
l = list(
  vec_numeric = 1:4,
  unif_sample = runif(100),
  mat = matrix(1:8, nrow = 2, ncol=4, byrow = TRUE),
  summary = summary(rnorm(1000))
)

l
```

    ## $vec_numeric
    ## [1] 1 2 3 4
    ## 
    ## $unif_sample
    ##   [1] 0.26550866 0.37212390 0.57285336 0.90820779 0.20168193 0.89838968
    ##   [7] 0.94467527 0.66079779 0.62911404 0.06178627 0.20597457 0.17655675
    ##  [13] 0.68702285 0.38410372 0.76984142 0.49769924 0.71761851 0.99190609
    ##  [19] 0.38003518 0.77744522 0.93470523 0.21214252 0.65167377 0.12555510
    ##  [25] 0.26722067 0.38611409 0.01339033 0.38238796 0.86969085 0.34034900
    ##  [31] 0.48208012 0.59956583 0.49354131 0.18621760 0.82737332 0.66846674
    ##  [37] 0.79423986 0.10794363 0.72371095 0.41127443 0.82094629 0.64706019
    ##  [43] 0.78293276 0.55303631 0.52971958 0.78935623 0.02333120 0.47723007
    ##  [49] 0.73231374 0.69273156 0.47761962 0.86120948 0.43809711 0.24479728
    ##  [55] 0.07067905 0.09946616 0.31627171 0.51863426 0.66200508 0.40683019
    ##  [61] 0.91287592 0.29360337 0.45906573 0.33239467 0.65087047 0.25801678
    ##  [67] 0.47854525 0.76631067 0.08424691 0.87532133 0.33907294 0.83944035
    ##  [73] 0.34668349 0.33377493 0.47635125 0.89219834 0.86433947 0.38998954
    ##  [79] 0.77732070 0.96061800 0.43465948 0.71251468 0.39999437 0.32535215
    ##  [85] 0.75708715 0.20269226 0.71112122 0.12169192 0.24548851 0.14330438
    ##  [91] 0.23962942 0.05893438 0.64228826 0.87626921 0.77891468 0.79730883
    ##  [97] 0.45527445 0.41008408 0.81087024 0.60493329
    ## 
    ## $mat
    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    2    3    4
    ## [2,]    5    6    7    8
    ## 
    ## $summary
    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -3.00805 -0.70816 -0.03532 -0.01783  0.68933  3.81028

``` r
l[["mat"]] [1,3]
```

    ## [1] 3

``` r
l[[4]]
```

    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -3.00805 -0.70816 -0.03532 -0.01783  0.68933  3.81028

Make a list that is more useful

``` r
list_norm =
  list(
    a = rnorm(20,0,5),
    b = rnorm(20,4,5),
    c = rnorm(20,0,10),
    d = rnorm(20,4,10)
  )
list_norm[["b"]]
```

    ##  [1]  8.27759611 -0.09981563  3.38198620  5.27474118 12.59463169 -0.79271764
    ##  [7] -4.02155131 -5.22804711  6.77868593  3.69940404  7.86043152  3.29580306
    ## [13]  5.96546963  5.12109287  4.11770993  0.88518670 10.31004690  1.97112979
    ## [19]  7.33381886  4.82319577

Lets re use function used last time

``` r
mean_and_sd = function(x) {
  
  if (!is.numeric(x)) {
    stop("Argument x should be numeric")
  } else if (length(x) == 1) {
    stop("Cannot be computed for length 1 vectors")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)

  tibble(
    mean = mean_x, 
    sd = sd_x
  )
}
```

Lets use the function to take mean and sd of all samples

``` r
mean_and_sd(list_norm[["a"]])
```

    ## # A tibble: 1 × 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.622  6.37

``` r
mean_and_sd(list_norm[["b"]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.08  4.46

``` r
mean_and_sd(list_norm[["c"]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.847  10.0

``` r
mean_and_sd(list_norm[["d"]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.119  8.86

## Use a for loop

``` r
output = vector('list', length =4)

for(i in 1:4){
  output[[i]]=mean_and_sd(list_norm[[i]])
}

output
```

    ## [[1]]
    ## # A tibble: 1 × 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.622  6.37
    ## 
    ## [[2]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.08  4.46
    ## 
    ## [[3]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.847  10.0
    ## 
    ## [[4]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.119  8.86

## Do the same thing with map

``` r
output = map(list_norm, mean_and_sd)
```

``` r
output = map_dbl(list_norm, IQR)
```

``` r
output = map_dfr(list_norm, mean_and_sd, .id = "input")

output
```

    ## # A tibble: 4 × 3
    ##   input   mean    sd
    ##   <chr>  <dbl> <dbl>
    ## 1 a     -0.622  6.37
    ## 2 b      4.08   4.46
    ## 3 c      0.847 10.0 
    ## 4 d      0.119  8.86

\##List columns

``` r
listcol_df = 
  tibble(
    name = c("a", "b", "c", "d"),
    samp = list_norm
  )
```

``` r
listcol_df |> pull(name)
```

    ## [1] "a" "b" "c" "d"

``` r
listcol_df |> pull(samp)
```

    ## $a
    ##  [1]   0.2099938   1.1171116  -5.0523254  12.0061105   4.0098090  -1.2560398
    ##  [7]   6.0644469  -3.1362904   8.5557925  -1.9718678 -11.6074543   6.8205960
    ## [13]   5.6611457  -3.8715816  -7.0518748  -9.1726379  -1.3450677  -9.1696429
    ## [19]  -4.0723401   0.8178606
    ## 
    ## $b
    ##  [1]  8.27759611 -0.09981563  3.38198620  5.27474118 12.59463169 -0.79271764
    ##  [7] -4.02155131 -5.22804711  6.77868593  3.69940404  7.86043152  3.29580306
    ## [13]  5.96546963  5.12109287  4.11770993  0.88518670 10.31004690  1.97112979
    ## [19]  7.33381886  4.82319577
    ## 
    ## $c
    ##  [1]  17.81524475   7.11213964  -3.37691156  -0.09148952  -1.25309208
    ##  [6] -20.90846097  16.97393895  10.63881154  -7.66616636   3.82007559
    ## [11]   2.41895904 -11.32759411  14.89907414  -2.48247105   1.83583708
    ## [16]   4.04871009  -9.94124469 -10.85429330  -0.48542555   5.76085601
    ## 
    ## $d
    ##  [1]   4.7383053  11.0594557   7.3498010   9.4538781 -10.0290591  10.7705389
    ##  [7]  -3.8980045  -0.6572889   2.9514793 -12.4785109   3.0046305  -0.3985764
    ## [13]  -3.1851145  -1.5459760  16.4548918  -8.5892135   1.8461552 -20.7196171
    ## [19]  -2.7416932  -1.0129719

``` r
map(listcol_df$samp, mean_and_sd)
```

    ## $a
    ## # A tibble: 1 × 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.622  6.37
    ## 
    ## $b
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.08  4.46
    ## 
    ## $c
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.847  10.0
    ## 
    ## $d
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.119  8.86

``` r
listcol_df = 
  listcol_df |> 
  mutate(summary = map(samp, mean_and_sd))

listcol_df
```

    ## # A tibble: 4 × 3
    ##   name  samp         summary         
    ##   <chr> <named list> <named list>    
    ## 1 a     <dbl [20]>   <tibble [1 × 2]>
    ## 2 b     <dbl [20]>   <tibble [1 × 2]>
    ## 3 c     <dbl [20]>   <tibble [1 × 2]>
    ## 4 d     <dbl [20]>   <tibble [1 × 2]>

## NSDUH

``` r
nsduh_table <- function(html, table_num) {
  
  table = 
    html |> 
    html_table() |> 
    nth(table_num) |>
    slice(-1) |> 
    select(-contains("P Value")) |>
    pivot_longer(
      -State,
      names_to = "age_year", 
      values_to = "percent") |>
    separate(age_year, into = c("age", "year"), sep = "\\(") |>
    mutate(
      year = str_replace(year, "\\)", ""),
      percent = str_replace(percent, "[a-c]$", ""),
      percent = as.numeric(percent)) |>
    filter(!(State %in% c("Total U.S.", "Northeast", "Midwest", "South", "West")))
  
  table
}
```

``` r
nsduh_url = "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads/2k15StateFiles/NSDUHsaeShortTermCHG2015.htm"

nsduh_html = read_html(nsduh_url)

output = vector("list", 3)

for (i in c(1, 4, 5)) {
  output[[i]] = nsduh_table(nsduh_html, i)
}

nsduh_results = bind_rows(output)
```

``` r
nsduh_results = 
  map(c(1, 4, 5), nsduh_table, html = nsduh_html) |> 
  bind_rows()
```

``` r
nsduh_results= 
  tibble(
    name = c("marj", "cocaine", "heroine"),
    number = c(1, 4, 5)) |> 
  mutate(table = map(number, \(num) nsduh_table(html = nsduh_html, num))) |> 
  unnest(cols = "table")
```

## Weather data

``` r
weather_df = 
  rnoaa::meteo_pull_monitors(
    c("USW00094728", "USW00022534", "USS0023B17S"),
    var = c("PRCP", "TMIN", "TMAX"), 
    date_min = "2021-01-01",
    date_max = "2022-12-31") |>
  mutate(
    name = recode(
      id, 
      USW00094728 = "CentralPark_NY", 
      USW00022534 = "Molokai_HI",
      USS0023B17S = "Waterhole_WA"),
    tmin = tmin / 10,
    tmax = tmax / 10) |>
  select(name, id, everything())
```

    ## Registered S3 method overwritten by 'hoardr':
    ##   method           from
    ##   print.cache_info httr

    ## using cached file: /Users/Tammy/Library/Caches/org.R-project.R/R/rnoaa/noaa_ghcnd/USW00094728.dly

    ## date created (size, mb): 2024-09-26 20:34:18.576194 (8.651)

    ## file min/max dates: 1869-01-01 / 2024-09-30

    ## using cached file: /Users/Tammy/Library/Caches/org.R-project.R/R/rnoaa/noaa_ghcnd/USW00022534.dly

    ## date created (size, mb): 2024-09-26 20:34:36.21331 (3.932)

    ## file min/max dates: 1949-10-01 / 2024-09-30

    ## using cached file: /Users/Tammy/Library/Caches/org.R-project.R/R/rnoaa/noaa_ghcnd/USS0023B17S.dly

    ## date created (size, mb): 2024-09-26 20:34:42.357773 (1.036)

    ## file min/max dates: 1999-09-01 / 2024-09-30

``` r
weather_nest = 
  nest(weather_df, data = date:tmin)

weather_nest
```

    ## # A tibble: 3 × 3
    ##   name           id          data              
    ##   <chr>          <chr>       <list>            
    ## 1 CentralPark_NY USW00094728 <tibble [730 × 4]>
    ## 2 Molokai_HI     USW00022534 <tibble [730 × 4]>
    ## 3 Waterhole_WA   USS0023B17S <tibble [730 × 4]>

``` r
weather_nest |> pull(name)
```

    ## [1] "CentralPark_NY" "Molokai_HI"     "Waterhole_WA"

``` r
weather_nest |> pull(data)
```

    ## [[1]]
    ## # A tibble: 730 × 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2021-01-01   157   4.4   0.6
    ##  2 2021-01-02    13  10.6   2.2
    ##  3 2021-01-03    56   3.3   1.1
    ##  4 2021-01-04     5   6.1   1.7
    ##  5 2021-01-05     0   5.6   2.2
    ##  6 2021-01-06     0   5     1.1
    ##  7 2021-01-07     0   5    -1  
    ##  8 2021-01-08     0   2.8  -2.7
    ##  9 2021-01-09     0   2.8  -4.3
    ## 10 2021-01-10     0   5    -1.6
    ## # ℹ 720 more rows
    ## 
    ## [[2]]
    ## # A tibble: 730 × 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2021-01-01     0  27.8  22.2
    ##  2 2021-01-02     0  28.3  23.9
    ##  3 2021-01-03     0  28.3  23.3
    ##  4 2021-01-04     0  30    18.9
    ##  5 2021-01-05     0  28.9  21.7
    ##  6 2021-01-06     0  27.8  20  
    ##  7 2021-01-07     0  29.4  21.7
    ##  8 2021-01-08     0  28.3  18.3
    ##  9 2021-01-09     0  27.8  18.9
    ## 10 2021-01-10     0  28.3  18.9
    ## # ℹ 720 more rows
    ## 
    ## [[3]]
    ## # A tibble: 730 × 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2021-01-01   254   3.2   0  
    ##  2 2021-01-02   152   0.9  -3.2
    ##  3 2021-01-03     0   0.2  -4.2
    ##  4 2021-01-04   559   0.9  -3.2
    ##  5 2021-01-05    25   0.5  -3.3
    ##  6 2021-01-06    51   0.8  -4.8
    ##  7 2021-01-07     0   0.2  -5.8
    ##  8 2021-01-08    25   0.5  -8.3
    ##  9 2021-01-09     0   0.1  -7.7
    ## 10 2021-01-10   203   0.9  -0.1
    ## # ℹ 720 more rows
