Starbucks
================
Fanyi Zeng
2022-05-03

``` r
starbucks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-21/starbucks.csv')
```

    ## Rows: 1147 Columns: 15

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr  (4): product_name, size, trans_fat_g, fiber_g
    ## dbl (11): milk, whip, serv_size_m_l, calories, total_fat_g, saturated_fat_g,...

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.6     v dplyr   1.0.7
    ## v tidyr   1.1.4     v stringr 1.4.0
    ## v readr   2.1.1     v forcats 0.5.1

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

First, let’s take a look at the descriptives.

``` r
summary(starbucks)
```

    ##  product_name           size                milk            whip       
    ##  Length:1147        Length:1147        Min.   :0.000   Min.   :0.0000  
    ##  Class :character   Class :character   1st Qu.:1.000   1st Qu.:0.0000  
    ##  Mode  :character   Mode  :character   Median :2.000   Median :0.0000  
    ##                                        Mean   :2.513   Mean   :0.2467  
    ##                                        3rd Qu.:4.000   3rd Qu.:0.0000  
    ##                                        Max.   :5.000   Max.   :1.0000  
    ##  serv_size_m_l      calories      total_fat_g     saturated_fat_g 
    ##  Min.   :  0.0   Min.   :  0.0   Min.   : 0.000   Min.   : 0.000  
    ##  1st Qu.:354.0   1st Qu.:130.0   1st Qu.: 1.000   1st Qu.: 0.200  
    ##  Median :473.0   Median :220.0   Median : 4.500   Median : 2.500  
    ##  Mean   :461.3   Mean   :228.4   Mean   : 6.186   Mean   : 3.881  
    ##  3rd Qu.:591.0   3rd Qu.:320.0   3rd Qu.:10.000   3rd Qu.: 7.000  
    ##  Max.   :887.0   Max.   :640.0   Max.   :28.000   Max.   :20.000  
    ##  trans_fat_g        cholesterol_mg    sodium_mg     total_carbs_g  
    ##  Length:1147        Min.   : 0.00   Min.   :  0.0   Min.   : 0.00  
    ##  Class :character   1st Qu.: 0.00   1st Qu.: 70.0   1st Qu.:20.00  
    ##  Mode  :character   Median : 5.00   Median :135.0   Median :37.00  
    ##                     Mean   :15.24   Mean   :139.7   Mean   :37.72  
    ##                     3rd Qu.:30.00   3rd Qu.:200.0   3rd Qu.:53.00  
    ##                     Max.   :75.00   Max.   :370.0   Max.   :96.00  
    ##    fiber_g             sugar_g       caffeine_mg    
    ##  Length:1147        Min.   : 0.00   Min.   :  0.00  
    ##  Class :character   1st Qu.:18.00   1st Qu.: 30.00  
    ##  Mode  :character   Median :34.00   Median : 75.00  
    ##                     Mean   :34.99   Mean   : 91.86  
    ##                     3rd Qu.:49.00   3rd Qu.:150.00  
    ##                     Max.   :89.00   Max.   :475.00

Choices for customers who want to order relatively low-calorie, sweet,
creamy drinks: mostly frapucinos and mochas, and some hot chocolate.

``` r
starbucks %>%
  filter(size == "grande", milk == 1, whip == 1, sugar_g != 0) %>%
  select(product_name, calories) %>%
  arrange(calories)
```

    ## # A tibble: 17 x 2
    ##    product_name                                    calories
    ##    <chr>                                              <dbl>
    ##  1 Cinnamon Dolce Latte                                 280
    ##  2 Oprah Cinnamon Chai Crème Frappuccino Blended        300
    ##  3 Caffè Mocha                                          320
    ##  4 Iced Caffè Mocha                                     320
    ##  5 Chai Crème Frappuccino Blended                       330
    ##  6 Strawberries & Crème Frappuccino Blended             340
    ##  7 Hot Chocolate                                        350
    ##  8 Vanilla Bean Crème Frappuccino Blended               360
    ##  9 Caramel Frappuccino Blended                          390
    ## 10 Iced Caffè Mocha                                     390
    ## 11 Double Chocolaty Chip Crème Frappuccino Blended      390
    ## 12 Caffè Vanilla Frappuccino Blended                    400
    ## 13 Green Tea Crème Frappuccino Blended                  400
    ## 14 White Chocolate Mocha                                420
    ## 15 Iced White Chocolate Mocha                           420
    ## 16 White Hot Chocolate                                  430
    ## 17 Java Chip Frappuccino Blended                        440

Choices for customers who want to order healthy, low-sodium, sugar-free
drinks: mostly brewed tea with no sodium and brewed coffee with low
sodium.

``` r
starbucks %>%
  filter(size == "grande", milk == 0, whip == 0, sugar_g == 0) %>%
  select(product_name, sodium_mg) %>%
  arrange(sodium_mg)
```

    ## # A tibble: 20 x 2
    ##    product_name                                  sodium_mg
    ##    <chr>                                             <dbl>
    ##  1 Earl Grey Brewed Tea                                  0
    ##  2 Emperor's Clouds and Mist Brewed Tea                  0
    ##  3 English Breakfast Black Brewed Tea                    0
    ##  4 Jade Citrus Mint Brewed tea                           0
    ##  5 Mint Majesty Brewed Tea                               0
    ##  6 Oprah Chai Herbal Brewed Tea                          0
    ##  7 Oprah Cinnamon Chai Brewed Tea                        0
    ##  8 Passion Tango Brewed Tea                              0
    ##  9 Peach Tranquility Brewed Tea                          0
    ## 10 Youthberry Brewed Tea                                 0
    ## 11 brewed coffee - medium roast                          5
    ## 12 brewed coffee - True North Blend Blonde roast         5
    ## 13 brewed coffee - dark roast                           10
    ## 14 brewed coffee - decaf pike place roast               10
    ## 15 Clover Brewed Coffee - Dark Roast                    10
    ## 16 Clover Brewed Coffee -  Light Roast                  10
    ## 17 Clover Brewed Coffee - Medium Roast                  10
    ## 18 Cold Brewed Coffee                                   15
    ## 19 Espresso - Caffè Americano                           15
    ## 20 Espresso - Iced Caffè Americano                      15

Choices for customers who want to order decaf, dairy free, low-sugar
drinks: mostly herbal and fruit tea.

``` r
starbucks %>%
  filter(size == "grande", caffeine_mg == 0, milk == 0) %>%
  select(product_name, sugar_g) %>%
  arrange(sugar_g)
```

    ## # A tibble: 11 x 2
    ##    product_name                    sugar_g
    ##    <chr>                             <dbl>
    ##  1 Mint Majesty Brewed Tea               0
    ##  2 Oprah Chai Herbal Brewed Tea          0
    ##  3 Passion Tango Brewed Tea              0
    ##  4 Peach Tranquility Brewed Tea          0
    ##  5 Skinny Hot Chocolate                 17
    ##  6 Iced Passion Tango Tea               20
    ##  7 Lemonade                             21
    ##  8 Iced Passion Tango Tea Lemonade      32
    ##  9 Blended Strawberry Lemonade          50
    ## 10 Caramel Apple Spice                  65
    ## 11 Caramel Apple Spice                  71

The most unhealthy drinks in starbucks: (recommended daily sodium intake
is 2300 mg, cholesterol is 300 mg, added sugar is 24g, trans-fat is 0)

``` r
starbucks %>%
  filter(size == "grande", milk == 1, whip == 1, calories > 320, sugar_g > 49, cholesterol_mg > 30, sodium_mg > 200, total_fat_g > 10, trans_fat_g !=0) %>%
  arrange(desc(calories))
```

    ## # A tibble: 11 x 15
    ##    product_name             size   milk  whip serv_size_m_l calories total_fat_g
    ##    <chr>                    <chr> <dbl> <dbl>         <dbl>    <dbl>       <dbl>
    ##  1 Java Chip Frappuccino B~ gran~     1     1           473      440          15
    ##  2 White Hot Chocolate      gran~     1     1           473      430          13
    ##  3 White Chocolate Mocha    gran~     1     1           473      420          13
    ##  4 Caffè Vanilla Frappucci~ gran~     1     1           473      400          11
    ##  5 Green Tea Crème Frappuc~ gran~     1     1           473      400          11
    ##  6 Caramel Frappuccino Ble~ gran~     1     1           473      390          12
    ##  7 Iced Caffè Mocha         gran~     1     1           473      390          12
    ##  8 Double Chocolaty Chip C~ gran~     1     1           473      390          15
    ##  9 Vanilla Bean Crème Frap~ gran~     1     1           473      360          11
    ## 10 Strawberries & Crème Fr~ gran~     1     1           473      340          11
    ## 11 Chai Crème Frappuccino ~ gran~     1     1           473      330          11
    ## # ... with 8 more variables: saturated_fat_g <dbl>, trans_fat_g <chr>,
    ## #   cholesterol_mg <dbl>, sodium_mg <dbl>, total_carbs_g <dbl>, fiber_g <chr>,
    ## #   sugar_g <dbl>, caffeine_mg <dbl>

The most healthy drinks in starbucks:

``` r
starbucks %>%
  filter(size == "grande", whip == 0, calories < 130, sugar_g < 18, cholesterol_mg == 0, sodium_mg < 70, total_fat_g < 1, trans_fat_g == 0) %>%
  arrange(desc(calories))
```

    ## # A tibble: 20 x 15
    ##    product_name             size   milk  whip serv_size_m_l calories total_fat_g
    ##    <chr>                    <chr> <dbl> <dbl>         <dbl>    <dbl>       <dbl>
    ##  1 Espresso - Caffè Americ~ gran~     0     0           473       15         0  
    ##  2 Espresso - Iced Caffè A~ gran~     0     0           473       15         0  
    ##  3 Clover Brewed Coffee - ~ gran~     0     0           473       10         0  
    ##  4 Clover Brewed Coffee - ~ gran~     0     0           473       10         0  
    ##  5 Clover Brewed Coffee - ~ gran~     0     0           473       10         0  
    ##  6 brewed coffee - dark ro~ gran~     0     0           473        5         0.1
    ##  7 brewed coffee - decaf p~ gran~     0     0           473        5         0.1
    ##  8 brewed coffee - medium ~ gran~     0     0           473        5         0.1
    ##  9 brewed coffee - True No~ gran~     0     0           473        5         0.1
    ## 10 Cold Brewed Coffee       gran~     0     0           473        3         0  
    ## 11 Earl Grey Brewed Tea     gran~     0     0           473        0         0  
    ## 12 Emperor's Clouds and Mi~ gran~     0     0           473        0         0  
    ## 13 English Breakfast Black~ gran~     0     0           473        0         0  
    ## 14 Jade Citrus Mint Brewed~ gran~     0     0           473        0         0  
    ## 15 Mint Majesty Brewed Tea  gran~     0     0           473        0         0  
    ## 16 Oprah Chai Herbal Brewe~ gran~     0     0           473        0         0  
    ## 17 Oprah Cinnamon Chai Bre~ gran~     0     0           473        0         0  
    ## 18 Passion Tango Brewed Tea gran~     0     0           473        0         0  
    ## 19 Peach Tranquility Brewe~ gran~     0     0           473        0         0  
    ## 20 Youthberry Brewed Tea    gran~     0     0           473        0         0  
    ## # ... with 8 more variables: saturated_fat_g <dbl>, trans_fat_g <chr>,
    ## #   cholesterol_mg <dbl>, sodium_mg <dbl>, total_carbs_g <dbl>, fiber_g <chr>,
    ## #   sugar_g <dbl>, caffeine_mg <dbl>

It should come as unsurprising that frapuccino, mocha, and hot chocolate
are the relatively unhealthy drinks in Starbucks. I am surprised to
discover that there are some healthier options such as brewed tea and,
of course, pure coffee. However, even the healthier tea and coffee
options don’t seem to have a lot of nutrients (low to no fiber). I would
suggest that Starbucks add vitamin info to the tea options to increase
marketability.
