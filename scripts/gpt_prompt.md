For the following table generate two vectors named `males_num_health` and `males_percent_health` that have the same number of values (119 each). Note that the column "Males % (periodontal health)" often has missing values that can be obtained from the "Males (n,%) (periodontal health)" column. If you see a fraction (e.g. 0.37) treat that as a percentage (37) to add to the new `males_percent_health` vector

``` r
# A tibble: 119 × 2
    `Males (n,%) (periodontal health)`        `Males % (periodontal health)`
    <chr>                                     <chr>                         
  1 0                                         0                             
  2 46 (31,5%)                                NA                            
  3 n=13                                      NA                            
  4 0.37                                      NA                            
  5 0.45                                      NA                            
  6 0% (exclusion criteria)                   NA                            
  7 N=6                                       NA                            
# ... [rest of table omitted; include in actual prompt]
```

Below are a few examples of how values should be fixed. Here are some of the original values:

``` r
# A tibble: 9 × 2
  `Males (n,%) (periodontal health)` `Males % (periodontal health)`
  <chr>                              <chr>                         
1 46 (31,5%)                         NA                            
2 n=13                               NA                            
3 0.37                               NA                            
4 17                                 32.1%*                        
5 7                                  50                            
6 1 (10.00%)                         NA                            
7 N=10                               NA                            
8 n 72                               NA                            
9 8                                  53.3    
```

and the corresponding fixed values:

``` r
males_num_health = c(
  46,    # 1, from (n, %) col, "46 (31,5%)"
  13,    # 2, from (n, %) col, "n=13"
  NA,    # 3, (n, %) col has fraction only "0.37"
  17,    # 4, from (n, %) col, "17"
  7,     # 5, from (n, %) col, "7"
  1,     # 6, from (n, %) col, "1 (10.00%)"
  72,    # 7, from (n, %) col, "n 72"
  8      # 8, from (n, %) col, "8"
)
males_percent_health = c(
  31.5,  # 1, from (n, %) col, "46 (31,5%)"
  NA,    # 2, percent absent in both of original cols
  37,    # 3, from (n, %) col, "0.37", treated as percentage
  32.1,  # 4, from % col, "32.1%*"
  50,    # 5, from % col, "50"
  10,    # 6, from (n, %) col, "1 (10.00%)"
  NA,    # 7, percent absent in both of original cols
  53.3   # 8, from % col, "53.3"
)
```

For each value in the fixed result please follow the below format for comments:

``` r
# 1, column obtained from, value of column where obtained from
```

The number indicates the corresponding value (should go from 1 to 119 inclusive for each of the two new vectors), the column obtained from should be describing the original column obtained from (e.g. (n, %) column or % column, and the value where obtained from should include the string value that the value was extracted from. The comment should be on the same line as the fixed value.
