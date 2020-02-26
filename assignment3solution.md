Statistical assignment 3
================
\[add date here\]

In this assignment we will explore political interest (*vote6*) and how
it changes over time.

## Read data

First we want to read and join the data for the first 7 waves of the
Understanding Society. (Wave 8 does not have a variable for political
interest). We only want five variables: personal identifier, sample
origin, sex, age and political interest. It is tedious to join all the
seven waves manually, and it makes sense to use a loop in this case.
Since you don’t yet know about iteration I’ll provide the code for you;
please see the explanation of the code here:
<http://abessudnov.net/dataanalysis3/iteration.html>.

The only thing you need to do for this code to work on your computer is
to provide a path to the directory where the data are stored on your
computer.

``` r
library(tidyverse)
library(data.table)
# data.table is faster compared to readr so we'll use it in this case (the function fread()). You need to install this package first to be able to run this code.
# create a vector with the file names and paths
files <- dir(
             # Select the folder where the files are stored.
             "C:\\Users\\ab789\\datan3_2019",
             # Tell R which pattern you want present in the files it will display.
             pattern = "indresp",
             # We want this process to repeat through the entire folder.
             recursive = TRUE,
             # And finally want R to show us the entire file path, rather than just
             # the names of the individual files.
             full.names = TRUE)
# Select only files from the UKHLS.
files <- files[stringr::str_detect(files, "ukhls")]
files
```

    ## [1] "C:\\Users\\ab789\\datan3_2019/data/UKDA-6614-tab/tab/ukhls_w1/a_indresp.tab"
    ## [2] "C:\\Users\\ab789\\datan3_2019/data/UKDA-6614-tab/tab/ukhls_w2/b_indresp.tab"
    ## [3] "C:\\Users\\ab789\\datan3_2019/data/UKDA-6614-tab/tab/ukhls_w3/c_indresp.tab"
    ## [4] "C:\\Users\\ab789\\datan3_2019/data/UKDA-6614-tab/tab/ukhls_w4/d_indresp.tab"
    ## [5] "C:\\Users\\ab789\\datan3_2019/data/UKDA-6614-tab/tab/ukhls_w5/e_indresp.tab"
    ## [6] "C:\\Users\\ab789\\datan3_2019/data/UKDA-6614-tab/tab/ukhls_w6/f_indresp.tab"
    ## [7] "C:\\Users\\ab789\\datan3_2019/data/UKDA-6614-tab/tab/ukhls_w7/g_indresp.tab"
    ## [8] "C:\\Users\\ab789\\datan3_2019/data/UKDA-6614-tab/tab/ukhls_w8/h_indresp.tab"

``` r
# create a vector of variable names
vars <- c("memorig", "sex_dv", "age_dv", "vote6")
for (i in 1:7) {
        # Create a vector of the variables with the correct prefix.
        varsToSelect <- paste(letters[i], vars, sep = "_")
        # Add pidp to this vector (no prefix for pidp)
        varsToSelect <- c("pidp", varsToSelect)
        # Now read the data. 
        data <- fread(files[i], select = varsToSelect)
        if (i == 1) {
                all7 <- data  
        }
        else {
                all7 <- full_join(all7, data, by = "pidp")
        }
        # Now we can remove data to free up the memory.
        rm(data)
} 
```

## Reshape data (20 points)

Now we have got the data from all 7 waves in the same data frame
**all7** in the wide format. Note that the panel is unbalanced, i.e. we
included all people who participated in at least one wave of the survey.
Reshape the data to the long format. The resulting data frame should
have six columns for six variables.

``` r
Long <- all7 %>%
  # convert data into the "very long" format.
  gather(a_memorig:g_vote6, key = "variable", value = "value") %>%
  # split the column with variable names into two (one for wave and one for generic variable name)
  separate(variable, into = c("wave", "variable"), sep = "_", extra = "merge") %>%
  # convert into the format we need
  spread(key = variable, value = value) %>%
  as_tibble()
Long
```

    ## # A tibble: 584,227 x 6
    ##     pidp wave  age_dv memorig sex_dv vote6
    ##    <int> <chr>  <int>   <int>  <int> <int>
    ##  1 22445 a         NA      NA     NA    NA
    ##  2 22445 b         NA      NA     NA    NA
    ##  3 22445 c         NA      NA     NA    NA
    ##  4 22445 d         27       3      2     2
    ##  5 22445 e         28       3      2     2
    ##  6 22445 f         29       3      2     2
    ##  7 22445 g         30       3      2     1
    ##  8 29925 a         NA      NA     NA    NA
    ##  9 29925 b         NA      NA     NA    NA
    ## 10 29925 c         NA      NA     NA    NA
    ## # ... with 584,217 more rows

``` r
# alternatively

Long <- all7 %>%
  pivot_longer(a_memorig:g_vote6, names_to = "variable", values_to = "value") %>%
  separate(variable, into = c("wave", "variable"), sep = "_", extra = "merge") %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  as_tibble()
Long
```

    ## # A tibble: 584,227 x 6
    ##        pidp wave  memorig sex_dv age_dv vote6
    ##       <int> <chr>   <int>  <int>  <int> <int>
    ##  1 68001367 a           1      1     39     3
    ##  2 68001367 b          NA     NA     NA    NA
    ##  3 68001367 c          NA     NA     NA    NA
    ##  4 68001367 d          NA     NA     NA    NA
    ##  5 68001367 e          NA     NA     NA    NA
    ##  6 68001367 f          NA     NA     NA    NA
    ##  7 68001367 g          NA     NA     NA    NA
    ##  8 68004087 a           1      1     59     2
    ##  9 68004087 b           1      1     60     2
    ## 10 68004087 c           1      1     61     2
    ## # ... with 584,217 more rows

## Filter and recode (20 points)

Now we want to filter the data keeping only respondents from the
original UKHLS sample for Great Britain (memorig == 1). We also want to
clean the variables for sex (recoding it to “male” or “female”) and
political interest (keeping the values from 1 to 4 and coding all
negative values as missing). Tabulate *sex* and *vote6* to make sure
your recodings were correct.

``` r
Long <- Long %>%
        filter(memorig == 1) %>%
        mutate(sex_dv = ifelse(sex_dv == 1, "male",
                                ifelse(sex_dv == 2, "female", NA))) %>%
        mutate(vote6 = recode(vote6, `-9` = NA_integer_,
                              `-7` = NA_integer_,
                              `-2` = NA_integer_,
                              `-1` = NA_integer_)
                )
Long %>%
        count(sex_dv)
```

    ## # A tibble: 3 x 2
    ##   sex_dv      n
    ##   <chr>   <int>
    ## 1 female 117665
    ## 2 male   100341
    ## 3 <NA>        9

``` r
Long %>%
        count(vote6)
```

    ## # A tibble: 5 x 2
    ##   vote6     n
    ##   <int> <int>
    ## 1     1 21660
    ## 2     2 70952
    ## 3     3 56134
    ## 4     4 52145
    ## 5    NA 17124

## Calculate mean political interest by sex and wave (10 points)

Political interest is an ordinal variable, but we will treat it as
interval and calculate mean political interest for men and women in each
wave.

``` r
meanVote6 <- Long %>%
        filter(!is.na(sex_dv)) %>%
        group_by(wave, sex_dv) %>%
        summarise(
                meanPolInterest = mean(vote6, na.rm = TRUE)
        )
meanVote6
```

    ## # A tibble: 14 x 3
    ## # Groups:   wave [7]
    ##    wave  sex_dv meanPolInterest
    ##    <chr> <chr>            <dbl>
    ##  1 a     female            2.84
    ##  2 a     male              2.53
    ##  3 b     female            2.82
    ##  4 b     male              2.51
    ##  5 c     female            2.87
    ##  6 c     male              2.54
    ##  7 d     female            2.89
    ##  8 d     male              2.55
    ##  9 e     female            2.87
    ## 10 e     male              2.51
    ## 11 f     female            2.81
    ## 12 f     male              2.47
    ## 13 g     female            2.73
    ## 14 g     male              2.42

## Reshape the data frame with summary statistics (20 points)

Your resulting data frame with the means is in the long format. Reshape
it to the wide format. It should look like this:

| sex\_dv | a | b | c | d | e | f | g |
| ------- | - | - | - | - | - | - | - |
| female  |   |   |   |   |   |   |   |
| male    |   |   |   |   |   |   |   |

In the cells of this table you should have mean political interest by
sex and wave.

Write a short interpretation of your findings.

``` r
meanVote6 %>%
        spread(wave, meanPolInterest)
```

    ## # A tibble: 2 x 8
    ##   sex_dv     a     b     c     d     e     f     g
    ##   <chr>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    ## 1 female  2.84  2.82  2.87  2.89  2.87  2.81  2.73
    ## 2 male    2.53  2.51  2.54  2.55  2.51  2.47  2.42

``` r
# alternatively

meanVote6 %>%
  pivot_wider(names_from = wave, values_from = meanPolInterest)
```

    ## # A tibble: 2 x 8
    ##   sex_dv     a     b     c     d     e     f     g
    ##   <chr>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    ## 1 female  2.84  2.82  2.87  2.89  2.87  2.81  2.73
    ## 2 male    2.53  2.51  2.54  2.55  2.51  2.47  2.42

Men show higher level of political interest than women (the scale is
reversed so that lower values mean higher interest; ideally we needed to
recode this making interpretations more intuitive). Political interest
seems to decrease slightly over time.

## Estimate stability of political interest (30 points)

Political scientists have been arguing how stable the level of political
interest is over the life course. Imagine someone who is not interested
in politics at all so that their value of *vote6* is always 4. Their
level of political interest is very stable over time, as stable as the
level of political interest of someone who is always very interested in
politics (*vote6* = 1). On the other hand, imagine someone who changes
their value of *votes6* from 1 to 4 and back every other wave. Their
level of political interest is very unstable.

Let us introduce a measure of stability of political interest that is
going to be equal to the sum of the absolute values of changes in
political interest from wave to wave. Let us call this measure Delta. It
is difficult for me to typeset a mathematical formula in Markdown, but
I’ll explain this informally.

Imagine a person with the level of political interest that is constant
over time: {1, 1, 1, 1, 1, 1, 1}. For this person, Delta is zero.

Now imagine a person who changes once from “very interested in politics”
to “fairly interested in politics”: {1, 1, 1, 1, 2, 2, 2}. For them,
Delta = (1 - 1) + (1 - 1) + (1 - 1) + (2 - 1) + (2 - 2) + (2 - 2) = 1.

Now imagine someone who changes from “very interested in politics” to
“not at all interested” every other wave: {1, 4, 1, 4, 1, 4, 1}. Delta
= (4 - 1) + abs(1 - 4) + (4 - 1) + abs(1 - 4) + (4 - 1) + abs(1 - 4) = 3
\* 6 = 18.

Large Delta indicates unstable political interest. Delta = 0 indicates a
constant level of political interest.

Write the R code that does the following.

1.  To simplify interpretation, keep only the respondents with
    non-missing values for political interest in all seven waves.
2.  Calculate Delta for each person in the data set.
3.  Calculate mean Delta for men and women.
4.  Calculate mean Delta by age (at wave 1) and plot the local
    polynomial curve showing the association between age at wave 1 and
    mean Delta. You can use either **ggplot2** or the *scatter.smooth()*
    function from base R.
5.  Write a short interpretation of your findings.

<!-- end list -->

``` r
# make the panel balanced keeping only people with non-missing vote6 in all 7 waves
Balanced <- Long %>%
        group_by(pidp) %>%
        mutate(nNA = sum(!is.na(vote6))) %>%
        filter(nNA == 7) %>%
        select(-nNA) %>%
        ungroup()
# Code age at wave 1
Balanced <- Balanced %>%
        group_by(pidp) %>%
        mutate(age1 = min(age_dv)) %>%
        ungroup()
# convert to the wide format
Wide <- Balanced %>%
        select(pidp, sex_dv, age1, wave, vote6) %>%
        spread(wave, vote6)
# check that pidp is unique
Wide %>%
        count(pidp) %>%
        filter(n > 1)
```

    ## # A tibble: 1 x 2
    ##         pidp     n
    ##        <int> <int>
    ## 1 1428755487     2

``` r
# drop this inconsistent observation
Wide <- Wide %>%
        filter(pidp != 1428755487)
# create a variable for Delta
Wide <- Wide %>%
        mutate(Delta = abs(b-a) + abs(c-b) + abs(d-c) + abs(e-d) + abs(f-e) + abs(g-f))
# Mean Delta by sex
Wide %>%
        group_by(sex_dv) %>%
        summarise(
                meanDelta = mean(Delta)
        )
```

    ## # A tibble: 2 x 2
    ##   sex_dv meanDelta
    ##   <chr>      <dbl>
    ## 1 female      2.49
    ## 2 male        2.53

``` r
# Mean delta by age: loess plot
ggplot(Wide,
       aes(x = age1, y = Delta)) + 
        geom_smooth()
```

![](assignment3solution_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

The variability of political interest over time is remarkably low.
People aged over 60 seem to change their level of political interest
more frequently.
