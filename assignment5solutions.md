Statistical assignment 5
================
\[add your name here\] \[add your candidate number here - mandatory\]
\[add date here\]

In this assignment we will revisit the data on ethnic intermarriage from assignment 2. First we need to read and join the data. I provide the code for this; as usual you'll need to add the file paths.

``` r
library(tidyverse)
Egoalt8 <- read_tsv("C:\\Users\\ab789\\datan3_2019\\data\\UKDA-6614-tab\\tab\\ukhls_w8\\h_egoalt.tab")
Stable <- read_tsv("C:\\Users\\ab789\\datan3_2019\\data\\UKDA-6614-tab\\tab\\ukhls_wx\\xwavedat.tab")
Partners8 <- Egoalt8 %>%
        filter(h_relationship_dv == 1 | h_relationship_dv == 2) %>%
        select(pidp, apidp, h_hidp, h_esex, h_asex, h_relationship_dv) %>%
        # filter out same-sex couples
        filter(h_esex != h_asex) %>%
        # keep only one observation per couple with women as egos
        filter(h_esex == 2)
# Selecting data on ethnicicty
Stable2 <- Stable %>%
        select(pidp, racel_dv)
# Joining the data sets.
JoinedEthn <- Partners8 %>%
        left_join(Stable2, by = "pidp")
JoinedEthn <- JoinedEthn %>%
        rename(egoRacel_dv = racel_dv)
JoinedEthn <- JoinedEthn %>%
        left_join(Stable2, by = c("apidp" = "pidp"))
JoinedEthn <- JoinedEthn %>%
        rename(alterRacel_dv = racel_dv)
rm(Egoalt8, Partners8, Stable, Stable2)
```

We now have a data frame with the information on partners' ethnicity and will work with it.

Recode ethnicity into a factor (10 points)
------------------------------------------

Recode ethnicity of both marital partners into a factor with the following levels (in this order): White British, Irish, Indian, Pakistani, Bangladeshi, Chinese, Black Caribbean, Black African, any other White background, other (includes all other codes). Missing values should be coded as NA.

``` r
JoinedEthn <- JoinedEthn %>%
        mutate(ethnF = recode_factor(egoRacel_dv,
                              `1` = "White British",
                              `2` = "Irish",
                              `9` = "Indian",
                              `10` = "Pakistani",
                              `11` = "Bangladeshi",
                              `12` = "Chinese",
                              `14` = "Black Caribbean",
                              `15` = "Black African",
                              `4` = "other White",
                              `-9` = NA_character_,
                              .default = "Other")) %>%
        mutate(ethnM = recode_factor(alterRacel_dv,
                              `1` = "White British",
                              `2` = "Irish",
                              `9` = "Indian",
                              `10` = "Pakistani",
                              `11` = "Bangladeshi",
                              `12` = "Chinese",
                              `14` = "Black Caribbean",
                              `15` = "Black African",
                              `4` = "other White",
                              `-9` = NA_character_,
                              .default = "Other"))
```

The following code will print a cross-tabulation of ethnicities of wives (in rows) and husbands (in columns). Many couples in the data set are cohabiting, but I will call them wives and husbands for simplicity.

``` r
EthnTable <- table(JoinedEthn$ethnF, JoinedEthn$ethnM)
EthnTable
```

    ##                  
    ##                   White British Irish Indian Pakistani Bangladeshi Chinese
    ##   White British            8655    93     20        18          10       7
    ##   Irish                      98   116      2         1           0       0
    ##   Indian                     24     1    499         4           2       1
    ##   Pakistani                  13     0     13       424           1       0
    ##   Bangladeshi                 5     0      0         2         243       0
    ##   Chinese                    23     0      2         0           0      29
    ##   Black Caribbean            34     1      0         0           0       1
    ##   Black African              15     0      2         2           0       0
    ##   other White               221     5      7         3           1       1
    ##   Other                     161     0     16         9           1       3
    ##                  
    ##                   Black Caribbean Black African other White Other
    ##   White British                27            17         145   100
    ##   Irish                         0             1           4     2
    ##   Indian                        3             1           4    11
    ##   Pakistani                     1             2           0    11
    ##   Bangladeshi                   0             0           0     2
    ##   Chinese                       0             1           5     3
    ##   Black Caribbean              68             7           4     7
    ##   Black African                 3           159           1    10
    ##   other White                   7             5         221    32
    ##   Other                        17            14          25   169

Probabilities of marrying a white British man or woman (20 points)
------------------------------------------------------------------

For women from all ethnic group, calculate the probabilities of being married to a White British man and print them. Then do the same for men from all ethnic groups (i.e. calculate the probabilities of being married to a white British woman). Write one or two sentences interpreting your findings.

``` r
# for women, probabilities of marrying a White British man
EthnTable[,1] / rowSums(EthnTable)
```

    ##   White British           Irish          Indian       Pakistani 
    ##      0.95193577      0.43750000      0.04363636      0.02795699 
    ##     Bangladeshi         Chinese Black Caribbean   Black African 
    ##      0.01984127      0.36507937      0.27868852      0.07812500 
    ##     other White           Other 
    ##      0.43936382      0.38795181

``` r
# for men, probabilities of marrying a White British woman
EthnTable[1,] / colSums(EthnTable) 
```

    ##   White British           Irish          Indian       Pakistani 
    ##      0.93577684      0.43055556      0.03565062      0.03887689 
    ##     Bangladeshi         Chinese Black Caribbean   Black African 
    ##      0.03875969      0.16666667      0.21428571      0.08212560 
    ##     other White           Other 
    ##      0.35452323      0.28818444

White British people have a very high probability of having a White British partner. For other ethnic groups, the probabilities vary; South Asian groups have the lowest probabilities of having a White British partner. Interestingly, Chinese women are considerably more likely to have a White British partner than Chinese men.

Odds ratios (20 points)
-----------------------

The probabilities you calculated in the previous question are affected by the ethnic composition of the population. Imagine a population of 200 people, where we have 90 white women, 90 white men, 10 non-white women, and 10 non-white men. Even if mixing is random, white people will have a much higher probability of having a white partner, compared to the probability of non-white people to have a non-white partner. (In other words, white people may get partnered with other white people more often not because they avoid non-White partners, but simply because there are more White partners available.)

To get a measure of ethnic endogamy that is independent of group size, we can calculate odds ratios. The odds for a white woman to have a white partner are calculated as: ω<sub>w</sub> = n<sub>ww</sub> / n<sub>wn</sub>, i.e. the number of white women who partnered a white man divided by the number of white women who partnered a non-White man. The odds for a non-white woman to have a white partner are calculated as: ω<sub>n</sub> = n<sub>nw</sub> / n<sub>nn</sub>, i.e. the number of non-White women who partnered a white man divided by the number of non-White women who partnered a non-White man. The odds ratio is then calculated as θ = ω<sub>w</sub>/ ω<sub>n</sub> = n<sub>ww</sub> n<sub>nn</sub> / (n<sub>wn</sub> n<sub>nw</sub>).

You can read more about how to calculate odds ratios in D.Powers & Y.Xie. (1999). Statistical Methods for Categorical Data Analysis, section 4.2.3. (Avilable here: <https://www.researchgate.net/profile/Nguyen_Trung_Hiep3/post/What_method_of_statistical_analysis_can_I_use_for_categorical_data_analysis/attachment/59d62ca379197b807798af4d/AS%3A347549920710656%401459873766230/download/Statistical+Methods+for+Categorical+Data+Analysis.pdf>)

Let me show how odds ratios work with an example.

``` r
# Let us generate some data from a fictional population of 120 couples.
df <- data.frame(female = c("White", "White", "non-White", "non-White"),
                 male = c("White", "non-White", "White", "non-White"),
                 n = c(85, 5, 5, 10))
# data represented as a table
xtabs(n ~ female + male, df)
```

    ##            male
    ## female      non-White White
    ##   non-White        10     5
    ##   White             5    85

``` r
# the odds ratio here is:

(85 * 10) / (5 * 5)
```

    ## [1] 34

``` r
# 34
```

In the example above, out of 90 white women, 85 married a white man and 5 married a non-White man. Out of 15 non-White women, 10 married a White man and 5 married a non-White man. The probability of a white woman to marry a white man is then 85/90 = 0.94. The probability of racial endogamy for a non-White woman is 5/15 = 0.33. The odds ratio is (85 \* 10) / (5 \* 5) = 34. This number shows that people of the same race are much more likely to marry each other. If mixing was random the odds ratio would have been 1. Note that in a 2x2 table only one odds ratio is meaningful. We can calculate the odds ratio for racial intermarriage by simply taking the reciprocal of 34 (1/34 = 0.03). The advantage of odds ratios is that they are independent of group size (marginal distributions in the 2x2 table) and therefore can provide a useful measure of group endogamy that can be compared across groups.

If we have more than two groups we can calculate the odds ratio separately for each group after collapsing the data to two levels: the group of interest vs. all other groups. Now calculate the odds ratio for ethnic endogamy for the White British (vs. all other groups) in our data.

``` r
WB <- JoinedEthn %>%
        filter(!is.na(ethnF)) %>%
        filter(!is.na(ethnM)) %>%
        mutate(ethn2F = ifelse(ethnF == "White British", 1, 0)) %>%
        mutate(ethn2M = ifelse(ethnM == "White British", 1, 0)) %>%
        count(ethn2F, ethn2M)
table2x2 <- xtabs(n ~ ethn2F + ethn2M, WB)
OR <-  (table2x2[[1,1]] * table2x2[[2,2]]) / (table2x2[[1,2]] * table2x2[[2,1]])      
OR
```

    ## [1] 73.08693

Write a function to calculate odds ratios (20 points)
-----------------------------------------------------

Now write a function that calculates odds ratios. A function must take as an argument a data frame with three variables ("female", "male" and "n") and four observations, and return the odds ratio. For example, for the data frame **df** (see above) the function should return 34 (once you've got your function written check that it actually returns 34 for **df**).

``` r
oddsRatio <- function(df){
        t <- xtabs(n ~ female + male, df)
        OR <-  (t[[1,1]] * t[[2,2]]) / (t[[1,2]] * t[[2,1]])
        OR
}
oddsRatio(df)
```

    ## [1] 34

Use iteration to calculate multiple odds ratios (30 points)
-----------------------------------------------------------

Now use a for() loop to iterate over the ethnic groups in our data set and calculate the odds ratio for ethnic endogamy for each of them. Use the function you have just written. Print the odds ratios for all the groups in your output document. Write a short interpretation of the results. Which groups are most endogamous?

``` r
ethnGroups <- levels(JoinedEthn$ethnF)

JoinedEthn <- JoinedEthn %>% 
        filter(!is.na(ethnF)) %>%
        filter(!is.na(ethnM)) 

# Initialise a data frame  for the results for 10 ethnic groups
results <- data.frame(ethnGroup = rep(NA, 10), or = rep(NA, 10))
# initialise a counter for iteration number
k <- 1

for (i in ethnGroups) {
        df <- JoinedEthn %>%
                mutate(female = ifelse(ethnF == i, 1, 0)) %>%
                mutate(male = ifelse(ethnM == i, 1, 0)) %>%
                count(female, male)
        results[k,1] <- i
        results[k,2] <- oddsRatio(df)
        k <- k + 1
}
results
```

    ##          ethnGroup          or
    ## 1    White British    73.08693
    ## 2            Irish   124.09852
    ## 3           Indian  1777.90449
    ## 4        Pakistani  3015.99500
    ## 5      Bangladeshi 20899.80000
    ## 6          Chinese   774.33937
    ## 7  Black Caribbean   253.97957
    ## 8    Black African  1168.20833
    ## 9      other White    46.63360
    ## 10           Other    43.55451

The levels of ethnic endogamy vary by ethnic group, with Bangladeshi, Pakistani and Indians being the most endogamous. This may be the effect both of the structure of opportunities (of meeting and developing intimate relationships with people from other ethnic groups) and choice.
