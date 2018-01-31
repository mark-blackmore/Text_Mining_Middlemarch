Text Mining - Middlemarch vs. Wuthering Heights
================
Mark Blackmore
January 30, 2018

### Total Words

``` r
# Middlemarch
nrow(tidy_middlemarch)
```

    ## [1] 320374

``` r
# Wuthering Heights
nrow(tidy_wuthering)
```

    ## [1] 117111

### Unique Words

``` r
# Middlemarch
(repo_count_middle <- tidy_middlemarch %>%
    summarise(keys = n_distinct(word)))
```

    ## # A tibble: 1 x 1
    ##    keys
    ##   <int>
    ## 1 15675

``` r
# Wuthering Heights
(repo_count_wuthering <- tidy_wuthering %>%
    summarise(keys = n_distinct(word)))
```

    ## # A tibble: 1 x 1
    ##    keys
    ##   <int>
    ## 1  9486

### Most common words, excluding stopwords (e.g. is, the, are,...)

    ## Joining, by = "word"
    ## Joining, by = "word"

![](middlemarch_report_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-4-1.png)![](middlemarch_report_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-4-2.png)

### Wordclouds

#### Middlemarch

![](middlemarch_report_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-5-1.png)

#### Wuthering Heights

![](middlemarch_report_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-6-1.png)

### Sentimnt: Positive Words minus Negative Words by Paragraph

    ## Joining, by = "word"

![](middlemarch_report_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-7-1.png)

    ## Joining, by = "word"

![](middlemarch_report_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-7-2.png)

### Middlemarch n-grams

![](middlemarch_report_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-8-1.png)![](middlemarch_report_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-8-2.png)![](middlemarch_report_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-8-3.png)![](middlemarch_report_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-8-4.png)![](middlemarch_report_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-8-5.png)
