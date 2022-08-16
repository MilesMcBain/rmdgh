
Here is my reprex:

``` r
  library(issuecreep)
  issuecreep:::repo_issues("doesnt/exist")
```

    ## Error in value[[3L]](cond): could not find repository on GitHub: doesnt/exist
