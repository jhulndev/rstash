<!-- README.md is generated from README.Rmd. Please edit that file -->
rstash
======

The purpose of this package is to abstract away where your data is being stored, giving a consistent means of accessing and moving your data whether it is on your local file system or in a cloud based object storage system.

rstash still has a way to go before being released to CRAN, but you can install the latest development version from GitHub with:

``` r
if (packageVersion('devtools') < 1.6) {
  install.packages('devtools')
}
devtools::install_github('jhulndev/rstash')
```

This package depends on [cloudyr/aws.s3](https://github.com/cloudyr/aws.s3) which is not available on CRAN at this time, but can be installed through the following:

``` r
install.packages('aws.s3', repos = c(getOption('repos'), 'http://cloudyr.github.io/drat'))
```
