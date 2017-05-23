## nbaTools

This is an R package for scraping NBA related data. 

It is meant to replace my previous package, [sportsTools](https://github.com/ccagrawal/sportsTools). This one will be easier to use, and the code quality is higher. The code is heavily influenced by a Python package, [nba_py](https://github.com/seemethere/nba_py).

### Installation and Usage

There is no version of nbaTools on CRAN. To get the current development version from Github:

```R
install.packages("devtools")    # If you don't have devtools already installed
devtools::install_github("ccagrawal/nbaTools")
library(nbaTools)
```

### Sources

Data is scraped from the following great sources:

- [NBA.com](http://stats.nba.com/)
