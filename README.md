# bounceR

<!-- badges: start -->
<!-- badges: end -->

The bounceR package provides a set of functions to extract the Hawkeye statistics from the ICC and BCCI webpages. The matches currently included are Men's and Women's Tests, ODIs and IPL T20 matches. The Hawkeye statistics included are Ball Speed (m/s), Pitch x,y location (m), Stumps x,y location (m) and the Field x,y location (m) after the Batter's shot. Some matches may only contain Ball Speed, Pitch and Stump location statistics, while others contain only the Field location statistics. Further, some matches may contain the Field Location for all shots, while others contain only scoring shot statistics.

## Installation

You can install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("alittlefitness/bounceR")
```
## Usage

As an example to retrieve all Hawkeye stats for Women's Test matches the following code can be used:

```{r example}
get_hawkeye_stats_all(type = "TEST", competition = "WOMEN")
```

Alternatively, to retrieve the Hawkeye stats for a single match, the matchId can be supplied to the get_hawkeye_stats function:

```{r example2}
get_hawkeye_stats(id = 23469)
```

The matchId can be found on the ICC or BCCI results webpage (the number in this example url https://www.icc-cricket.com/match/23531#scorecard) or via the get_completed_matches function:

```{r example2}
get_completed_matches(type = "TEST", competition = "MEN")
```
