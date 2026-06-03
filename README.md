# rolleda

[![](https://github.com/jasonjfoster/rolleda/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/jasonjfoster/rolleda/actions/workflows/check-standard.yaml)

## Overview

'rolleda' provides exploratory data analysis of rolling statistics for time-series data.

The 'rolleda' package explores the rolling statistics of the 'roll' package in a 'Shiny' web application that is launched with the `roll_eda()` function.

## Installation

Install the released version from CRAN:

```r
# install.packages("rolleda")
```

Or the development version from GitHub:

```r
# install.packages("pak")
pak::pak("jasonjfoster/rolleda")
```

## Usage

Load the package and launch the application:

```r
library(rolleda)

# rolling exploratory data analysis
roll_eda()
```
