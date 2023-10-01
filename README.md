
<!-- README.md is generated from README.Rmd. Please edit that file -->

# TimeGPTapi

<!-- badges: start -->
<!-- badges: end -->

`TimeGPT` is a generative pre-trained transformer model for time series
analysis ([https://docs.nixtla.io/docs](docs.nixtla.io/docs)) developed
by **Nixtla** (<https://nixtla.io/terms>). It was trained on the largest
collection of public time series and can predict the future values of a
single time series based on the provided. The `TimeGPT API` developed by
**Nixtla** provides an interface to this powerful model, allowing users
to leverage its forecasting capabilities to predict future events based
on past. This R package provides an interface to interact with the
`TimeGPT API`.

## Installation

You can install the development version of TimeGPTapi from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("danigiro/TimeGPTapi")
```

## Quickstart

This is a basic example which shows you how to solve a common problem:

``` r
library(TimeGPTapi)

# Loading the air passengers dataset and the time vector
y <- AirPassengers
time_air <- zoo::as.Date(AirPassengers)

# Set and get the TimeGPT token
set_TimeGPT_token("<TIMEGPT_TOKEN>")
obj <- TimeGPT(AirPassengers, time_air, freq = "M", verbose = TRUE, historic = TRUE)

# Forecasting the next 12 horizons using TimeGPT
fore <- forecast(obj2, h = 12)
```

## Copyright

This R package is released under the *GNU General Public License v3.0*.
The `TimeGPT` model and `API` are proprietary technologies developed by
**Nixtla**, and all rights are reserved. The use of the `TimeGPT API` is
subject to Nixtla’s terms and conditions, which can be found at
<https://nixtla.io/terms>.
