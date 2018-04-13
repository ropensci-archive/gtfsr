## Description

This is MTC's fork of the gtfsr package. Here we maintain functions to do things for MTC business that are probably not directly relevant to the gtfsr project.

There are multiple uses of the general functions of the gtfsr package for MTC work. See [here](https://bayareametro.github.io/Data-And-Visualization-Projects/sb827/sb827_amendment_example.html) for a how to and walk through of some of the more basic functions. 

## Installation

You can install this package from GitHub using the devtools package:

    if (!require(devtools)) {
        install.packages('devtools')
    }
    devtools::install_github('BayAreaMetro/gtfsr')

If you have already installed `gtfsr`, you can get the latest version by
running

    remove.packages('gtfsr')
    devtools::install_github('ropensci/gtfsr')

If you’d like to build the accompanying vignette, then run

    devtools::install_github('BayAreaMetro/gtfsr', build_vignettes = TRUE)

## Example Usage

Output bus stops for sb827 april amendments

```
library(gtfsr)
library(sf)
library(dplyr)
library(lubridate)
library(readr)

o511 <- read_csv("https://gist.githubusercontent.com/tibbl35/d49fa2c220733b0072fc7c59e0ac412b/raw/cff45d8c8dd2ea951b83c0be729abe72f35b13f7/511_orgs.csv")

#Sys.setenv(APIKEY511 = "YOURKEYHERE")
api_key = Sys.getenv("APIKEY511")

results <- apply(o511, 1, function(x) try(process_april_amendment1(x)))
is.error <- function(x) inherits(x, "try-error")
succeeded <- !vapply(results, is.error, logical(1))
get.error.message <- function(x) {attr(x,"condition")$message}
message <- vapply(results[!succeeded], get.error.message, "")
df_stops <- do.call("rbind", results[succeeded])
st_write(df_stops,"827_april_amendment1.csv", driver="CSV")
st_write(df_stops,"827_april_amendment1.gpkg",driver="GPKG")
st_write(df_stops,"827_april_amendment1.shp", driver="ESRI Shapefile")

o511['processed1'] <- TRUE
o511['succeeded1'] <- succeeded
o511['error_message1'] <- ""
o511[!succeeded,'error_message1'] <- message
write_csv(o511,"gtfs_processing.csv")

results <- apply(o511[1,], 1, function(x) try(process_april_amendment2(x)))
is.error <- function(x) inherits(x, "try-error")
is.sf_df <- function(x) inherits(x, "sf")
succeeded <- !vapply(results, is.error, logical(1))
get.error.message <- function(x) {attr(x,"condition")$message}
message <- vapply(results[!succeeded], get.error.message, "")
df_stops <- do.call("rbind", results[succeeded])
st_write(df_stops,"827_april_amendment2.csv", driver="CSV")
st_write(df_stops,"827_april_amendment2.gpkg",driver="GPKG")
st_write(df_stops,"827_april_amendment2.shp", driver="ESRI Shapefile")

o511['processed2'] <- TRUE
o511['succeeded2'] <- succeeded
o511['error_message2'] <- ""
o511[!succeeded,'error_message2'] <- message

results <- apply(o511[1,], 1, function(x) try(process_april_amendment3(x)))
is.error <- function(x) inherits(x, "try-error")
is.sf_df <- function(x) inherits(x, "sf")
succeeded <- !vapply(results, is.error, logical(1))
get.error.message <- function(x) {attr(x,"condition")$message}
message <- vapply(results[!succeeded], get.error.message, "")
df_stops <- do.call("rbind", results[succeeded])
st_write(df_stops,"827_april_amendment3.csv", driver="CSV")
st_write(df_stops,"827_april_amendment3.gpkg",driver="GPKG")
st_write(df_stops,"827_april_amendment3.shp", driver="ESRI Shapefile")

#write processing records back out
o511['processed3'] <- TRUE
o511['succeeded3'] <- succeeded
o511['error_message3'] <- ""
o511[!succeeded,'error_message3'] <- message
write_csv(o511,"gtfs_processing.csv")
```

