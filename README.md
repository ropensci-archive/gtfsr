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

Calculate headways for one provider based on MTC/State of California "stop/route headway" standard. 

``` r
library(gtfsr)
library(sf)
library(dplyr)

setwd("~/Documents/Projects/mtc/Data-And-Visualization-Projects/sb827")

major_stops_url <- "https://opendata.arcgis.com/datasets/561dc5b42fa9451b95faf615a3054260_0.geojson"
major_stops_sf <- st_read(major_stops_url)

major_stops_df <- major_stops_sf[,c('agency_id','route_id')]
st_geometry(major_stops_df) <- NULL

library(readr)

#read 511 orgs list
o511 <- read_csv("https://gist.githubusercontent.com/tibbl35/d49fa2c220733b0072fc7c59e0ac412b/raw/cff45d8c8dd2ea951b83c0be729abe72f35b13f7/511_orgs.csv")

#filter for just those agencies that have high frequency stops (these are unlikely to have gtfs data that will fail in processing)
agency_ids <- filter(o511, PrivateCode %in% major_stops_df$agency_id) %>% select(PrivateCode)
agency_ids <- agency_ids[[1]]

#Sys.setenv(APIKEY511 = "")
api_key = Sys.getenv("APIKEY511")

agency_id1 <- agency_ids[[1]] 
zip_request_url = paste0('https://api.511.org/transit/datafeeds?api_key=',
                         api_key,
                         '&operator_id=',
                         agency_id1)

g1 <- zip_request_url %>% import_gtfs

help(save)

#cache the data
save(g1, file=paste0(agency_id1,"_gtfsr.rda"))

df_sr <- join_all_gtfs_tables(g1)
df_sr <- make_arrival_hour_less_than_24(df_sr)

am_stops <- flag_and_filter_peak_periods_by_time(df_sr,
                                                 period_name="AM_Peak", 
                                                 time_start="06:00:00", 
                                                 time_end="09:59:00")
am_stops <- remove_duplicate_stops(am_stops) #todo: see https://github.com/BayAreaMetro/RegionalTransitDatabase/issues/31
am_stops <- count_trips(am_stops) 

pm_stops <- flag_and_filter_peak_periods_by_time(df_sr,
                                                 period_name="PM_Peak", 
                                                 time_start="15:00:00", 
                                                 time_end="18:59:00")
pm_stops <- remove_duplicate_stops(pm_stops) #todo: see https://github.com/BayAreaMetro/RegionalTransitDatabase/issues/31 
pm_stops <- count_trips(pm_stops)

write_csv(pm_stops,"pm_stops.csv")
write_csv(pm_stops,"am_stops.csv")

df_sf <- stops_df_as_sf(g1$stops_df)

st_write(df_sf,"stops_df.shp")
```

