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

```
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

#cache the data
save(g1, file=paste0(agency_id1,"_gtfsr.rda"))

df_sr <- join_all_gtfs_tables(g1)
df_sr <- make_arrival_hour_less_than_24(df_sr)

am_stops <- headways_by_trip(df_sr,
                             time_start="06:00:00", 
                             time_end="09:59:00")
am_stops <- rename(am_stops,am_trips=Trips,am_headway=Headways)

pm_stops <- headways_by_trip(df_sr,
                             time_start="15:00:00", 
                             time_end="18:59:00")
pm_stops <- rename(pm_stops,pm_trips=Trips,pm_headway=Headways)

stops_trips_and_frequencies_am_or_pm <- full_join(am_stops,pm_stops, 
                                                by=c("agency_id", 
                                                    "route_id", 
                                                    "direction_id", 
                                                    "trip_headsign", 
                                                    "stop_id", 
                                                    "stop_sequence"))

write_csv(stops_trips_and_frequencies_am_or_pm,
          paste0("stops_trips_and_frequencies_am_or_pm",
                 agency_id1,".csv"))

st_write(df_sf,
         paste0("stops_df_",agency_id1,".gpkg"), 
         driver='GPKG')

### Appendix

#verification check on a single route outcome, from major stops opendata

major_stops_for_agency <- major_stops_sf[major_stops_sf$agency_id==agency_id1,]

major_stops_for_agency <- inner_join(major_stops_for_agency,stops_trips_and_frequencies)

major_stops_for_agency$bus_headway <- as.integer(major_stops_for_agency$bus_headway)

route_18_stop_ids <- df_sr[df_sr$route_id=="18",]$stop_id

route_18_stops <- df_sf[df_sf$stop_id %in% route_18_stop_ids,]

mapview(route_18_stops, 
        col.regions="blue", alpha=0.8) + 
  mapview(major_stops_for_agency[major_stops_for_agency$route_id=="18",], 
          col.regions="green", alpha=0.8) 

#conclusion: the R method used here necessitates the application 
#of the stop average headway over the whole set of stops for the route after the fact
#for agencies that do not provide interpolated stop times. 

major_stops_for_agency <- major_stops_for_agency %>% 
  filter(bus_peak_period=="PM Peak" && bus_headway <16) %>% 
  mutate(headway_diff = pm_headway-bus_headway)

hist(r1$headway_diff)

#conclusion: looks like the methods here are providing different estimates for headway than
#what is available on the major stops data
#did a verification against published schedule for route 18 and the methods here were more accurate
#more manual QA would be ideal
```

