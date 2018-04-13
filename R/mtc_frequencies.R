#' Make a dataframe GTFS tables all joined together for route frequency calculations
#' @param a GTFSr object for a given provider with routes, stops, stop_times, etc
#' @export
#' @return a mega-GTFSr data frame with stops, stop_times, trips, calendar, and routes all joined
join_all_gtfs_tables <- function(g) {
  df <- list(g$stops_df,g$stop_times_df,g$trips_df,g$calendar_df,g$routes_df)
  Reduce(inner_join,df) %>%
    select(agency_id, stop_id, trip_id, service_id,
           monday, tuesday, wednesday, thursday, friday,
           saturday,sunday,
           route_id, trip_headsign, direction_id,
           arrival_time, stop_sequence,
           route_type, stop_lat, stop_lon) %>%
    arrange(agency_id, trip_id, service_id,
            monday, tuesday, wednesday, thursday, friday,
            saturday,sunday,
            route_id, trip_headsign, direction_id,
            arrival_time, stop_sequence) -> df_sr
  #clean up source data
  rm(df)
  df_sr$Route_Pattern_ID<-paste0(df_sr$agency_id,
                                 "-",df_sr$route_id,"-",
                                 df_sr$direction_id)
  return(df_sr)
}

#' Get the service id's for weekdays
#' @param a gtfsr object
#' @export
#' @return service_ids for weekday trips
weekday_service_ids <- function(g1) {
  gtfs_df <- g1$calendar_df
  gtfs_df <- subset(gtfs_df, gtfs_df$monday == 1 & 
                      gtfs_df$tuesday == 1 & 
                      gtfs_df$wednesday == 1 & 
                      gtfs_df$thursday == 1 & 
                      gtfs_df$friday == 1)
  return(gtfs_df$service_id)
}

#' Get the service id's for saturday
#' @param a gtfsr object
#' @export
#' @return service_ids character vector
saturday_service_ids <- function(g1) {
  gtfs_df <- g1$calendar_df
  gtfs_df <- subset(gtfs_df, gtfs_df$saturday == 1)
  return(gtfs_df$service_id)
}

#' Get the service id's for sunday
#' @param a gtfsr object
#' @export
#' @return service_ids character vector
sunday_service_ids <- function(g1) {
  gtfs_df <- g1$calendar_df
  gtfs_df <- subset(gtfs_df, gtfs_df$sunday == 1)
  return(gtfs_df$service_id)
}


#' Get all times a bus stops during weekday service for any service id
#' @param a dataframe made by joining all the GTFS tables together
#' @export
#' @return a mega-GTFSr dataframe filtered to weekday services
all_weekday_bus_service <- function(gtfs_df) {
  gtfs_df <- subset(gtfs_df, gtfs_df$monday == 1 & 
                      gtfs_df$tuesday == 1 & 
                      gtfs_df$wednesday == 1 & 
                      gtfs_df$thursday == 1 & 
                      gtfs_df$friday == 1 & 
                      gtfs_df$route_type == 3)
  return(gtfs_df)
}

#' Get all times a bus stops during saturday service for any service id
#' @param a dataframe made by joining all the GTFS tables together
#' @export
#' @return a mega-GTFSr dataframe filtered to saturday services
saturday_bus_service <- function(gtfs_df) {
  gtfs_df <- subset(gtfs_df, gtfs_df$saturday == 1 & 
                      gtfs_df$route_type == 3)
  return(gtfs_df)
}

#' Get all times a bus stops during sunday service for any service id
#' @param a dataframe made by joining all the GTFS tables together
#' @export
#' @return a mega-GTFSr dataframe filtered to sunday services
sunday_bus_service <- function(gtfs_df) {
  gtfs_df <- subset(gtfs_df, gtfs_df$sunday == 1 &
                      gtfs_df$route_type == 3)
  return(gtfs_df)
}

######
##Custom Time Format Functions
######

#' Make a dataframe GTFS arrival_time column into standard time variable
#' @param dataframe containing a GTFS-style "arrival_time" column (time values at +24:00:00)
#' @export
#' @return dataframe containing a GTFS-style "arrival_time" column (no time values at +24:00:00)
make_arrival_hour_less_than_24 <- function(df) {
  t1 <- df$arrival_time
  if (!(typeof(t1) == "character")) {
    stop("column not a character string--may already be fixed")
  }
  df$arrival_time <- sapply(t1,FUN=fix_hour)
  df$arrival_time <- as.POSIXct(df$arrival_time, format= "%H:%M:%S")
  df
}

#' @export
has_service <- function(df_sr) {
  return(!dim(df_sr)[[1]]==0)
}

#' Format a time string in the expected format
#' @param x a GTFS hour string with an hour greater than 24
#' @param hour_replacement the hour to replace the >24 value with
#' @export
#' @return a string formatted hh:mm:ss 
format_new_hour_string <- function(x,hour_replacement) {
  xl <- length(unlist(strsplit(x,":")))
  if (xl > 3){
    stop("unexpected time string")
  }
  minute <- as.integer(unlist(strsplit(x,":"))[[2]])
  second <- as.integer(unlist(strsplit(x,":"))[[3]])
  x <- paste(c(hour_replacement,minute,second),collapse=":")
  return(x)
}

#' Format GTFS Time strings as standard time string
#' @param a GTFS Time string
#' @export
#' @return Time string with no hours greater than 24
fix_hour <- function(x) {
  # use:
  #   t1 <- stop_times$arrival_time
  #   stop_times$arrival_time <- sapply(t1,FUN=fix_hour)
  if(!is.na(x)) {
    hour <- as.integer(unlist(strsplit(x,":"))[[1]])
    if(!is.na(hour) & hour > 23) {
      hour <- hour-24
      x <- format_new_hour_string(x, hour)
      if (hour > 47){
        stop("hour is greater than 47 in stop times")
      }
    }
  }
  x
}

######
##Custom Bus Frequency Functions
######

#' Get trips and headways based on the number of trips a set of buses on a route take in a specified time frame
#' @param a dataframe made by joining all the GTFS tables together
#' @param a start time filter hh:mm:ss
#' @param an end time filter hh:mm:ss
#' @export
#' @return a mega-GTFSr dataframe filtered to rows of interest
filter_by_time <- function(rt_df, 
                           time_start="06:00:00", 
                           time_end="09:59:00") {
  time_start <- paste(c(format(Sys.Date(), "%Y-%m-%d"),
                        time_start),collapse=" ")
  time_end <- paste(c(format(Sys.Date(), "%Y-%m-%d"),
                      time_end),collapse=" ")
  rt_df_out <- subset(rt_df, 
                      rt_df$arrival_time > time_start
                      & rt_df$arrival_time < time_end)
  return(rt_df_out)
}

#' for a mega-GTFSr dataframe, count the number of trips a bus takes through a given stop within a given time period
#' @param a mega-GTFSr dataframe
#' @param wide if true, then return a wide rather than tidy data frame
#' @param service_id (optional) a service id to filter by
#' @export
#' @return a dataframe of stops with a "Trips" variable representing the count trips taken through each stop for a route within a given time frame
count_departures <- function(rt_df, select_service_id, wide=FALSE) {
  rt_df_out <- rt_df %>%
    group_by(agency_id,
             route_id,
             direction_id,
             trip_headsign,
             stop_id,
             service_id) %>%
    summarise(departures = n()) %>% 
    as.data.frame()
  #with this summary, every route has 1 stop with a departure count of 1
  #this isn't right and throws off headway calculations
  #need to describe this better but for now slicing out
  rt_df_out <- rt_df_out %>% 
    group_by(route_id) %>%
      arrange(departures) %>%
        slice(1:n()-1)
  if(!missing(select_service_id)) {
    rt_df_out <- rt_df_out %>% filter(service_id %in% select_service_id)
  }
  if(wide==TRUE){
    rt_df_out <- rt_df_out %>%
      unite(service_and_direction, direction_id,service_id) %>%
      tibble::rowid_to_column() %>%
      spread(service_and_direction, departures, sep="_")
  }

  return(rt_df_out)
}

#` Get a set of stops for a route
#' @param a gtfsr object
#' @return count of service by id
#' @export
count_trips_for_service <- function(g1) {
  df <- group_by(g1$trips_df,service_id) %>% summarise(n_trips = n())
  return(arrange(df,
                 desc(n_trips)))
}

#` Get the service_id of the day of service with the most trips
#' @param a gtfsr object
#' @param service_ids (optional) a set of service ids you want to check against
#' @return the service with the most trips (likely the most representative)
#' @export
get_representative_service <- function(g1, service_ids) {
  df <- count_trips_for_service(g1)
  if(!missing(service_ids)) {
    df <- df %>% filter(df$service_id %in% service_ids)
  }
  return(df[[1,1]])
}

#` Get a set of stops for a route
#' @param a dataframe output by join_mega_and_hf_routes()
#' @param route_id the id of the route
#' @param service_id the service for which to get stops 
#' @return stops for a route
#' @export
get_stops_for_route <- function(g1, select_route_id, select_service_id) {
  some_trips <- g1$trips_df %>%
    filter(route_id %in% select_route_id & service_id %in% select_service_id)
  
  some_stop_times <- g1$stop_times_df %>% 
    filter(trip_id %in% some_trips$trip_id) 
  
  some_stops <- g1$stops_df %>%
    filter(stop_id %in% some_stop_times$stop_id)
  
  some_stops$route_id <- select_route_id
  return(some_stops)
}

#` Get a set of stops for a set of routes
#' @param a dataframe output by join_mega_and_hf_routes()
#' @param route_ids the ids of the routes
#' @param service_id the service for which to get stops 
#' @return stops for routes
#' @export
get_stops_for_routes <- function(g1, route_ids, select_service_ids) {
  l1 = list()
  i <- 1
  for (route_id in route_ids) {
    l1[[i]] <- get_stops_for_route(g1,route_id, select_service_ids)
    i <- i + 1
  }
  df_stops <- do.call("rbind", l1)
  return(df_stops)
}

get_routes_for_stops <- function(stop_ids) {
  some_stop_times <- g1$stop_times_df %>% 
    filter(stop_id %in% some_stops$stop_id) 
  
  some_trips <- g1$trips_df %>%
    filter(trip_id %in% some_stop_times$trip_id)
  
  some_routes <- g1$routes_df %>%
    filter(route_id %in% some_trips$route_id)
}

#` Get stop frequency for buses based on mtc headway calculations
#' @param g1 a gtfsr object
#' @param start_time the start of the period of interest
#' @param end_time the end of the period of interest
#' @param service default to "weekday", can also use "weekend" currently
#' @return stops for routes
#' @export
get_stop_frequency <- function(g1, start_time, 
                               end_time, 
                               service="weekday") {
  df_sr <- join_all_gtfs_tables(g1)
  df_sr <- make_arrival_hour_less_than_24(df_sr)
  if(service=="weekday"){
    df_sr <- all_weekday_bus_service(df_sr)
  } else if (service=="saturday") {
    df_sr <- saturday_bus_service(df_sr) }
    else if (service=="sunday") {
    df_sr <- sunday_bus_service(df_sr)
  } else {
    print("unknown service-should be weekend or weekday")
  }
  
  if (has_service(df_sr)) {
    
    output_stops <- filter_by_time(df_sr,
                               time_start=start_time, 
                               time_end=end_time)
    
    output_stops <- count_departures(output_stops) 
    
    #departure count to headway
    t1 <- hms(end_time) - hms(start_time)
    minutes1 <- period_to_seconds(t1)/60
    output_stops$headway <- minutes1/output_stops$departures
    return(output_stops)
  }
}

#` Get stop frequency for buses based on mtc headway calculations
#' @param x a row from a csv describing mtc 511 data sources
#' @return a spatial dataframe for april amendment 1, or an error message
#' @export
process_april_amendment1 <- function(x) {
  agency_id1 <- x[['PrivateCode']]
  print(agency_id1)
  zip_request_url = paste0('https://api.511.org/transit/datafeeds?api_key=',
                           api_key,
                           '&operator_id=',
                           agency_id1)
  
  g1 <- zip_request_url %>% import_gtfs
  
  time_start1 <- "6:00:00" 
  time_end1 <- "9:59:00"
  
  am_stops <- get_stop_frequency(g1, 
                                 time_start1, 
                                 time_end1, 
                                 service="weekday")
  
  time_start1 <- "15:00:00" 
  time_end1 <- "18:59:00"
  
  pm_stops <- get_stop_frequency(g1, 
                                 time_start1, 
                                 time_end1, 
                                 service="weekday")
  
  
  
  if (has_service(am_stops) & has_service(pm_stops)) {
    stops_am_pm <- inner_join(am_stops,
                              pm_stops, 
                              suffix = c("_am", "_pm"),
                              by=c("agency_id", 
                                   "route_id", 
                                   "direction_id", 
                                   "trip_headsign", 
                                   "stop_id"))
    
    route_headways <- stops_am_pm %>%
      group_by(route_id) %>%
      summarise(headways_am = as.integer(round(median(headway_am),0)), ### we use median here because it is the most representative, 
                headways_pm = as.integer(round(median(headway_pm),0))) ### and more robust against outliers than mean
    
    qualifying_routes_stats <- route_headways %>% 
      filter(route_headways$headways_am < 16 & 
               route_headways$headways_pm < 16) %>%
      as.data.frame()
    
    route_ids <- unique(qualifying_routes_stats$route_id)
    
    qualifying_stops <- get_stops_for_routes(g1,route_ids,weekday_service_ids(g1))
    
    qualifying_stops_sf <- left_join(qualifying_stops,
                                     qualifying_routes_stats, 
                                     by="route_id")
    
    qualifying_stops_sf <- stops_df_as_sf(qualifying_stops_sf)
    qualifying_stops_sf <- qualifying_stops_sf %>% 
      select(stop_id,route_id,stop_name,headways_am,headways_pm)
  }
  return(qualifying_stops_sf)
}

#` Get stop frequency for buses based on mtc headway calculations
#' @param x a row from a csv describing mtc 511 data sources
#' @return a spatial dataframe for april amendment 2, or an error message
#' @export
process_april_amendment2 <- function(x) {
  time_start1 <- "06:00:00" 
  time_end1 <- "19:59:00"
  
  stops_freq <- get_stop_frequency(g1, 
                                   time_start1, 
                                   time_end1, 
                                   service="weekday")
  
  if (has_service(stops_freq)) {
    route_headways <- stops_freq %>%
      group_by(route_id) %>%
      summarise(headways = as.integer(round(median(headway),0)))
    ### we use median here because it is the most representative, 
    ### and more robust against outliers than mean
    
    qualifying_routes_stats <- route_headways %>% 
      filter(headways<21) %>%
      as.data.frame()
    
    route_ids <- unique(qualifying_routes_stats$route_id)
    
    qualifying_stops <- get_stops_for_routes(g1,route_ids,weekday_service_ids(g1))
    
    qualifying_stops_sf <- left_join(qualifying_stops,
                                     qualifying_routes_stats, 
                                     by="route_id")
    
    qualifying_stops_sf <- stops_df_as_sf(qualifying_stops_sf)
    
    qualifying_stops_sf <- qualifying_stops_sf %>% select(stop_id,route_id,stop_name,headways)
  }
  return(qualifying_stops_sf)
}

#` Get stop frequency for buses based on mtc headway calculations
#' @param x a row from a csv describing mtc 511 data sources
#' @return a spatial dataframe for april amendment 2, or an error message
#' @export
process_april_amendment3 <- function(x) {
  time_start1 <- "08:00:00" 
  time_end1 <- "19:59:00"
  
  sat_stops <- get_stop_frequency(g1, 
                                  time_start1, 
                                  time_end1, 
                                  service="saturday")
  
  sun_stops <- get_stop_frequency(g1, 
                                  time_start1, 
                                  time_end1, 
                                  service="sunday")
  
  if (has_service(sat_stops) & has_service(sun_stops)) {
    stops_sat_sun <- inner_join(sat_stops,
                                sun_stops, 
                                suffix = c("_sat", "_sun"),
                                by=c("agency_id", 
                                     "route_id", 
                                     "direction_id", 
                                     "trip_headsign", 
                                     "stop_id"))
    route_headways <- stops_sat_sun %>%
      group_by(route_id) %>%
      summarise(headways_sat = as.integer(round(median(headway_sat),0)), ### we use median here because it is the most representative, 
                headways_sun = as.integer(round(median(headway_sun),0))) ### and more robust against outliers than mean
    
    qualifying_routes_stats <- route_headways %>% 
      filter(route_headways$headways_sat < 31 & route_headways$headways_sun < 31) %>%
      as.data.frame()
    
    route_ids <- unique(qualifying_routes_stats$route_id)
    
    qualifying_stops <- get_stops_for_routes(g1,route_ids,saturday_service_ids(g1))
    
    qualifying_stops_sf <- left_join(qualifying_stops,
                                     qualifying_routes_stats, 
                                     by="route_id")
    
    qualifying_stops_sf <- stops_df_as_sf(qualifying_stops_sf)
    
    qualifying_stops_sf <- qualifying_stops_sf %>% select(stop_id,route_id,stop_name,headways_sat,headways_sun)
  }
  return(qualifying_stops_sf)
}