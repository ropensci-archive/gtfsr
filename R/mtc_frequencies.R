#' Make a dataframe GTFS tables all joined together for route frequency calculations
#' @param a GTFSr object for a given provider with routes, stops, stop_times, etc
#' @export
#' @return a mega-GTFSr data frame with stops, stop_times, trips, calendar, and routes all joined
join_all_gtfs_tables <- function(g) {
  df <- list(g$stops_df,g$stop_times_df,g$trips_df,g$calendar_df,g$routes_df)
  Reduce(inner_join,df) %>%
    select(agency_id, stop_id, trip_id, service_id,
           monday, tuesday, wednesday, thursday, friday,
           route_id, trip_headsign, direction_id,
           arrival_time, stop_sequence,
           route_type, stop_lat, stop_lon) %>%
    arrange(agency_id, trip_id, service_id,
            monday, tuesday, wednesday, thursday, friday,
            route_id, trip_headsign, direction_id,
            arrival_time, stop_sequence) -> df_sr
  #clean up source data
  rm(df)
  df_sr$Route_Pattern_ID<-paste0(df_sr$agency_id,
                                 "-",df_sr$route_id,"-",
                                 df_sr$direction_id)
  return(df_sr)
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

#' for a mega-GTFSr dataframe, remove rows with duplicate stop times 
#' @param a dataframe of stops with a stop_times column 
#' @export
#' @return a dataframe of stops with a stop_times column in which there are no duplicate stop times for a given stop
remove_duplicate_stops <- function(rt_df){
  rt_df %>%
    distinct(agency_id, route_id, direction_id,
             trip_headsign, stop_id, stop_sequence, arrival_time) %>%
    arrange(agency_id, route_id, direction_id,
            arrival_time,stop_sequence)->rt_df_out
  return(rt_df_out)
}

#' for a mega-GTFSr dataframe, count the number of trips a bus takes through a given stop within a given time period
#' @param a mega-GTFSr dataframe
#' @param wide if true, then return a wide rather than tidy data frame
#' @export
#' @return a dataframe of stops with a "Trips" variable representing the count trips taken through each stop for a route within a given time frame
count_departures <- function(rt_df, wide=FALSE) {
  rt_df_out <- rt_df %>%
    group_by(agency_id,
             route_id,
             direction_id,
             trip_headsign,
             stop_id,
             service_id) %>%
    summarise(departures = n()) %>% 
    as.data.frame() %>%
    select(-trip_headsign)
  if(wide==TRUE){
    rt_df_out <- rt_df_out %>%
      unite(service_and_direction, direction_id,service_id) %>%
      tibble::rowid_to_column() %>%
      spread(service_and_direction, departures, sep="_")
  }
  return(rt_df_out)
}

#' 
#' @param a mega-GTFSr dataframe filtered to AM peak commute period stops
#' @param a mega-GTFSr dataframe filtered to PM peak commute period stops
#' @param a mega-GTFSr get_routes reduced dataframe filtered to AM peak commute period stops
#' @param a mega-GTFSr get_routes reduced dataframe filtered to PM peak commute period stops
#' @export
#' @return a dataframe of stops/routes flagged as TPA eligible or not
join_high_frequency_routes_to_stops <- function(am_stops,pm_stops,am_routes,pm_routes){
  # Combine Weekday High Frequency Bus Service Data Frames for AM/PM Peak Periods
  df1 <- rbind(am_routes,
               pm_routes)
  
  # This ID is used for grouping and headway counts 
  #(same name as another id in here but dropped anyway)
  #should probably replace at some point
  if (!(is.data.frame(am_routes) && nrow(am_routes)==0)){
    df1$Route_Pattern_ID<-paste0(df1$agency_id,
                                 "-",df1$route_id,"-",
                                 df1$Peak_Period)
  } else 
  {
    df1$Route_Pattern_ID <-  df1$Peak_Period
  }
  
  # Count number of routes that operate in both directions during peak periods.
  #TPA_Criteria = 2 or 3 then Route operates in both directions during peak periods
  #TPA Criteria = 1 possible loop route or route only operates in ection during peak periods.
  
  df2 <- df1 %>%
    group_by(agency_id, route_id, Peak_Period, Route_Pattern_ID) %>%
    summarise(TPA_Criteria = n())
  
  # 6C. Join Total By Direction with Weekday High Frequency Bus Service tables to flag those routes that meet the criteria.
  df3 <- list(df1,df2)
  df4 <- Reduce(inner_join,df3) %>%
    select(agency_id, route_id, direction_id, trip_headsign,Total_Trips, Headway, Peak_Period, TPA_Criteria) %>%
    arrange(agency_id, route_id, direction_id, Peak_Period)
  
  # 6D. Update values in TPA Criteria field. 2,3 = Meets Criteria, 1 = Review for Acceptance
  df4$TPA_Criteria[df3$TPA_Criteria==3] <- "Meets TPA Criteria"
  df4$TPA_Criteria[df3$TPA_Criteria==2] <- "Meets TPA Criteria"
  df4$TPA_Criteria[df3$TPA_Criteria==1] <- "Does Not Meet TPA Criteria"
  # 6D-1. Update values in TPA Criteria field.  All Loops in AM/PM Peak periods that have 15 mins or better headways = Meets TPA Criteria
  df4$TPA_Criteria[grepl('loop', df3$trip_headsign, ignore.case = TRUE)] <- "Meets TPA Criteria"
  
  df5 <- rbind(am_stops,pm_stops)
  
  # 6G. Join Weekday_Peak_Bus_Routes with df3 to generate a stop schedule for all AM/PM Peak Period stops that have headways of 15 mins. or better.
  df6 <- list(df5,df4)
  df7 <- Reduce(inner_join,df6) %>%
    select(agency_id, route_id, direction_id, trip_headsign, stop_id, stop_sequence, Total_Trips, Headway, Peak_Period, TPA_Criteria)
  
  return(df7)
}

#' 
#' @param a mega-GTFSr dataframe 
#' @param a mega-GTFSr get_routes reduced dataframe
#' @export
#' @return a dataframe of stops/routes flagged as TPA eligible, with some of the variables dropped from the stops table above joined back on the table
join_mega_and_hf_routes <- function(df_sr,df_rt_hf){
  df<- list(df_sr,df_rt_hf)
  
  df_stp_rt_hf <- Reduce(inner_join,df) %>%
    group_by(agency_id, route_id, direction_id, trip_id,Peak_Period, Route_Pattern_ID,
             trip_headsign, stop_id, stop_sequence, Total_Trips, Headway, Peak_Period,
             TPA_Criteria, stop_lon, stop_lat) %>%
    select(agency_id, route_id, direction_id, trip_id, Route_Pattern_ID,
           trip_headsign, stop_id, stop_sequence, Total_Trips,
           Headway, Peak_Period, TPA_Criteria,
           stop_lon, stop_lat) %>%
    arrange(agency_id, route_id, direction_id,
            trip_id, Peak_Period, stop_sequence)
  
  rm(df)
  return(df_stp_rt_hf)
}

#` Select Distinct Records based upon Agency Route Direction values.  Removes stop ids from output.
#' @param a dataframe output by join_mega_and_hf_routes()
#' @return a deduplicated version of the input dataframe
#' @export
deduplicate_final_table <- function(df_stp_rt_hf) {
  df_stp_rt_hf <- group_by(df_stp_rt_hf,
                           agency_id, route_id, direction_id, Route_Pattern_ID,trip_headsign,
                           stop_id, stop_sequence, Total_Trips, Headway, Peak_Period,
                           TPA_Criteria, stop_lon, stop_lat) %>%
    distinct(agency_id, route_id, direction_id, Route_Pattern_ID,
             trip_headsign, stop_id, stop_sequence, Total_Trips,
             Headway, Peak_Period, TPA_Criteria, stop_lon, stop_lat)
  return(df_stp_rt_hf)
}

#' Make a dataframe GTFS tables all joined together for route frequency calculations
#' @export
#' @param a GTFSr object for a given provider with routes, stops, stop_times, etc
#' @return a mega-GTFSr data frame with stops, stop_times, trips, calendar, and routes all joined
join_all_gtfs_tables <- function(g) {
  df <- list(g$stops_df,g$stop_times_df,g$trips_df,g$calendar_df,g$routes_df)
  Reduce(inner_join,df) %>%
    select(agency_id, stop_id, trip_id, service_id,
           monday, tuesday, wednesday, thursday, friday,
           route_id, trip_headsign, direction_id,
           arrival_time, stop_sequence,
           route_type, stop_lat, stop_lon) %>%
    arrange(agency_id, trip_id, service_id,
            monday, tuesday, wednesday, thursday, friday,
            route_id, trip_headsign, direction_id,
            arrival_time, stop_sequence) -> df_sr
  #clean up source data
  rm(df)
  df_sr$Route_Pattern_ID<-paste0(df_sr$agency_id,
                                 "-",df_sr$route_id,"-",
                                 df_sr$direction_id)
  return(df_sr)
}

#' Filter a mega-GTFSr dataframe to rows/stops that occur on all weekdays, are buses, and
#' have a stop_time between 2 time periods
#' @param a dataframe made by joining all the GTFS tables together
#' @param a start time filter hh:mm:ss
#' @param an end time filter hh:mm:ss
#' @export
#' @return a mega-GTFSr dataframe filtered to rows of interest
filter_weekday_by_time <- function(rt_df, start_filter, end_filter) {
  time_start <- paste(c(format(Sys.Date(), "%Y-%m-%d"),
                        start_filter),collapse=" ")
  time_end <- paste(c(format(Sys.Date(), "%Y-%m-%d"),
                      end_filter),collapse=" ")
  rt_df_out <- subset(rt_df, rt_df$monday == 1
                      & rt_df$tuesday == 1
                      & rt_df$wednesday == 1
                      & rt_df$thursday == 1
                      & rt_df$friday == 1
                      & rt_df$route_type == 3
                      & rt_df$arrival_time >time_start
                      & rt_df$arrival_time < time_end)
  return(rt_df_out)
}


#' for a mega-GTFSr dataframe, reduce it to just a listing of routes
#' @param a mega-GTFSr dataframe
#' @export
#' @return a dataframe of routes  
get_routes <- function(rt_df) {
  group_by(rt_df,
           agency_id,
           route_id,
           direction_id,
           trip_headsign,
           Peak_Period) %>%
    mutate(Total_Trips = round(mean(Trips),0),
           Headway = round(mean(Headways),0)) %>%
    distinct(agency_id,
             route_id,
             direction_id,
             trip_headsign,
             Total_Trips,
             Headway) ->
    rt_df_out
}

#' 
#' @param a mega-GTFSr dataframe filtered to AM peak commute period stops
#' @param a mega-GTFSr dataframe filtered to PM peak commute period stops
#' @param a mega-GTFSr get_routes reduced dataframe filtered to AM peak commute period stops
#' @param a mega-GTFSr get_routes reduced dataframe filtered to PM peak commute period stops
#' @export
#' @return a dataframe of stops/routes flagged as TPA eligible or not
join_high_frequency_routes_to_stops <- function(am_stops,pm_stops,am_routes,pm_routes){
  # Combine Weekday High Frequency Bus Service Data Frames for AM/PM Peak Periods
  df1 <- rbind(am_routes,
               pm_routes)
  
  # This ID is used for grouping and headway counts 
  #(same name as another id in here but dropped anyway)
  #should probably replace at some point
  if (!(is.data.frame(am_routes) && nrow(am_routes)==0)){
    df1$Route_Pattern_ID<-paste0(df1$agency_id,
                                 "-",df1$route_id,"-",
                                 df1$Peak_Period)
  } else 
  {
    df1$Route_Pattern_ID <-  df1$Peak_Period
  }
  
  # Count number of routes that operate in both directions during peak periods.
  #TPA_Criteria = 2 or 3 then Route operates in both directions during peak periods
  #TPA Criteria = 1 possible loop route or route only operates in ection during peak periods.
  
  df2 <- df1 %>%
    group_by(agency_id, route_id, Peak_Period, Route_Pattern_ID) %>%
    summarise(TPA_Criteria = n())
  
  # 6C. Join Total By Direction with Weekday High Frequency Bus Service tables to flag those routes that meet the criteria.
  df3 <- list(df1,df2)
  df4 <- Reduce(inner_join,df3) %>%
    select(agency_id, route_id, direction_id, trip_headsign,Total_Trips, Headway, Peak_Period, TPA_Criteria) %>%
    arrange(agency_id, route_id, direction_id, Peak_Period)
  
  # 6D. Update values in TPA Criteria field. 2,3 = Meets Criteria, 1 = Review for Acceptance
  df4$TPA_Criteria[df3$TPA_Criteria==3] <- "Meets TPA Criteria"
  df4$TPA_Criteria[df3$TPA_Criteria==2] <- "Meets TPA Criteria"
  df4$TPA_Criteria[df3$TPA_Criteria==1] <- "Does Not Meet TPA Criteria"
  # 6D-1. Update values in TPA Criteria field.  All Loops in AM/PM Peak periods that have 15 mins or better headways = Meets TPA Criteria
  df4$TPA_Criteria[grepl('loop', df3$trip_headsign, ignore.case = TRUE)] <- "Meets TPA Criteria"
  
  df5 <- rbind(am_stops,pm_stops)
  
  # 6G. Join Weekday_Peak_Bus_Routes with df3 to generate a stop schedule for all AM/PM Peak Period stops that have headways of 15 mins. or better.
  df6 <- list(df5,df4)
  df7 <- Reduce(inner_join,df6) %>%
    select(agency_id, route_id, direction_id, trip_headsign, stop_id, stop_sequence, Total_Trips, Headway, Peak_Period, TPA_Criteria)
  
  return(df7)
}


#' 
#' @param a mega-GTFSr dataframe 
#' @param a mega-GTFSr get_routes reduced dataframe
#' @export
#' @return a dataframe of stops/routes flagged as TPA eligible, with some of the variables dropped from the stops table above joined back on the table
join_mega_and_hf_routes <- function(df_sr,df_rt_hf){
  df<- list(df_sr,df_rt_hf)
  
  df_stp_rt_hf <- Reduce(inner_join,df) %>%
    group_by(agency_id, route_id, direction_id, trip_id,Peak_Period, Route_Pattern_ID,
             trip_headsign, stop_id, stop_sequence, Total_Trips, Headway, Peak_Period,
             TPA_Criteria, stop_lon, stop_lat) %>%
    select(agency_id, route_id, direction_id, trip_id, Route_Pattern_ID,
           trip_headsign, stop_id, stop_sequence, Total_Trips,
           Headway, Peak_Period, TPA_Criteria,
           stop_lon, stop_lat) %>%
    arrange(agency_id, route_id, direction_id,
            trip_id, Peak_Period, stop_sequence)
  
  rm(df)
  return(df_stp_rt_hf)
}

#` Select Distinct Records based upon Agency Route Direction values.  Removes stop ids from output.
#' @param a dataframe output by join_mega_and_hf_routes()
#' @export
#' @return a deduplicated version of the input dataframe
deduplicate_final_table <- function(df_stp_rt_hf) {
  df_stp_rt_hf <- group_by(df_stp_rt_hf,
                           agency_id, route_id, direction_id, Route_Pattern_ID,trip_headsign,
                           stop_id, stop_sequence, Total_Trips, Headway, Peak_Period,
                           TPA_Criteria, stop_lon, stop_lat) %>%
    distinct(agency_id, route_id, direction_id, Route_Pattern_ID,
             trip_headsign, stop_id, stop_sequence, Total_Trips,
             Headway, Peak_Period, TPA_Criteria, stop_lon, stop_lat)
  return(df_stp_rt_hf)
}


#' make high frequency routes df into just routes and directions df, for use in construction geoms by route and direction sldf
#' @param a dataframe made of am_routes and pm_routes
#' @export
#' @return a dataframe of routes by direction with headway stats for peak periods
get_route_stats <- function(df1) {
  df2 <- dcast(df1,route_id+direction_id~Peak_Period, value.var="Headway", fun.aggregate=mean)
  names(df2)[3:4] <- c("am_headway","pm_headway")
  df3 <- dcast(df1,route_id+direction_id~Peak_Period, value.var="Total_Trips", fun.aggregate=mean)
  names(df3)[3:4] <- c("am_trips","pm_trips")
  df4 <- inner_join(df2,df3)
  row.names(df4) <- paste(df2$route_id,df2$direction_id,sep="-")
  return(df4)
}

#' make high frequency routes df into just routes and directions df, for use in construction geoms by route and direction sldf
#' @param a dataframe made of am_routes and pm_routes
#' @export
#' @return a dataframe of routes with headway stats for peak periods averaged over both directions
get_route_stats_no_direction <- function(df1) {
  df2 <- dcast(df1,route_id~Peak_Period, value.var="Headway", fun.aggregate=mean)
  names(df2)[2:3] <- c("avg_am_headway","avg_pm_headway")
  df3 <- dcast(df1,route_id~Peak_Period, value.var="Total_Trips", fun.aggregate=mean)
  names(df3)[2:3] <- c("avg_am_trips","avg_pm_trips")
  df4 <- inner_join(df2,df3,by=c("route_id"))
  return(df4)
}

#' get a Route Pattern ID
#' @param dataframe
#' @export
#' @returns a vector which combines the agency id, route id, and direction id in a string 
get_route_pattern_id <- function(df) {
  df$Route_Pattern_ID<-paste0(df$agency_id,
                              "-",df$route_id,"-",
                              df$direction_id)
}

#' string match check for loop routes
#' @param headsign vector
#' @export
#' @return boolean/logical vector indicating whether its a loop
is_loop_route <- function(headsign){
  grepl('loop', headsign, ignore.case = TRUE)
}

#' check for bidirectional routes
#' @param dataframe with route_id and direction_id columns
#' @export
#' @return boolean/logical vector indicating whether the route goes in both directions
is_in_both_directions <- function(df_rt_dr){
  g1 <- group_by(df_rt_dr,route_id)
  s1 <- summarise(g1, both=both_directions_bool_check(direction_id))
  s2 <- df_rt_dr$route_id %in% s1[s1$both==TRUE,]$route_id
  return(s2)
}

#' check for dual period stops
#' @param dataframe with stop_id and "peak" columns
#' @export
#' @return boolean/logical vector indicating whether the stop is in both am and pm periods
is_in_both_periods <- function(df_rt_dr){
  g1 <- group_by(df_rt_dr,stop_id)
  s1 <- summarise(g1, both=both_periods_bool_check(Peak_Period))
  s2 <- df_rt_dr$stop_id %in% s1[s1$both==TRUE,]$stop_id
  return(s2)
}

#'given a string vector, check whether both 0 and 1 are in it
#' @param string vector of
#' @export
#' @return logical vector
both_periods_bool_check <- function(direction_ids){
  "AM Peak" %in% direction_ids & "PM Peak" %in% direction_ids
}



#'given a string vector, check whether both 0 and 1 are in it
#' @param string vector of
#' @export
#' @return logical vector
both_directions_bool_check <- function(direction_ids){
  1 %in% direction_ids & 0 %in% direction_ids
}

##################
#geospatial work
###############

#' Return a spatial dataframe with the geometries of high frequency routes
#' @param l1 is a route name
#' @param gtfs_obj is a gtfsr list of gtfs dataframes
#' @export
#' @return routes geometries as polygons, for weekend service
get_geoms <- function(route_id,gtfs_obj,weekday=TRUE,buffer=402.336) {
  out <- tryCatch({
    #get the spatial dataframe list from gtfsr
    l2 <- get_routes_sldf(gtfs_obj,route_id,NULL,NULL)
    names(l2$gtfslines) <- c("shape_id")
    
    #subset the sldf from gtfsr for weekday only
    if(weekday==FALSE) stop("we only handle weekdays")
    weekday_subset <- gtfs_obj$calendar_df$monday==1 & 
      gtfs_obj$calendar_df$tuesday==1 & 
      gtfs_obj$calendar_df$wednesday==1 & 
      gtfs_obj$calendar_df$thursday==1 & 
      gtfs_obj$calendar_df$friday==1
    chosen_services <- gtfs_obj$calendar_df[weekday_subset,c("service_id")]
    df1 <- l2$shapes_routes_df
    weekday_service_shapes <- df1[df1$service_id %in% chosen_services$service_id,]$shape_id
    lines_df <- l2$gtfslines
    df2 <- lines_df[lines_df$shape_id %in% weekday_service_shapes,]
    
    #collapse to the filtered sldf list to 1 sp Polygons class per route
    #needed to use polygons since these aren't proper Lines (connected at endpoints)
    g1 <- geometry(df2)
    g1 <- spTransform(g1, CRS("+init=epsg:26910"))
    g1 <- gBuffer(g1,width=buffer)
    g2 <- gUnaryUnion(g1,id=route_id)
    return(g2)
  }, 
  error = function(e) {NULL})
  return(out)
}

#' Return the geometries for a route as single line
#' @param a list with route_id and direction id
#' @param a list output by get_hf_geoms 
#' @export
#' @return linestring with an id for route and direction
get_single_route_geom <- function(x,hf_l) {
  r_id <- x["route_id"]
  d_id <- x["direction_id"]
  rd_id <- paste(r_id,d_id,sep="-")
  t1 <- as.data.frame(hf_l$df[hf_l$df$route_id == r_id & hf_l$df$direction_id == d_id,"shape_id"])
  dfsp1 <- hf_l$sldf[hf_l$sldf$shape_id %in% t1[,1],]
  g1 <- geometry(dfsp1)
  g2 <- gLineMerge(g1,byid=FALSE,id=rd_id)
  if(length(g2)>1){stop("more than 1 sp Line after merge of gtfs shapes for route")}
  l1 <- Line(coordinates(g2))
  l2 <- Lines(list(l1),ID=rd_id)
  return(l2)
}

get_route_geometries <- function(route_ids,buffer){
  l3 <- lapply(route_ids,FUN=get_geoms,gtfs_obj=gtfs_obj, buffer=buffer)
  
  list.condition <- sapply(l3, function(x) class(x)!="SpatialPolygons")
  l4  <- l3[list.condition]
  print(l4)
  
  list.condition <- sapply(l3, function(x) class(x)=="SpatialPolygons")
  l3  <- l3[list.condition]
  
  l3_flattened <- SpatialPolygons(lapply(l3, function(x){{x@polygons[[1]]}}))
  
  return(l3_flattened)
}

#tried doing this with "outer" but ran into s4 class coersion error
get_stop_distance_from_route <- function(bus_stop,routes) {
  route_id <- bus_stop$route_id
  route <- routes[routes$route_id==route_id,]
  distance <- gDistance(route, bus_stop)
  return(distance)
}

get_stops_distances_from_routes <- function(stops,routes) {
  distances <- numeric()
  dflength <- dim(stops)[1]
  
  #tried lapply but had s4 class issue
  for(bstop_ix in 1:dflength) {
    distances <- c(distances,get_stop_distance_from_route(stops[bstop_ix,],routes))
  }
  return(distances)
}

#' put a list of spatial dataframes (with agency_id as the list key) together into one
#' @param a list of routes sp class dataframes
#' @export
#' @return an sp class dataframe
#' 
bind_list_of_routes_spatial_dataframes <- function(l1) { 
  spdf <- l1[[1]]
  spdf$agency <- names(l1[1])
  for (s in names(l1[2:length(l1)])) {
    if(dim(l1[[s]])[1]>0) {
      tmp_sdf <- l1[[s]]
      tmp_sdf$agency <- rep(s,nrow(tmp_sdf))
      spdf <- rbind(spdf,tmp_sdf)
    }
  }
  proj4string(spdf) <- CRS("+init=epsg:26910")
  return(spdf)
}

#'write a spatial dataframe to the current working directory as a geopackage (with date in name-seconds since the epoch)
#'@param spatial dataframe
#'@export
#'@return nothing
write_to_geopackage_with_date <- function(spdf) {
  library(rgdal)
  the_name <- deparse(substitute(spdf))
  writeOGR(spdf,
           paste0(format(Sys.time(),"%s"),the_name,"_",".gpkg"),
           driver="GPKG",
           layer = the_name, 
           overwrite_layer = TRUE)
}

#'given am routes and pm routes, return a spatial dataframe with the routes and their stats
#'@param am_routes
#'@param pm_routes
#'@export
#'@return spatial dataframe (polygons) of routes
get_routes_with_geoms_and_stats <- function(am_routes,pm_routes) {
  df1 <- rbind(am_routes,pm_routes)
  
  route_ids <- names(table(df1$route_id))
  spply_rts <- get_route_geometries(route_ids, buffer=0.10)
  
  df1_stats <- get_route_stats_no_direction(df1)
  row.names(df1_stats) <- df1_stats$route_id
  
  df1_sbst <- df1_stats[df1_stats$route_id %in% getSpPPolygonsIDSlots(spply_rts),]
  spdf <- SpatialPolygonsDataFrame(Sr=spply_rts, data=as.data.frame(df1_sbst),FALSE)
  return(spdf)
}
