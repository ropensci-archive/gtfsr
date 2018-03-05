#' Get a vector of headways at a stop for a stop_times_df
#' 
#' @param stop_times_df stop_times for just 1 stop
#' @return a vector of headways
stop_times_to_headways <- function(stop_times_df) {
  stop_times_dt <- stop_times_df_as_dt(stop_times_df)
  dt1 <- stop_times_dt$departure_time
  dt1 <- dt1[order(dt1)]
  headways <- dt1[2:length(dt1)] - dt1[1:length(dt1)-1]
  return(headways)
} 

#' Get a `sf` dataframe for gtfs stops 
#' 
#' @param gtfs_obj a valid GTFS object
#' @param stop_id a valid stop_id
#' @param service_id : a valid service_id from the calendar_df
#' @param units : lubridate types: default to 'mins' 
#' @export 
#' @return a vector of headways
#' @examples 
#' a_stop_id <- gtfs_obj$stops_df[sample(nrow(gtfs_obj$stops_df), 1),]$stop_id
#' a_service_id <- gtfs_obj$calendar_df[sample(nrow(gtfs_obj$calendar_df), 1),]$service_id
#' some_headways <- stop_headway(gtfs_obj$stop_times_df,gtfs_obj$trips_df,a_stop_id,a_service_id)
#' summary(some_headways)
stop_headway <- function(a_stop_id,
                         a_service_id,
                         stop_times_df, 
                          trips_df,
                          by_route=FALSE,
                          the_units="mins") {
  trips_df <- trips_df %>%
    dplyr::filter(service_id == a_service_id)
  
  stop_times <- stop_times_df %>% 
    dplyr::filter(stop_id == a_stop_id
           & trip_id %in% trips_df$trip_id)

  if (dim(stop_times)[[1]]>1) {
    headways <- stop_times_to_headways(stop_times)
    headways <- as.numeric(headways, units=the_units)
  } else
  {
    print(paste0("there are no stops for stop ",
                 a_stop_id, " with the service_id: ",
                 a_service_id))
    headways <- c(0)
  }
  return(headways)
}

#' Get a `sf` dataframe for gtfs stops 
#' 
#' @param stop_ids a character vector of stop ids of interest
#' @param service_id a string, indicates which services (e.g. weekday) to use
#' @param stop_times a valid GTFS stop times table
#' @param trips_df a valid GTFS trips table
#' @export 
#' @return a dataframe with stats for stops
#' @examples 
#' some_stops <- gtfs_obj$stops_df[sample(nrow(gtfs_obj$stops_df), 40),]
#' some_stops_sf <- stops_df_as_sf(some_stops)
#' plot(some_stops_sf)
stops_headways <- function(stop_ids, service_id, stop_times_df, trips_df) {
  headways <- sapply(stop_ids, function(x) stop_headway(x,
                                                        service_id,
                                                        stop_times_df,
                                                        trips_df))
  summary_column_names = names(summary(headways[[1]]))
  #remove max b/c of split in peak time periods
  headway_summary_m <- t(sapply(headways,function(x) as.vector(summary(x[x < max(x)]))))
  headway_summary_df <- as.data.frame(headway_summary_m)
  names(headway_summary_df) <- summary_column_names
  headway_summary_df$stop_id <- rownames(headway_summary_df) 
  rownames(headway_summary_df) <- 1:nrow(headway_summary_df)
  return(headway_summary_df)
}
