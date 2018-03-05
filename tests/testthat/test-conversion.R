library(gtfsr)
context('Converting GTFS routes and shapes into simple feature (sf) dataframes')

not_working <- function() {
	url <- "http://data.trilliumtransit.com/gtfs/duke-nc-us/duke-nc-us.zip"
	connecting <- function(url) {
		r <- base::try(httr::GET(url, httr::timeout(5)))
		if(!assertthat::is.error(r)) r$status_code == 200 else FALSE
	}
	connecting(url)
}

check_url <- function() {
  if (not_working()) {
    skip("Test URL not available.")
  }
}

url <- "http://data.trilliumtransit.com/gtfs/duke-nc-us/duke-nc-us.zip"
gtfs_obj <- url %>% import_gtfs(quiet=TRUE)

# convert_gtfs_routes_to_sf()
test_that('Can convert a gtfsr routes dataframe to a simple features dataframe', {
	expect_is(routes_df_as_sf(gtfs_obj), 'sf')
})

context('Converting GTFS stop times columns into lubridate columns')

# stop_times_df_as_dt()
test_that('Can convert a gtfsr stop_times dataframe to lubridate type', {
  stop_times_df <- gtfs_obj$stop_times_df
  stop_times_dt <- stop_times_df_as_dt(stop_times_df)
  expect_is(stop_times_dt$departure_time, 'Period')
})