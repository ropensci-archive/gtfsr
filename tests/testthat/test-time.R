library(gtfsr)
context('Converting GTFS stop times columns into lubridate columns')

# stop_times_df_as_dt()
test_that('Can convert a gtfsr stop_times dataframe to lubridate type', {
  stop_times_df <- gtfs_obj$stop_times_df
  stop_times_dt <- stop_times_df_as_dt(stop_times_df)
  expect_is(stop_times_dt$departure_time, 'Period')
})
