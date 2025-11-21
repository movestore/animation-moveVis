load_test_data <- function(test_file) {
  readRDS(file = file.path(test_path("data"), test_file))
}

# Create a smaller filtered data source for use throughout tests
test_data <- function() {
  load_test_data("input4_move2loc_LatLon.rds") |> 
    dplyr::filter(timestamp < "2013-08-18") |>
    move2::mt_filter_per_interval(unit = "day")
}