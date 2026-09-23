load_test_data <- function(test_file) {
  readRDS(file = file.path(test_path("data"), test_file))
}

# Create a smaller filtered data source for use throughout tests
test_data <- function() {
  load_test_data("input4_move2loc_LatLon.rds") |> 
    dplyr::filter(timestamp < "2013-08-18") |>
    move2::mt_filter_per_interval(unit = "day")
}

# Synthetic tracks crossing the international date line
dateline_data <- function() {
  wrap_lon <- function(x) ((x + 180) %% 360) - 180
  n <- 16
  ts <- as.POSIXct("2024-06-01 00:00:00", tz = "UTC") + (seq_len(n) - 1) * 3600

  df <- data.frame(
    x = c(
      wrap_lon(seq(178.8, 181.2, length.out = n)),
      wrap_lon(seq(181.2, 178.8, length.out = n))
    ),
    y = c(
      52.5 + sin(seq(0, pi, length.out = n)) * 0.25,
      52.2 - sin(seq(0, pi, length.out = n)) * 0.25
    ),
    timestamp = rep(ts, 2),
    track = rep(c("A_east", "B_west"), each = n)
  )

  move2::mt_as_move2(
    df,
    coords = c("x", "y"),
    time_column = "timestamp",
    track_id_column = "track",
    crs = sf::st_crs(4326)
  )
}
