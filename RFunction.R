library('move2')
library('moveVis')
library('basemaps')
library('fields')
library('sf')

## The parameter "data" is reserved for the data object passed on from the previous app

# to display messages to the user in the log file of the App in MoveApps
# one can use the function from the logger.R file:
# logger.fatal(), logger.error(), logger.warn(), logger.info(), logger.debug(), logger.trace()

rFunction <- function(data,
                      res = "mean",
                      unit = "hours",
                      map_type = "osm_streets",
                      map_token = NULL,
                      map_res = 1,
                      fps = 25,
                      col_opt = "one",
                      colour_paths_by = "sex",
                      path_legend = TRUE,
                      caption = "",
                      file_format = "mp4",
                      margin_factor = 1) {
  # Copy data so we can return a non-modified version
  data_orig <- data
  
  # Ensure move2 object is ordered correctly before aligning
  if(!mt_is_track_id_cleaved(data)){
    logger.info("Regrouping data by individual/track.")
    data <- dplyr::arrange(data, mt_track_id(data))
  }
  
  if(!mt_is_time_ordered(data)){
    logger.info("Ordering track data chronologically.")
    data <- dplyr::arrange(data, mt_track_id(data), mt_time(data))
  }
  
  if (!mt_has_unique_location_time_records(data)){
    n_dupl <- length(which(duplicated(paste(mt_track_id(data), mt_time(data)))))
    
    logger.info(
      paste0(
        "Your data has ", n_dupl, " duplicated location-time records. ",
        "Removing duplicates by selecting the most complete records."
      )
    )
    
    # In case of duplicates, keep the entry with fewest missing values
    data <- data %>%
      mutate(n_na = rowSums(is.na(pick(everything())))) %>%
      arrange(n_na) %>% # TODO: Does this not disrupt chronological ordering?
      mt_filter_unique(criterion = "first")
  }
  
  # Parse resolution info, which can either be a string or a number with units
  # (which are specified with separate MoveApps inputs)
  res <- tryCatch({
    res <- match.arg(res, c("mean", "minimum", "maximum", "median"))
    logger.info(paste0("Using resolution: ", res))
    res
  },
  error = function(cnd) {
    res <- suppressWarnings(as.numeric(res))
    
    if (is.na(res)) {
      logger.warn("Unrecognized resolution. Using resolution: mean")
      res <- "mean"
    } else {
      logger.info(paste0("Using resolution: ", res, " (", unit, ")"))
      res <- units::set_units(res, unit)
    }
    
    res
  })
  
  map_service <- sub("_.*$", "", map_type)
  map_type <- sub("^[^_]*_", "", map_type)
  
  if (map_res < 0 | map_res > 1) {
    logger.warn(
      "Map resolution must be between 0 and 1. Setting map resolution to 1."
    )
    map_res <- 1
  }
  
  if (margin_factor <= 0) {
    logger.warn(
      paste0(
        "Adaptation factor must be greater than 0. ",
        "Setting adaptation factor to 1."
      )
    )
    margin_factor <- 1
  }
  
  if (col_opt == "one") {
    logger.info("Using single color (red) for all tracks.")
    path_colors <- "red"
    colour_paths_by <- move2::mt_track_id_column(data)
    legend_title <- "Track IDs"
  } else if (col_opt == "trackid") {
    logger.info("Coloring tracks by track ID.")
    path_colors <- function(x) tim.colors(x)
    colour_paths_by <- move2::mt_track_id_column(data)
    legend_title <- "Track IDs"
  } else if (col_opt == "other") {
    logger.info(paste("Coloring tracks by attribute ", colour_paths_by, "."))
    path_colors <- function(x) tim.colors(x)
    colour_paths_by <- colour_paths_by
    legend_title <- colour_paths_by
  } else {
    logger.warn(
      "Unrecognized color option. Using single color (red) for all tracks."
    )
    path_colors <- "red"
    colour_paths_by <- move2::mt_track_id_column(data)
    legend_title <- "Track IDs"
  }
  
  m <- align_move(data, res = res)
  
  # Note: this is based on experimental moveVis version awaiting review and not
  # yet released to dev.
  frames <- frames_spatial(
    m,
    path_colours = path_colours, # New handling in dev moveVis being used here
    colour_paths_by = colour_paths_by, # New handling in dev moveVis being used here
    margin_factor = margin_factor,
    path_legend = path_legend,
    path_legend_title = legend_title,
    map_service = map_service,
    map_token = map_token,
    map_type = map_type, 
    map_res = map_res,
    path_alpha = 0.5
  )
  
  frames <- frames |>
    add_labels(x = "Longitude", y = "Latitude", caption = caption) |>
    add_northarrow() |>
    add_scalebar() |>
    add_timestamps(type = "label") |>
    add_progress(colour = "white")
  
  # animate frames
  animate_frames(
    frames, 
    out_file = appArtifactPath(paste0("animation_moveVis.", file_format)),
    overwrite = TRUE, 
    fps = fps
  )
  
  data_orig
}
