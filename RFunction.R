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
                      reso = NULL,
                      uni = "hours",
                      maptype = "topographic",
                      mapres = 1,
                      frames_per_sec = 25,
                      col_opt = "one",
                      other = "sex",
                      show_legend = TRUE,
                      capt = "",
                      file_format = "mp4",
                      ext_adap = 1) {
  # Copy data so we can return a non-modified version
  data_orig <- data
  
  # Ensure move2 object is ordered correctly before aligning
  if(!mt_is_track_id_cleaved(data)){
    logger.info("Your data set was not grouped by individual/track. We regroup it for you.") # Is this confusing as we return the original, ungrouped data?
    data <- dplyr::arrange(data, mt_track_id(data))
  }
  
  if(!mt_is_time_ordered(data)){
    logger.info("Your data is not time ordered (within the individual/track groups). We reorder the locations for you.")
    data <- dplyr::arrange(data, mt_track_id(data), mt_time(data))
  }
  
  if (!mt_has_unique_location_time_records(data)){
    n_dupl <- length(which(duplicated(paste(mt_track_id(data), mt_time(data)))))
    
    logger.info(
      paste0(
        "Your data has ", n_dupl, " duplicated location-time records. ",
        "We removed here those with less info and then select the", 
        " first if still duplicated."
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
  if (is.character(reso)) {
    reso <- match.arg(reso, c("mean", "minimum", "maximum", "median"))
    logger.info(paste0("Using resolution :", reso, "."))
  } else if (is.numeric(reso)) {
    reso <- units::set_units(reso, uni)
    logger.info(paste0("Using resolution :", reso, " and units: ", uni, "."))
  } else {
    logger.warn(paste0("Unrecognized resolution ", reso, ". Using resolution: mean."))
    reso <- "mean"
  }
  
  if (mapres < 0 | mapres > 1) {
    logger.warn(
      "Map resolution must be between 0 and 1. Setting map resolution to 1."
    )
    mapres <- 1
  }
  
  if (ext_adap <= 0) {
    logger.warn(
      paste0(
        "Adaptation factor must be greater than 0. ",
        "Setting adaptation factor to 1."
      )
    )
    ext_adap <- 1
  }

  if (col_opt == "one") {
    logger.info("Using single color (red) for all tracks.")
    path_colors <- "red"
    colour_paths_by <- move2::mt_track_id_column(data)
    legend_title <- "Track IDs"
  } else if (col_opt=="trackid") {
    logger.info("Coloring tracks by track ID.")
    path_colors <- function(x) tim.colors(x)
    colour_paths_by <- move2::mt_track_id_column(data)
    legend_title <- "Track IDs"
  } else if (col_opt=="other") {
    logger.info(paste("Coloring tracks by attribute ", other, "."))
    path_colors <- function(x) tim.colors(x)
    colour_paths_by <- other
    legend_title <- other
  } else {
    logger.warn(
      "Unrecognized color option. Using single color (red) for all tracks."
    )
    path_colors <- "red"
    colour_paths_by <- move2::mt_track_id_column(data)
    legend_title <- "Track IDs"
  }
  
  m <- align_move(data, res = reso)
  
  # Note: this is based on experimental moveVis version awaiting review and not
  # yet released to dev.
  frames <- frames_spatial(
    m,
    path_colours = path_colours,
    colour_paths_by = colour_paths_by,
    margin_factor = ext_adap,
    path_legend = show_legend,
    path_legend_title = legend_title,
    map_service = "osm",
    map_type = maptype, 
    map_res = mapres, 
    path_alpha = 0.5
  )
  
  frames <- frames |>
    add_labels(x = "Longitude", y = "Latitude", caption = capt) |>
    add_northarrow() |>
    add_scalebar() |>
    add_timestamps(type = "label") |>
    add_progress(colour = "white")
  
  # animate frames
  animate_frames(
    frames, 
    out_file = appArtifactPath(paste0("animation_moveVis.", file_format)),
    overwrite = TRUE, 
    fps = frames_per_sec
  )
  
  data_orig
}
