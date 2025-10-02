## The parameter "data" is reserved for the data object passed on from the previous app

# to display messages to the user in the log file of the App in MoveApps
# one can use the function from the logger.R file:
# logger.fatal(), logger.error(), logger.warn(), logger.info(), logger.debug(), logger.trace()

rFunction <- function(data,
                      res = "mean",
                      unit = "hours",
                      map_type = "osm:streets",
                      map_token = NULL,
                      map_res = 1,
                      fps = 25,
                      col_opt = "one",
                      path_pal = "Set 2",
                      colour_paths_by = "",
                      path_legend = TRUE,
                      caption = "",
                      file_format = "mp4",
                      margin_factor = 1.1,
                      equidistant = FALSE,
                      hide_attribution = FALSE,
                      verbose = !testthat::is_testing()) {
  # Copy data so we can return a non-modified version
  data_orig <- data
  
  frames <- generate_frames(
    data = data,
    res = res,
    unit = unit,
    map_type = map_type,
    map_token = map_token,
    map_res = map_res,
    col_opt = col_opt,
    path_pal = path_pal,
    colour_paths_by = colour_paths_by,
    path_legend = path_legend,
    caption = caption,
    margin_factor = margin_factor,
    equidistant = equidistant,
    hide_attribution = hide_attribution,
    verbose = verbose
  )
  
  # Allow for unit testing on frames output
  if (testthat::is_testing()) {
    out_file <- file.path(tempdir(), paste0("animation_moveVis.", file_format))
  } else {
    out_file <- appArtifactPath(paste0("animation_moveVis.", file_format))
  }
  
  if (file_format %in% c("3gp", "mpeg")) {
    logger.info("Using codec libx264 for file format: ", file_format)
    # For these formats, force a codec that is available on MoveApps system
    # Note that MPEG file should work but the animation quality is poor under
    # current settings.
    moveVis::animate_frames(
      frames, 
      out_file = out_file,
      codec = "libx264",
      overwrite = TRUE, 
      fps = fps,
      display = FALSE,
      verbose = verbose
    )
  } else {
    moveVis::animate_frames(
      frames, 
      out_file = out_file, 
      overwrite = TRUE, 
      fps = fps,
      display = FALSE,
      verbose = verbose
    )
  }
  
  data_orig
}

group_data <- function(x) {
  # Ensure move2 object is ordered correctly before aligning
  if(!move2::mt_is_track_id_cleaved(x)){
    logger.info("Regrouping data by individual/track.")
    x <- dplyr::arrange(x, move2::mt_track_id(x))
  }
  
  x
}

time_order_data <- function(x) {
  if(!move2::mt_is_time_ordered(x)){
    logger.info("Ordering track data chronologically.")
    x <- dplyr::arrange(x, move2::mt_track_id(x), move2::mt_time(x))
  }
  
  x
}

deduplicate <- function(x) {
  if (!move2::mt_has_unique_location_time_records(x)){
    n_dupl <- length(
      which(duplicated(paste(move2::mt_track_id(x), move2::mt_time(x))))
    )
    
    logger.info(
      paste0(
        "Your data has ", n_dupl, " duplicated location-time records. ",
        "Removing duplicates by selecting the most complete records."
      )
    )
    
    # In case of duplicates, keep the entry with fewest missing values
    x <- dplyr::mutate(x, n_na = rowSums(is.na(pick(dplyr::everything()))))
    x <- dplyr::arrange(x, "n_na") # TODO: Does this not disrupt chronological ordering?
    x <- move2::mt_filter_unique(x, criterion = "first")
  }
  
  x
}

# Parse resolution info, which can either be a string or a number with units
# (which are specified with separate MoveApps inputs)
parse_resolution <- function(res, unit) {
  res_options <- c("mean", "minimum", "maximum", "median")
  
  res <- tryCatch({
    res <- match.arg(res, res_options)
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
      res <- units::as_units(res, unit)
    }
    
    res
  })
  
  res
}

# Service and type are contained in a single setting and concatenated with `:`
# Extract each element here for use in `frames_spatial()`
parse_map_spec <- function(map_type, map_token) {
  map_service <- sub(":.*$", "", map_type)
  map_type <- sub("^[^:]*:", "", map_type)
  
  key_req <- c("osm_stamen", "osm_stadia", 
               "osm_thunderforest", "mapbox", "maptiler")
  
  if (map_service %in% key_req && map_token == "") {
    logger.warn(
      paste0(
        "Map service ", map_service, 
        " requires API authorization, but no key was provided. ",
        "You can obtain a key at the map service's website. ",
        "Using OSM topographic basemap."
      )
    )
    
    map_service <- "osm"
    map_type <- "topographic"
  }
  
  list(map_service = map_service, map_type = map_type)
}

# Wrapper for preprocessing, alignment, and static frame generation
# Bundling these features together makes it easier to write unit tests for
# frame behavior as the app itself produces only an animated file output.
generate_frames <- function(data,
                            res = "mean",
                            unit = "hours",
                            map_type = "osm:streets",
                            map_token = "",
                            map_res = 1,
                            col_opt = "one",
                            path_pal = "Set 2",
                            colour_paths_by = "",
                            path_legend = TRUE,
                            caption = "",
                            margin_factor = 1.1,
                            equidistant = FALSE,
                            hide_attribution = FALSE,
                            verbose = !testthat::is_testing()) {
  # Reorganize data as needed
  data <- deduplicate(time_order_data(group_data(data)))
  
  # Interpret resolution/unit input
  res <- parse_resolution(res, unit)
  
  # Split map provider from map type and check API access
  map_spec <- parse_map_spec(map_type, map_token)
  map_service <- map_spec[["map_service"]]
  map_type <- map_spec[["map_type"]]
  
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
        "Setting adaptation factor to 1.1"
      )
    )
    margin_factor <- 1.1
  }
  
  if (col_opt == "one") {
    logger.info("Using single colour (red) for all tracks.")
    path_colours <- "red"
    colour_paths_by <- move2::mt_track_id_column(data)
    legend_title <- "Track IDs"
  } else if (col_opt == "trackid") {
    logger.info("Colouring tracks by track ID.")
    path_colours <- function(x) grDevices::hcl.colors(x, path_pal)
    colour_paths_by <- move2::mt_track_id_column(data)
    legend_title <- "Track IDs"
  } else if (col_opt == "other") {
    logger.info(paste0("Colouring tracks by attribute \"", colour_paths_by, "\"."))
    path_colours <- function(x) grDevices::hcl.colors(x, path_pal)
    legend_title <- colour_paths_by
  } else {
    logger.warn(
      "Unrecognized colour option. Using single colour (red) for all tracks."
    )
    path_colors <- "red"
    colour_paths_by <- move2::mt_track_id_column(data)
    legend_title <- "Track IDs"
  }
  
  # If path colour var was originally integer, convert to factor to force
  # qualitative color palette (usually what we want). Needs to happen before
  # alignment as `align_move` coerces some event data column types
  data <- int_to_factor(data, colour_paths_by)
  
  m <- moveVis::align_move(data, res = res, verbose = verbose)
  
  # Note: this is based on experimental moveVis version awaiting review and not
  # yet released to dev.
  frames <- moveVis::frames_spatial(
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
    path_alpha = 0.5,
    equidistant = equidistant,
    verbose = verbose
  )
  
  logger.info(
    paste0(
      "Citation/sources for basemap '", map_type, "' from map service '", 
      map_service, "': ", get_attribution(map_service, map_type, url = TRUE)
    )
  )
  
  frames <- frames |>
    moveVis::add_labels(x = "Longitude", y = "Latitude", caption = caption) |>
    moveVis::add_northarrow() |>
    moveVis::add_scalebar() |>
    moveVis::add_timestamps(type = "label") |> 
    moveVis::add_progress(colour = "white")
  
  if (!hide_attribution) {
    frames <- add_attribution(
      frames,
      map_service,
      map_type,
      alpha = 0.8, 
      linewidth = 0, 
      size = 3
    )
  }
  
  frames
}

int_to_factor <- function(m, colour_paths_by) {
  is_track_var <- colour_paths_by %in% colnames(move2::mt_track_data(m))
  is_event_var <- colour_paths_by %in% colnames(m)
  
  if (is_track_var) {
    m <- move2::mutate_track_data(
      m, 
      "{colour_paths_by}" := int_as_factor(.data[[colour_paths_by]])
    )
  } else if (is_event_var) {
    m[[colour_paths_by]] <- int_as_factor(m[[colour_paths_by]])
  } else {
    stop("`colour_paths_by` not found in input data.")
  }
  
  m
}

int_as_factor <- function(x) {
  if (any(class(x) %in% c("integer", "integer64"))) {
    x <- as.factor(x)
  }
  
  x
}

# Helpers to generate correct basemap attributions
# (This should probably be incorporated into basemaps package in some way
# but for now this will at least mean that the app itself is not in
# violation of user agreements)
osm_attribution <- function(url = FALSE) {
  x <- "\u00A9 OpenStreetMap contributors, under ODbL"
  
  if (url) {
    x <- paste0(x, " (https://www.openstreetmap.org/copyright)")
  }
  
  x
}

stadia_attribution <- function(stamen = FALSE, url = FALSE) {
  stadia_text <- "\u00A9 Stadia Maps"
  stamen_text <- "\u00A9 Stamen Design"
  omt_text <- "\u00A9 OpenMapTiles"
  
  if (url) {
    stadia_text <- paste0(stadia_text, " (https://stadiamaps.com/)")
    stamen_text <- paste0(stamen_text, " (https://stamen.com/)")
    omt_text <- paste0(omt_text, " (https://openmaptiles.org/)")
  }
  
  if (stamen) {
    x <- paste0(
      stadia_text, " ", 
      stamen_text, " ", 
      omt_text, " ", 
      osm_attribution(url)
    )
  } else {
    x <- paste0(
      stadia_text, " ",
      omt_text, " ", 
      osm_attribution(url)
    )
  }
  
  x
}

thunderforest_attribution <- function(url = FALSE) {
  x <- "\u00A9 Thunderforest"
  
  if (url) {
    x <- paste0(x, " (http://www.thunderforest.com/)")
  }
  
  paste0(x, " ", osm_attribution(url))
}

carto_attribution <- function(url = FALSE) {
  x <- "\u00A9 CARTO"
  
  if (url) {
    x <- paste0(x, " (http://www.carto.com/attributions/)")
  }
  
  paste0(x, " ", osm_attribution(url))
}

mapbox_attribution <- function(url = FALSE) {
  x <- "\u00A9 Mapbox"
  
  
  if (url) {
    x <- paste0(x, " (https://www.mapbox.com/about/maps/)")
  }
  
  paste0(x, " ", osm_attribution(url))
}

maptiler_attribution <- function(url = FALSE) {
  x <- "\u00A9 MapTiler"
  
  
  if (url) {
    x <- paste0(x, " (https://www.maptiler.com/copyright/)")
  }
  
  paste0(x, " ", osm_attribution(url))
}

# These come from the ESRI API endpoints for each of these maptypes.
esri_attribution <- function(map_type) {
  switch(
    map_type,
    "natgeo_world_map" = "National Geographic, ESRI, Garmin, HERE, UNEP-WCMC, USGS, NASA, ESA, METI, NRCAN, GEBCO, NOAA, increment P Corp.",
    "usa_topo_maps" = "\u00A9 2013 National Geographic Society, i-cubed",
    "world_imagery" = "Source: ESRI, Maxar, Earthstar Geographics, and the GIS User Community",
    "world_physical_map" = "Source: US National Park Service",
    "world_shaded_relief" = "\u00A9 2014 ESRI",
    "world_street_map" = "Sources: ESRI, HERE, Garmin, USGS, Intermap, INCREMENT P, NRCan, ESRI Japan, METI, ESRI China (Hong Kong), ESRI Korea, ESRI (Thailand), NGCC, \u00A9 OpenStreetMap contributors, and the GIS User Community",
    "world_terrain_base" = "Sources: ESRI, USGS, NOAA",
    "world_topo_map" = "Sources: ESRI, HERE, Garmin, Intermap, increment P Corp., GEBCO, USGS, FAO, NPS, NRCAN, GeoBase, IGN, Kadaster NL, Ordnance Survey, ESRI Japan, METI, ESRI China (Hong Kong), \u00A9 OpenStreetMap contributors, and the GIS User Community",
    "world_dark_gray_base" = "ESRI, HERE, Garmin, \u00A9 OpenStreetMap contributors, and the GIS user community",
    "world_dark_gray_reference" = "ESRI, HERE, Garmin, \u00A9 OpenStreetMap contributors, and the GIS user community",
    "world_light_gray_base" = "ESRI, HERE, Garmin, \u00A9 OpenStreetMap contributors, and the GIS user community",
    "world_light_gray_reference" = "ESRI, HERE, Garmin, \u00A9 OpenStreetMap contributors, and the GIS user community",
    "world_hillshade_dark" = "Sources: ESRI, Maxar, Airbus DS, USGS, NGA, NASA, CGIAR, N Robinson, NCEAS, NLS, OS, NMA, Geodatastyrelsen, Rijkswaterstaat, GSA, Geoland, FEMA, Intermap, and the GIS user community",
    "world_hillshade" = "Sources: ESRI, Maxar, Airbus DS, USGS, NGA, NASA, CGIAR, N Robinson, NCEAS, NLS, OS, NMA, Geodatastyrelsen, Rijkswaterstaat, GSA, Geoland, FEMA, Intermap, and the GIS user community",
    "world_ocean_base" = "ESRI, Garmin, GEBCO, NOAA NGDC, and other contributors",
    "world_ocean_reference" = "Sources: ESRI, GEBCO, NOAA, National Geographic, Garmin, HERE, Geonames.org, and other contributors",
    "antarctic_imagery" = "Source: Earthstar Geographics",
    "arctic_imagery" = "Source: Earthstar Geographics",
    "arctic_ocean_base" = "ESRI, Garmin, GEBCO, NOAA NGDC, and other contributors",
    "arctic_ocean_reference" = "Sources: ESRI, GEBCO, NOAA, National Geographic, Garmin, HERE, Geonames.org, and other contributors",
    "world_boundaries_and_places_alternate" = "ESRI, HERE, Garmin, \u00A9 OpenStreetMap contributors, and the GIS user community",
    "world_boundaries_and_places" = "ESRI, HERE, Garmin, \u00A9 OpenStreetMap contributors, and the GIS user community",
    "world_reference_overlay" = "Sources: ESRI, Garmin, USGS, NPS",
    "world_transportation" = "ESRI, HERE, Garmin, \u00A9 OpenStreetMap contributors",
    "world_navigation_charts" = "\u00A9 2013 East View Cartographic"
  )
}

attribution_config <- function() {
  list(
    osm = list(
      attribution = function(x, url = FALSE) osm_attribution(url = url)
    ),
    osm_stamen = list(
      attribution = function(x, url = FALSE) stadia_attribution(stamen = TRUE, url = url)
    ),
    osm_stadia = list(
      attribution = function(x, url = FALSE) stadia_attribution(url = url)
    ),
    osm_thunderforest = list(
      attribution = function(x, url = FALSE) thunderforest_attribution(url = url)
    ),
    carto = list(
      attribution = function(x, url = FALSE) carto_attribution(url = url)
    ),
    mapbox = list(
      attribution = function(x, url = FALSE) mapbox_attribution(url = url)
    ),
    esri = list(
      attribution = function(x) esri_attribution(x)
    ),
    maptiler = list(
      attribution = function(x, url = FALSE) maptiler_attribution(url = url)
    )
  )
}

get_attribution <- function(map_service, map_type, url = FALSE) {
  config <- attribution_config()
  config[[map_service]]$attribution(map_type, url)
}

add_attribution <- function(frames, 
                            map_service, 
                            map_type, 
                            hjust = 1,
                            vjust = 0, 
                            ...) {
  map_attr <- get_attribution(map_service, map_type, url = FALSE)
  extra_args <- list(...)
  
  moveVis::add_gg(
    frames, 
    gg = ggplot2::expr(
      ggplot2::geom_label(
        ggplot2::aes(
          x = frames$aesthetics$gg.ext[3], 
          y = frames$aesthetics$gg.ext[2],
          label = !!map_attr
        ),
        hjust = !!hjust,
        vjust = !!vjust,
        !!!extra_args
      )
    )
  )
}
