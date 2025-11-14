## The parameter "data" is reserved for the data object passed on from the previous app

# to display messages to the user in the log file of the App in MoveApps
# one can use the function from the logger.R file:
# logger.fatal(), logger.error(), logger.warn(), logger.info(), logger.debug(), logger.trace()

#' Generate a track animation using moveVis
#'
#' @param data Input move2 object
#' @param res Numeric value expressing the temporal resolution for track 
#'   alignment in units specified by `unit`.
#' @param unit Unit of temporal alignment used with the provided `res`.
#' @param map_type Desired basemap. Combination of basemap provider and map
#'   type, separated with a colon. These correspond to the `map_service` and 
#'   `map_type` arguments in `basemaps::basemap()`, respectively
#' @param map_token API key to be used when requesting the indicated `map_type`,
#'   if needed.
#' @param map_res Value between 0 and 1 controlling the number of tiles 
#'   requested from the map tile provider indicated in `map_type`. Essentially,
#'   this controls the zoom level used when requesting tiles. At the moment,
#'   this value is not exposed in the app settings, as the default value of
#'   1 should be appropriate. Note that it is not possible to use this argument
#'   to increase detail beyond the number of tiles requested when `map_res = 1`.
#'   To retrieve more "zoomed-in" tiles, you must pass `custom_zoom` to
#'   `frames_spatial()` instead.
#' @param high_res Logical indicating whether to retrieve high resolution
#'   tiles from the map tile provider in `map_type`. This is not exposed to the
#'   end user. Currently, we retrieve detailed tiles. This increases processing
#'   time but produces sharper maps, particularly when rendered in large format.
#' @param lat_ext,lon_ext Geographic extent to use for the animation basemap, in
#'   latitude/longitude coordinates.
#' @param fps Frames per second to use in the rendered animation
#' @param col_opt Selection indicating how tracks are to be colored. Either
#'   `"one"`, `"trackid"`, or `"other"`. For `"other"`, a variable in the input
#'   data is used for coloring (see `colour_paths_by`).
#' @param path_pal Color palette to use when coloring the tracks, if `col_opt`
#'   is not `"one"`
#' @param colour_paths_by Name of the event or track attribute to use when
#'   coloring the tracks, if `col_opt = "other"`.
#' @param path_legend Logical indicating whether a legend should be included
#'   in the output animation.
#' @param caption A caption to include in the output animation, if any.
#' @param file_format File format for the output animation.
#' @param out_file Name of the output animation file.
#' @param hide_attribution Logical indicating whether the basemap citation
#'   information included in the map should be hidden.
#' @param out_width,out_height Output dimensions for the animation, in centimeters. 
#' @param out_res Output resolution. Not exposed to the user. Fixed at 300.
#' @param verbose Logical indicating whether to print logger information.
rFunction <- function(data,
                      res = NULL,
                      unit = "hour",
                      map_type = "osm:streets",
                      map_token = "",
                      map_res = 1,
                      lat_ext = NULL,
                      lon_ext = NULL,
                      fps = 25,
                      col_opt = "one",
                      path_pal = "Set 2",
                      colour_paths_by = "",
                      path_legend = TRUE,
                      caption = "",
                      file_format = "mp4",
                      out_file = NULL,
                      hide_attribution = FALSE,
                      out_width = 15,
                      out_height = 15,
                      out_res = 300,
                      dry_run = FALSE,
                      verbose = TRUE) {
  # Copy data so we can return a non-modified version
  data_orig <- data
  
  # Wrapper to interpret default settings and generate frames
  frames <- generate_frames(
    data = data,
    res = res,
    unit = unit,
    map_type = map_type,
    map_token = map_token,
    map_res = map_res,
    lat_ext = lat_ext,
    lon_ext = lon_ext,
    col_opt = col_opt,
    path_pal = path_pal,
    colour_paths_by = colour_paths_by,
    path_legend = path_legend,
    caption = caption,
    hide_attribution = hide_attribution,
    verbose = verbose
  )
  
  out_width <- cm_to_px(out_width, res = out_res)
  out_height <- cm_to_px(out_height, res = out_res)
  
  if (dry_run) {
    # Get mid-animation frame for better sense of track style?
    i <- floor(length(frames) / 2)
    
    out_file <- out_file %||% 
      appArtifactPath(paste0("animation_moveVis-frame", i, ".png"))
    
    grDevices::png(
      out_file, 
      width = out_width, 
      height = out_height, 
      res = out_res
    )
    
    print(frames[[i]])
    grDevices::dev.off()
    
    return(data_orig)
  }
  
  out_file <- out_file %||% 
    appArtifactPath(paste0("animation_moveVis.", file_format))
  
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
      verbose = verbose,
      width = out_width,
      height = out_height,
      res = out_res
    )
  } else {
    moveVis::animate_frames(
      frames, 
      out_file = out_file, 
      overwrite = TRUE, 
      fps = fps,
      display = FALSE,
      verbose = verbose,
      width = out_width,
      height = out_height,
      res = out_res
    )
  }
  
  data_orig
}

group_data <- function(x) {
  # Ensure move2 object is ordered correctly before aligning
  if(!move2::mt_is_track_id_cleaved(x)){
    logger.info(
      "Data not grouped by track. Regrouping data by individual/track."
    )
    x <- dplyr::arrange(x, move2::mt_track_id(x))
  }
  
  x
}

time_order_data <- function(x) {
  if(!move2::mt_is_time_ordered(x)){
    logger.info(
      paste0(
        "Input data not in chronological order. ",
        "Ordering data chronologically within tracks."
      )
    )
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
        "Detected ", n_dupl, " duplicated location-time records. ",
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
  if (!is.numeric(res)) {
    stop("Alignment resolution `res` must be a numeric value")
  }
  unit <- match.arg(unit, c("sec", "min", "hour", "day"))
  
  logger.info(paste0("Aligning tracks with temporal resolution: ", res, " (", unit, ")"))
  units::as_units(res, unit)
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
                            unit = "hour",
                            map_type = "osm:streets",
                            map_token = "",
                            map_res = 1,
                            lat_ext = NULL,
                            lon_ext = NULL,
                            col_opt = "one",
                            path_pal = "Set 2",
                            colour_paths_by = "",
                            path_legend = TRUE,
                            caption = "",
                            hide_attribution = FALSE,
                            verbose = TRUE) {
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
  
  # If either y or x extent is provided, build custom bbox
  if (!is.null(lat_ext) || !is.null(lon_ext)) {
    bbox <- sf::st_bbox(sf::st_transform(data, "epsg:4326"))
    
    # If one of the axes is not provided, use the bbox extent as a default
    lat_ext <- lat_ext %||% paste(bbox[2], bbox[4])
    lon_ext <- lon_ext %||% paste(bbox[1], bbox[3])
    
    # Construct geog extent for the output map
    map_ext <- get_map_ext(
      lat_ext, 
      lon_ext, 
      crs = sf::st_crs("epsg:4326"), 
      default_bbox = bbox
    )
    
    # Input extent is in 4326, but map output will be in Web Mercator,
    # so we transform the extent to Web Mercator
    map_ext <- sf::st_bbox(
      sf::st_transform(sf::st_as_sfc(map_ext), "epsg:3857")
    )
  } else {
    # Otherwise use moveVis default extent
    map_ext <- NULL
    logger.info("Using default extent for background map.")
  }
  
  if (!is.null(map_ext)) {
    logger.info(
      paste0(
        "Using background map extent: ",
        "Y: (", map_ext$ymin, ", ", map_ext$ymax, ") ",
        "X: (", map_ext$xmin, ", ", map_ext$xmax, ")"
      )
    )
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
  
  # Custom alignment function to set smarter defaults based on colouring 
  # variable and improve moveVis::align_move() processing time
  m <- align_move_(
    data, 
    res = res, 
    colour_paths_by = colour_paths_by, 
    verbose = verbose
  )
  
  frames <- moveVis::frames_spatial(
    m,
    map_service = map_service,
    map_token = map_token,
    map_type = map_type,
    map_res = map_res,
    path_size = 2,
    ext = map_ext,
    crs = sf::st_crs("epsg:3857"),
    crs_graticule = sf::st_crs("epsg:4326"),
    path_colours = path_colours,
    colour_paths_by = colour_paths_by,
    path_legend = path_legend,
    path_legend_title = legend_title,
    equidistant = FALSE,
    verbose = verbose,
    interpolate = TRUE
  )
  
  logger.info(
    paste0(
      "Citation info for basemap '", map_type, "' from map service '", 
      map_service, "': ", get_attribution(map_service, map_type, url = TRUE)
    )
  )
  
  frames <- frames |>
    moveVis::add_labels(x = "X", y = "Y", caption = caption) |>
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

# Wrapper of moveVis::align_move() to improve speed based on app input.
# 
# By default, `fill_na_values = TRUE`, which with large data sets can take 
# a long time and cause memory issues in MoveApps.
#
# This function subsets the input move2 object
# to only include the essential columns for animation, converts integers
# to factors (typically integers represent ID values in move2 objects)
# and sets `fill_na_values` for `align_move()` based on whether we need
# to interpolate attribute values for colouring or not. These steps
# help speed up `align_move()` significantly. 
align_move_ <- function(data, res, colour_paths_by, verbose) {
  is_track_var <- colour_paths_by %in% colnames(move2::mt_track_data(data))
  is_event_var <- colour_paths_by %in% colnames(data)
  
  if (is_track_var) {
    data <- dplyr::select(data)
    data <- move2::select_track_data(data, dplyr::all_of(colour_paths_by))
    
    # Convert integer to factor
    data <- move2::mutate_track_data(
      data, 
      "{colour_paths_by}" := int_as_factor(.data[[colour_paths_by]])
    )
    
    # No need to interpolate event vars if coloring by a track attribute
    fill_na_values <- FALSE
  } else if (is_event_var) {
    # Retain only necessary columns to improve speed of `align_move()`
    data <- dplyr::select(data, dplyr::all_of(colour_paths_by))
    data <- move2::select_track_data(data)
    
    # If path colour var was originally integer, convert to factor to force
    # qualitative color palette (usually what we want). Needs to happen before
    # alignment as `align_move()` coerces some event data column types
    data[[colour_paths_by]] <- int_as_factor(data[[colour_paths_by]])
    
    # Need to interpolate event variable values as we are colouring by one.
    fill_na_values <- TRUE
  } else {
    logger.error(
      paste0(
        "Unrecognized colouring attribute: \"", colour_paths_by, 
        "\". Make sure this attribute exists in your input data."
      )
    )
    stop("Attribute \"", colour_paths_by, "\" not found in input data.")
  }
  
  moveVis::align_move(
    data, 
    res = res, 
    verbose = verbose,
    fill_na_values = fill_na_values
  )
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
      attribution = function(x, url = FALSE) esri_attribution(x)
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

# This identifies and separates all numbers from an arbitrary string
# while preserving negative signs and decimals where appropriate.
split_coords <- function(x) {
  x <- regmatches(x, gregexpr("-?\\d*\\.?\\d+", x))
  suppressWarnings(as.numeric(x[[1]]))
}

# Basic check that parsed lat/lon coordinates from user input string are
# valid
coords_valid <- function(x) {
  length(x) == 2 && all(!is.na(x)) && all(is.numeric(x)) && x[1] != x[2]
}

# Wrapper to parse user input coordinates
parse_coords <- function(x) {
  x <- split_coords(x)
  valid <- coords_valid(x)
  
  if (!valid) {
    stop("Invalid extent coordinates provided.")
  }
  
  x
}

# Construct map extent from a set of input lat/lon coordinates, using
# a given bounding box as a default fallback in the event of malformed
# user input
get_map_ext <- function(lat_ext, lon_ext, crs, default_bbox) {
  # Try to parse input coords
  lat_ext <- try(parse_coords(lat_ext), silent = TRUE)
  lon_ext <- try(parse_coords(lon_ext), silent = TRUE)
  
  # If they both fail, use moveVis default map extent
  # Otherwise use backup bbox extent for the failed dimension
  if (inherits(lat_ext, "try-error") && inherits(lon_ext, "try-error")) {
    logger.warn("Invalid map extent. Using default extent for background map.")
    map_ext <- NULL
  } else {
    if (inherits(lat_ext, "try-error")) {
      logger.warn("Invalid Y extent. Using Y extent of track data.")
      lat_ext <- c(default_bbox[2], default_bbox[4])
    } else if (inherits(lon_ext, "try-error")) {
      logger.warn("Invalid X extent. Using X extent of track data.")
      lon_ext <- c(default_bbox[1], default_bbox[3])
    }
    
    # Construct extent
    map_ext <- sf::st_bbox(
      c(
        xmin = min(lon_ext), 
        ymin = min(lat_ext), 
        xmax = max(lon_ext), 
        ymax = max(lat_ext)
      ),
      crs = crs
    )
    
    if (!sf::st_is_valid(sf::st_as_sfc(map_ext))) {
      logger.warn(paste0(
        "Input extent produced invalid geometries. ",
        "Using default extent for background map."
      ))
      
      map_ext <- NULL
    }
  }
  
  map_ext
}

cm_to_px <- function(x, res = 300) {
  # Convert from cm to pixels at a given resolution
  x <- (x / 2.54) * res
  
  # Ensure output is divisible by 2 for video encoding
  floor(x / 2) * 2
}

`%||%` <- function(x, y) {
  if (is.null(x)) {
    y 
  } else {
    x
  }
}
