# Better way to handle imports?
library(testthat)
source(test_path("helper.R"))
source("../../src/common/logger.R")
source("../../RFunction.R")

# Tests can be run in-session with
# testthat::test_file(testthat::test_path("test_RFunction.R"))

d <- test_data()

test_that("Can animate frames with default values", {
  out_file <- file.path(tempdir(), "animation_moveVis.mp4")
  
  capture.output(
    rFunction(d, res = 1, unit = "day", out_file = out_file, verbose = FALSE)
  )
  
  expect_true(file.exists(out_file))
  expect_true(file.size(out_file) > 0)
})

test_that("Can generate a static test frame", {
  out_file <- file.path(tempdir(), "animation_moveVis-frame4.png")
  
  capture.output(
    rFunction(
      d, 
      res = 1, 
      unit = "day", 
      map_res = 0.1, 
      dry_run = TRUE, 
      out_file = out_file
    )
  )
  
  expect_true(file.exists(out_file))
  expect_true(file.size(out_file) > 0)
})

test_that("Can color with single color", {
  capture.output(
    frames <- generate_frames(d, res = 1, unit = "day", map_res = 0.1)
  )
  vdiffr::expect_doppelganger("frames-5-one", frames[[5]])
})

test_that("Can color by track ID", {
  capture.output(
    frames <- generate_frames(
      d, 
      res = 1, 
      unit = "day", 
      col_opt = "trackid", 
      map_res = 0.1
    )
  )
  vdiffr::expect_doppelganger("frames-5-trackid", frames[[5]])
  
  capture.output(
    frames <- generate_frames(
      d, 
      res = 1, 
      unit = "day", 
      col_opt = "trackid", 
      path_pal = "Viridis", 
      map_res = 0.1
    )
  )
  vdiffr::expect_doppelganger("frames-5-trackid-viridis", frames[[5]])
})

# Int handling is not yet in the latest moveVis dev package so this will fail
test_that("Can color by attribute", {
  capture.output(
    frames <- generate_frames(
      d,
      res = 1, 
      unit = "day",
      col_opt = "other",
      colour_paths_by = "tag_id",
      path_pal = "Harmonic",
      map_res = 0.1
    )
  )
  
  vdiffr::expect_doppelganger("frames-5-tagid", frames[[5]])
})

test_that("Warn if no API token", {
  withr::local_envvar(list(STADIA_API_KEY = NA))
  
  expect_output(
    frames <- generate_frames(
      d, 
      res = 1, 
      unit = "day", 
      map_type = "osm_stadia:alidade_smooth"
    ),
    paste0(
      "\\[WARN\\] Map service osm_stadia requires API authorization, ",
      "but no key was provided.+"
    )
  )
  expect_equal(frames$aesthetics$map_service, "osm")
  expect_equal(frames$aesthetics$map_type, "streets")
})

test_that("Produce correct map tile citation", {
  expect_output(
    frames <- generate_frames(
      d, 
      res = 1, 
      unit = "day", 
      map_type = "osm:streets"
    ),
    paste0(
      "\\[INFO\\].+Citation.+for basemap 'streets' from map service 'osm': ",
      "\u00A9 OpenStreetMap contributors, under ODbL ",
      "\\(https://www.openstreetmap.org/copyright\\)"
    )
  )
  
  vdiffr::expect_doppelganger("frames-5-osm", frames[[5]])
})

test_that("Can provide custom map extent", {
  bbox <- sf::st_bbox(d)
  crs <- sf::st_crs("epsg:4326")
  out_crs <- sf::st_crs("epsg:3857")
  
  capture.output(
    frames <- generate_frames(
      d, 
      res = 1, 
      unit = "day",
      map_res = 0.1,
      lat_ext = "[69;  70",
      lon_ext = "(47, 50)"
    )
  )
  
  # Output CRS should match map
  expect_equal(frames$crs, out_crs)
  
  expect_equal(
    frames$aesthetics$gg.ext,
    sf::st_transform(
      sf::st_bbox(
        c(xmin = 47, ymin = 69, xmax = 50, ymax = 70), 
        crs = crs
      ),
      crs = out_crs
    )
  )
  
  expect_output(
    frames <- generate_frames(
      d, 
      res = 1, 
      unit = "day",
      map_res = 0.1,
      lat_ext = "[69;  70",
      lon_ext = "(47"
    ),
    "Invalid longitude extent.+Using longitude extent of track data"
  )
  
  expect_equal(
    frames$aesthetics$gg.ext,
    sf::st_transform(
      sf::st_bbox(
        c(xmin = bbox[[1]], ymin = 69, xmax = bbox[[3]], ymax = 70), 
        crs = crs
      ),
      crs = out_crs
    )
  )
  
  expect_output(
    frames <- generate_frames(
      d, 
      res = 1, 
      unit = "day",
      map_res = 0.1,
      lat_ext = "[69;  69",
      lon_ext = "(48, 49"
    ),
    "Invalid latitude extent.+Using latitude extent of track data"
  )
  
  expect_equal(
    frames$aesthetics$gg.ext,
    sf::st_transform(
      sf::st_bbox(
        c(xmin = 48, ymin = bbox[[2]], xmax = 49, ymax = bbox[[4]]), 
        crs = crs
      ),
      crs = out_crs
    )
  )
  
  # Should be no "Invalid" log if nothing is provided for that extent dimension
  expect_output(
    frames <- generate_frames(
      d, 
      res = 1, 
      unit = "day",
      map_res = 0.1,
      lat_ext = "[69;  70"
    ),
    "\\[INFO\\] Using custom background map extent"
  )
  
  # Check handling of decimals and negatives
  capture.output(
    frames <- generate_frames(
      d, 
      res = 1, 
      unit = "day",
      map_res = 0.1,
      lat_ext = "[69.1ab70.)",
      lon_ext = "(.-1, 49"
    )
  )
  
  expect_equal(
    frames$aesthetics$gg.ext,
    sf::st_transform(
      sf::st_bbox(
        c(xmin = -1, ymin = 69.1, xmax = 49, ymax = 70), 
        crs = crs
      ),
      crs = out_crs
    )
  )
  
  expect_error(
    capture.output(
      generate_frames(
        d, 
        res = 1, 
        unit = "day", 
        map_res = 0.1, 
        lat_ext = "[-5;  5", 
        lon_ext = "(47, 50)"
      )
    ),
    "Argument 'ext' does not overlap"
  )
})

test_that("Render map in web mercator despite input CRS", {
  capture.output(
    frames <- generate_frames(
      sf::st_transform(d, "epsg:32637"), 
      res = 1, 
      unit = "day",
      map_res = 0.1
    )
  )
  
  expect_equal(frames$crs, sf::st_crs("epsg:3857"))
  vdiffr::expect_doppelganger("frames-5-crs", frames[[5]])
})

test_that("Can provide res as text or numeric", {
  expect_error(
    frames <- generate_frames(d, res = "max"),
    "Alignment resolution `res` must be a numeric value"
  )
  expect_output(
    frames <- generate_frames(d, res = 1, unit = "day"),
    "\\[INFO\\] Aligning tracks with temporal resolution: 1 \\(day\\)"
  )
})

test_that("Can detect tracks crossing the date line", {
  expect_true(crosses_dateline(dateline_data()))
  expect_false(crosses_dateline(d))

  # a genuinely wide spread is not made narrower by shifting, so it is not
  # mistaken for a date line crossing
  spread <- function(lons) {
    sf::st_as_sf(
      data.frame(x = lons, y = rep(20, length(lons))),
      coords = c("x", "y"), crs = 4326
    )
  }
  expect_false(crosses_dateline(spread(seq(-120, 20, by = 10))))
  expect_false(crosses_dateline(spread(seq(-170, 170, by = 20))))
})

test_that("Date line data is centered on the date line by default", {
  expect_output(
    frames <- generate_frames(
      dateline_data(), res = 1, unit = "hour", map_res = 0.1
    ),
    "\\[WARN\\] Track data appear to cross the international date line"
  )

  expect_equal(frames$crs, sf::st_crs("epsg:4326"))
  expect_gt(frames$aesthetics$gg.ext[["xmax"]], 180)
})

test_that("Standard data is unaffected by date line detection", {
  out <- capture.output(
    frames <- generate_frames(d, res = 1, unit = "day", map_res = 0.1)
  )

  expect_false(any(grepl("date line", out)))
  expect_equal(frames$crs, sf::st_crs("epsg:3857"))
})

test_that("Descending longitude extent crosses the date line", {
  expect_output(
    frames <- generate_frames(
      dateline_data(), res = 1, unit = "hour", map_res = 0.1,
      lat_ext = "51.5, 53.2",
      lon_ext = "179, -179"
    ),
    "\\[INFO\\] Longitude extent is ordered east to west"
  )

  # "179, -179" describes the 2 degrees spanning the date line, not the 358
  # degrees the other way around
  expect_equal(frames$crs, sf::st_crs("epsg:4326"))
  expect_equal(
    frames$aesthetics$gg.ext,
    sf::st_bbox(
      c(xmin = 179, ymin = 51.5, xmax = 181, ymax = 53.2),
      crs = sf::st_crs("epsg:4326")
    )
  )
})

test_that("A blank longitude extent across the date line is not reported invalid", {
  # The track extent used for the blank axis is in shifted (0-360) longitudes,
  # which must not be rejected as if the user had entered them
  out <- capture.output(
    frames <- generate_frames(
      dateline_data(), res = 1, unit = "hour", map_res = 0.1,
      lat_ext = "51.5, 53.2"
    )
  )

  expect_false(any(grepl("Invalid", out)))
  expect_equal(frames$crs, sf::st_crs("epsg:4326"))
  expect_gt(frames$aesthetics$gg.ext[["xmax"]], 180)
})

test_that("Ascending longitude extent overrides date line detection", {
  capture.output(
    frames <- generate_frames(
      dateline_data(), res = 1, unit = "hour", map_res = 0.1,
      lat_ext = "51.5, 53.2",
      lon_ext = "-179, 179"
    )
  )

  expect_equal(frames$crs, sf::st_crs("epsg:3857"))
  expect_equal(
    frames$aesthetics$gg.ext,
    sf::st_bbox(
      sf::st_transform(
        sf::st_as_sfc(sf::st_bbox(
          c(xmin = -179, ymin = 51.5, xmax = 179, ymax = 53.2),
          crs = sf::st_crs("epsg:4326")
        )),
        sf::st_crs("epsg:3857")
      )
    )
  )
})

test_that("Extent coordinates outside the valid range are rejected", {
  expect_true(coords_valid(c(-50, 50), range = c(-180, 180)))
  expect_false(coords_valid(c(-50, 200), range = c(-180, 180)))
  expect_false(coords_valid(c(-200, 50), range = c(-180, 180)))
  expect_false(coords_valid(c(0, 400), range = c(-180, 180)))

  # bounds themselves are valid
  expect_true(coords_valid(c(-180, 180), range = c(-180, 180)))
  expect_true(coords_valid(c(-90, 90), range = c(-90, 90)))
  expect_false(coords_valid(c(0, 100), range = c(-90, 90)))
})

test_that("Descending longitude must only cross the date line", {
  expect_equal(parse_lon("170, -170"), c(170, -170))
  expect_equal(parse_lon("0, -10"), c(0, -10))

  # These would wrap across both the date line and the prime meridian
  expect_error(parse_lon("50.3, 45.2"), "Invalid extent")
  expect_error(parse_lon("-170, -175"), "Invalid extent")
  expect_false(resolve_dateline(d, "50.3, 45.2"))
})

test_that("Descending longitude on one side of the date line falls back", {
  expect_output(
    frames <- generate_frames(
      dateline_data(), res = 1, unit = "hour", map_res = 0.1,
      lon_ext = "-170, -175"
    ),
    "Invalid longitude extent.+Using longitude extent of track data"
  )

  # Falls back to detection, which still centers on the date line
  expect_equal(frames$crs, sf::st_crs("epsg:4326"))
})

test_that("Longitude outside -180 to 180 falls back to the track extent", {
  bbox <- sf::st_bbox(d)

  # "200" would otherwise be reinterpreted as -160 once projected
  expect_output(
    frames <- generate_frames(
      d, res = 1, unit = "day", map_res = 0.1,
      lat_ext = "69, 70",
      lon_ext = "-50, 200"
    ),
    "Invalid longitude extent.+Using longitude extent of track data"
  )

  expect_equal(
    frames$aesthetics$gg.ext,
    sf::st_transform(
      sf::st_bbox(
        c(xmin = bbox[[1]], ymin = 69, xmax = bbox[[3]], ymax = 70),
        crs = sf::st_crs("epsg:4326")
      ),
      crs = sf::st_crs("epsg:3857")
    )
  )
})

test_that("An out-of-range longitude does not suppress date line detection", {
  # An invalid extent should fall back to the same default map view as
  # if no input was provided
  expect_output(
    frames <- generate_frames(
      dateline_data(), res = 1, unit = "hour", map_res = 0.1,
      lon_ext = "-50, 200"
    ),
    "\\[WARN\\] Track data appear to cross the international date line"
  )

  expect_equal(frames$crs, sf::st_crs("epsg:4326"))

  expect_equal(
    frames$aesthetics$gg.ext,
    sf::st_bbox(sf::st_shift_longitude(
      sf::st_transform(dateline_data(), "epsg:4326")
    ))
  )
})

test_that("Latitude outside -90 to 90 falls back to the track extent", {
  bbox <- sf::st_bbox(d)

  expect_output(
    frames <- generate_frames(
      d, res = 1, unit = "day", map_res = 0.1,
      lat_ext = "0, 100",
      lon_ext = "48, 49"
    ),
    "Invalid latitude extent.+Using latitude extent of track data"
  )

  expect_equal(
    frames$aesthetics$gg.ext,
    sf::st_transform(
      sf::st_bbox(
        c(xmin = 48, ymin = bbox[[2]], xmax = 49, ymax = bbox[[4]]),
        crs = sf::st_crs("epsg:4326")
      ),
      crs = sf::st_crs("epsg:3857")
    )
  )
})

test_that("An unusable extent falls back instead of failing", {
  # when neither axis can be parsed there is no extent to transform, which
  # previously reached sf::st_as_sfc(NULL) and errored
  expect_output(
    frames <- generate_frames(
      d, res = 1, unit = "day", map_res = 0.1,
      lat_ext = "garbage",
      lon_ext = "nonsense"
    ),
    "Invalid map extent.+Using default extent for background map"
  )

  expect_is(frames, "moveVis")
  expect_equal(frames$crs, sf::st_crs("epsg:3857"))
})
