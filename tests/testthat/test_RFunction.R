# Better way to handle imports?
library(testthat)
source(test_path("helper.R"))
source("../../src/common/logger.R")
source("../../RFunction.R")

# Tests can be run in-session with
# testthat::test_file(testthat::test_path("test_RFunction.R"))

d <- test_data()

test_that("Can animate frames with default values", {
  capture.output(
    rFunction(
      d, 
      out_file = file.path(tempdir(), paste0("animation_moveVis.mp4")), 
      verbose = FALSE
    )
  )
  expect_true("animation_moveVis.mp4" %in% list.files(tempdir()))
})

test_that("Can color with single color", {
  capture.output(
    frames <- generate_frames(d, map_res = 0.1)
  )
  vdiffr::expect_doppelganger("frames-5-one", frames[[5]])
})

test_that("Can color by track ID", {
  capture.output(
    frames <- generate_frames(d, col_opt = "trackid", map_res = 0.1)
  )
  vdiffr::expect_doppelganger("frames-5-trackid", frames[[5]])
  
  capture.output(
    frames <- generate_frames(d, col_opt = "trackid", path_pal = "Viridis", map_res = 0.1)
  )
  vdiffr::expect_doppelganger("frames-5-trackid-viridis", frames[[5]])
})

# Int handling is not yet in the latest moveVis dev package so this will fail
test_that("Can color by attribute", {
  capture.output(
    frames <- generate_frames(
      d,
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
    frames <- generate_frames(d, map_type = "osm_stadia:alidade_smooth"),
    paste0(
      "\\[WARN\\] Map service osm_stadia requires API authorization, ",
      "but no key was provided.+"
    )
  )
  expect_equal(frames$aesthetics$map_service, "osm")
  expect_equal(frames$aesthetics$map_type, "topographic")
})

test_that("Produce correct map tile citation", {
  expect_output(
    frames <- generate_frames(d, map_type = "carto:dark"),
    paste0(
      "\\[INFO\\].+Citation.+for basemap 'dark' from map service 'carto': ",
      "\u00A9 CARTO \\(http://www.carto.com/attributions/\\) ",
      "\u00A9 OpenStreetMap contributors, under ODbL ",
      "\\(https://www.openstreetmap.org/copyright\\)"
    )
  )
  
  vdiffr::expect_doppelganger("frames-5-carto", frames[[5]])
})

test_that("Can provide custom map extent", {
  bbox <- sf::st_bbox(d)
  crs <- sf::st_crs(d)
  
  capture.output(
    frames <- generate_frames(
      d, 
      map_res = 0.1,
      y_ext = "[69;  70",
      x_ext = "(47, 50)"
    )
  )
  
  # Output CRS should match map
  expect_equal(frames$crs, crs)
  
  expect_equal(
    frames$aesthetics$gg.ext,
    sf::st_bbox(
      c(xmin = 47, ymin = 69, xmax = 50, ymax = 70), 
      crs = crs
    )
  )
  
  expect_output(
    frames <- generate_frames(
      d, 
      map_res = 0.1,
      y_ext = "[69;  70",
      x_ext = "(47"
    ),
    "Invalid X extent.+Using background map extent"
  )
  
  expect_equal(
    frames$aesthetics$gg.ext,
    sf::st_bbox(
      c(xmin = bbox[[1]], ymin = 69, xmax = bbox[[3]], ymax = 70), 
      crs = crs
    )
  )
  
  expect_output(
    frames <- generate_frames(
      d, 
      map_res = 0.1,
      y_ext = "[69;  69",
      x_ext = "(48, 49"
    ),
    "Invalid Y extent.+Using background map extent"
  )
  
  expect_equal(
    frames$aesthetics$gg.ext,
    sf::st_bbox(
      c(xmin = 48, ymin = bbox[[2]], xmax = 49, ymax = bbox[[4]]), 
      crs = crs
    )
  )
  
  # Should be no "Invalid" log if nothing is provided for that extent dimension
  expect_output(
    frames <- generate_frames(
      d, 
      map_res = 0.1,
      y_ext = "[69;  70"
    ),
    "\\[INFO\\] Using background map extent"
  )
  
  # Check handling of decimals and negatives
  capture.output(
    frames <- generate_frames(
      d, 
      map_res = 0.1,
      y_ext = "[69.1ab70.)",
      x_ext = "(.-1, 49"
    )
  )
  
  expect_equal(
    frames$aesthetics$gg.ext,
    sf::st_bbox(
      c(xmin = -1, ymin = 69.1, xmax = 49, ymax = 70), 
      crs = crs
    )
  )
  
  expect_error(
    capture.output(
      generate_frames(d, map_res = 0.1, y_ext = "[-5;  5", x_ext = "(47, 50)")
    ),
    "Argument 'ext' does not overlap"
  )
})

test_that("Render map in data CRS", {
  capture.output(
    frames <- generate_frames(sf::st_transform(d, "epsg:32637"), map_res = 0.1)
  )
  
  expect_equal(frames$crs, sf::st_crs("epsg:32637"))
  vdiffr::expect_doppelganger("frames-5-crs", frames[[5]])
})

test_that("Can provide res as text or numeric", {
  # Use output as proxy here, since it's difficult to programatically
  # determine what alignment was used without returning the output of
  # `align_move()`
  expect_output(
    frames <- generate_frames(d, res = "max"),
    "\\[INFO\\] Aligning tracks with temporal resolution: maximum"
  )
  expect_output(
    frames <- generate_frames(d, res = 1, unit = "days"),
    "\\[INFO\\] Aligning tracks with temporal resolution: 1 \\(days\\)"
  )
  expect_output(
    frames <- generate_frames(d, res = "foobar"),
    "\\[WARN\\] Unrecognized resolution. Aligning tracks with temporal resolution: mean"
  )
})
