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
    rFunction(d)
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

test_that("`margin_factor` works", {
  capture.output(
    frames <- generate_frames(d, margin_factor = 0.6, map_res = 0.1)
  )
  vdiffr::expect_doppelganger("frames-5-mf-low", frames[[5]])
  
  capture.output(
    frames <- generate_frames(d, margin_factor = 1.5, map_res = 0.1)
  )
  vdiffr::expect_doppelganger("frames-5-mf-high", frames[[5]])
})

test_that("Can provide res as text or numeric", {
  # Use output as proxy here, since it's difficult to programatically
  # determine what alignment was used without returning the output of
  # `align_move()`
  expect_output(
    frames <- generate_frames(d, res = "max"),
    "\\[INFO\\] Using resolution: maximum"
  )
  expect_output(
    frames <- generate_frames(d, res = 1, unit = "days"),
    "\\[INFO\\] Using resolution: 1 \\(days\\)"
  )
  expect_output(
    frames <- generate_frames(d, res = "foobar"),
    "\\[WARN\\] Unrecognized resolution. Using resolution: mean"
  )
})
