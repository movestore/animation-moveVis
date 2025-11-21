source("../../src/io/rds.R")

test_that("read move2", {
  capture.output(
    actual <- readRdsInput(
      sourceFile = test_path("data", "input4_move2loc_LatLon.rds")
    )
  )
  
  expect_true(move2::mt_is_move2(actual))
})
