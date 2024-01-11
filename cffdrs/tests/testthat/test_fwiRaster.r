test_that("Output length is input length", {

  ## Equal input / output length.
  test_fwi <- rast("tests/data/rasters/fwiRaster_test1/fwiRaster_test1.tif")
  names(test_fwi) <- c("TEMP", "RH", "WS", "PREC", "FFMC", "DMC", "DC", "ISI", "BUI", "FWI", "DSR")
  day01src <- rast(
    system.file("extdata", "test_rast_day01.tif", package = "cffdrs")
  )
  day01 <- crop(day01src, c(250, 255, 47, 51))
  test_fwi <- crop(test_fwi, c(250, 255, 47, 51))
  # assign variable names:
  names(day01) <- c("temp", "rh", "ws", "prec")
  day01 <- fwiRaster(input = day01)
  #day01 <- subset(x = day01, subset = "LAT", negate = T)
  m <- minmax(day01 - test_fwi)
  expect_true(all(abs(m) < 1e-5))

  day01_dt <- data.frame(day01)
  fwi_dt <- data.frame(test_fwi)

  which((day01_dt - fwi_dt) > 1e-5)
})

# test_that("The function produces the expected output.", {
#   ## Equal Output
#   test_fwiRaster_out <- rast(system.file("./inst/extdata/test_fwiRaster_out.tif", package = "cffdrs"))
#   test_fwiRaster_out <- mask(test_fwiRaster_out,day01)
#   expect_equal(object = round(fwiRaster(input = day01)[[1]][],2),expected = round(test_fwiRaster_out[[1]][],2))
#
# })