test_raster <- function(name, fct) {
  input <- rast(system.file("extdata", "test_rast_hour01.tif", package = "cffdrs"))
  names(input) <- c("temp", "rh", "ws", "prec")
  # input <- crop(input, c(250, 255, 47, 51))
  input2 <- stack(system.file(  "extdata","test_rast_hour02.tif", package = "cffdrs"))
  # Assign variable names to the layers:
  names(input2) <- c("temp", "rh", "ws", "prec")

  test_fbp <- read_raster(name)

  out_cols <- setdiff(names(output), toupper(names(input)))
  # we don't actually know the names of the columns from the file, so assign from output
  names(test_fbp) <- names(output)

  m <- minmax(output[[out_cols]] - test_fbp[[out_cols]])
  expect_true(all(abs(m) < 1e-2))

  output_dt <- data.frame(output)
  fbp_dt <- data.frame(test_fbp)

  expect_true(all(abs(output_dt - fbp_dt) < 1e-2))
}

test_that("fbpRaster_test1", {
  test_raster(
    "hffmcRaster_test1",
    function(input) {
      hffmcRaster(input = input)
    }
  )
})

test_that("fbpRaster_test2", {
  test_raster(
    "hffmcRaster_test2",
    function(input2) {
      hffmcRaster(input, ffmc_old =  hffmcRaster(input = input))
    }
  )
})

test_that("hffmcRaster_test3", {
  test_raster(
    "hffmcRaster_test3",
    function(input) {
      hffmcRaster(input = input,output="S")
    }
  )
})
