test_raster <- function(name, fct) {
  set.seed(666)
  input <- raster(  nrows = 25,  ncols = 25,  crs = "EPSG:3402",  resolution = 100,  ymn = 5652012,  ymx = 5652012 + (25 * 100),  xmn = 565550,  xmx = 565550 + (25 * 100),  vals = sample(x = 19:27, size = 25 * 25, replace = TRUE))

  input <- stack(  test_gfmc_r,  setValues(test_gfmc_r, sample(x = 0:3, size = 25 * 25, replace = TRUE)),  setValues(test_gfmc_r, sample(x = 10:20, size = 25 * 25, replace = TRUE)),  setValues(test_gfmc_r, sample(x = 30:70, size = 25 * 25, replace = TRUE)),  setValues(test_gfmc_r,sample(x = (5:950) / 1000,size = 25 * 25,replace = TRUE)
  )
  )
  names(input) <- c("temp", "prec", "ws", "rh", "isol")
  # input <- crop(input, c(250, 255, 47, 51))

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

test_that("gfmcRaster_test1", {
  test_raster(
    "gfmcRaster_test1",
    function(input) {
      gfmcRaster(input)
    }
  )
})

test_that("gfmcRaster_test2", {
  test_raster(
    "gfmcRaster_test2",
    function(input) {
      gfmcRaster(input, out="GFMCandMC")
    }
  )
})

