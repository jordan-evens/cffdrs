test_that("Checking fbpRaster...", {

  ## Equal input / output length.
  test_fbp <- rast("tests/data/rasters/fbpRaster_test3/fbpRaster_test3.tif")
  names(test_fbp) <- c( "CFB","CFC","FD","HFI","RAZ","ROS","SFC","TFC")
  input <- rast(system.file("extdata", "test_fbpRaster.tif", package = "cffdrs"))
  # Stack doesn't hold the raster layer names, we have to assign
  # them:
  names(input) <- c("FuelType","LAT","LONG","ELV","FFMC","BUI", "WS","WD","GS","Dj","D0","hr","PC",
                    "PDF","GFL","cc","theta","Accel","Aspect","BUIEff","CBH","CFL","ISI")
  # Primary outputs:
  #system.time(foo <- fbpRaster(input = input))
  # Using the "select" option:
  #system.time(foo <- fbpRaster(input = input, select = c("HFI", "TFC", "ROS")))
  # Secondary outputs:
  #system.time(foo <- fbpRaster(input = input, output = "S"))
  # All outputs:
  #system.time(foo <- fbpRaster(input = input, output = "A"))

  ### Additional, longer running examples  ###
  # Keep only the required input layers, the other layers would be
  # assigned with default values:
  # keep only the required inputs:
  dat0 <- input[[c(
    "FuelType", "LAT", "LONG", "FFMC", "BUI", "WS", "GS", "Dj", "Aspect"
  )]]
  system.time(foo <- fbpRaster(input = dat0, output = "S"))
  m <- minmax(foo1 - test_fbp)
  expect_true(all(abs(m) < 1e-5))


  foo1_dt <- data.frame(foo)
  test_fbp_dt <- data.frame(test_fbp)
  names(test_fbp_dt) <- names(foo1_dt)
  (foo1_dt-test_fbp_dt) < 1e-5
})