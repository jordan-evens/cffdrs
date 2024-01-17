test_that("Test Hffmc Raster",{

  input1 <- rast(system.file("extdata", "test_rast_hour01.tif", package = "cffdrs"))
  names(input1) <- c("temp", "rh", "ws", "prec")
  output1 <- hffmcRaster(input1)

  input2 <- rast(system.file("extdata", "test_rast_hour02.tif", package = "cffdrs"))
  names(input) <- c("temp", "rh", "ws", "prec")

  test_that("hffmcRaster_test1", {
    test_raster("hffmcRaster_test1",
                input1,
                function(input) { output1 })
    }
  )

  test_that("hffmcRaster_test2", {
    test_raster(
      "hffmcRaster_test2",
      input2,
      function(input) {
        hffmcRaster(input, ffmc_old =  output1)
        }
      )
    }
  )

  test_that("hffmcRaster_test3", {
    test_raster(
      "hffmcRaster_test3",
      input2,
      function(input) {
        hffmcRaster(input,ffmc_old = output1, hourlyFWI = TRUE)
        }
      )
    }
  )

})