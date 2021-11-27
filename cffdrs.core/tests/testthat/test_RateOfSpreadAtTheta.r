test_that("RateOfSpreadAtTheta", {
  expect_warning({
    checkData('RateOfSpreadAtTheta',
              cffdrs:::.ROSthetacalc,
              list(data.table(ROS=ROS),
                   data.table(FROS=ROS),
                   data.table(BROS=ROS),
                   data.table(THETA=THETA)))
  })
})

