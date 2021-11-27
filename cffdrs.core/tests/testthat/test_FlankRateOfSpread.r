test_that("FlankRateOfSpread", {
  checkData('FlankRateOfSpread',
            cffdrs:::.FROScalc,
            list(data.table(ROS=ROS),
                 data.table(BROS=ROS),
                 data.table(LB=LB)))
})
