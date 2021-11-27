test_that("DistanceAtTime", {
  checkData('DistanceAtTime',
            cffdrs:::.DISTtcalc,
            list(data.table(FUELTYPE=FUELTYPE),
                 data.table(ROSeq=ROS),
                 data.table(HR=HR),
                 data.table(CFB=CFB)))
})
