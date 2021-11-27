test_that("RateOfSpreadAtTime", {
  checkData('RateOfSpreadAtTime',
            cffdrs:::.ROStcalc,
            list(data.table(FUELTYPE=FUELTYPE),
                 data.table(ROSeq=ROS),
                 data.table(HR=HR),
                 data.table(CFB=CFB)))
})
