test_that("LengthToBreadthRatio", {
  checkData('LengthToBreadthRatio',
            cffdrs:::.LBcalc,
            list(data.table(FUELTYPE=FUELTYPE),
                 data.table(WSV=WSV)))})
