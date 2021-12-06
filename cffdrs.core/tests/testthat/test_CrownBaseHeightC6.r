test_that("CrownBaseHeightC6", {
  fctCBH <-function(FUELTYPE, CBH, SD, SH)
  {
    return(.CrownBaseHeight(FUELS[[FUELTYPE]], CBH, SD, SH))
  }
  checkData('CrownBaseHeightC6',
            fctCBH,
            list(data.table(FUELTYPE=c("C6")),
                 data.table(CBH=CBH),
                 data.table(SD=SD),
                 data.table(SH=SH)))
})
