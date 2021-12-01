test_that("C6CrownRateOfSpread", {
  fct <- function(FUELTYPE, ISI, BUI, FMC, SFC, CBH, ROS, CFB, RSC, option)
  {
    return(CrownRateOfSpreadC6(ISI, FMC))
  }
  checkData('C6CrownRateOfSpread',
            fct,
            list(data.table(FUELTYPE=c("C6")),
                 data.table(ISI=ISI),
                 data.table(BUI=BUI),
                 data.table(FMC=FMC),
                 data.table(SFC=SFC),
                 data.table(CBH=CBH),
                 data.table(ROS=ROS),
                 data.table(CFB=CFB),
                 data.table(RSC=ROS),
                 data.table(option=c("RSC"))))
})
