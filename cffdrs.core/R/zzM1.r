.M1 <- setClass(".M1", contains="Fuel")
setMethod(".BackRateOfSpread",
          ".M1",
          function(this, FFMC, BUI, WSV, FMC, SFC, PC, PDF, CC, CBH)
          {
            return(BackRateOfSpread("M1", FFMC, BUI, WSV, FMC, SFC, PC, PDF, CC, CBH))
          }
)
setMethod(".BuildupEffect",
          ".M1",
          function(this, BUI)
          {
            return(BuildupEffect("M1", BUI))
          }
)
setMethod(".CrownFuelConsumption",
          ".M1",
          function(this, CFL, CFB, PC, PDF)
          {
            return(CrownFuelConsumption("M1", CFL, CFB, PC, PDF))
          }
)
setMethod(".DistanceAtTime",
          ".M1",
          function(this, ROSeq, HR, CFB)
          {
            return(DistanceAtTime("M1", ROSeq, HR, CFB))
          }
)
setMethod(".LengthToBreadthRatio",
          ".M1",
          function(this, WSV)
          {
            return(LengthToBreadthRatio("M1", WSV))
          }
)
setMethod(".LengthToBreadthRatioAtTime",
          ".M1",
          function(this, LB, HR, CFB)
          {
            return(LengthToBreadthRatioAtTime("M1", LB, HR, CFB))
          }
)
setMethod(".RateOfSpread",
          ".M1",
          function(this, ISI, BUI, FMC, SFC, PC, PDF, CC, CBH)
          {
            return(RateOfSpread("M1", ISI, BUI, FMC, SFC, PC, PDF, CC, CBH))
          }
)
setMethod(".RateOfSpreadAtTime",
          ".M1",
          function(this, ROSeq, HR, CFB)
          {
            return(RateOfSpreadAtTime("M1", ROSeq, HR, CFB))
          }
)
setMethod(".SlopeAdjust",
          ".M1",
          function(this, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI)
          {
            return(SlopeAdjust("M1", FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI))
          }
)
setMethod(".SurfaceFuelConsumption",
          ".M1",
          function(this, FFMC, BUI, PC, GFL)
          {
            return(SurfaceFuelConsumption("M1",
                                          FFMC,
                                          BUI,
                                          PC,
                                          GFL))
          }
)
