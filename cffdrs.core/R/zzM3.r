.M3 <- setClass(".M3", contains="Fuel")
setMethod(".BackRateOfSpread",
          ".M3",
          function(this, FFMC, BUI, WSV, FMC, SFC, PC, PDF, CC, CBH)
          {
            return(BackRateOfSpread("M3", FFMC, BUI, WSV, FMC, SFC, PC, PDF, CC, CBH))
          }
)
setMethod(".BuildupEffect",
          ".M3",
          function(this, BUI)
          {
            return(BuildupEffect("M3", BUI))
          }
)
setMethod(".CrownFuelConsumption",
          ".M3",
          function(this, CFL, CFB, PC, PDF)
          {
            return(CrownFuelConsumption("M3", CFL, CFB, PC, PDF))
          }
)
setMethod(".DistanceAtTime",
          ".M3",
          function(this, ROSeq, HR, CFB)
          {
            return(DistanceAtTime("M3", ROSeq, HR, CFB))
          }
)
setMethod(".LengthToBreadthRatio",
          ".M3",
          function(this, WSV)
          {
            return(LengthToBreadthRatio("M3", WSV))
          }
)
setMethod(".LengthToBreadthRatioAtTime",
          ".M3",
          function(this, LB, HR, CFB)
          {
            return(LengthToBreadthRatioAtTime("M3", LB, HR, CFB))
          }
)
setMethod(".RateOfSpread",
          ".M3",
          function(this, ISI, BUI, FMC, SFC, PC, PDF, CC, CBH)
          {
            return(RateOfSpread("M3", ISI, BUI, FMC, SFC, PC, PDF, CC, CBH))
          }
)
setMethod(".RateOfSpreadAtTime",
          ".M3",
          function(this, ROSeq, HR, CFB)
          {
            return(RateOfSpreadAtTime("M3", ROSeq, HR, CFB))
          }
)
setMethod(".SlopeAdjust",
          ".M3",
          function(this, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI)
          {
            return(SlopeAdjust("M3", FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI))
          }
)
setMethod(".SurfaceFuelConsumption",
          ".M3",
          function(this, FFMC, BUI, PC, GFL)
          {
            return(SurfaceFuelConsumption("M3",
                                          FFMC,
                                          BUI,
                                          PC,
                                          GFL))
          }
)
