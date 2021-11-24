.M4 <- setClass(".M4", contains="Fuel")
setMethod(".BackRateOfSpread",
          ".M4",
          function(this, FFMC, BUI, WSV, FMC, SFC, PC, PDF, CC, CBH)
          {
            return(BackRateOfSpread("M4", FFMC, BUI, WSV, FMC, SFC, PC, PDF, CC, CBH))
          }
)
setMethod(".BuildupEffect",
          ".M4",
          function(this, BUI)
          {
            return(BuildupEffect("M4", BUI))
          }
)
setMethod(".CrownFuelConsumption",
          ".M4",
          function(this, CFL, CFB, PC, PDF)
          {
            return(CrownFuelConsumption("M4", CFL, CFB, PC, PDF))
          }
)
setMethod(".DistanceAtTime",
          ".M4",
          function(this, ROSeq, HR, CFB)
          {
            return(DistanceAtTime("M4", ROSeq, HR, CFB))
          }
)
setMethod(".LengthToBreadthRatio",
          ".M4",
          function(this, WSV)
          {
            return(LengthToBreadthRatio("M4", WSV))
          }
)
setMethod(".LengthToBreadthRatioAtTime",
          ".M4",
          function(this, LB, HR, CFB)
          {
            return(LengthToBreadthRatioAtTime("M4", LB, HR, CFB))
          }
)
setMethod(".RateOfSpread",
          ".M4",
          function(this, ISI, BUI, FMC, SFC, PC, PDF, CC, CBH)
          {
            return(RateOfSpread("M4", ISI, BUI, FMC, SFC, PC, PDF, CC, CBH))
          }
)
setMethod(".RateOfSpreadAtTime",
          ".M4",
          function(this, ROSeq, HR, CFB)
          {
            return(RateOfSpreadAtTime("M4", ROSeq, HR, CFB))
          }
)
setMethod(".SlopeAdjust",
          ".M4",
          function(this, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI)
          {
            return(SlopeAdjust("M4", FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI))
          }
)
setMethod(".SurfaceFuelConsumption",
          ".M4",
          function(this, FFMC, BUI, PC, GFL)
          {
            return(SurfaceFuelConsumption("M4",
                                          FFMC,
                                          BUI,
                                          PC,
                                          GFL))
          }
)
