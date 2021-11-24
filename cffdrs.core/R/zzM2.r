.M2 <- setClass(".M2", contains="Fuel")
setMethod(".BackRateOfSpread",
          ".M2",
          function(this, FFMC, BUI, WSV, FMC, SFC, PC, PDF, CC, CBH)
          {
            return(BackRateOfSpread("M2", FFMC, BUI, WSV, FMC, SFC, PC, PDF, CC, CBH))
          }
)
setMethod(".BuildupEffect",
          ".M2",
          function(this, BUI)
          {
            return(BuildupEffect("M2", BUI))
          }
)
setMethod(".CrownFuelConsumption",
          ".M2",
          function(this, CFL, CFB, PC, PDF)
          {
            return(CrownFuelConsumption("M2", CFL, CFB, PC, PDF))
          }
)
setMethod(".DistanceAtTime",
          ".M2",
          function(this, ROSeq, HR, CFB)
          {
            return(DistanceAtTime("M2", ROSeq, HR, CFB))
          }
)
setMethod(".LengthToBreadthRatio",
          ".M2",
          function(this, WSV)
          {
            return(LengthToBreadthRatio("M2", WSV))
          }
)
setMethod(".LengthToBreadthRatioAtTime",
          ".M2",
          function(this, LB, HR, CFB)
          {
            return(LengthToBreadthRatioAtTime("M2", LB, HR, CFB))
          }
)
setMethod(".RateOfSpread",
          ".M2",
          function(this, ISI, BUI, FMC, SFC, PC, PDF, CC, CBH)
          {
            return(RateOfSpread("M2", ISI, BUI, FMC, SFC, PC, PDF, CC, CBH))
          }
)
setMethod(".RateOfSpreadAtTime",
          ".M2",
          function(this, ROSeq, HR, CFB)
          {
            return(RateOfSpreadAtTime("M2", ROSeq, HR, CFB))
          }
)
setMethod(".SlopeAdjust",
          ".M2",
          function(this, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI)
          {
            return(SlopeAdjust("M2", FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI))
          }
)
setMethod(".SurfaceFuelConsumption",
          ".M2",
          function(this, FFMC, BUI, PC, GFL)
          {
            return(SurfaceFuelConsumption("M2",
                                          FFMC,
                                          BUI,
                                          PC,
                                          GFL))
          }
)
