.C4 <- setClass(".C4", contains="Fuel")
setMethod(".BackRateOfSpread",
          ".C4",
          function(this, FFMC, BUI, WSV, FMC, SFC, PC, PDF, CC, CBH)
          {
            return(BackRateOfSpread("C4", FFMC, BUI, WSV, FMC, SFC, PC, PDF, CC, CBH))
          }
)
setMethod(".BuildupEffect",
          ".C4",
          function(this, BUI)
          {
            return(BuildupEffect("C4", BUI))
          }
)
setMethod(".CrownFuelConsumption",
          ".C4",
          function(this, CFL, CFB, PC, PDF)
          {
            return(CrownFuelConsumption("C4", CFL, CFB, PC, PDF))
          }
)
setMethod(".DistanceAtTime",
          ".C4",
          function(this, ROSeq, HR, CFB)
          {
            return(DistanceAtTime("C4", ROSeq, HR, CFB))
          }
)
setMethod(".LengthToBreadthRatio",
          ".C4",
          function(this, WSV)
          {
            return(LengthToBreadthRatio("C4", WSV))
          }
)
setMethod(".LengthToBreadthRatioAtTime",
          ".C4",
          function(this, LB, HR, CFB)
          {
            return(LengthToBreadthRatioAtTime("C4", LB, HR, CFB))
          }
)
setMethod(".RateOfSpread",
          ".C4",
          function(this, ISI, BUI, FMC, SFC, PC, PDF, CC, CBH)
          {
            return(RateOfSpread("C4", ISI, BUI, FMC, SFC, PC, PDF, CC, CBH))
          }
)
setMethod(".RateOfSpreadAtTime",
          ".C4",
          function(this, ROSeq, HR, CFB)
          {
            return(RateOfSpreadAtTime("C4", ROSeq, HR, CFB))
          }
)
setMethod(".SlopeAdjust",
          ".C4",
          function(this, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI)
          {
            return(SlopeAdjust("C4", FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI))
          }
)
setMethod(".SurfaceFuelConsumption",
          ".C4",
          function(this, FFMC, BUI, PC, GFL)
          {
            return(SurfaceFuelConsumption("C4",
                                          FFMC,
                                          BUI,
                                          PC,
                                          GFL))
          }
)
