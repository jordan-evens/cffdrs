.C6 <- setClass(".C6", contains="Fuel")
setMethod(".BackRateOfSpread",
          ".C6",
          function(this, FFMC, BUI, WSV, FMC, SFC, PC, PDF, CC, CBH)
          {
            return(BackRateOfSpread("C6", FFMC, BUI, WSV, FMC, SFC, PC, PDF, CC, CBH))
          }
)
setMethod(".BuildupEffect",
          ".C6",
          function(this, BUI)
          {
            return(BuildupEffect("C6", BUI))
          }
)
setMethod(".CrownFuelConsumption",
          ".C6",
          function(this, CFL, CFB, PC, PDF)
          {
            return(CrownFuelConsumption("C6", CFL, CFB, PC, PDF))
          }
)
setMethod(".DistanceAtTime",
          ".C6",
          function(this, ROSeq, HR, CFB)
          {
            return(DistanceAtTime("C6", ROSeq, HR, CFB))
          }
)
setMethod(".LengthToBreadthRatio",
          ".C6",
          function(this, WSV)
          {
            return(LengthToBreadthRatio("C6", WSV))
          }
)
setMethod(".LengthToBreadthRatioAtTime",
          ".C6",
          function(this, LB, HR, CFB)
          {
            return(LengthToBreadthRatioAtTime("C6", LB, HR, CFB))
          }
)
setMethod(".RateOfSpread",
          ".C6",
          function(this, ISI, BUI, FMC, SFC, PC, PDF, CC, CBH)
          {
            return(RateOfSpread("C6", ISI, BUI, FMC, SFC, PC, PDF, CC, CBH))
          }
)
setMethod(".RateOfSpreadAtTime",
          ".C6",
          function(this, ROSeq, HR, CFB)
          {
            return(RateOfSpreadAtTime("C6", ROSeq, HR, CFB))
          }
)
setMethod(".SlopeAdjust",
          ".C6",
          function(this, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI)
          {
            return(SlopeAdjust("C6", FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI))
          }
)
setMethod(".SurfaceFuelConsumption",
          ".C6",
          function(this, FFMC, BUI, PC, GFL)
          {
            return(SurfaceFuelConsumption("C6",
                                          FFMC,
                                          BUI,
                                          PC,
                                          GFL))
          }
)
