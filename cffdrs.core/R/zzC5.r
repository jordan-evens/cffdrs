.C5 <- setClass(".C5", contains="Fuel")
setMethod(".BackRateOfSpread",
          ".C5",
          function(this, FFMC, BUI, WSV, FMC, SFC, PC, PDF, CC, CBH)
          {
            return(BackRateOfSpread("C5", FFMC, BUI, WSV, FMC, SFC, PC, PDF, CC, CBH))
          }
)
setMethod(".BuildupEffect",
          ".C5",
          function(this, BUI)
          {
            return(BuildupEffect("C5", BUI))
          }
)
setMethod(".CrownFuelConsumption",
          ".C5",
          function(this, CFL, CFB, PC, PDF)
          {
            return(CrownFuelConsumption("C5", CFL, CFB, PC, PDF))
          }
)
setMethod(".DistanceAtTime",
          ".C5",
          function(this, ROSeq, HR, CFB)
          {
            return(DistanceAtTime("C5", ROSeq, HR, CFB))
          }
)
setMethod(".LengthToBreadthRatio",
          ".C5",
          function(this, WSV)
          {
            return(LengthToBreadthRatio("C5", WSV))
          }
)
setMethod(".LengthToBreadthRatioAtTime",
          ".C5",
          function(this, LB, HR, CFB)
          {
            return(LengthToBreadthRatioAtTime("C5", LB, HR, CFB))
          }
)
setMethod(".RateOfSpread",
          ".C5",
          function(this, ISI, BUI, FMC, SFC, PC, PDF, CC, CBH)
          {
            return(RateOfSpread("C5", ISI, BUI, FMC, SFC, PC, PDF, CC, CBH))
          }
)
setMethod(".RateOfSpreadAtTime",
          ".C5",
          function(this, ROSeq, HR, CFB)
          {
            return(RateOfSpreadAtTime("C5", ROSeq, HR, CFB))
          }
)
setMethod(".SlopeAdjust",
          ".C5",
          function(this, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI)
          {
            return(SlopeAdjust("C5", FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI))
          }
)
setMethod(".SurfaceFuelConsumption",
          ".C5",
          function(this, FFMC, BUI, PC, GFL)
          {
            return(SurfaceFuelConsumption("C5",
                                          FFMC,
                                          BUI,
                                          PC,
                                          GFL))
          }
)
