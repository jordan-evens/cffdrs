.C7 <- setClass(".C7", contains="Fuel")
setMethod(".BackRateOfSpread",
          ".C7",
          function(this, FFMC, BUI, WSV, FMC, SFC, PC, PDF, CC, CBH)
          {
            return(BackRateOfSpread("C7", FFMC, BUI, WSV, FMC, SFC, PC, PDF, CC, CBH))
          }
)
setMethod(".BuildupEffect",
          ".C7",
          function(this, BUI)
          {
            return(BuildupEffect("C7", BUI))
          }
)
setMethod(".CrownFuelConsumption",
          ".C7",
          function(this, CFL, CFB, PC, PDF)
          {
            return(CrownFuelConsumption("C7", CFL, CFB, PC, PDF))
          }
)
setMethod(".DistanceAtTime",
          ".C7",
          function(this, ROSeq, HR, CFB)
          {
            return(DistanceAtTime("C7", ROSeq, HR, CFB))
          }
)
setMethod(".LengthToBreadthRatio",
          ".C7",
          function(this, WSV)
          {
            return(LengthToBreadthRatio("C7", WSV))
          }
)
setMethod(".LengthToBreadthRatioAtTime",
          ".C7",
          function(this, LB, HR, CFB)
          {
            return(LengthToBreadthRatioAtTime("C7", LB, HR, CFB))
          }
)
setMethod(".RateOfSpread",
          ".C7",
          function(this, ISI, BUI, FMC, SFC, PC, PDF, CC, CBH)
          {
            return(RateOfSpread("C7", ISI, BUI, FMC, SFC, PC, PDF, CC, CBH))
          }
)
setMethod(".RateOfSpreadAtTime",
          ".C7",
          function(this, ROSeq, HR, CFB)
          {
            return(RateOfSpreadAtTime("C7", ROSeq, HR, CFB))
          }
)
setMethod(".SlopeAdjust",
          ".C7",
          function(this, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI)
          {
            return(SlopeAdjust("C7", FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI))
          }
)
setMethod(".SurfaceFuelConsumption",
          ".C7",
          function(this, FFMC, BUI, PC, GFL)
          {
            return(SurfaceFuelConsumption("C7",
                                          FFMC,
                                          BUI,
                                          PC,
                                          GFL))
          }
)
