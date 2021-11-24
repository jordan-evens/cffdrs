.C3 <- setClass(".C3", contains="Fuel")
setMethod(".BackRateOfSpread",
          ".C3",
          function(this, FFMC, BUI, WSV, FMC, SFC, PC, PDF, CC, CBH)
          {
            return(BackRateOfSpread("C3", FFMC, BUI, WSV, FMC, SFC, PC, PDF, CC, CBH))
          }
)
setMethod(".BuildupEffect",
          ".C3",
          function(this, BUI)
          {
            return(BuildupEffect("C3", BUI))
          }
)
setMethod(".CrownFuelConsumption",
          ".C3",
          function(this, CFL, CFB, PC, PDF)
          {
            return(CrownFuelConsumption("C3", CFL, CFB, PC, PDF))
          }
)
setMethod(".DistanceAtTime",
          ".C3",
          function(this, ROSeq, HR, CFB)
          {
            return(DistanceAtTime("C3", ROSeq, HR, CFB))
          }
)
setMethod(".LengthToBreadthRatio",
          ".C3",
          function(this, WSV)
          {
            return(LengthToBreadthRatio("C3", WSV))
          }
)
setMethod(".LengthToBreadthRatioAtTime",
          ".C3",
          function(this, LB, HR, CFB)
          {
            return(LengthToBreadthRatioAtTime("C3", LB, HR, CFB))
          }
)
setMethod(".RateOfSpread",
          ".C3",
          function(this, ISI, BUI, FMC, SFC, PC, PDF, CC, CBH)
          {
            return(RateOfSpread("C3", ISI, BUI, FMC, SFC, PC, PDF, CC, CBH))
          }
)
setMethod(".RateOfSpreadAtTime",
          ".C3",
          function(this, ROSeq, HR, CFB)
          {
            return(RateOfSpreadAtTime("C3", ROSeq, HR, CFB))
          }
)
setMethod(".SlopeAdjust",
          ".C3",
          function(this, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI)
          {
            return(SlopeAdjust("C3", FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI))
          }
)
setMethod(".SurfaceFuelConsumption",
          ".C3",
          function(this, FFMC, BUI, PC, GFL)
          {
            return(SurfaceFuelConsumption("C3",
                                          FFMC,
                                          BUI,
                                          PC,
                                          GFL))
          }
)
