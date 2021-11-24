.C2 <- setClass(".C2", contains="Fuel")
setMethod(".BackRateOfSpread",
          ".C2",
          function(this, FFMC, BUI, WSV, FMC, SFC, PC, PDF, CC, CBH)
          {
            return(BackRateOfSpread("C2", FFMC, BUI, WSV, FMC, SFC, PC, PDF, CC, CBH))
          }
)
setMethod(".BuildupEffect",
          ".C2",
          function(this, BUI)
          {
            return(BuildupEffect("C2", BUI))
          }
)
setMethod(".CrownFuelConsumption",
          ".C2",
          function(this, CFL, CFB, PC, PDF)
          {
            return(CrownFuelConsumption("C2", CFL, CFB, PC, PDF))
          }
)
setMethod(".DistanceAtTime",
          ".C2",
          function(this, ROSeq, HR, CFB)
          {
            return(DistanceAtTime("C2", ROSeq, HR, CFB))
          }
)
setMethod(".LengthToBreadthRatio",
          ".C2",
          function(this, WSV)
          {
            return(LengthToBreadthRatio("C2", WSV))
          }
)
setMethod(".LengthToBreadthRatioAtTime",
          ".C2",
          function(this, LB, HR, CFB)
          {
            return(LengthToBreadthRatioAtTime("C2", LB, HR, CFB))
          }
)
setMethod(".RateOfSpread",
          ".C2",
          function(this, ISI, BUI, FMC, SFC, PC, PDF, CC, CBH)
          {
            return(RateOfSpread("C2", ISI, BUI, FMC, SFC, PC, PDF, CC, CBH))
          }
)
setMethod(".RateOfSpreadAtTime",
          ".C2",
          function(this, ROSeq, HR, CFB)
          {
            return(RateOfSpreadAtTime("C2", ROSeq, HR, CFB))
          }
)
setMethod(".SlopeAdjust",
          ".C2",
          function(this, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI)
          {
            return(SlopeAdjust("C2", FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI))
          }
)
setMethod(".SurfaceFuelConsumption",
          ".C2",
          function(this, FFMC, BUI, PC, GFL)
          {
            return(SurfaceFuelConsumption("C2",
                                          FFMC,
                                          BUI,
                                          PC,
                                          GFL))
          }
)
