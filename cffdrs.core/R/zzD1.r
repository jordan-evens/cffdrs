.D1 <- setClass(".D1", contains="Fuel")
setMethod(".BackRateOfSpread",
          ".D1",
          function(this, FFMC, BUI, WSV, FMC, SFC, PC, PDF, CC, CBH)
          {
            return(BackRateOfSpread("D1", FFMC, BUI, WSV, FMC, SFC, PC, PDF, CC, CBH))
          }
)
setMethod(".BuildupEffect",
          ".D1",
          function(this, BUI)
          {
            return(BuildupEffect("D1", BUI))
          }
)
setMethod(".CrownFuelConsumption",
          ".D1",
          function(this, CFL, CFB, PC, PDF)
          {
            return(CrownFuelConsumption("D1", CFL, CFB, PC, PDF))
          }
)
setMethod(".DistanceAtTime",
          ".D1",
          function(this, ROSeq, HR, CFB)
          {
            return(DistanceAtTime("D1", ROSeq, HR, CFB))
          }
)
setMethod(".LengthToBreadthRatio",
          ".D1",
          function(this, WSV)
          {
            return(LengthToBreadthRatio("D1", WSV))
          }
)
setMethod(".LengthToBreadthRatioAtTime",
          ".D1",
          function(this, LB, HR, CFB)
          {
            return(LengthToBreadthRatioAtTime("D1", LB, HR, CFB))
          }
)
setMethod(".RateOfSpread",
          ".D1",
          function(this, ISI, BUI, FMC, SFC, PC, PDF, CC, CBH)
          {
            return(RateOfSpread("D1", ISI, BUI, FMC, SFC, PC, PDF, CC, CBH))
          }
)
setMethod(".RateOfSpreadAtTime",
          ".D1",
          function(this, ROSeq, HR, CFB)
          {
            return(RateOfSpreadAtTime("D1", ROSeq, HR, CFB))
          }
)
setMethod(".SlopeAdjust",
          ".D1",
          function(this, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI)
          {
            return(SlopeAdjust("D1", FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI))
          }
)
setMethod(".SurfaceFuelConsumption",
          ".D1",
          function(this, FFMC, BUI, PC, GFL)
          {
            return(SurfaceFuelConsumption("D1",
                                          FFMC,
                                          BUI,
                                          PC,
                                          GFL))
          }
)
