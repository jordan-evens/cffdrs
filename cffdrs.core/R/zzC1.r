.C1 <- setClass(".C1", contains="Fuel")
setMethod(".BackRateOfSpread",
          ".C1",
          function(this, FFMC, BUI, WSV, FMC, SFC, PC, PDF, CC, CBH)
          {
            return(BackRateOfSpread("C1", FFMC, BUI, WSV, FMC, SFC, PC, PDF, CC, CBH))
          }
)
setMethod(".BuildupEffect",
          ".C1",
          function(this, BUI)
          {
            return(BuildupEffect("C1", BUI))
          }
)
setMethod(".CrownFuelConsumption",
          ".C1",
          function(this, CFL, CFB, PC, PDF)
          {
            return(CrownFuelConsumption("C1", CFL, CFB, PC, PDF))
          }
)
setMethod(".DistanceAtTime",
          ".C1",
          function(this, ROSeq, HR, CFB)
          {
            return(DistanceAtTime("C1", ROSeq, HR, CFB))
          }
)
setMethod(".LengthToBreadthRatio",
          ".C1",
          function(this, WSV)
          {
            return(LengthToBreadthRatio("C1", WSV))
          }
)
setMethod(".LengthToBreadthRatioAtTime",
          ".C1",
          function(this, LB, HR, CFB)
          {
            return(LengthToBreadthRatioAtTime("C1", LB, HR, CFB))
          }
)
setMethod(".RateOfSpread",
          ".C1",
          function(this, ISI, BUI, FMC, SFC, PC, PDF, CC, CBH)
          {
            return(RateOfSpread("C1", ISI, BUI, FMC, SFC, PC, PDF, CC, CBH))
          }
)
setMethod(".RateOfSpreadAtTime",
          ".C1",
          function(this, ROSeq, HR, CFB)
          {
            return(RateOfSpreadAtTime("C1", ROSeq, HR, CFB))
          }
)
setMethod(".SlopeAdjust",
          ".C1",
          function(this, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI)
          {
            return(SlopeAdjust("C1", FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI))
          }
)
setMethod(".SurfaceFuelConsumption",
          ".C1",
          function(this, FFMC, BUI, PC, GFL)
          {
            return(SurfaceFuelConsumption("C1",
                                          FFMC,
                                          BUI,
                                          PC,
                                          GFL))
          }
)
