.S1 <- setClass(".S1", contains="Fuel")
setMethod(".BackRateOfSpread",
          ".S1",
          function(this, FFMC, BUI, WSV, FMC, SFC, PC, PDF, CC, CBH)
          {
            return(BackRateOfSpread("S1", FFMC, BUI, WSV, FMC, SFC, PC, PDF, CC, CBH))
          }
)
setMethod(".BuildupEffect",
          ".S1",
          function(this, BUI)
          {
            return(BuildupEffect("S1", BUI))
          }
)
setMethod(".CrownFuelConsumption",
          ".S1",
          function(this, CFL, CFB, PC, PDF)
          {
            return(CrownFuelConsumption("S1", CFL, CFB, PC, PDF))
          }
)
setMethod(".DistanceAtTime",
          ".S1",
          function(this, ROSeq, HR, CFB)
          {
            return(DistanceAtTime("S1", ROSeq, HR, CFB))
          }
)
setMethod(".LengthToBreadthRatio",
          ".S1",
          function(this, WSV)
          {
            return(LengthToBreadthRatio("S1", WSV))
          }
)
setMethod(".LengthToBreadthRatioAtTime",
          ".S1",
          function(this, LB, HR, CFB)
          {
            return(LengthToBreadthRatioAtTime("S1", LB, HR, CFB))
          }
)
setMethod(".RateOfSpread",
          ".S1",
          function(this, ISI, BUI, FMC, SFC, PC, PDF, CC, CBH)
          {
            return(RateOfSpread("S1", ISI, BUI, FMC, SFC, PC, PDF, CC, CBH))
          }
)
setMethod(".RateOfSpreadAtTime",
          ".S1",
          function(this, ROSeq, HR, CFB)
          {
            return(RateOfSpreadAtTime("S1", ROSeq, HR, CFB))
          }
)
setMethod(".SlopeAdjust",
          ".S1",
          function(this, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI)
          {
            return(SlopeAdjust("S1", FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI))
          }
)
setMethod(".SurfaceFuelConsumption",
          ".S1",
          function(this, FFMC, BUI, PC, GFL)
          {
            return(SurfaceFuelConsumption("S1",
                                          FFMC,
                                          BUI,
                                          PC,
                                          GFL))
          }
)
