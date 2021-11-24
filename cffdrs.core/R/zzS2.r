.S2 <- setClass(".S2", contains="Fuel")
setMethod(".BackRateOfSpread",
          ".S2",
          function(this, FFMC, BUI, WSV, FMC, SFC, PC, PDF, CC, CBH)
          {
            return(BackRateOfSpread("S2", FFMC, BUI, WSV, FMC, SFC, PC, PDF, CC, CBH))
          }
)
setMethod(".BuildupEffect",
          ".S2",
          function(this, BUI)
          {
            return(BuildupEffect("S2", BUI))
          }
)
setMethod(".CrownFuelConsumption",
          ".S2",
          function(this, CFL, CFB, PC, PDF)
          {
            return(CrownFuelConsumption("S2", CFL, CFB, PC, PDF))
          }
)
setMethod(".DistanceAtTime",
          ".S2",
          function(this, ROSeq, HR, CFB)
          {
            return(DistanceAtTime("S2", ROSeq, HR, CFB))
          }
)
setMethod(".LengthToBreadthRatio",
          ".S2",
          function(this, WSV)
          {
            return(LengthToBreadthRatio("S2", WSV))
          }
)
setMethod(".LengthToBreadthRatioAtTime",
          ".S2",
          function(this, LB, HR, CFB)
          {
            return(LengthToBreadthRatioAtTime("S2", LB, HR, CFB))
          }
)
setMethod(".RateOfSpread",
          ".S2",
          function(this, ISI, BUI, FMC, SFC, PC, PDF, CC, CBH)
          {
            return(RateOfSpread("S2", ISI, BUI, FMC, SFC, PC, PDF, CC, CBH))
          }
)
setMethod(".RateOfSpreadAtTime",
          ".S2",
          function(this, ROSeq, HR, CFB)
          {
            return(RateOfSpreadAtTime("S2", ROSeq, HR, CFB))
          }
)
setMethod(".SlopeAdjust",
          ".S2",
          function(this, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI)
          {
            return(SlopeAdjust("S2", FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI))
          }
)
setMethod(".SurfaceFuelConsumption",
          ".S2",
          function(this, FFMC, BUI, PC, GFL)
          {
            return(SurfaceFuelConsumption("S2",
                                          FFMC,
                                          BUI,
                                          PC,
                                          GFL))
          }
)
