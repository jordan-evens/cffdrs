.O1B <- setClass(".O1B", contains="Fuel")
setMethod(".BackRateOfSpread",
          ".O1B",
          function(this, FFMC, BUI, WSV, FMC, SFC, PC, PDF, CC, CBH)
          {
            return(BackRateOfSpread("O1B", FFMC, BUI, WSV, FMC, SFC, PC, PDF, CC, CBH))
          }
)
setMethod(".BuildupEffect",
          ".O1B",
          function(this, BUI)
          {
            return(BuildupEffect("O1B", BUI))
          }
)
setMethod(".CrownFuelConsumption",
          ".O1B",
          function(this, CFL, CFB, PC, PDF)
          {
            return(CrownFuelConsumption("O1B", CFL, CFB, PC, PDF))
          }
)
setMethod(".DistanceAtTime",
          ".O1B",
          function(this, ROSeq, HR, CFB)
          {
            return(DistanceAtTime("O1B", ROSeq, HR, CFB))
          }
)
setMethod(".LengthToBreadthRatio",
          ".O1B",
          function(this, WSV)
          {
            return(LengthToBreadthRatio("O1B", WSV))
          }
)
setMethod(".LengthToBreadthRatioAtTime",
          ".O1B",
          function(this, LB, HR, CFB)
          {
            return(LengthToBreadthRatioAtTime("O1B", LB, HR, CFB))
          }
)
setMethod(".RateOfSpread",
          ".O1B",
          function(this, ISI, BUI, FMC, SFC, PC, PDF, CC, CBH)
          {
            return(RateOfSpread("O1B", ISI, BUI, FMC, SFC, PC, PDF, CC, CBH))
          }
)
setMethod(".RateOfSpreadAtTime",
          ".O1B",
          function(this, ROSeq, HR, CFB)
          {
            return(RateOfSpreadAtTime("O1B", ROSeq, HR, CFB))
          }
)
setMethod(".SlopeAdjust",
          ".O1B",
          function(this, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI)
          {
            return(SlopeAdjust("O1B", FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI))
          }
)
setMethod(".SurfaceFuelConsumption",
          ".O1B",
          function(this, FFMC, BUI, PC, GFL)
          {
            return(SurfaceFuelConsumption("O1B",
                                          FFMC,
                                          BUI,
                                          PC,
                                          GFL))
          }
)
