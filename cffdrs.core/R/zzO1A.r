.O1A <- setClass(".O1A", contains="Fuel")
setMethod(".BackRateOfSpread",
          ".O1A",
          function(this, FFMC, BUI, WSV, FMC, SFC, PC, PDF, CC, CBH)
          {
            return(BackRateOfSpread("O1A", FFMC, BUI, WSV, FMC, SFC, PC, PDF, CC, CBH))
          }
)
setMethod(".BuildupEffect",
          ".O1A",
          function(this, BUI)
          {
            return(BuildupEffect("O1A", BUI))
          }
)
setMethod(".CrownFuelConsumption",
          ".O1A",
          function(this, CFL, CFB, PC, PDF)
          {
            return(CrownFuelConsumption("O1A", CFL, CFB, PC, PDF))
          }
)
setMethod(".DistanceAtTime",
          ".O1A",
          function(this, ROSeq, HR, CFB)
          {
            return(DistanceAtTime("O1A", ROSeq, HR, CFB))
          }
)
setMethod(".LengthToBreadthRatio",
          ".O1A",
          function(this, WSV)
          {
            return(LengthToBreadthRatio("O1A", WSV))
          }
)
setMethod(".LengthToBreadthRatioAtTime",
          ".O1A",
          function(this, LB, HR, CFB)
          {
            return(LengthToBreadthRatioAtTime("O1A", LB, HR, CFB))
          }
)
setMethod(".RateOfSpread",
          ".O1A",
          function(this, ISI, BUI, FMC, SFC, PC, PDF, CC, CBH)
          {
            return(RateOfSpread("O1A", ISI, BUI, FMC, SFC, PC, PDF, CC, CBH))
          }
)
setMethod(".RateOfSpreadAtTime",
          ".O1A",
          function(this, ROSeq, HR, CFB)
          {
            return(RateOfSpreadAtTime("O1A", ROSeq, HR, CFB))
          }
)
setMethod(".SlopeAdjust",
          ".O1A",
          function(this, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI)
          {
            return(SlopeAdjust("O1A", FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI))
          }
)
setMethod(".SurfaceFuelConsumption",
          ".O1A",
          function(this, FFMC, BUI, PC, GFL)
          {
            return(SurfaceFuelConsumption("O1A",
                                          FFMC,
                                          BUI,
                                          PC,
                                          GFL))
          }
)
