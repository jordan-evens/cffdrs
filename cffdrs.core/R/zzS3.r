.S3 <- setClass(".S3", contains="Fuel")
setMethod(".BackRateOfSpread",
          ".S3",
          function(this, FFMC, BUI, WSV, FMC, SFC, PC, PDF, CC, CBH)
          {
            return(BackRateOfSpread("S3", FFMC, BUI, WSV, FMC, SFC, PC, PDF, CC, CBH))
          }
)
setMethod(".BuildupEffect",
          ".S3",
          function(this, BUI)
          {
            return(BuildupEffect("S3", BUI))
          }
)
setMethod(".CrownFuelConsumption",
          ".S3",
          function(this, CFL, CFB, PC, PDF)
          {
            return(CrownFuelConsumption("S3", CFL, CFB, PC, PDF))
          }
)
setMethod(".DistanceAtTime",
          ".S3",
          function(this, ROSeq, HR, CFB)
          {
            return(DistanceAtTime("S3", ROSeq, HR, CFB))
          }
)
setMethod(".LengthToBreadthRatio",
          ".S3",
          function(this, WSV)
          {
            return(LengthToBreadthRatio("S3", WSV))
          }
)
setMethod(".LengthToBreadthRatioAtTime",
          ".S3",
          function(this, LB, HR, CFB)
          {
            return(LengthToBreadthRatioAtTime("S3", LB, HR, CFB))
          }
)
setMethod(".RateOfSpread",
          ".S3",
          function(this, ISI, BUI, FMC, SFC, PC, PDF, CC, CBH)
          {
            return(RateOfSpread("S3", ISI, BUI, FMC, SFC, PC, PDF, CC, CBH))
          }
)
setMethod(".RateOfSpreadAtTime",
          ".S3",
          function(this, ROSeq, HR, CFB)
          {
            return(RateOfSpreadAtTime("S3", ROSeq, HR, CFB))
          }
)
setMethod(".SlopeAdjust",
          ".S3",
          function(this, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI)
          {
            return(SlopeAdjust("S3", FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI))
          }
)
setMethod(".SurfaceFuelConsumption",
          ".S3",
          function(this, FFMC, BUI, PC, GFL)
          {
            return(SurfaceFuelConsumption("S3",
                                          FFMC,
                                          BUI,
                                          PC,
                                          GFL))
          }
)
