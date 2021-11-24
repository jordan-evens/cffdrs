.C2 <- setClass(".C2", contains=".FuelClosed")
setMethod(".SlopeAdjust",
          ".C2",
          function(this, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI)
          {
            return(SlopeAdjust("C2", FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI))
          }
)
