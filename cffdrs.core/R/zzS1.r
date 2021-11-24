.S1 <- setClass(".S1", contains=".FuelSlash")
setMethod(".SlopeAdjust",
          ".S1",
          function(this, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI)
          {
            return(SlopeAdjust("S1", FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI))
          }
)
