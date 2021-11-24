.O1A <- setClass(".O1A", contains=".FuelGrass")
setMethod(".SlopeAdjust",
          ".O1A",
          function(this, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI)
          {
            return(SlopeAdjust("O1A", FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI))
          }
)
