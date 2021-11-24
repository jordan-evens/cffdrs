.C7 <- setClass(".C7", contains=".FuelClosed")
setMethod(".SlopeAdjust",
          ".C7",
          function(this, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI)
          {
            return(SlopeAdjust("C7", FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI))
          }
)
setMethod(".SurfaceFuelConsumption",
          ".C7",
          function(this, FFMC, BUI, PC, GFL)
          {
            #Eqs. 13, 14, 15 (FCFDG 1992) - C7 Fuel Types
            return (ifelse(FFMC > 70,
                           2 * (1 - exp(-0.104 * (FFMC - 70))),
                           0) + 1.5 * (1 - exp(-0.0201 * BUI)))
            
          }
)
