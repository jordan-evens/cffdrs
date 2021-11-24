.C1 <- setClass(".C1", contains=".FuelOpen")
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
            #Eqs. 9a, 9b (Wotton et. al. 2009) - Solving the lower bound of FFMC value
            # for the C1 fuel type SFC calculation
            return (ifelse(FFMC > 84,
                           0.75 + 0.75 * (1 - exp(-0.23 * (FFMC - 84)))**0.5,
                           0.75 - 0.75 * (1 - exp(-0.23 * (84 - FFMC)))**0.5))
          }
)
