.M2 <- setClass(".M2", contains=".FuelMixedwood")
setMethod(".RateOfSpread",
          ".M2",
          function(this, ISI, BUI, FMC, SFC, PC, PDF, CC, CBH)
          {
            NoBUI <- -1
            #Eq. 27 (FCFDG 1992) - Initial Rate of Spread for M2 Mixedwood type
            RSI <- PC / 100 * .RateOfSpread(FUELS[["C2"]], ISI, NoBUI, FMC, SFC, PC, PDF, CC, CBH)
            + 0.2 * ((100 - PC) / 100) * .RateOfSpread(FUELS[["D1"]], ISI, NoBUI, FMC, SFC, PC, PDF, CC, CBH)
            return(RSI)
          }
)
setMethod(".SlopeAdjust",
          ".M2",
          function(this, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI)
          {
            return(SlopeAdjust("M2", FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI))
          }
)
