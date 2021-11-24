.M1 <- setClass(".M1", contains=".FuelMixedwood")
setMethod(".RateOfSpread",
          ".M1",
          function(this, ISI, BUI, FMC, SFC, PC, PDF, CC, CBH)
          {
            NoBUI <- -1
            #Eq. 27 (FCFDG 1992) - Initial Rate of Spread for M1 Mixedwood type
            RSI <- PC / 100 * .RateOfSpread(FUELS[["C2"]], ISI, NoBUI, FMC, SFC, PC, PDF, CC, CBH)
            + (100 - PC) / 100 * .RateOfSpread(FUELS[["D1"]], ISI, NoBUI, FMC, SFC, PC, PDF, CC, CBH)
            return(RSI)
          }
)
setMethod(".SlopeAdjust",
          ".M1",
          function(this, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI)
          {
            return(SlopeAdjust("M1", FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI))
          }
)
