.M3 <- structure(.Data=list(name="M3",
                            a=120,
                            b=0.0572,
                            c0=1.4,
                            BUIo=50,
                            Q=0.8,
                            sfcA=5.0,
                            sfcB=-0.0115,
                            sfcC=1.0,
                            CBH=6,
                            CFL=0.8),
                 class=c(".M3", ".FuelMixedDead", ".FuelClosed", "Fuel", ".FuelBase")
)
.BaseRateOfSpread..M3 <- function(this, ISI, BUI, FMC, SFC, PC, PDF, CC, CBH)
{
  NoBUI <- -1
  #Initial Rate of Spread for M3 Mixedwood
  #Eq. 30 (Wotton et. al 2009)
  RSI_m3 <- this[["a"]] * ((1 - exp(-this[["b"]] * ISI)) ** this[["c0"]])
  #Eq. 29 (Wotton et. al 2009)
  RSI <- PDF / 100 * RSI_m3 +
    (1 - PDF / 100) * .BaseRateOfSpread(.D1, ISI, NoBUI, FMC, SFC, PC, PDF, CC, CBH)
  return(RSI)
}
.SlopeEquivalentInitialSpreadIndex..M3 <- function(this, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI)
{
  #Set % Dead Balsam Fir to 100%
  PDF100 <- 100
  #Eq. 41a (Wotton 2009) - Calculate the slope equivalent ISI
  #Eq. 41b (Wotton 2009) - Calculate the slope equivalent ISI
  # HACK: call superclass function
  ISF_M3 <- .SlopeEquivalentInitialSpreadIndex.Fuel(this, FFMC, BUI=-1, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF100, CC, CBH, ISI)
  ISF_D1 <- .SlopeEquivalentInitialSpreadIndex(.D1, FFMC, BUI=-1, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF100, CC, CBH, ISI)
  #Eq. 42b (Wotton 2009) - Calculate weighted average for the M3 type
  ISF <- PDF / 100 * ISF_M3 + (1 - PDF / 100) * ISF_D1
  return(ISF)
}
