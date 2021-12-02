.M4 <- structure(.Data=list(name="M4",
                            a=100,
                            b=0.0404,
                            c0=1.48,
                            BUIo=50,
                            Q=0.8,
                            sfcA=5.0,
                            sfcB=-0.0115,
                            sfcC=1.0,
                            CBH=6,
                            CFL=0.8),
                 class=c(".M4", ".FuelMixedDead", ".FuelClosed", "Fuel", ".FuelBase")
)
.BaseRateOfSpread..M4 <- function(this, ISI, BUI, FMC, SFC, PC, PDF, CC, CBH)
{
  #Initial Rate of Spread for M4 Mixedwood
  #Eq. 30 (Wotton et. al 2009)
  RSI_m4 <- this$a * ((1 - exp(-this$b * ISI))**this$c0)
  #Eq. 33 (Wotton et. al 2009)
  RSI <- PDF / 100* RSI_m4 +
    0.2 * (1 - PDF / 100)* .BaseRateOfSpread(.D1, ISI, BUI=-1, FMC, SFC, PC, PDF, CC,CBH)
  return(RSI)
}
.SlopeEquivalentInitialSpreadIndex..M4 <- function(this, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI)
{
  #Set % Dead Balsam Fir to 100%
  PDF100 <- 100
  #Eq. 41a (Wotton 2009) - Calculate the slope equivalent ISI
  #Eq. 41b (Wotton 2009) - Calculate the slope equivalent ISI
  # HACK: call superclass function
  ISF_M4 <- .SlopeEquivalentInitialSpreadIndex.Fuel(this, FFMC, BUI=-1, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF100, CC, CBH, ISI)
  ISF_D1 <- .SlopeEquivalentInitialSpreadIndex(.D1, FFMC, BUI=-1, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF100, CC, CBH, ISI)
  #Eq. 42b (Wotton 2009) - Calculate weighted average for the M3 type
  ISF <- PDF / 100 * ISF_M4 + (1 - PDF / 100) * ISF_D1
  if (!is.na(ISF) && ISF < 0)
  {
    print(this)
    print(ISF)
    stopifnot(ISF >= 0)
  }
  return(ISF)
}
