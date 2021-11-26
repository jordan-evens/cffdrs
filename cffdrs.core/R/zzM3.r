.M3 <- structure(.Data=list(name="M3",
                            a=120,
                            b=0.0572,
                            c0=1.4,
                            BUIo=50,
                            Q=0.8,
                            sfcA=5.0,
                            sfcB=-0.0115,
                            sfcC=1.0,
                            sfcD=as.numeric(NA),
                            CBH=6,
                            CFL=0.8),
                 class=c(".M3", ".FuelMixedDead", ".FuelClosed", "Fuel", ".FuelBase")
)
.RateOfSpread..M3 <- function(this, ISI, BUI, FMC, SFC, PC, PDF, CC, CBH)
{
  NoBUI <- -1
  #Initial Rate of Spread for M3 Mixedwood
  #Eq. 30 (Wotton et. al 2009)
  RSI_m3 <- this[["a"]] * ((1 - exp(-this[["b"]] * ISI)) ** this[["c0"]])
  #Eq. 29 (Wotton et. al 2009)
  RSI <- PDF / 100 * RSI_m3
  + (1 - PDF / 100) * .RateOfSpread(FUELS[["D1"]], ISI, NoBUI, FMC, SFC, PC, PDF, CC, CBH)
  return(RSI)
}
.SlopeAdjust..M3 <- function(this, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI)
{
  return(SlopeAdjust("M3", FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI))
}
