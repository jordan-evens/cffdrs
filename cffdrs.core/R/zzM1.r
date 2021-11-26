.M1 <- structure(.Data=list(name="M1",
                            a=0,
                            b=0,
                            c0=0,
                            BUIo=50,
                            Q=0.8,
                            sfcA=as.numeric(NA),
                            sfcB=as.numeric(NA),
                            sfcC=as.numeric(NA),
                            sfcD=as.numeric(NA),
                            CBH=6,
                            CFL=0.8),
                 class=c(".M1", ".FuelMixedwood", ".FuelClosed", "Fuel", ".FuelBase")
)
.RateOfSpread..M1 <- function(this, ISI, BUI, FMC, SFC, PC, PDF, CC, CBH)
{
  NoBUI <- -1
  #Eq. 27 (FCFDG 1992) - Initial Rate of Spread for M1 Mixedwood type
  RSI <- PC / 100 * .RateOfSpread(FUELS[["C2"]], ISI, NoBUI, FMC, SFC, PC, PDF, CC, CBH)
  + (100 - PC) / 100 * .RateOfSpread(FUELS[["D1"]], ISI, NoBUI, FMC, SFC, PC, PDF, CC, CBH)
  return(RSI)
}
