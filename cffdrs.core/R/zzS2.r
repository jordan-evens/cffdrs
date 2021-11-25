.S2 <- structure(.Data=list(name="S2",
                            a=40,
                            b=0.0438,
                            c0=1.7,
                            BUIo=63,
                            Q=0.75,
                            sfcA=10.0,
                            sfcB=-0.013,
                            sfcC=6.0,
                            sfcD=-0.060,
                            CBH=0,
                            CFL=0),
                 class=c(".S2", ".FuelSlash", ".FuelOpen", "Fuel", ".FuelBase")
)
.SlopeAdjust..S2 <- function(this, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI)
{
  return(SlopeAdjust("S2", FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI))
}
