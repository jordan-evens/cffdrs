.S1 <- structure(.Data=list(name="S1",
                            a=75,
                            b=0.0297,
                            c0=1.3,
                            BUIo=38,
                            Q=0.75,
                            sfcA=4.0,
                            sfcB=-0.025,
                            sfcC=4.0,
                            sfcD=-0.034,
                            CBH=0,
                            CFL=0),
                 class=c(".S1", ".FuelSlash", ".FuelOpen", "Fuel", ".FuelBase")
)
.SlopeAdjust..S1 <- function(this, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI)
{
  return(SlopeAdjust("S1", FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI))
}
.S1$SlopeAdjust <- .SlopeAdjust..S1
