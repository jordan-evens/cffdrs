.C3 <- structure(.Data=list(name="C3",
                            a=110,
                            b=0.0444,
                            c0=3.0,
                            BUIo=62,
                            Q=0.75,
                            sfcA=5.0,
                            sfcB=-0.0164,
                            sfcC=2.24,
                            sfcD=as.numeric(NA),
                            CBH=8,
                            CFL=1.15),
                 class=c(".C3", ".FuelClosed", "Fuel", ".FuelBase")
)
.SlopeAdjust..C3 <- function(this, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI)
{
  return(SlopeAdjust("C3", FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI))
}
.C3$SlopeAdjust <- .SlopeAdjust..C3
