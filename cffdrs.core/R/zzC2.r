.C2 <- structure(.Data=list(name="C2",
                            a=110,
                            b=0.0282,
                            c0=1.5,
                            BUIo=64,
                            Q=0.7,
                            sfcA=5.0,
                            sfcB=-0.0115,
                            sfcC=1.0,
                            sfcD=as.numeric(NA),
                            CBH=3,
                            CFL=0.8),
                 class=c(".C2", ".FuelClosed", "Fuel", ".FuelBase")
)
.SlopeAdjust..C2 <- function(this, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI)
{
  return(SlopeAdjust("C2", FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI))
}
.C2$SlopeAdjust <- .SlopeAdjust..C2
