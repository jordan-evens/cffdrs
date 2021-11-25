.C5 <- structure(.Data=list(name="C5",
                            a=30,
                            b=0.0697,
                            c0=4.0,
                            BUIo=56,
                            Q=0.8,
                            sfcA=5.0,
                            sfcB=-0.0149,
                            sfcC=2.48,
                            sfcD=as.numeric(NA),
                            CBH=18,
                            CFL=1.2),
                 class=c(".C5", ".FuelClosed", "Fuel", ".FuelBase")
)
.SlopeAdjust..C5 <- function(this, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI)
{
  return(SlopeAdjust("C5", FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI))
}
.C5$SlopeAdjust <- .SlopeAdjust..C5
