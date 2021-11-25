.C4 <- structure(.Data=list(name="C4",
                            a=110,
                            b=0.0293,
                            c0=1.5,
                            BUIo=66,
                            Q=0.8,
                            sfcA=5.0,
                            sfcB=-0.0164,
                            sfcC=2.24,
                            sfcD=as.numeric(NA),
                            CBH=4,
                            CFL=1.2),
                 class=c(".C4", ".FuelClosed", "Fuel", ".FuelBase")
)
.SlopeAdjust..C4 <- function(this, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI)
{
  return(SlopeAdjust("C4", FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI))
}
.C4$SlopeAdjust <- .SlopeAdjust..C4
