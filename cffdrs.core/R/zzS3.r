.S3 <- structure(.Data=list(name="S3",
                            a=55,
                            b=0.0829,
                            c0=3.2,
                            BUIo=31,
                            Q=0.75,
                            sfcA=12.0,
                            sfcB=-0.0166,
                            sfcC=20.0,
                            sfcD=-0.0210,
                            CBH=0,
                            CFL=0),
                 class=c(".S3", ".FuelSlash", ".FuelOpen", "Fuel", ".FuelBase")
)
.SlopeAdjust..S3 <- function(this, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI)
{
  return(SlopeAdjust("S3", FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI))
}
.S3$SlopeAdjust <- .SlopeAdjust..S3
