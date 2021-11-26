.O1B <- structure(.Data=list(name="O1B",
                             a=250,
                             b=0.0350,
                             c0=1.7,
                             BUIo=01,
                             Q=1.0,
                             sfcA=as.numeric(NA),
                             sfcB=as.numeric(NA),
                             sfcC=as.numeric(NA),
                             sfcD=as.numeric(NA),
                             CBH=0,
                             CFL=0),
                  class=c(".O1B", ".FuelGrass", ".FuelOpen", "Fuel", ".FuelBase")
)
.SlopeAdjust..O1B <- function(this, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI)
{
  return(SlopeAdjust("O1B", FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI))
}
