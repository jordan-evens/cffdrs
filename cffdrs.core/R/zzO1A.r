.O1A <- structure(.Data=list(name="O1A",
                             a=190,
                             b=0.0310,
                             c0=1.4,
                             BUIo=01,
                             Q=1.0,
                             sfcA=as.numeric(NA),
                             sfcB=as.numeric(NA),
                             sfcC=as.numeric(NA),
                             sfcD=as.numeric(NA),
                             CBH=0,
                             CFL=0),
                  class=c(".O1A", ".FuelGrass", ".FuelOpen", "Fuel", ".FuelBase")
)
.SlopeAdjust..O1A <- function(this, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI)
{
  return(SlopeAdjust("O1A", FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI))
}
