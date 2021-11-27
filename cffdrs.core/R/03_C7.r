.C7 <- structure(.Data=list(name="C7",
                            a=45,
                            b=0.0305,
                            c0=2.0,
                            BUIo=106,
                            Q=0.85,
                            CBH=10,
                            CFL=0.5),
                 class=c(".C7", ".FuelClosed", "Fuel", ".FuelBase")
)
.SurfaceFuelConsumption..C7 <- function(this, FFMC, BUI, PC, GFL)
{
  #Eqs. 13, 14, 15 (FCFDG 1992) - C7 Fuel Types
  return (ifelse(FFMC > 70,
                 2 * (1 - exp(-0.104 * (FFMC - 70))),
                 0) + 1.5 * (1 - exp(-0.0201 * BUI)))
  
}
