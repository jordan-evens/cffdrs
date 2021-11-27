.C1 <- structure(.Data=list(name="C1",
                            a=90,
                            b=0.0649,
                            c0=4.5,
                            BUIo=72,
                            Q=0.9,
                            CBH=2,
                            CFL=0.75),
                 class=c(".C1", ".FuelOpen", "Fuel", ".FuelBase")
)
.SurfaceFuelConsumption..C1 <- function(this, FFMC, BUI, PC, GFL)
{
  #Eqs. 9a, 9b (Wotton et. al. 2009) - Solving the lower bound of FFMC value
  # for the C1 fuel type SFC calculation
  return (ifelse(FFMC > 84,
                 0.75 + 0.75 * (1 - exp(-0.23 * (FFMC - 84)))**0.5,
                 0.75 - 0.75 * (1 - exp(-0.23 * (84 - FFMC)))**0.5))
}
