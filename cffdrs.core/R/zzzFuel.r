makeFuel <- function(FUELTYPE)
{
  return(new(paste0(".", FUELTYPE),
             name=FUELTYPE,
             a=.FUELS[[FUELTYPE]]$a,
             b=.FUELS[[FUELTYPE]]$b,
             c0=.FUELS[[FUELTYPE]]$c0,
             BUIo=.FUELS[[FUELTYPE]]$BUIo,
             Q=.FUELS[[FUELTYPE]]$Q,
             sfcA=.FUELS[[FUELTYPE]]$sfcA,
             sfcB=.FUELS[[FUELTYPE]]$sfcB,
             sfcC=.FUELS[[FUELTYPE]]$sfcC,
             sfcD=.FUELS[[FUELTYPE]]$sfcD,
             CBH=.FUELS[[FUELTYPE]]$CBH,
             CFL=.FUELS[[FUELTYPE]]$CFL
  ))
}

FUELS <- list(NF=new(".FuelNF", name="NF"),
              WA=new(".FuelNF", name="WA"),
              C1=makeFuel("C1"),
              C2=makeFuel("C2"),
              C3=makeFuel("C3"),
              C4=makeFuel("C4"),
              C5=makeFuel("C5"),
              C6=makeFuel("C6"),
              C7=makeFuel("C7"),
              D1=makeFuel("D1"),
              M1=makeFuel("M1"),
              M2=makeFuel("M2"),
              M3=makeFuel("M3"),
              M4=makeFuel("M4"),
              S1=makeFuel("S1"),
              S2=makeFuel("S2"),
              S3=makeFuel("S3"),
              O1A=makeFuel("O1A"),
              O1B=makeFuel("O1B"))
