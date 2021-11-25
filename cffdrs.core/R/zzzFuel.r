FUELS <- list(NF=.FuelNF,
              WA=.FuelWA,
              C1=.C1,
              C2=.C2,
              C3=.C3,
              C4=.C4,
              C5=.C5,
              C6=.C6,
              C7=.C7,
              D1=.D1,
              M1=.M1,
              M2=.M2,
              M3=.M3,
              M4=.M4,
              S1=.S1,
              S2=.S2,
              S3=.S3,
              O1A=.O1A,
              O1B=.O1B)
fixClass <- function(base)
{
  if (length(class(base)) > 1)
  {
    for (s in class(base)[2:length(class(base))])
    {
      print(s)
      cur <- get(s)
      # fixClass(cur)
      # cur <- get(s)
      for (f in names(cur))
      {
        print(f)
        if (!(f %in% names(base)))
        {
          print(paste0("Overriding ", f))
          base[[f]] <- cur[[f]]
        }
      }
    }
  }
  return(base)
}

fixClass(.FuelBase)
fixClass(Fuel)
fixClass(.FuelOpen)
fixClass(.FuelClosed)
fixClass(.FuelGrass)
fixClass(.FuelSlash)
fixClass(.FuelMixedwood)
fixClass(.FuelMixedDead)
# HACK: force inheritance
for (n in names(FUELS))
{
  base <- FUELS[[n]]
  FUELS[[n]] <- fixClass(base)
}
