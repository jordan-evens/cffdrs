CrownBaseHeight <- Vectorize(function(FUELTYPE, CBH, SD, SH)
{
  return(.CrownBaseHeight(FUELS[[FUELTYPE]], CBH, SD, SH))
})
