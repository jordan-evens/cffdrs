GrassFuelMoistureCode <- function(grass_moisture)
{
  return(59.5 * ((250 - grass_moisture) / (147.27723 + grass_moisture)))
}

GrassFuelMoisture <- function(GFMCold, temp, rh, ws, prec, isol, time.step = 1, roFL = 0.3)
{
  t0 <- time.step
  #Eq. 13 - Calculate previous moisture code
  MCold <- 147.27723 * ((101 - GFMCold) / (59.5 + GFMCold))
  #Eq. 11 - Calculate the moisture content of the layer in % after rainfall
  MCr <- ifelse(prec > 0, MCold + 100 * (prec / roFL), MCold)
  #Constrain to 250
  MCr <- ifelse(MCr > 250, 250, MCr)
  MCold <- MCr
  #Eq. 2 - Calculate Fuel temperature
  Tf <- temp + 35.07 * isol * exp(-0.06215 * ws)
  #Eq. 3 - Calculate Saturation Vapour Pressure (Baumgartner et a. 1982)
  eS.T <- 6.107 * 10^(7.5 * temp / (237 + temp))
  #Eq. 3 for Fuel temperature
  eS.Tf <- 6.107 * 10^(7.5 * Tf / (237 + Tf))
  #Eq. 4 - Calculate Fuel Level Relative Humidity
  RH.f <- rh * (eS.T / eS.Tf)
  #Eq. 7 - Calculate Equilibrium Moisture Content for Drying phase
  EMC.D <- (1.62 * RH.f^0.532 + 13.7 * exp((RH.f - 100) / 13.0)) +
            0.27 * (26.7 - Tf) * (1 - exp(-0.115 * RH.f))
  #Eq. 7 - Calculate Equilibrium Moisture Content for Wetting phase
  EMC.W <- (1.42 * RH.f^0.512 + 12.0 * exp((RH.f - 100) / 18.0)) +
            0.27 * (26.7 - Tf) * (1 - exp(-0.115 * RH.f))
  #RH in terms of RH/100 for desorption
  Rf <- ifelse(MCold > EMC.D, RH.f / 100, rh)
  #RH in terms of 1-RH/100 for absorption
  Rf <- ifelse(MCold < EMC.W, (100 - RH.f) / 100, Rf)
  #Eq. 10 - Calculate Inverse Response time of grass (hours)
  K.GRASS <- 0.389633 * exp(0.0365 * Tf) * (0.424 * (1 - Rf^1.7) + 0.0694 *
                                            sqrt(ws) * (1 - Rf^8))
  #Fuel is drying, calculate Moisture Content
  MC0 <- ifelse(MCold > EMC.D, EMC.D + (MCold - EMC.D) *
                exp(-1.0 * log(10.0) * K.GRASS * t0), MCold)
  #Fuel is wetting, calculate moisture content
  MC0 <- ifelse(MCold < EMC.W, EMC.W + (MCold - EMC.W) *
                exp(-1.0 * log(10.0) * K.GRASS * t0), MC0)
  return(MC0)
}
