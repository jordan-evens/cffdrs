#' @export ShelteredDuffMoistureCode
ShelteredDuffMoistureCode <- function(temp, prec, rh, dmc, mon, sdmc_old=NULL){
  #Constrain rh and precipitation
  rh <- ifelse(rh > 99.9, 99.9, rh)
  rh <- ifelse(rh < 0.0, 10.0, rh)
  prec <- ifelse(prec < 0.0, 0.0, prec)
  #Constrain temperature
  temp <- ifelse(temp < -1.1, -1.1, temp)     
  #initialize sdmc if it does not exist
  if (is.null(sdmc_old)){
    sdmc_old <- 2.6 + (1.7 * dmc) 
    sdmc_old <- sdmc_old - 6.0
    sdmc_old <- ifelse(sdmc_old < 12, 12, sdmc_old)
  } 
  
  #Reference latitude for DMC day length adjustment
  #Using the Canadian reference only
  el <- c(6.5, 7.5, 9.0, 12.8, 13.9, 13.9, 12.4, 10.9, 9.4, 8.0, 7.0, 6.0)

    #This is a modification multplier at front
  rk = 4.91 / 3.57 * 1.894 * (temp + 1.1) * (100 - rh) * el[mon] * 0.0001
  #Eq.7 (Wotton et. al. 2005) calculates rain throughfall.
  rw <- ifelse(prec < 7.69, 0.218 * prec - 0.094, 0.83 * prec - 4.8)
  #Alteration to Eq. 12 (Van Wagner & Pickett 1985)
  wmi <- 20.0 + 280.0 / exp(0.023 * sdmc_old)
  #Eqs. 13a, 13b, 13c (Van Wagner & Pickett 1985)
  b <- ifelse(sdmc_old <= 33, 100.0 / (0.5 + 0.3 * sdmc_old), 14.0 - 1.3 * 
                log(sdmc_old))
  b <- ifelse(sdmc_old > 65, 6.2 * log(sdmc_old) - 17.2, b)
  #Eq. 14 (Van Wagner & Pickett 1985) - Moisture content after rain
  wmr <- wmi + 1000.0 * rw / (48.77 + b * rw)
  #Alteration to Eq. 15 (Van Wagner & Pickett 1985)
  pr <- ifelse(prec <= 0.44, sdmc_old, 43.43 * (5.6348 - log(wmr - 20)))
  #Constrain p
  pr<-ifelse(pr<0,0,pr)
  #Calculate final SDMC
  SDMC0 <- pr + rk
  #Constrain result
  SDMC0 <- ifelse(SDMC0 < 0, 0, SDMC0)
  return(SDMC0)
}
