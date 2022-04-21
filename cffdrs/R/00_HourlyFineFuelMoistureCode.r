#' Hourly Fine Fuel Moisture Code Calculation
#'
#' @param temp       Temperature (centigrade)
#' @param rh         Relative Humidity (\%)
#' @param ws         Wind speed (km/h)
#' @param prec       Precipitation (mm)
#' @param ffmc_old   The Fine Fuel Moisture Code from previous iteration
#' @param time.step  Time step (hours) [default 1 hour]
#'
#' @return A single hourly fine fuel moisture code value
#' @export HourlyFineFuelMoistureCode
HourlyFineFuelMoistureCode <- function(temp, rh, ws, prec, ffmc_old, time.step=1)
{
  Tp <- temp
  H <- rh
  W <- ws
  ro <- prec
  Fo <- ffmc_old
  t0 <- time.step
  #Eq. 1 (with a more precise multiplier than the daily)
  mo <- 147.27723 * (101 - Fo)/(59.5 + Fo)
  rf <- ro
  #Eqs. 3a & 3b (Van Wagner & Pickett 1985)
  mr <- ifelse(mo <= 150,
               mo + 42.5 * rf * exp(-100 / (251 - mo)) * (1 - exp(-6.93 / rf)),
               mo + 42.5 * rf * exp(-100 / (251 - mo)) * (1 - exp(-6.93 / rf)) +
                 0.0015 * ((mo - 150)^2) * (rf^0.5))
  #The real moisture content of pine litter ranges up to about 250 percent,
  # so we cap it at 250
  mr <- ifelse(mr > 250, 250, mr)
  mo <- ifelse(ro > 0.0, mr, mo)
  #Eq. 2a Equilibrium moisture content from drying
  Ed <- 0.942 * (H^0.679) + 11 * exp((H - 100) / 10) + 0.18 *
    (21.1 - Tp) * (1 - exp(-0.115 * H))
  #Eq. 3a Log drying rate at the normal temperature of 21.1C
  ko <- 0.424 * (1 - (H / 100)^1.7) + 0.0694 * (W^0.5) *
    (1 - (H / 100)^8)
  #Eq. 3b
  kd <- ko * 0.0579 * exp(0.0365 * Tp)
  #Eq. 8 (Van Wagner & Pickett 1985)
  md <- Ed + (mo - Ed) * (10^(-kd * t0))
  #Eq. 2b Equilibrium moisture content from wetting
  Ew <- 0.618 * (H^0.753) + 10 * exp((H - 100) / 10) + 0.18 *
    (21.1 - Tp) * (1 - exp(-0.115 * H))
  #Eq. 7a Log wetting rate at the normal temperature of 21.1 C
  k1 <- 0.424 * (1 - ((100 - H) / 100)^1.7) + 0.0694 *
    (W^0.5) * (1 - ((100 - H) / 100)^8)
  #Eq. 4b
  kw <- k1 * 0.0579 * exp(0.0365 * Tp)
  #Eq. 8 (Van Wagner & Pickett 1985)
  mw <- Ew - (Ew - mo) * (10^(-kw * t0))
  #Constraints
  m <- ifelse(mo > Ed, md, mw)
  m <- ifelse(Ed >= mo & mo >= Ew, mo, m)
  #Eq. 6 - Final hffmc calculation (modified 3rd constant to 147.27723)
  Fo <- 59.5 * (250 - m) / (147.27723 + m)
  Fo <- ifelse(Fo <=0, 0, Fo)
  return(Fo)
}
