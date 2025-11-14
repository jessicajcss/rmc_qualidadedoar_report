# Helpers simples para cálculos AQI (placeholder)
breaks_pm25 <- c(0, 12, 35.4, 55.4, 150.4, 250.4, 350.4, 500.4)
aqi_scale <- c(0, 50, 100, 150, 200, 300, 400, 500)

calc_aqi_pm25 <- function(conc) {
  if (is.na(conc)) return(NA_real_)
  idx <- findInterval(conc, breaks_pm25, rightmost.closed=TRUE)
  if (idx == 0) return(NA_real_)
  Cl <- breaks_pm25[idx]
  Ch <- breaks_pm25[idx+1]
  Il <- aqi_scale[idx]
  Ih <- aqi_scale[idx+1]
  aqi <- (Ih - Il)/(Ch - Cl) * (conc - Cl) + Il
  round(aqi,1)
}