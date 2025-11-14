u.wind <- function(ws, wd) ws * sin(wd * pi/180)
v.wind <- function(ws, wd) ws * cos(wd * pi/180)
wind_direction <- function(u, v) {
  dir <- (atan2(u, v) * 180/pi)
  ((dir + 360) %% 360)
}