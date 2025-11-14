suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
})

options(tz = "America/Sao_Paulo")

safe_load_rds <- function(url, name) {
  tryCatch({
    obj <- readRDS(url(url))
    assign(name, obj, envir = .GlobalEnv)
    message("Carregado: ", name)
    TRUE
  }, error = function(e) {
    message("Falha ao carregar ", name, ": ", e$message)
    FALSE
  })
}

safe_load_rda <- function(url, expected) {
  ok <- FALSE
  try({
    load(url(url), envir = .GlobalEnv)
    ok <- TRUE
    message("Carregado (Rda): ", expected)
  }, silent = TRUE)
  if (!ok) message("Não carregado: ", expected)
  ok
}

# Localização sensores / tabelas
safe_load_rds("https://raw.githubusercontent.com/jessicajcss/ScheduledUpdate/refs/heads/main/data/localizacao.rds","localizacao")
safe_load_rds("https://raw.githubusercontent.com/jessicajcss/ScheduledUpdate/refs/heads/main/data/pollutants_table.rds","Poltab")
safe_load_rds("https://raw.githubusercontent.com/jessicajcss/ScheduledUpdate/refs/heads/main/data/AQItab.rds","AQItab")

# Dados principais
safe_load_rda("https://github.com/jessicajcss/ScheduledUpdate/raw/refs/heads/main/data/meteo_hour.Rda","meteo_hour")
safe_load_rda("https://github.com/jessicajcss/ScheduledUpdate/raw/refs/heads/main/data/data_purpleair.Rda","data_purpleair")
safe_load_rda("https://github.com/jessicajcss/ScheduledUpdate/raw/refs/heads/main/data/air_quality_data.Rda","air_quality_data")
safe_load_rda("https://github.com/jessicajcss/ScheduledUpdate/raw/refs/heads/main/data/air_quality_data_ugm3.Rda","air_quality_data_ugm3")
safe_load_rda("https://github.com/jessicajcss/ScheduledUpdate/raw/refs/heads/main/data/data_thermo_instantaneo_ugm3.Rda","data_thermo_instantaneo")

# Harmonizações
if (exists("meteo_hour")) {
  meteo_hour <- meteo_hour %>%
    mutate(date = as_datetime(date, tz="America/Sao_Paulo"))
}

if (exists("air_quality_data")) {
  Datafinal <- air_quality_data %>%
    mutate(CO = CO*1.15,
           O3 = O3*1.96,
           NO2 = NO2*1.88,
           SO2 = SO2*2.62,
           Date = sample_day,
           Year = format(Date, "%Y"))
}

if (exists("data_purpleair")) {
  purpleair <- data_purpleair %>%
    mutate(Date = sample_day,
           AQI_PM25 = AQI_PM2.5)
}

if (exists("air_quality_data_ugm3")) {
  air_quality_data_ugm3 <- air_quality_data_ugm3 %>%
    mutate(date = as_datetime(date, tz="America/Sao_Paulo"))
}

# WHO limits
who_limit_all <- tibble(
  value = as.numeric(c(5, 15, 15, 45, 100, 10, 25, 40, 4)),
  Diretriz_OMS = c("média anual","24-h","média anual","24-h","8-h","média anual","24-h","24-h","24-h"),
  variable = c("pm2.5","pm2.5","pm10","pm10","o3","no2","no2","so2","co")
)

message("Dados carregados e padronizados.")