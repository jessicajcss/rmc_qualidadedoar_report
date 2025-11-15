# Carregamento seguro de dados remotos (.rds / .Rda) com fallback local
suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
})

options(tz = "America/Sao_Paulo")

safe_read_rds_remote <- function(url, name) {
  tryCatch({
    obj <- readRDS(url(url))
    assign(name, obj, envir = .GlobalEnv)
    message("Carregado RDS remoto: ", name)
    TRUE
  }, error = function(e) {
    message("Falha ao carregar RDS remoto (", name, "): ", e$message)
    FALSE
  })
}

safe_load_rda_remote <- function(url) {
  tryCatch({
    load(url(url), envir = .GlobalEnv)
    message("Carregado Rda remoto: ", url)
    TRUE
  }, error = function(e) {
    message("Falha ao carregar Rda remoto: ", e$message)
    FALSE
  })
}

# Lista de caminhos remotos (ScheduledUpdate)
base_raw <- "https://raw.githubusercontent.com/jessicajcss/ScheduledUpdate/refs/heads/main/data/"

# Tabelas auxiliares
safe_read_rds_remote(paste0(base_raw, "localizacao.rds"), "localizacao")
safe_read_rds_remote(paste0(base_raw, "pollutants_table.rds"), "Poltab")
safe_read_rds_remote(paste0(base_raw, "AQItab.rds"), "AQItab")

# Dados principais (.Rda)
safe_load_rda_remote(paste0("https://github.com/jessicajcss/ScheduledUpdate/raw/refs/heads/main/data/meteo_hour.Rda"))
safe_load_rda_remote(paste0("https://github.com/jessicajcss/ScheduledUpdate/raw/refs/heads/main/data/data_purpleair.Rda"))
safe_load_rda_remote(paste0("https://github.com/jessicajcss/ScheduledUpdate/raw/refs/heads/main/data/air_quality_data.Rda"))
safe_load_rda_remote(paste0("https://github.com/jessicajcss/ScheduledUpdate/raw/refs/heads/main/data/air_quality_data_ugm3.Rda"))
safe_load_rda_remote(paste0("https://github.com/jessicajcss/ScheduledUpdate/raw/refs/heads/main/data/data_thermo_instantaneo_ugm3.Rda"))

# Se dados locais existirem, priorize-os (permitir trabalho offline)
if (file.exists("data/data_input/data_purpleair_hour.csv")) {
  purpleair <- read_csv("data/data_input/data_purpleair_hour.csv", show_col_types = FALSE) %>%
    mutate(Date = force_tz(as.POSIXct(date), tz = "America/Sao_Paulo"))
  message("Carregado purpleair CSV local.")
}

# Ajustes/normalizações básicos (conforme seu pipeline)
if (exists("meteo_hour")) {
  meteo_hour <- meteo_hour %>% mutate(date = as_datetime(date, tz = "America/Sao_Paulo"))
  message("meteo_hour: linhas=", nrow(meteo_hour))
}
if (exists("air_quality_data")) {
  Datafinal <- air_quality_data %>%
    mutate(CO = CO * 1.15,
           O3 = O3 * 1.96,
           NO2 = NO2 * 1.88,
           SO2 = SO2 * 2.62,
           Date = sample_day,
           Year = format(Date, "%Y"))
  message("Datafinal (thermo) carregado: linhas=", nrow(Datafinal))
}
if (exists("data_purpleair")) {
  purpleair <- data_purpleair %>%
    mutate(Date = sample_day)
  message("purpleair (data_purpleair) carregado: linhas=", nrow(purpleair))
}
if (exists("air_quality_data_ugm3")) {
  air_quality_data_ugm3 <- air_quality_data_ugm3 %>%
    mutate(date = as_datetime(date, tz = "America/Sao_Paulo"))
  message("air_quality_data_ugm3 carregado: linhas=", nrow(air_quality_data_ugm3))
}

# WHO limits (consistência)
who_limit_all <- tibble::tibble(
  value = as.numeric(c(5, 15, 15, 45, 100, 10, 25, 40, 4)),
  Diretriz_OMS = c("média anual", "24-h", "média anual", "24-h", "8-h", "média anual", "24-h", "24-h", "24-h"),
  variable = c("pm2.5", "pm2.5", "pm10", "pm10", "o3", "no2", "no2", "so2", "co")
)

message("scripts/00-load_data.R finalizado.")
