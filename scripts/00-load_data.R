# Carregamento seguro de dados remotos (.rds / .Rda) com fallback local
suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(readr)
})

options(tz = "America/Sao_Paulo")

# Helpers --------------------------------------------------------------------
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
    env_before <- ls(envir = .GlobalEnv)
    load(url(url), envir = .GlobalEnv)
    env_after <- ls(envir = .GlobalEnv)
    new_objs <- setdiff(env_after, env_before)
    message("Carregado Rda remoto: ", url, " -> objetos: ", paste(new_objs, collapse = ", "))
    TRUE
  }, error = function(e) {
    message("Falha ao carregar Rda remoto: ", e$message)
    FALSE
  })
}

safe_read_csv_local <- function(path, name = NULL, date_col = NULL, tz = "America/Sao_Paulo") {
  tryCatch({
    df <- readr::read_csv(path, show_col_types = FALSE)
    if (!is.null(date_col) && date_col %in% names(df)) {
      df[[date_col]] <- force_tz(as.POSIXct(df[[date_col]]), tz = tz)
    }
    if (!is.null(name)) assign(name, df, envir = .GlobalEnv)
    message("Carregado CSV local: ", path)
    TRUE
  }, error = function(e) {
    message("Falha ao carregar CSV local (", path, "): ", e$message)
    FALSE
  })
}

# Remote base ----------------------------------------------------------------
base_raw <- "https://raw.githubusercontent.com/jessicajcss/ScheduledUpdate/refs/heads/main/data/"

# Try load RDS files (named objects)
safe_read_rds_remote(paste0(base_raw, "localizacao.rds"), "localizacao")
safe_read_rds_remote(paste0(base_raw, "pollutants_table.rds"), "Poltab")
safe_read_rds_remote(paste0(base_raw, "AQItab.rds"), "AQItab")

# Try load Rda files (may create objects with original names)
safe_load_rda_remote(paste0("https://github.com/jessicajcss/ScheduledUpdate/raw/refs/heads/main/data/meteo_hour.Rda"))
safe_load_rda_remote(paste0("https://github.com/jessicajcss/ScheduledUpdate/raw/refs/heads/main/data/data_purpleair.Rda"))
safe_load_rda_remote(paste0("https://github.com/jessicajcss/ScheduledUpdate/raw/refs/heads/main/data/air_quality_data.Rda"))
safe_load_rda_remote(paste0("https://github.com/jessicajcss/ScheduledUpdate/raw/refs/heads/main/data/air_quality_data_ugm3.Rda"))
safe_load_rda_remote(paste0("https://github.com/jessicajcss/ScheduledUpdate/raw/refs/heads/main/data/data_thermo_instantaneo_ugm3.Rda"))

# Local fallbacks (CSV files) -----------------------------------------------
# PurpleAir CSV fallback (hourly)
if (file.exists("data/data_input/data_purpleair_hour.csv")) {
  safe_read_csv_local("data/data_input/data_purpleair_hour.csv", name = "purpleair", date_col = "date")
}

# Thermo hourly CSV fallback
if (file.exists("data/data_input/data_thermo_hour.csv")) {
  safe_read_csv_local("data/data_input/data_thermo_hour.csv", name = "thermo_hour", date_col = "localDate")
}

# Localizacao CSV fallback
if (file.exists("data/data_input/localizacao.csv")) {
  # expect columns: Local, Tipo, Cidade, Lat, Long (or similar)
  safe_read_csv_local("data/data_input/localizacao.csv", name = "localizacao")
}

# If some RDS loaded objects have different internal names, attempt common renames
# (This is safe: only rename if common alternative names exist)
if (!exists("localizacao") && exists("localizacao_df")) {
  assign("localizacao", localizacao_df, envir = .GlobalEnv)
}

# Harmonizações / criação de objetos esperados --------------------------------
# METEO
if (exists("meteo_hour")) {
  try({
    meteo_hour <- meteo_hour %>%
      mutate(date = as_datetime(date, tz = "America/Sao_Paulo"))
    assign("meteo_hour", meteo_hour, envir = .GlobalEnv)
    message("meteo_hour: linhas = ", nrow(meteo_hour))
  }, silent = TRUE)
}

# AIR QUALITY (daily thermo) -> Datafinal (unit conversions)
if (exists("air_quality_data") && !exists("Datafinal")) {
  try({
    Datafinal <- air_quality_data %>%
      mutate(CO = CO * 1.15,    # ppm -> mg/m3 (as in your script)
             O3 = O3 * 1.96,
             NO2 = NO2 * 1.88,
             SO2 = SO2 * 2.62,
             PM10 = PM10) %>%
      rename(Date = sample_day) %>%
      mutate(Year = format(Date, "%Y"))
    assign("Datafinal", Datafinal, envir = .GlobalEnv)
    message("Datafinal (thermo) criado a partir de air_quality_data: linhas = ", nrow(Datafinal))
  }, silent = TRUE)
}

# PurpleAir (from data_purpleair Rda or purpleair CSV) -> ensure object name 'purpleair'
if (!exists("purpleair") && exists("data_purpleair")) {
  try({
    purpleair <- data_purpleair %>%
      mutate(Date = sample_day) %>%
      select(everything())
    assign("purpleair", purpleair, envir = .GlobalEnv)
    message("Objeto 'purpleair' criado a partir de data_purpleair")
  }, silent = TRUE)
}

# If purpleair exists but column names differ, attempt to harmonize common names
if (exists("purpleair")) {
  pa <- purpleair
  # common fixes: ensure PM2.5 column exists and named consistently
  if (!("PM2.5" %in% names(pa)) && ("PM25" %in% names(pa))) {
    pa <- pa %>% rename(`PM2.5` = PM25)
  }
  if (!("Date" %in% names(pa)) && ("date" %in% names(pa))) {
    pa <- pa %>% mutate(Date = as_datetime(date, tz = "America/Sao_Paulo"))
  }
  assign("purpleair", pa, envir = .GlobalEnv)
  message("purpleair harmonizado: colunas = ", paste(names(pa), collapse = ", "))
}

# Hourly thermo (air_quality_data_ugm3) -> ensure tz and 'date' column
if (exists("air_quality_data_ugm3")) {
  try({
    air_quality_data_ugm3 <- air_quality_data_ugm3 %>%
      mutate(date = as_datetime(date, tz = "America/Sao_Paulo"))
    assign("air_quality_data_ugm3", air_quality_data_ugm3, envir = .GlobalEnv)
    message("air_quality_data_ugm3 carregado: linhas = ", nrow(air_quality_data_ugm3))
  }, silent = TRUE)
}

# Instantaneo thermo (if present)
if (exists("data_thermo_instantaneo") && !exists("df_instantaneo")) {
  try({
    # convert to long if needed; keep a simple safe version
    df_instantaneo <- data_thermo_instantaneo %>%
      mutate(date = as_datetime(date, tz = "America/Sao_Paulo"))
    assign("df_instantaneo", df_instantaneo, envir = .GlobalEnv)
    message("df_instantaneo criado a partir de data_thermo_instantaneo")
  }, silent = TRUE)
}

# Ensure Poltab/AQItab available -----------------------------------------------
if (!exists("Poltab") && file.exists("data/data_input/pollutants_table.csv")) {
  safe_read_csv_local("data/data_input/pollutants_table.csv", name = "Poltab")
}
if (!exists("AQItab") && file.exists("data/data_input/AQItab.csv")) {
  safe_read_csv_local("data/data_input/AQItab.csv", name = "AQItab")
}

# Final checks & summary -----------------------------------------------------
objs_expected <- c(
  "localizacao", "Poltab", "AQItab",
  "meteo_hour", "Datafinal", "air_quality_data_ugm3",
  "purpleair", "df_instantaneo"
)

present <- objs_expected[objs_expected %in% ls(envir = .GlobalEnv)]
missing <- setdiff(objs_expected, present)

message("Carregamento finalizado. Objetos presentes: ", paste(present, collapse = ", "))
if (length(missing) > 0) {
  message("Objetos faltantes (considere prover localmente em data/ ou verificar downloads remotos): ", paste(missing, collapse = ", "))
}

# Small convenience: create safe lightweight versions for plotting if full objects missing
if (!exists("purpleair") && exists("data_purpleair")) {
  try({
    purpleair <- data_purpleair %>% mutate(Date = sample_day)
    assign("purpleair", purpleair, envir = .GlobalEnv)
    message("Fallback: purpleair criado (segunda tentativa).")
  }, silent = TRUE)
}

# End of file
message("scripts/00-load_data.R finalizado.")
