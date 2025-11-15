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
if (file.exists("data/data_input/data_purpleair_hour.csv")) {
  safe_read_csv_local("data/data_input/data_purpleair_hour.csv", name = "purpleair", date_col = "date")
}
if (file.exists("data/data_input/data_thermo_hour.csv")) {
  safe_read_csv_local("data/data_input/data_thermo_hour.csv", name = "thermo_hour", date_col = "localDate")
}
if (file.exists("data/data_input/localizacao.csv")) {
  safe_read_csv_local("data/data_input/localizacao.csv", name = "localizacao")
}

# Harmonizações / criação de objetos esperados --------------------------------
# METEO
if (exists("meteo_hour")) {
  try({
    meteo_hour <- meteo_hour %>% mutate(date = as_datetime(date, tz = "America/Sao_Paulo"))
    assign("meteo_hour", meteo_hour, envir = .GlobalEnv)
    message("meteo_hour: linhas = ", nrow(meteo_hour))
  }, silent = TRUE)
}

# AIR QUALITY (daily thermo) -> Datafinal (unit conversions)
if (exists("air_quality_data") && !exists("Datafinal")) {
  try({
    Datafinal <- air_quality_data %>%
      mutate(CO = CO * 1.15,
             O3 = O3 * 1.96,
             NO2 = NO2 * 1.88,
             SO2 = SO2 * 2.62,
             PM10 = PM10) %>%
      rename(Date = sample_day) %>%
      mutate(Year = format(Date, "%Y"))
    assign("Datafinal", Datafinal, envir = .GlobalEnv)
    message("Datafinal criado: linhas = ", nrow(Datafinal))
  }, silent = TRUE)
}

# PurpleAir (from data_purpleair Rda or purpleair CSV) -> ensure object name 'purpleair'
if (!exists("purpleair") && exists("data_purpleair")) {
  try({
    purpleair <- data_purpleair %>% mutate(Date = sample_day)
    assign("purpleair", purpleair, envir = .GlobalEnv)
    message("purpleair criado a partir de data_purpleair")
  }, silent = TRUE)
}
if (exists("purpleair")) {
  pa <- purpleair
  if (!("PM2.5" %in% names(pa)) && ("PM25" %in% names(pa))) pa <- pa %>% rename(`PM2.5` = PM25)
  if (!("Date" %in% names(pa)) && ("date" %in% names(pa))) pa <- pa %>% mutate(Date = as_datetime(date, tz = "America/Sao_Paulo"))
  assign("purpleair", pa, envir = .GlobalEnv)
  message("purpleair harmonizado: colunas = ", paste(names(pa), collapse = ", "))
}

# Hourly thermo (air_quality_data_ugm3)
if (exists("air_quality_data_ugm3")) {
  try({
    air_quality_data_ugm3 <- air_quality_data_ugm3 %>% mutate(date = as_datetime(date, tz = "America/Sao_Paulo"))
    assign("air_quality_data_ugm3", air_quality_data_ugm3, envir = .GlobalEnv)
    message("air_quality_data_ugm3 carregado: linhas = ", nrow(air_quality_data_ugm3))
  }, silent = TRUE)
}

# Instantaneo thermo (if present)
if (exists("data_thermo_instantaneo") && !exists("df_instantaneo")) {
  try({
    df_instantaneo <- data_thermo_instantaneo %>% mutate(date = as_datetime(date, tz = "America/Sao_Paulo"))
    assign("df_instantaneo", df_instantaneo, envir = .GlobalEnv)
    message("df_instantaneo criado a partir de data_thermo_instantaneo")
  }, silent = TRUE)
}

# --- PLACEHOLDERS: criam objetos mínimos se faltarem (evita abortar render) ----
if (!exists("localizacao")) {
  localizacao <- tibble::tibble(Local = character(), Tipo = character(), Cidade = character(), Lat = double(), Long = double())
  assign("localizacao", localizacao, envir = .GlobalEnv)
  message("Placeholder criado: localizacao")
}
if (!exists("Poltab")) {
  Poltab <- tibble::tibble(Poluente = character(), Limite = numeric(), Info = character())
  assign("Poltab", Poltab, envir = .GlobalEnv)
  message("Placeholder criado: Poltab")
}
if (!exists("AQItab")) {
  AQItab <- tibble::tibble(Category = character(), Min = numeric(), Max = numeric(), Color = character())
  assign("AQItab", AQItab, envir = .GlobalEnv)
  message("Placeholder criado: AQItab")
}
if (!exists("meteo_hour")) {
  meteo_hour <- tibble::tibble(
    date = seq(as.POSIXct(Sys.Date()-7), as.POSIXct(Sys.Date()), by = "1 hour"),
    Cidade = "Rio Branco do Sul",
    ws = NA_real_, wd = NA_real_, temp = NA_real_, umid = NA_real_, prec = 0, press = NA_real_, rad = NA_real_, uv = NA_real_
  )
  assign("meteo_hour", meteo_hour, envir = .GlobalEnv)
  message("Placeholder criado: meteo_hour")
}
if (!exists("air_quality_data_ugm3")) {
  air_quality_data_ugm3 <- tibble::tibble(
    date = seq(as.POSIXct(Sys.Date()-7), as.POSIXct(Sys.Date()), by = "1 hour"),
    Cidade = rep("Rio Branco do Sul", length.out = 24*8 + 1),
    SO2 = NA_real_, NO2 = NA_real_, O3 = NA_real_, CO = NA_real_, PM10 = NA_real_, `PM2.5` = NA_real_
  )
  assign("air_quality_data_ugm3", air_quality_data_ugm3, envir = .GlobalEnv)
  message("Placeholder criado: air_quality_data_ugm3")
}
if (!exists("Datafinal")) {
  Datafinal <- tibble::tibble(Date = seq.Date(Sys.Date()-30, Sys.Date(), by = "day"),
                              Cidade = "Rio Branco do Sul",
                              SO2 = NA_real_, NO2 = NA_real_, O3 = NA_real_, CO = NA_real_, PM10 = NA_real_, `PM2.5` = NA_real_)
  assign("Datafinal", Datafinal, envir = .GlobalEnv)
  message("Placeholder criado: Datafinal")
}
if (!exists("purpleair")) {
  purpleair <- tibble::tibble(Date = as.Date(Sys.Date()), Cidade = "Rio Branco do Sul", sensor_id = "placeholder", `PM2.5` = NA_real_, humidity = NA_real_, tempC = NA_real_)
  assign("purpleair", purpleair, envir = .GlobalEnv)
  message("Placeholder criado: purpleair")
}
if (!exists("df_instantaneo")) {
  df_instantaneo <- tibble::tibble(date = seq(as.POSIXct(Sys.Date()-1), as.POSIXct(Sys.Date()), by = "hour"),
                                   Cidade = "Rio Branco do Sul",
                                   Poluente = NA_character_,
                                   `Concentração (ug/m³)` = NA_real_)
  assign("df_instantaneo", df_instantaneo, envir = .GlobalEnv)
  message("Placeholder criado: df_instantaneo")
}

# Persist: salvar objetos-chave em data/processed para que capítulos (sessões separadas) possam ler
proc_dir <- file.path("data", "processed")
dir.create(proc_dir, recursive = TRUE, showWarnings = FALSE)

objs_to_save <- c(
  "localizacao", "Poltab", "AQItab",
  "meteo_hour", "Datafinal", "air_quality_data_ugm3",
  "purpleair", "df_instantaneo"
)

for (obj in objs_to_save) {
  if (exists(obj, envir = .GlobalEnv)) {
    safe_path <- file.path(proc_dir, paste0(obj, ".rds"))
    tryCatch({
      saveRDS(get(obj, envir = .GlobalEnv), file = safe_path)
      message("Salvo cache RDS: ", safe_path)
    }, error = function(e) {
      message("Falha ao salvar RDS para ", obj, ": ", e$message)
    })
  }
}

# Final checks & summary -----------------------------------------------------
objs_expected <- objs_to_save
present <- objs_expected[objs_expected %in% ls(envir = .GlobalEnv)]
missing <- setdiff(objs_expected, present)

message("Carregamento finalizado. Objetos presentes: ", paste(present, collapse = ", "))
if (length(missing) > 0) {
  message("Objetos faltantes (considere prover localmente em data/ ou verificar downloads remotos): ", paste(missing, collapse = ", "))
}

message("scripts/00-load_data.R finalizado.")

