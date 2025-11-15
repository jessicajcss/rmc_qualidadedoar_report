# scripts/check_data.R
# Verifica existência e dimensões dos objetos de dados esperados, grava relatório local e retorna tibble.

suppressPackageStartupMessages({
  library(tibble)
  library(dplyr)
})

expected_objects <- c(
  "localizacao", "Poltab", "AQItab",
  "meteo_hour", "Datafinal", "air_quality_data_ugm3",
  "purpleair", "df_instantaneo"
)

report <- lapply(expected_objects, function(obj) {
  present <- exists(obj, envir = .GlobalEnv)
  rows <- NA_integer_
  cols <- NA_integer_
  info <- NA_character_
  if (present) {
    val <- get(obj, envir = .GlobalEnv)
    if (is.data.frame(val) || inherits(val, "tbl") || inherits(val, "tibble")) {
      rows <- nrow(val)
      cols <- ncol(val)
      info <- paste0("data.frame/tibble (", rows, " x ", cols, ")")
    } else {
      info <- paste0(class(val)[1])
    }
  }
  tibble::tibble(
    object = obj,
    present = present,
    info = info,
    rows = rows,
    cols = cols
  )
}) %>% bind_rows()

# grava em _book para inspecionar após render
dir.create("_book", showWarnings = FALSE)
readr::write_csv(report, file = file.path("_book", "debug_data_status.csv"))

# também grava um TXT legível
txt <- paste0(
  "Data status report - ", Sys.time(), "\n\n",
  paste0(apply(report, 1, function(r) {
    paste0(r["object"], ": present=", r["present"], " | info=", r["info"])
  }), collapse = "\n")
)
writeLines(txt, con = file.path("_book", "debug_data_status.txt"))

# return
report
