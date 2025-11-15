# scripts/load_cached_data.R
# Carrega todos os objetos serializados em data/processed/*.rds para o GlobalEnv
proc_dir <- file.path("data", "processed")
if (!dir.exists(proc_dir)) {
  message("Diretório de cache não existe: ", proc_dir, " — execute scripts/00-load_data.R primeiro ou adicione dados em data/processed/")
} else {
  files <- list.files(proc_dir, pattern = "\\.rds$", full.names = TRUE)
  for (f in files) {
    nm <- tools::file_path_sans_ext(basename(f))
    tryCatch({
      obj <- readRDS(f)
      assign(nm, obj, envir = .GlobalEnv)
      message("Carregado cache: ", nm, " (", nrow(obj) %||% "NA", " linhas)")
    }, error = function(e) {
      message("Falha ao ler RDS ", f, ": ", e$message)
    })
  }
}
