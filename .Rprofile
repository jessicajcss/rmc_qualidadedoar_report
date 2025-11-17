source("renv/activate.R")
# .Rprofile do projeto - carrega automaticamente cache de dados para sessões R iniciadas no diretório do projeto

try({
  proj_loader <- file.path(getwd(), "scripts", "load_cached_data.R")
  if (file.exists(proj_loader)) {
    # não usar local = TRUE: queremos que os objetos sejam colocados no GlobalEnv
    try(source(proj_loader, local = FALSE), silent = TRUE)
  }
}, silent = TRUE)

# Também definimos opções gráficas compatíveis para ambientes headless
options(
  repr.plot.width = 7,
  repr.plot.height = 5
)

# Configure knitr default device if knitr estiver disponível
if (requireNamespace("knitr", quietly = TRUE)) {
  knitr::opts_chunk$set(
    dev = "png",
    dev.args = list(type = "cairo"),
    dpi = 150,
    message = FALSE,
    warning = FALSE
  )
}
