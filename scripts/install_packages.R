# Instala (ou verifica) pacotes R usados no relatório e no app Shiny
pkgs <- c(
  "tidyverse","lubridate","openair","openairmaps","reshape2","zoo","janitor",
  "ggplot2","ggh4x","patchwork","DT","leaflet","leaflegend","plotly",
  "corrplot","PerformanceAnalytics","data.table","jsonlite","shiny",
  "shinydashboard","shinythemes","shinycssloaders","htmlwidgets","RColorBrewer",
  "scales","ggtext","ggdist","MetBrewer","Hmisc"
)

repos <- getOption("repos")
if (is.null(repos) || repos["CRAN"] == "@CRAN@") options(repos = c(CRAN = "https://cloud.r-project.org"))

to_install <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(to_install) > 0) {
  install.packages(to_install, dependencies = TRUE)
} else {
  message("Todos os pacotes já instalados.")
}

# Small check
missing_now <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(missing_now) > 0) {
  stop("Falha na instalação de pacotes: ", paste(missing_now, collapse = ", "))
}
message("Pacotes instalados/verificados com sucesso.")

