# Instala (ou verifica) pacotes R usados no relatório e no app Shiny (usa pak para maior velocidade)
if (!requireNamespace("pak", quietly = TRUE)) {
  install.packages("pak", repos = "https://r-lib.github.io/p/pak/dev/") # fallback
}
pkgs <- c(
  "tidyverse","lubridate","openair","openairmaps","reshape2","zoo","janitor",
  "ggplot2","ggh4x","patchwork","DT","leaflet","leaflegend","plotly",
  "corrplot","PerformanceAnalytics","data.table","jsonlite","shiny",
  "shinydashboard","shinythemes","shinycssloaders","htmlwidgets","RColorBrewer",
  "scales","ggtext","ggdist","MetBrewer","Hmisc"
)

# Use pak for fast installs and resolution; 'pak::pkg_install' will use RSPM / binaries when available.
pkg_install_safe <- function(pkgs) {
  tryCatch({
    pak::pkg_install(pkgs)
  }, error = function(e) {
    message("pak installation failed - falling back to install.packages(): ", e$message)
    install.packages(pkgs, dependencies = TRUE)
  })
}

pkg_install_safe(pkgs)

# small verification
missing_now <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(missing_now) > 0) {
  stop("Falha na instalação de pacotes: ", paste(missing_now, collapse = ", "))
}
message("Pacotes instalados/verificados com sucesso.")
