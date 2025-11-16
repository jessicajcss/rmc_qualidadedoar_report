# Instala apenas pacotes faltantes usando pak (usa RSPM binários quando disponíveis)
# R_LIBS_USER deve ser configurado no runner (env var)

# Lista de pacotes do seu projeto (ajuste se quiser reduzir)
pkgs <- c(
  "tidyverse","lubridate","openair","openairmaps","reshape2","zoo","janitor",
  "ggplot2","ggh4x","patchwork","DT","leaflet","leaflegend","plotly",
  "corrplot","PerformanceAnalytics","data.table","jsonlite","shiny",
  "shinydashboard","shinythemes","shinycssloaders","htmlwidgets","RColorBrewer",
  "scales","ggtext","ggdist","MetBrewer","Hmisc"
)

# Ensure pak is available
if (!requireNamespace("pak", quietly = TRUE)) {
  install.packages("pak", repos = "https://r-lib.github.io/p/pak/dev/")
}

# Compute which packages are missing
installed <- installed.packages(lib.loc = .libPaths())[ , "Package"]
to_install <- setdiff(pkgs, installed)

if (length(to_install) == 0) {
  message("Todos os pacotes já instalados.")
} else {
  message("Instalando pacotes faltantes: ", paste(to_install, collapse = ", "))
  # pak::pkg_install irá preferir binários via RSPM (quando available)
  pak::pkg_install(to_install)
}

# Verify
not_installed <- setdiff(pkgs, installed.packages(lib.loc = .libPaths())[ , "Package"])
if (length(not_installed) > 0) {
  stop("Falha na instalação de pacotes: ", paste(not_installed, collapse = ", "))
}
message("Pacotes instalados/verificados com sucesso.")
