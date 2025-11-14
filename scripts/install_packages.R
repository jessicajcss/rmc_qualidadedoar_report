# Instalação automatizada
pkgs <- c(
  "tidyverse","lubridate","openair","openairmaps","reshape2","zoo","janitor",
  "ggplot2","ggh4x","patchwork","DT","leaflet","leaflegend","plotly",
  "corrplot","PerformanceAnalytics","data.table","jsonlite","shiny",
  "shinydashboard","shinycssloaders","htmlwidgets","RColorBrewer"
)
to_install <- pkgs[!pkgs %in% installed.packages()[,"Package"]]
if (length(to_install)) install.packages(to_install, dependencies=TRUE)
message("Pacotes instalados/verificados.")
