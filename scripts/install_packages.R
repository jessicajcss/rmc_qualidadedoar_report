# Install required packages using pak for speed
if (!requireNamespace("pak", quietly = TRUE)) {
  install.packages("pak", repos = "https://r-lib.github.io/p/pak/dev/")
}
pkgs <- c(
  "tidyverse","lubridate","openair","openairmaps","reshape2","zoo","janitor",
  "ggplot2","ggh4x","patchwork","DT","leaflet","leaflegend","plotly",
  "corrplot","PerformanceAnalytics","data.table","jsonlite","shiny",
  "shinydashboard","shinythemes","shinycssloaders","htmlwidgets","RColorBrewer",
  "scales","ggtext","ggdist","MetBrewer","Hmisc"
)
# install with pak (uses RSPM/CRAN binaries when available)
pak::pkg_install(pkgs)
# verify
missing_now <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(missing_now) > 0) {
  stop("Failed to install: ", paste(missing_now, collapse = ", "))
}
message("Packages installed.")
