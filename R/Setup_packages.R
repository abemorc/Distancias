
# Instalacion librerias faltantes -----------------------------------------

oldw <- getOption("warn")
options(warn = -1)

packages <- c("ggplot2", "readxl", "dplyr", "knitr", "corrplot", "psych",
              "here", "bestglm", "visreg", "Hmisc", "sjPlot")

installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

library(here)
# Packages loading
#invisible(lapply(packages, library, character.only = TRUE))

options(warn = oldw)
