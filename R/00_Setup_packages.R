
# Instalacion librerias faltantes -----------------------------------------


oldw <- getOption("warn")
options(warn = -1)

packages <- c("readxl", "tidyverse", "knitr", "corrplot", "psych",
              "here", "bestglm", "visreg", "Hmisc", "sjPlot", "broom", "MuMIn",
              "PerformanceAnalytics", "pscl", "e1071", "naivebayes")

installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Carga librerias ---------------------------------------------------------


library(here)
library(MuMIn)
library(visreg)
library(tidyverse)

# Packages loading
#invisible(lapply(packages, library, character.only = TRUE))


options(warn = oldw)

#load(here(".RData"))
