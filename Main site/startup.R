
library(shiny)
library(shinythemes)
library(shinyBS)
library(shinyalert)
library(bsplus)
library(shinydashboard)
library(shinyWidgets)


library(EValue) #include confounded_meta and sens_plot below to test, will eventually be loaded into EValue package and can remove the functions below

# utils.R
# EValue.R
# meta-analysis.R
# effect_measures.R


library(MetaUtility)
library(purrr)
library(plogr)
library(dplyr)
library(boot)

# keeps original error messages
options(shiny.sanitize.errors = FALSE)

