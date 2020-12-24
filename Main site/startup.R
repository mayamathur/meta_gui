# install.packages("shinyWidgets")
# install.packages("dplyr")
# install.packages("EValue")

library(shiny)
library(plotly)
library(shinythemes)
library(shinyBS)
library(shinyalert)
library(bsplus)
library(shinydashboard)
library(shinyWidgets)


library(EValue) #include confounded_meta and sens_plot below to test, will eventually be loaded into EValue package and can remove the functions below
library(MetaUtility)
# try to fix deployment problem
library(purrr)
library(plogr)
library(dplyr)
library(boot)

# keeps original error messages
options(shiny.sanitize.errors = FALSE)


























