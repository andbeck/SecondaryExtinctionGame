# global.R — packages and shared, non-reactive application setup
library(shiny)
library(shinyjs)
library(igraph)
library(cheddar)
library(png)

# Shared data and helper functions are loaded once when the app starts.
source("R/11_foodweb_data.R", local = FALSE)
source("R/12_foodweb_logic.R", local = FALSE)
source("R/13_foodweb_plot.R", local = FALSE)
source("R/14_mod_extinctions.R", local = FALSE)
