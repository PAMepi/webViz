library(shiny)
library(tidyverse)
library(shinyWidgets)
library(shinydashboard)
library(leaflet)
library(sf)
library(highcharter)

library(shinyalert)
library(rhandsontable)

capital <- read_csv("https://raw.githubusercontent.com/ModelingTaskForce/webViz/master/data/dataCapital.csv")
state <- read_csv("https://raw.githubusercontent.com/ModelingTaskForce/webViz/master/data/dataState.csv")
inland <- read_csv("https://raw.githubusercontent.com/ModelingTaskForce/webViz/master/data/dataInland.csv")

br_mapa <- read_sf("data/mapa_br/map.json")

