library(shiny)
library(tidyverse)
library(shinyWidgets)
library(shinydashboard)
library(leaflet)
library(sf)
library(highcharter)

library(shinyalert)
library(rhandsontable)
library(rintrojs)

capital <- read_csv("https://raw.githubusercontent.com/ModelingTaskForce/webViz/master/data/dataCapital.csv")
state <- read_csv("https://raw.githubusercontent.com/ModelingTaskForce/webViz/master/data/dataState.csv")
inland <- read_csv("https://raw.githubusercontent.com/ModelingTaskForce/webViz/master/data/dataInland.csv")

last_cases <- state %>% 
  group_by(state) %>% 
  top_n(date, n = 1) %>% 
  select(state, cases)

br_mapa <- read_sf("data/mapa_br/map.json") %>% 
  inner_join(
    last_cases,
    by = c("sigla" = "state")
  )

info_tab <- read_csv("data/info_tab.csv")
seiir_fits <- read_csv("data/seiir_fits.csv",
                       col_types = list(
                         col_double(), col_character(), col_character(), 
                         col_double(),col_double(),col_double(),col_double(),
                         col_double(),col_double(),col_double(),col_double(),
                         col_double()
                       )
)

cap_rts <- read_csv("data/rts/Rtcapital.csv")
est_rts <- read_csv("data/rts/Rtestado.csv")
int_rts <- read_csv("data/rts/Rtinterior.csv")