library(shiny)
library(tidyverse)
library(shinyWidgets)
library(shinydashboard)
library(leaflet)
library(sf)
library(highcharter)
library(lubridate)

library(shinyalert)
library(rhandsontable)
library(rintrojs)


capital <- read_csv("data/model/datacapital.csv")
state <- read_csv("data/model/dataestado.csv")
inland <- read_csv("data/model/datainterior.csv")

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

distancing <- read_csv("data/adst/distancing_series.csv") %>% 
  mutate(date = date %>% 
           str_replace("[[:space:]].*", "") %>% 
           dmy()) %>% select(-state) %>% 
  mutate_if(is.numeric, round, 2)

stringency <- read_csv("data/adst/stringency_series.csv") %>% 
  mutate(date = Date %>% 
           str_replace("[[:space:]].*", "") %>% 
           dmy()) %>% transmute(date, UF, indGeneral) %>% 
  mutate_if(is.numeric, round, 2)