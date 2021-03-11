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


capital <- read_csv("data/model/datacapital.csv") %>% 
  mutate(
    # Fix annotation error
    cases = case_when(
      cases < 0 ~ round((lead(cases) + lag(cases)) / 2),
      TRUE      ~ cases
    )
  )
state <- read_csv("data/model/dataestado.csv") %>% 
  mutate(
    cases = case_when(
      cases < 0 ~ round((lead(cases) + lag(cases)) / 2),
      TRUE      ~ cases
    )
  )
inland <- read_csv("data/model/datainterior.csv") %>% 
  mutate(
    cases = case_when(
      cases < 0 ~ round((lead(cases) + lag(cases)) / 2),
      TRUE      ~ cases
    )
  )

br_mapa <- read_sf("data/mapa_br/map.json") %>% 
  inner_join(
    state %>% 
      group_by(state) %>% 
      filter(!is.na(cases)) %>% 
      top_n(date, n = 1) %>% 
      mutate(inc_state = (cases * 1e5)/population) %>% 
      select(state, inc_state),
    by = c("sigla" = "state")
  )# %>% 
  #left_join(
  #  capital %>% 
  #    group_by(state) %>% 
  #    top_n(date, n = 1) %>% 
  #    mutate(inc_capital = (cases * 1e5)/population) %>% 
  #    select(state, inc_capital),
  #  by = c("sigla" = "state")
  #) %>% 
  #left_join(
  #  inland %>% 
  #    group_by(state) %>% 
  #    top_n(date, n = 1) %>% 
  #    mutate(inc_inland = (cases * 1e5)/population) %>% 
  #    select(state, inc_inland),
  #  by = c("sigla" = "state")
  #)

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



vertical_lines <- function(date_cut, t1, t2){
  
  pl_lines <- if(is.na(t2)){
    
    list(
      list(color = "#FA3B42", 
           value = JS(
             paste0(
               "Date.UTC(2020,",
               month(date_cut[round(t1)]) - 1,
               ",",
               day(date_cut[round(t1)]),
               ")"
             )
           ), 
           width = 1.5, dashStyle = "ShortDash")
    )
    
  } else {
    list(
      list(color = "#FA3B42", 
           value = JS(
             paste0(
               "Date.UTC(2020,",
               month(date_cut[round(t1)]) - 1,
               ",",
               day(date_cut[round(t1)]),
               ")"
             )
           ), 
           width = 1.5, dashStyle = "ShortDash"),
      list(
        color = "#FA3B42", 
        value = JS(
          paste0(
            "Date.UTC(2020,",
            month(date_cut[round(t2)]) - 1,
            ",",
            day(date_cut[round(t2)]),
            ")"
          )
        ),
        width = 1.5, dashStyle = "ShortDash"
      )
    )
  }
  
  return(pl_lines)
  
}