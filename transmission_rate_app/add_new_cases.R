library(tidyverse)
library(lubridate)

covid_cases <- read_csv("caso_full.csv.gz") %>% 
  filter(date <= ymd("2020-06-06"))


state <- covid_cases %>%
  filter(place_type == "state") %>% 
  select(date, state, place_type, new_confirmed)

cap <- c("2800308", "1501402", "3106200", "1400100",
         "5300108", "5002704", "5103403", "4106902",
         "4205407", "2304400", "5208707", "2507507",
         "1600303", "2704302", "1302603", "2408102",
         "1721000", "4314902", "1100205", "2611606",
         "1200401", "3304557", "2927408", "2111300",
         "3550308", "2211001", "3205309"
)

capitais <- covid_cases %>% 
  filter(as.character(city_ibge_code) %in% cap) %>% 
  select(date, state, place_type, new_confirmed)


interior <- state %>% 
  left_join(
    capitais %>% rename(cases_cap = new_confirmed) %>% 
      select(-place_type)
  ) %>% 
  drop_na() %>%
  mutate(casos_inland = new_confirmed - cases_cap)



state_certo <- read_csv("transmission_rate_app/data/model/dataestado.csv")
inland_certo <- read_csv("transmission_rate_app/data/model/datainterior.csv") 
capital_certo <- read_csv("transmission_rate_app/data/model/datacapital.csv") 



state_certo %>% 
  left_join(state) %>%
  transmute(date, state, cases = new_confirmed,
            population, Infec, Infec_ub, Infec_lb,
            Infec_1, Infec_2) %>% 
  write_csv("dataestado.csv")

inland_certo %>% 
  left_join(interior %>% select(date, state, casos_inland)) %>% 
  transmute(date, state, cases = casos_inland,
            population, Infec, Infec_ub, Infec_lb,
            Infec_1, Infec_2) %>% 
  write_csv("datainterior.csv")

capital_certo %>% 
  left_join(capitais) %>% 
  transmute(date, state, cases = new_confirmed,
            population, Infec, Infec_ub, Infec_lb,
            Infec_1, Infec_2) %>% 
  write_csv("datacapital.csv")
