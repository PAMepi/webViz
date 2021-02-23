seiir_fits <- list.files(pattern = "seiir_fits", 
                         path = "seiir_bootstrap",
                         full.names = TRUE) %>% 
  map_df(~read_csv(.))

seiir_fits %>% 
  write_csv(file = "seiir_fits.csv")

info_tab <- seiir_fits %>% 
  select(state:delta) %>% 
  group_by(state, type) %>% 
  summarise(
    across(
      where(is.numeric),
      list(
        low = ~ quantile(.x, na.rm = TRUE, .25) %>% round(digits = 2),
        median = ~ median(.x, na.rm = TRUE) %>% round(digits = 2),
        high = ~ quantile(.x, na.rm = TRUE, .95) %>% round(digits = 2)
      )
    )
  )
p <- .2
gammaA <- 1/3.5
gammaS <- 1/4


info_tab %>% 
  transmute(
    type = case_when(
      type == "estado" ~ "State",
      type == "interior" ~ "Inland cities",
      TRUE ~ "Capital"
    ),
    beta0 = paste0(
      beta0_median, " (", beta0_low, " - ",  beta0_high, ")"
    ),
    beta1 = paste0(
      beta1_median, " (", beta1_low, " - ",  beta1_high, ")"
    ),
    beta2 = ifelse(
      is.na(beta2_median), "-",
      paste0(
        beta2_median, " (", beta2_low, " - ",  beta2_high, ")"
      )
    ),
    tcut0 = paste0(
      tcut0_median, " (", tcut0_low, " - ",  tcut0_high, ")"
    ),
    tcut1 = 
      ifelse(
        is.na(tcut1_median), "-",
        paste0(
          tcut1_median, " (", tcut1_low, " - ",  tcut1_high, ")"
        )
      ),
    delta = paste0(
      delta_median, " (", delta_low, " - ",  delta_high, ")"
    ),
    R0 = paste0(
      round(
        
        (beta0_median*p)/(gammaS) +
          (beta0_median*delta_median*(1-p))/(gammaA),
        
        digits = 2)
    )
  ) %>% 
  write_csv(file = "info_tab.csv")
