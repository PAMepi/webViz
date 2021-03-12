shinyServer(function(input, output, session) {
  
  output$brasil_map <- renderLeaflet({
    
    bins <- quantile(br_mapa$inc_state, 
                     probs = c(seq(0, 100, by = 25))/100)
    #bins <- seq(-80, 80, by = 20)
    
    pal <- colorBin("YlOrRd", domain = br_mapa$inc_state, bins = bins)
    
    labels <- paste0(
      "Incidence index in ", 
      "<strong>",br_mapa$name,"</strong> :","<br/>",
      "<strong>", round(br_mapa$inc_state, digits = 2), "</strong>") %>%
      lapply(htmltools::HTML)
    
    leaflet(
      data = br_mapa,
      options = leafletOptions(
        zoomControl=FALSE, doubleClickZoom =FALSE, bounceAtZoomLimits = FALSE,
        dragging = FALSE, scrollWheelZoom = FALSE, closePopupOnClick = FALSE,
        minZoom = 4, maxZoom = 4
      )
    ) %>% 
      addTiles(options = providerTileOptions(opacity = 0.5)) %>% 
      setView(lng=-55.761,lat=-14.446,zoom=4) %>% 
      addPolygons(color = "#718075", layerId = ~sigla, label = ~name,
                  
                  fillColor = ~pal(inc_state),
                  popup = labels,
                  dashArray = "3",
                  popupOptions = popupOptions(autoClose = TRUE, closeOnClick = TRUE ,
                                              closeButton = FALSE),
                  
                  opacity = 1.0, fillOpacity = 0.9, weight = 1,
                  highlightOptions = highlightOptions(color = "#FFEE58", weight = 3,
                                                      dashArray = 2,
                                                      bringToFront = FALSE)) %>% 
      addLegend(pal = pal, values = ~density, opacity = 1,
                labFormat = labelFormat(digits = 0, between = "  &ndash;  "), 
                position = "bottomleft", 
                title = "Incidence per 100k in<br>the study period")
    
    
  })
  
  shinyalert(
    title = "Warning",
    text = "
    This webpage is an on-line ancillary material to the work 
    <a href='https://www.medrxiv.org/content/10.1101/2020.06.26.20140780v1.article-info' target='_blank'>
    Assessing the nation wide impact of COVID-19
    mitigation policies on the transmission rate of SARS-CoV-2
    in Brazil</a>.
    Data shown here were collected during the period 02/25 - 05/22, 2020, and is not intended 
    to be continuously updated.<br>
    <b>If you wish to learn more about every functionality of this platform click on the following tour button.</b>",
    size = "m", 
    inputId = "shinyalerttour",
    closeOnEsc = TRUE,
    closeOnClickOutside = TRUE,
    html = TRUE,
    type = "info",
    showConfirmButton = TRUE,
    showCancelButton = FALSE,
    confirmButtonText = "OK",
    confirmButtonCol = "#AEDBF5",
    timer = 0,
    imageUrl = "",
    animation = FALSE
  )
  
  observeEvent(input$geral_tour,
               introjs(session, options = list("nextLabel"="Next",
                                               "prevLabel"="Previos",
                                               "skipLabel"="Skip tour"),
                       events = list("oncomplete"=I('alert("You are ready to use this plataform")')))
  )
  
  
  state_proxy <- reactive(
    {
      click <- input$brasil_map_shape_click
      
      if(is.null(click) 
      )
        return(
          "TOTAL"
        )
      else
        leafletProxy("brasil_mapa");click
    }
  ) 
  
  data_select <- reactive({
    
    df <- switch(
      input$model_loc,
      "state" = state,
      "capital" = capital,
      "inland" = inland
    )
    
    
  })
  
  data_beta <- reactive({
    beta <- switch(
      input$model_loc,
      "state" = seiir_fits %>% 
        filter(type %in% "estado"),
      "capital" = seiir_fits %>% 
        filter(type %in% "capital"),
      "inland" = seiir_fits %>% 
        filter(type %in% "interior")
    )
  })
  
  data_rt <- reactive({
    rt <- switch(
      input$model_loc,
      "state" = est_rts,
      "capital" = cap_rts,
      "inland" = int_rts
    )
  })
  
  output$trans_rate_output <- renderHighchart({
    
    validate(
      need(state_proxy()[1] != "TOTAL", "Please click on a state")
    )
    
    df <- data_select() %>%
      filter(state %in% state_proxy()[1])
    df <- if(input$d_c){
      df
    } else{
      df %>% 
        mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
        mutate_if(is.numeric, cumsum)
    }
    
    df <- if(input$lin_log){
      df %>% 
        mutate_if(is.numeric, log, 10) %>% 
        mutate_if(is.numeric, list(~ na_if(., -Inf)))
    } else {
      df
    }
    
    axis_text <- if(input$d_c){
      if(input$lin_log){
        "Log(Daily cases)"
      } else {
        "Daily cases"
      } 
    } else {
      if(input$lin_log){
        "Log(Cummulative cases)"
      } else "Cummulative cases"
    }
    
    
    beta_df <- data_beta() %>% 
      filter(state %in% state_proxy()[1])
    
    date_cut <- seq(ymd('2020-01-01'),ymd('2020-05-22'), by = '1 day')
    
    rt_df <- data_rt() %>% 
      filter(state %in% state_proxy()[1])
    
    df_lm_res <- lm(Infec ~ cases, data = df)
    RMSE <- sqrt(mean(df_lm_res$residuals^2)) %>% round(digits = 2)
    
    k <- 50
    t <- seq(1, 200, 1)
    t1 <- beta_df$tcut0 %>% median()
    t2 <- beta_df$tcut1 %>% median(na.rm = TRUE)
    b <- beta_df$beta0 %>% median() 
    b1 <- beta_df$beta1 %>% median()
    b2 <- beta_df$beta2 %>% median(na.rm = TRUE)
    
    H <- function(t){
      h <- 1.0/(1.0+ exp(-2.0*k*t))
    }
    
    beta <- function(t,t1,t2,b,b1,b2){
      
      if(is.na(beta_df$beta2[1])){
        # B_1 & B_0
        beta <- b*H(t1-t) + b1*H(t-t1) 
      } else{
        # B_1 & B_0 & B_2
        beta <- b*H(t1-t) + b1*H(t2-t)*H(t-t1) + b2*H(t-t2)
      }
      
      return(
        beta
      )
    }
    
    beta_vec <- beta(t,t1,t2,b,b1,b2)
    aux_df <- tibble(
      date = seq(
        df$date[1], max(date_cut), "1 day"
      )
    )
    
    plot_beta <- if(length(unique(beta_vec)) == 2){
      aux_df %>% 
        mutate(
          beta = case_when(
            date < date_cut[round(t1)]                              ~ unique(beta_vec)[1],
            TRUE                                                    ~ unique(beta_vec)[2]
          )
        )
    } else if(length(unique(beta_vec)) == 3 & !is.na(beta_df$beta2[1])){
      aux_df %>% 
        mutate(
          beta = case_when(
            date < date_cut[round(t1)]                              ~ unique(beta_vec)[1],
            date >= date_cut[round(t1)] & date < date_cut[round(t2)]~ unique(beta_vec)[2],
            TRUE                                                    ~ unique(beta_vec)[3]
          )
        )
    } else if(length(unique(beta_vec)) == 4 & !is.na(beta_df$beta2[1])){
      aux_df %>% 
        mutate(
          beta = case_when(
            date < date_cut[round(t1)]                              ~ unique(beta_vec)[1],
            date == date_cut[round(t1)]                             ~ unique(beta_vec)[2],
            date >= date_cut[round(t1)] & date < date_cut[round(t2)]~ unique(beta_vec)[3],
            TRUE                                                    ~ unique(beta_vec)[4]
          )
        )
    } else if(is.na(beta_df$beta2[1])){
      aux_df %>% 
        mutate(
          beta = case_when(
            date < date_cut[round(t1)]                              ~ unique(beta_vec)[1],
            date == date_cut[round(t1)]                             ~ unique(beta_vec)[2],
            TRUE                                                    ~ unique(beta_vec)[3]
          )
        )
    } else {
      aux_df %>% 
        mutate(
          beta = case_when(
            date < date_cut[round(t1)]                              ~ unique(beta_vec)[1],
            date == date_cut[round(t1)]                             ~ unique(beta_vec)[2],
            date > date_cut[round(t1)] & date < date_cut[round(t2)] ~ unique(beta_vec)[3],
            date == date_cut[round(t2)]                             ~ unique(beta_vec)[4],
            TRUE                                                    ~ unique(beta_vec)[5]
          )
        )
    }
    
    dist_df <- distancing %>% 
      filter(name %in% state_proxy()[1])
    str_df <- stringency %>% 
      filter(UF %in% state_proxy()[1])
    
    switch(
      input$model_or_rt,
      "str" = # str ----
        highchart() %>% 
        hc_title(text = paste0("Social mobility reduction index in: ",
                               "<b>",
                               state_proxy()[1],
                               "</b>"),
                 margin = 20, align = "left",
                 style = list(color = "#05091A", useHTML = TRUE, fontSize = "15px")) %>% 
        hc_xAxis(type = "datetime", dateTimeLabelFormats = list(day = '%d %b')) %>% 
        hc_yAxis_multiples(
          list(
            title = list(text = "Stringency index"), opposite = TRUE,
            min = 0, max = 100
          ),
          list(
            title = list(text = "SMRI (%)",color = "#7E2F8E"),
            showLastLabel = TRUE, opposite = FALSE, 
            color = "#7E2F8E",
            min = 25, max = 100
          )
        ) %>%
        hc_add_series(data = dist_df, 
                      hcaes(x = date, y = indUF),
                      type = "line", name = "State", 
                      yAxis = 1, color = "#0072BD"
        ) %>% 
        hc_add_series(data = dist_df,
                      hcaes(x = date, y = indCAP),
                      type = "line", name = "Capital", 
                      yAxis = 1, color = "#D95319"
        ) %>% 
        hc_add_series(data = dist_df,
                      hcaes(x = date, y = indInland),
                      type = "line", name = "Inland", 
                      yAxis = 1, color = "#EDB120"
        ) %>% 
        hc_add_series(data = str_df,
                      hcaes(x = date, y = indGeneral),
                      type = "line", name = "Stringency index",
                      color = "#7E2F8E"
        ) %>% 
        hc_tooltip(crosshairs = TRUE),
      "model" = # model ----
        highchart() %>% 
        hc_add_series(data = df,
                      hcaes(x = date, y = Infec_1),
                      type = "line", dashStyle = "Dash",
                      name = "Uncontrolled epidemic", color = "#0EA8E6",
                      visible = FALSE
        ) %>% 
        hc_add_series(
          data = df %>% 
            filter(!is.na(cases)), hcaes(x = date, low = Infec_lb,
                            high = Infec_ub ),
          showInLegend = FALSE,  enableMouseTracking = FALSE, 
          type = "arearange",color = hex_to_rgba("#0EA8E6", 0.5),
          linkedTo = "mod",
          fillOpacity = 0.2
        ) %>% 
        hc_add_series(data = df,
                      hcaes(x = date, y = cases), type = "scatter",
                      name = "Observed", color = "#000000"
        ) %>% 
        hc_add_series(data = df %>% 
                        filter(!is.na(cases)),
                      hcaes(x = date, y = Infec), type = "line",
                      
                      name = "Fitted", color = "#0EA8E6"
        ) %>% 
        hc_add_series(data = df %>% 
                        filter(date >= ymd('2020-05-22')),
                      hcaes(x = date, y = Infec), type = "line",
                      name = "Forecast (15 days ahead)", color = "#D93E30"
                      ) %>% 
        hc_add_series(
          data = df %>% 
            filter(date >= ymd('2020-05-22')), 
          hcaes(x = date, low = Infec_lb,
                                         high = Infec_ub ),
          showInLegend = FALSE,  enableMouseTracking = FALSE, 
          type = "arearange",color = hex_to_rgba("#D93E30", 0.5),
          linkedTo = "mod",
          fillOpacity = 0.2
        ) %>% 
        hc_xAxis(type = "datetime", dateTimeLabelFormats = list(day = '%d of %b'),
                 plotLines = vertical_lines(date_cut, t1, t2)
        ) %>% 
        hc_yAxis(title = list(text = paste0(axis_text)),
                 min = 0) %>% 
        hc_title(text = paste0(axis_text, " ", paste0(input$model_loc), ": ",
                               "<b>",
                               state_proxy()[1],
                               "</b>"),
                 margin = 20, align = "left",
                 style = list(color = "#05091A", useHTML = TRUE, fontSize = "15px")) %>% 
        hc_plotOptions(line = list(marker = list(enabled = FALSE)),
                       arearange = list(marker = list(enabled = FALSE)),
                       scatter = list(marker = list(symbol = "circle", radius = 3))) %>% 
        hc_tooltip(
          formatter = JS(
            paste0("function(){
                return (
                this.series.name + ': ' + this.y +
                ' <br> Data: ' + Highcharts.dateFormat('%e/%b/%y',
                new Date(this.x)
                )", ")}")),
          style = list(fontSize = '13px')
        ) %>% 
        hc_exporting(
          enabled = TRUE#,
          #buttons = list(
          #  customButton = list(text = 'Linear',
          #                      onclick = JS("function() {this.yAxis[0].update({type: 'linear'});}")
          #  ),
          #  customButton2 = list(text = 'Logarithmic',
          #                       onclick = JS("function() {this.yAxis[0].update({type: 'logarithmic'});}")
          #  )
          #)
        ),
      "beta_series" = # beta ----
        highchart() %>%
        hc_title(text = paste0("<b>β</b> ", paste0(input$model_loc), ": ",
                               "<b>",
                               state_proxy()[1],
                               "</b>"),
                 margin = 20, align = "left",
                 style = list(color = "#05091A", useHTML = TRUE, fontSize = "15px")) %>%
        hc_add_series(data = plot_beta, 
                      hcaes(x = date, y = round(beta, digits = 2)),
                      type = "line", name = "Beta", 
                      color = "#0072BD"
        ) %>% 
        hc_xAxis(type = "datetime", dateTimeLabelFormats = list(day = '%d %b')) %>% 
        hc_yAxis(title = list(text = "<b>β</b>")
        ) %>% 
        hc_plotOptions(line = list(marker = list(enabled = FALSE)),
                       arearange = list(marker = list(enabled = FALSE)),
                       scatter = list(marker = list(symbol = "circle", radius = 3))
        ),
      "model_qual" = # good fit ----
        highchart() %>%
        hc_title(text = paste0("Goodness of fit ", paste0(input$model_loc), ": ",
                               "<b>",
                               state_proxy()[1],
                               "</b>"),
                 margin = 20, align = "left",
                 style = list(color = "#05091A", useHTML = TRUE, fontSize = "15px")) %>% 
        hc_xAxis(title = list(text = "Observed"), min = 0, 
                 max = max(c(df$cases,df$Infec))) %>% 
        hc_yAxis(title = list(text = "Fit"), min = 0, 
                 max = max(c(df$cases,df$Infec))) %>% 
        hc_add_series(showInLegend = FALSE,
                      color = "#A9A9A9", dashStyle = 'ShortDot',
                      data = list(list(0, 0), 
                                  list(max(c(df$cases,df$Infec)),
                                       max(c(df$cases,df$Infec)))),
                      enableMouseTracking = FALSE) %>% 
        hc_plotOptions(line = list(color = "#4471EB",
                                   marker = list(enabled = FALSE)),
                       scatter = list(color = "black")) %>% 
        hc_add_series(data = df, hcaes(x = cases, y = round(Infec)),
                      tooltip = list(pointFormat = paste0(
                        "<b>Fit<b>: {point.y}<br><b>RMSE<b>: ", RMSE, "<br>"
                      ),
                      headerFormat = "<b>Observed<b>: {point.x}<br>"),
                      type = "scatter", showInLegend = FALSE),
      "rt" = # rt ----
        highchart() %>% 
        #hc_add_series(
        #  data = rt_df , hcaes(x = date, low = Rtmod_lb,
        #                       high = Rtmod_ub),
        #  showInLegend = FALSE,  enableMouseTracking = FALSE, 
        #  type = "arearange",color = hex_to_rgba("#F59B67", 0.5),
        #  linkedTo = "mod",
        #  fillOpacity = 0.3
        #) %>% 
        hc_add_series(data = rt_df ,
                      hcaes(x = date, y = round(Rtdata, 3)), type = "point",
                      name = "Observed", color = "black"
        ) %>%
        hc_add_series(data = rt_df ,
                      hcaes(x = date, y = round(Rtmod, 3)), type = "line",
                      name = "Smoothed", color = "#17a2b8", id = "mod",
                      dashStyle = "LongDash"
        ) %>% 
        hc_plotOptions(line = list(marker = list(enabled = FALSE)),
                       arearange = list(marker = list(enabled = FALSE))
        ) %>% 
        hc_yAxis(plotLines = list(list(color = "#FA3B42", value = 1, 
                                       width = 1.5, dashStyle = "ShortDash")),
                 min = 0, title = list(text = "Reproduction effective number")
        ) %>%
        #hc_add_series(rt_df, type = "scatter", hcaes(x = date, y = reproductionNumber,
        #                                               group = infec#, group = infec
        #),
        #color = c("#36B36D", "#B33024"),
        #name = c("Rt < 1", "Rt >= 1")) %>%
        hc_xAxis(type = "datetime", dateTimeLabelFormats = list(day = '%d %b')) %>% 
        hc_title(text = paste0("Reproduction effective number (Rt) ", paste0(input$model_loc), ": ",
                               "<b>",
                               state_proxy()[1],
                               "</b>"),
                 margin = 20, align = "left",
                 style = list(color = "#05091A", useHTML = TRUE, fontSize = "15px"))
      #hc_tooltip(formatter = JS("function(){
      #                                        return (
      #                                                'Rt : ' + this.y +
      #                                        ' <br> Data: ' + Highcharts.dateFormat('%e. %b', new Date(this.x))
      #                                                )
      #                        }")) %>% 
      #hc_exporting(enabled = TRUE)
    )
    
    
    
  })
  
  observeEvent(input$tab_tour, {
    shinyalert("About the confidence intervals", 
               "The  confidence intervals lies within the boundaries of the 25% and 95% percentile of each parameter", 
               type = "info")
  })
  
  output$selec_state <- renderUI({
    validate(
      need(state_proxy()[1] != "TOTAL", "Please click on a state")
    )
    HTML(
      paste0(
        "<h1>", state_proxy()[1], "</h1>"
      )
    )
  })
  output$info_tab <- renderRHandsontable({
    
    validate(
      need(state_proxy()[1] != "TOTAL", "Please click on a state")
    )
    df <- info_tab %>% 
      filter(state %in% state_proxy()[1]) %>% 
      mutate(
        type = factor(type,
                      levels = c("State", "Capital", "Inland cities"))
      ) %>% 
      arrange(type) %>% 
      rename(
        " "=type,
        "&beta;&#8320;"=beta0,
        "&beta;&#8321;"=beta1,
        "&beta;&#8322;"=beta2,
        "t&#8320;"=tcut0,
        "t&#8321;"=tcut1,
        "&delta;"=delta,
        "R&#8320;"=R0,
        "Date of first case" = date_first_case,
        "Date of community transmission" = date_com_trans
      ) %>% 
      select(-state) %>% t() 
    
    rhandsontable(
      df,
      rowHeaderWidth = 215, readOnly = TRUE,
      width = 1200, height = 400
    ) %>% 
      hot_cols(colWidths = 200)
    
  })
  
  
  
})