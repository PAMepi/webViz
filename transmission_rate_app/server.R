shinyServer(function(input, output, session) {
  
  output$brasil_map <- renderLeaflet({
    
    bins <- quantile(br_mapa$coefVar, 
                     probs = c(seq(0, 100, by = 30), 100)/100)
    #bins <- seq(-80, 80, by = 20)
    
    pal <- colorBin("YlOrRd", domain = br_mapa$coefVar, bins = bins)
    
    labels <- paste0(
      "<strong>",br_mapa$name,"</strong><br/>",
      "Variation on transmission rate <strong>", br_mapa$coefVar,"</strong>") %>%
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
                  
                  fillColor = ~pal(coefVar),
                  popup = labels,
                  dashArray = "3",
                  popupOptions = popupOptions(autoClose = TRUE, closeOnClick = TRUE ,
                                              closeButton = FALSE),
                  
                  opacity = 1.0, fillOpacity = 0.9, weight = 1,
                  highlightOptions = highlightOptions(color = "#FFEE58", weight = 3,
                                                      dashArray = 2,
                                                      bringToFront = FALSE)) %>% 
      addLegend(pal = pal, values = ~density, opacity = 1,
                position = "bottomleft", title = "Variation on transmission rate")
    
    
  })
  
  shinyalert(
    title = "Warning",
    text = "This is an app developed to assist in the
    understanding of the paper: 
    <a href='https://www.medrxiv.org/content/10.1101/2020.06.26.20140780v1.article-info' target='_blank'>
    Assessing the nation wide impact of COVID-19
    mitigation policies on the transmission rate of SARS-CoV-2
    in Brazil</a> and the last input date is from last year. User discretion is advised.",
    size = "m", 
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
  
  output$trans_rate_output <- renderHighchart({
    
    validate(
      need(state_proxy()[1] != "TOTAL", "Please click on a state")
    )
    
    df <- data_select() %>%
      filter(state %in% state_proxy()[1])
    
    switch(
      input$model_or_rt,
      "model" = 
        highchart() %>% 
        hc_add_series(data = df,
                      hcaes(x = date, y = cases), type = "scatter",
                      name = "Observed", color = "#0EA8E6"
        ) %>% 
        hc_add_series(data = df,
                      hcaes(x = date, y = round(Infec)), type = "line",
                      
                      name = "Fitted", color = "#FA4921"
        ) %>% 
        hc_xAxis(type = "datetime", dateTimeLabelFormats = list(day = '%d of %b')) %>% 
        hc_yAxis(title = list(text = "Cummulative cases")) %>% 
        hc_title(text = paste0("Cummulative cases ", paste0(input$model_loc), ": ",
                               "<b>",
                               state_proxy()[1],
                               "</b>"),
                 margin = 20, align = "left",
                 style = list(color = "#05091A", useHTML = TRUE, fontSize = "15px")) %>% 
        hc_plotOptions(line = list(marker = list(enabled = FALSE)),
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
          enabled = TRUE,
          buttons = list(
            customButton = list(text = 'Linear',
                                onclick = JS("function() {this.yAxis[0].update({type: 'linear'});}")
            ),
            customButton2 = list(text = 'Logarithmic',
                                  onclick = JS("function() {this.yAxis[0].update({type: 'logarithmic'});}")
            )
          )
        ),
      "rt" = 
        highchart() %>% 
        hc_add_series(data = df,
                      hcaes(x = date, y = round(Rtdata, 3)), type = "line",
                      name = "Observed", color = "black"
        ) %>%
        hc_add_series(data = df,
                      hcaes(x = date, y = round(Rtmod, 3)), type = "line",
                      name = "Smoothed", color = "blue",
                      dashStyle = "LongDash"
        ) %>% 
        hc_plotOptions(line = list(marker = list(enabled = FALSE))) %>% 
        hc_yAxis(plotLines = list(list(color = "#FA3B42", value = 1, 
                                       width = 1.5, dashStyle = "ShortDash")),
                 min = min(df$Rtdata), title = list(text = "Reproduction effective number")
        ) %>% 
        #hc_add_series(
        #  data = df, hcaes(x = date, low = reproductionNumberLow,
        #                        high = reproductionNumberHigh),
        #  showInLegend = FALSE,  enableMouseTracking = FALSE, 
        #  type = "errorbar",color = "black",
        #  stemWidth = 1.5,  whiskerLength = 5
        #) %>% 
        #hc_add_series(TsRt_df, type = "scatter", hcaes(x = date, y = reproductionNumber,
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
  
  
  output$tab_int <- renderRHandsontable({
    
    my_df <- tibble(
      Parameter = c(
        "beta_0", "beta_1", "beta_2",
        "t_1", "t_2", "δ", "p", "k", "γ_a",
        "γ_s", "h", "1 - ξ", "γ_H", "γ_U",
        "μ_H", "μ_U", "ω_H", "ω_U"
      ),
      Description = c(
        "Pre-intervention transmission rate",
        "Post-intervention transmission rate",
        "Post-intervention transmission rate",
        "Time of transmission rate change",
        "Time of transmission rate change",
        "Asymptomatic/non-detected infectivity factor",
        "Proportion of latent (E) that proceed
        to symptomatic infective",
        "Mean exposed period",
        "Mean asymptomatic period",
        "Mean symptomatic period",
        "Proportion of symptomatic needing
        hospitalization or ICU",
        "Proportion of symptomatic that proceed to ICU",
        "Mean hospitalization (clinical beds) period",
        "Mean ICU period",
        "Death rate of hospitalized individuals",
        "Death rate of ICU individuals",
        "Proportion od hospitalized that goes to ICU",
        "Proportion of ICu that goes to hospítalization"
      ),
      Interval = c(
        "[0-2]", "[0-2]", "[0-2]",
        "[March 15th, April 15th]",
        "[April 15th, September 13th]",
        "[0, 0.75]", "[0.13, 0.5]",
        "[1/6, 1/3]", "[1/3.70, 1/3.24]",
        "[1/5, 1/3]", "[0.05, 0.25]",
        "[0.01, 0.5]", "[1/12, 1/4]",
        "[1/12, 3]", "[0.1, 0.2]",
        "[0.4, 0.5]", "[0.1, 0.3]",
        "[0.1, 0.3]"
      ),
      Fixed = c(
        "-", "-", "-", "-", "-", "-",
        "0.2", "1/4", "1/3.5", "1/4",
        "-", "0.47", "-", "-", "0.15",
        "0.4", "0.14", "0.29"
      ),
      Estimated = c(
        "1.40 (1.37 - 1.43)",
        "0.96 (0.94 - 0.98)",
        "0.66 (0.65 - 0.68)",
        "April 3nd", "June 11th",
        "0.31 (0.30 - 0.32)",
        "-", "-", "-", "-",
        "0.06 (0.06, 0.064)",
        "-", "0.18 (0.17 - 0.18)",
        "0.13 (0.13, 0.14)",
        "-","-","-","-"
      )
    )
    rhandsontable(my_df,
                  rowHeaders = NULL,readOnly = TRUE,
                  width = 700, height = 600)
  })
  
  
})