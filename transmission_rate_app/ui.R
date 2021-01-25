library(shiny)


shinyUI(shiny::bootstrapPage(
  
  useShinydashboard(),
  useShinyalert(),
  fluidPage(
    column(width = 5,
           box(leaflet::leafletOutput("brasil_map",
                                      height = "500px"),width = 12)
    ),
    box(
      width = 7,
      tabsetPanel(
      tabPanel(
        "Visualizations",
        column(width = 12,
               fluidRow(
                 splitLayout(
                   div(br(), pickerInput("model_loc", "", 
                                         width = "70%",
                                         choices = c(
                                           "State" = "state",
                                           "Capital" = "capital",
                                           "Inland cities" = "inland"
                                         )
                   ), style = "height:150px;"
                   ),
                   div(br(), pickerInput("model_or_rt", "",
                                         width = "70%",
                                         choices = c(
                                           "R(t)" = "rt",
                                           "Model" = "model"
                                         ), selected = "model"
                   ), style = "height:150px;"
                   )
                 )
               ),
               highchartOutput("trans_rate_output", height = "400px")
        )
      ),
      tabPanel(
        "Tables",
        rHandsontableOutput("tab_int", 100, 120)
      )
    )
    )
  ),
  tags$style(type = "text/css", 
             HTML('img {
                      vertical-align: middle;
                      height: auto;
                        }')
  ),
  tags$style(type = 'text/css', HTML('background-color: #f8f9fa;')),
  tags$style(type = 'text/css', 
             HTML('.navbar { background-color: #660000;}
                  .navbar-default .navbar-nav>li>a {
                    color: darkgray;}
                   .navbar-default .navbar-brand{color: white;}
                   .navbar-default .navbar-nav > .active > a, 
                   .navbar-default .navbar-nav > .active > a:focus, 
                   .navbar-default .navbar-nav > .active > a:hover {
                   color: white;
                   background-color: #660000;
                   }'),
             HTML('.btn {
          background-color: #17a2b8; 
          border-radius: 8px;
          border: none;
          color: white; }'),
          HTML('.nav-tabs>li>a {
                   border: 1px solid #17a2b8;
                   border-radius: 4px 4px 0 0;
                   }'),
          HTML('.selectize-input, .selectize-control.single .selectize-input.input-active {
                   background: #17a2b8;}
                  .selectize-input.full {
                   background-color: #17a2b8; color: white;}
                  .select-selected:after {
                  border-color: white transparent transparent transparent;}
                  .selectize-control.single .selectize-input:after {
                  border-color: white transparent transparent transparent;}'),
          HTML('.tab {
                   border: 1px solid #17a2b8;
                   background-color: white;}
                   .tab button {
                   background-color: #17a2b8;
                   float: left;
                   border: none;
                   outline: none;}
                   .tab button.active {
                   background-color: #17a2b8;
                   color: white;}
                   nav>li>a:focus, .nav>li>a:hover {
                   background-color: #17a2b8;
                   color: white;
                   }
                  '),
          HTML('electize-input.full {
                   background-color: #17a2b8;
                   color: white;}
                  '), 
          HTML('.nav-tabs>li.active>a, .nav-tabs>li.active>a:focus, .nav-tabs>li.active>a:hover {
    color: white;
    cursor: default;
    background-color: #17a2b8;
    border: 1px solid #ddd;
    border-bottom-color: transparent;
}'), HTML('footer{ background-color: #17a2b8; text-align: center; } '),
HTML('
              .nav-pills > li.active > a, .nav-pills > li.active > a:hover, .nav-pills > li.active > a:focus {
                color:#fff;
                background-color:#17a2b8;
    }'),
HTML('
                  .navbar-default .navbar-nav>li>a:focus, .navbar-default .navbar-nav>li>a:hover {
    color: #BDBDBD !important;
    background-color: #660000 !important;
}
                  ')
  )
)
)