library(shiny)


shinyUI(shiny::bootstrapPage(
  
  useShinydashboard(),
  useShinyalert(),
  
  fluidPage(
    fluidRow(
      column(2),
      column(8,
         HTML("<br><br><center> <h1>Assessing the nation wide impact of COVID-19 
         mitigation policies on the transmission rate of SARS-CoV-2 in Brazil</h1> </center><br>")
      ),
      column(2)
    ),
    fluidRow(
      
      style = "height:50px;"),
    
    tags$hr(),
    
    fluidRow(
      column(2),
      column(8,
             h4("COVID-19 is now identified in almost all countries in the world, 
              with poorer regions being particularly more disadvantaged to efficiently 
              mitigate the impacts of the pandemic. In the absence of efficient therapeutics or 
              vaccines, control strategies are currently based on non-pharmaceutical interventions, 
              comprising changes in population behavior and governmental interventions, among which
              the prohibition of mass gatherings, closure of non-essential establishments, 
              quarantine and movement restrictions. In this work we analyzed the effects of 547
              published governmental interventions, and population adherence thereof, on the dynamics
              of COVID-19 cases across all 27 Brazilian states, with emphasis on state capitals and
              remaining inland cities.", style="text-align: justify;")
      ),
      column(2)
    ),
    fluidRow(
      
      style = "height:50px;"),
    
    tags$hr(),
    
    
    column(width = 5,
           box(leaflet::leafletOutput("brasil_map",
                                      height = "500px"),width = 12)
    ),
    tabBox(
      width = 7,
      tabPanel(
        "Visualizations",
        column(width = 12,
               fluidRow(
                 column(
                   width = 5,
                   pickerInput("model_loc", "", 
                               width = "100%",
                               choices = c(
                                 "State" = "state",
                                 "Capital" = "capital",
                                 "Inland cities" = "inland"
                               )
                   )
                 ),
                 column(
                   width = 5,
                   pickerInput("model_or_rt", "",
                               width = "100%",
                               choices = c(
                                 "R(t)" = "rt",
                                 "Model" = "model"
                               ), selected = "model"
                   )
                 )
               ),
               highchartOutput("trans_rate_output", height = "400px")
        )
      ),
      tabPanel(
        "Table",
        rHandsontableOutput("tab_int", 100, 120)
      )
    ),
    
    fluidRow(style = "height:50px;"),
    
    tags$hr(),
    
    fluidRow(
      column(12,
             HTML("<hr><center>"),HTML("<h1>Support:</h1>"),HTML("&emsp;&emsp;"),
             a(href= "https://www.gov.br/en",img(src="capes_logo.png",width=400*0.25), target="_blank"),
             HTML("&emsp;&emsp;"),
             a(href= "https://cidacs.bahia.fiocruz.br/en/",
               img(src="CIDACS_logoBW.png",width=400*0.6), target="_blank"),            
             HTML("&emsp;&emsp;</center></hr>")
      )
    ),
    fluidRow(style = "height:50px;"),
    
    tags$hr()
    
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