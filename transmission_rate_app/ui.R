library(shiny)


shinyUI(shiny::bootstrapPage(
  
  useShinydashboard(),
  useShinyalert(),
  introjsUI(),
  
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
              remaining inland cities.", style="text-align: justify;"),
             br(),
             column(width = 3, offset = 5,actionBttn(inputId = "geral_tour", label = "Tour"))
      ),
      column(2)
    ),
    fluidRow(
      
      style = "height:50px;"),
    
    tags$hr(),
    
    
    column(width = 5,
           
           box(
             #pickerInput("map_shp", "", 
             #            width = "100%",
             #            choices = c(
             #              "State" = "state",
             #              "Capital" = "capital",
             #              "Inland cities" = "inland"
             #            )
             #),
             introBox(
               leaflet::leafletOutput("brasil_map",
                                      height = "500px"),
               data.step = 1,
               data.intro = "To begin click on a state",
               data.hint = "click"
             ),
             width = 12
           )
    ),
    tabBox(
      width = 7,
      tabPanel(
        "Model",
        column(width = 12,
               fluidRow(
                 
                 conditionalPanel(
                   condition = "input.model_or_rt != 'str'",
                   column(
                     width = 2,
                     
                     introBox(
                       data.step = 3,
                       data.intro = "Dummy text3",
                       data.hint = "click",
                       pickerInput("model_loc", "", 
                                   width = "100%",
                                   choices = c(
                                     "State" = "state",
                                     "Capital" = "capital",
                                     "Inland cities" = "inland"
                                   )
                       )
                     )
                   )
                 ),
                 column(
                   width = 2,
                   introBox(
                     data.step = 2,
                     data.intro = "Dummy text2",
                     data.hint = "click",
                     pickerInput("model_or_rt", "",
                                 width = "100%",
                                 choices = c(
                                   "R(t)" = "rt",
                                   "Model" = "model",
                                   "Goodness of fit" = "model_qual",
                                   "Betas" = "beta_series",
                                   "Stringency" = "str"
                                 ), selected = "model"
                     )
                   )
                 ),
                 conditionalPanel(
                   condition = "input.model_or_rt == 'model'",
                   column(
                     width = 5,
                     br(),
                     introBox(
                       switchInput(
                         inputId = "d_c",
                         onLabel = "Daily",
                         offLabel = "Cummulative",
                       ),
                       data.step = 4,
                       data.intro = "Dummy text4",
                       data.hint = "click"
                     )
                   ),
                   column(
                     width = 3,
                     br(),
                     
                     introBox(
                       data.step = 5,
                       data.intro = "Dummy text5",
                       data.hint = "click",
                       switchInput(
                         inputId = "lin_log",
                         onLabel = "Logarithmic",
                         offLabel = "Linear"
                       )
                     )
                   )
                 ),
                 
                 introBox(
                   data.step = 6,
                   data.intro = "Dummy text6",
                   data.hint = "click",
                   highchartOutput("trans_rate_output", height = "400px")
                 )
               )
        )
      ),
      tabPanel(
        "Table",
        shiny::fluidRow(
          column(htmlOutput("selec_state"), width = 1),
          column(br(),actionBttn("tab_tour", icon = icon("info"), 
                                 color = "primary",style = "material-circle",
                                 size = "sm",
                                 label = ""), width = 3)
        ),
        rHandsontableOutput("info_tab", "100%", 120)
        
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