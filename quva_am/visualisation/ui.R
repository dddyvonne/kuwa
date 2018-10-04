ui <- navbarPage(
  useShinyjs(),
  extendShinyjs(text = jsCode),
  id = "quva_dashboard",
  title = div(img(src = "Alchemy_logo_RBG.png", width = "100px")),
  windowTitle = "QuVa Pharma",
  selected = "Map",
  theme = "styles.css",
  tabPanel(
    "Map",
    div(
      class = "outer",
      leafletOutput("mymap", width = "100%", height = "100%"),
      absolutePanel(
        id = 'checkbox_panel',
        class = 'panel',
        fixed = TRUE,
        top = 250,
        left = 20,
        bottom = 'auto',
        width = 250,
        right = 'auto',
        selectInput('filterStates',
                    'Select Regions:',c('Whole US',state_limits[state_limits$State!='Whole US',]$State)
        ),
        selectizeInput(
          inputId = 'search_input',
          label = 'Search Hospitals',
          choices = hospitals$NAME,
          selected = NULL,
          multiple = TRUE,
          width = '100%'
        ),
        checkboxGroupInput(
          inputId = 'lead_priority_input',
          label = 'Lead Priority',
          choices = c('A', 'B', 'C', 'D'),
          inline = TRUE,
          width = '100%'
        ),
        br()
      ),
      absolutePanel(
        id = 'lead_scoring_panel',
        class = 'panel',
        fixed = TRUE,
        top = 70,
        left = 'auto',
        right = 20,
        bottom = 'auto',
        width = 400,
        tags$h4('Lead Information'),
<<<<<<< HEAD
        #tags$h4(textOutput('text_name')),
        highchartOutput('hc_income', height = 50),
        highchartOutput('hc_beds', height = 50),
        highchartOutput('hc_ops', height =50),
        highchartOutput('hc_cmi', height =50),
        highchartOutput('hc_avgstay', height =50),
        highchartOutput('hc_households', height =50),
        highchartOutput('hc_safety', height =50),
        highchartOutput('hc_performance', height =50),
        
        
        
=======
        highchartOutput('hc_income', height = 50),
        highchartOutput('hc_beds', height = 50),
>>>>>>> 04155285cd2c186c9b00b0162b6ab716d0614f51
        
        hr(),
        tags$h4('IDN'),
        div(
          div(style = 'width: 20%; float:left; text-align:left',
              tags$h4('Name: '),
              tags$h4(''),
              tags$h4('Beds: ')),
          div(style = 'width: 80%; float:right;text-align:left',
              tags$h4(
                textOutput("text_name")),
              tags$h4(''),
              tags$h4(textOutput("text_beds"))
          )
          
        )
        
      )
    )
  ),
  tabPanel("Data Tables")
)