ui <- navbarPage(
  useShinyjs(),
  extendShinyjs(text = jsCode),
  id = "quva_dashboard",
  title = div(div(id='quva_logo_nav',img(src = 'logo1.png', height = '45px')),div(id='alchemy_logo_nav',img(src = 'Alchemy_logo_RBG.png', width = '120px'))),
  windowTitle = "QuVa Pharma",
  selected = "Map",
  theme = "styles.css",
  tabPanel(
    "Map",
      div(class='loading-overlay',
         div(class='loading-overlay-alchemy',
            tags$img(src = 'alchemy.svg', class = 'alchemy-overlay-image'),
            tags$p(class='loading-title','Powering Your Experience....'),
            div(class='loading-progress')
         )
      ),
      div(
        class = "outer",
        leafletOutput('mymap',width='100%',height='100%'),
        selectInput('markerSize','Size:',c(15,30))
      ),
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
          choices = c('A','B','C','D'),
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
        width = 600,
        tags$a(class='lead_detail','<< Show Detail'),
        #tags$label(class='showmore','Show Detail'),
        tags$h4('Lead Information'),
        div(id='feature_bars',width = 398,
          highchartOutput('hc_income', height = 50),
          highchartOutput('hc_beds', height = 50),
          highchartOutput('hc_ops', height = 50),
          highchartOutput('hc_costs', height = 50),
          highchartOutput('hc_cmi', height = 50),
          highchartOutput('hc_avgstay', height = 50),
          highchartOutput('hc_households', height = 50),
          highchartOutput('hc_safety', height = 50),
          highchartOutput('hc_performance', height = 50)
        ),
        div(id='feature_scores_list',
          textOutput('score_income'),        
          textOutput('score_beds'),        
          textOutput('score_ops'),        
          textOutput('score_cmi'),        
          textOutput('score_avgstay'),        
          textOutput('score_households'),        
          textOutput('score_safety'),        
          textOutput('score_costs'),        
          textOutput('score_performance')      
        ),
        div(id='feature_values_list',class='list_hidden',
          textOutput('value_income'),        
          textOutput('value_beds'),        
          textOutput('value_ops'),        
          textOutput('value_cmi'),        
          textOutput('value_avgstay'),        
          textOutput('value_households'),        
          textOutput('value_safety'),        
          textOutput('value_costs'),        
          textOutput('value_performance')      
        ),
        hr(),
        tags$h4('IDN Information'),
        div(id='idn_info',style = 'width: 400px; float:left',
        div(style = 'width: 50%; float:left',
          div(style = 'width: 30%; float:left; text-align:left',
              tags$h5('Name: '),
              tags$h5(''),
              tags$h5('CMI Recent Value: ')),
          div(style = 'width: 30%; float:right;text-align:left',
              tags$h5(
                ), #textOutput("text_name")),
              tags$h5(''),
              tags$h5()#textOutput("text_beds"))
          )
        ),
        div(style = 'width: 50%; float:left',
          div(style = 'width: 30%; float:left; text-align:left',
              tags$h5('Income: '),
              tags$h5(''),
              tags$h5('Average Stay: ')),
          div(style = 'width: 30%; float:right;text-align:left',
              tags$h5(
                textOutput("text_name")),
              tags$h5(''),
              tags$h5(textOutput("text_beds"))
          )
        ),
        div(style = 'width: 50%; float:left',
            div(style = 'width: 30%; float:left; text-align:left',
                tags$h5('Beds: '),
                tags$h5(''),
                tags$h5('Households: ')),
            div(style = 'width: 30%; float:right;text-align:left',
                tags$h5(
                ), #textOutput("text_name")),
                tags$h5(''),
                tags$h5()#textOutput("text_beds"))
            )
        ),
        div(style = 'width: 50%; float:left',
            div(style = 'width: 30%; float:left; text-align:left',
                tags$h5('Ops Claimed: '),
                tags$h5(''),
                tags$h5('Safety Domain Score: ')),
            div(style = 'width: 30%; float:right;text-align:left',
                tags$h5(
                ), #textOutput("text_name")),
                tags$h5(''),
                tags$h5()#textOutput("text_beds"))
            )
        ),
        div(style = 'width: 50%; float:left',
            div(style = 'width: 30%; float:left; text-align:left',
                tags$h5('Total Costs: '),
                tags$h5(''),
                tags$h5('Performance Score: ')),
            div(style = 'width: 30%; float:right;text-align:left',
                tags$h5(
                ), #textOutput("text_name")),
                tags$h5(''),
                tags$h5()#textOutput("text_beds"))
            )
        ))
    )
  ),
  tabPanel("Data Tables")
)