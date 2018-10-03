server <- function(input, output, session) {
  
  # Remove the useless <li> created by navbar header
  shinyjs::runjs("$('.navbar-nav li').first().remove();")
  
  output$hc_income <- renderHighchart({make_chart('Income', count(unique(hospitals$income_bucket)), 0, income_labs)})
  output$hc_beds <- renderHighchart({make_chart('Beds', count(unique(hospitals$beds_bucket)), 0, bed_labs)})
  
  # Michigan county map
  output$mymap <- renderLeaflet({
    leaflet()  %>%
      fitBounds(-127.68,
                24.43199,
                -64.39877,
                51.14134) %>%
      addTiles() %>%
      #addProviderTiles(providers$CartoDB.Positron) %>%
      addMarkers(
        layerId = ~NAME,
        group = 'hospitals_A',
        data = hospitals[hospitals$lead == 'A', ],
        lng = ~Longitude,
        lat = ~Latitude,
        popup = ~popup_content,
        icon = A_icon
      ) %>%
      hideGroup('hospitals_A') %>%
      addMarkers(
        layerId = ~NAME,
        group = 'hospitals_B',
        data = hospitals[hospitals$lead == 'B', ],
        lng = ~Longitude,
        lat = ~Latitude,
        popup = ~popup_content,
        icon = B_icon
      ) %>%
      hideGroup('hospitals_B') %>%
      addMarkers(
        layerId = ~NAME,
        group = 'hospitals_C',
        data = hospitals[hospitals$lead == 'C', ],
        lng = ~Longitude,
        lat = ~Latitude,
        popup = ~popup_content,
        icon = C_icon
      ) %>%
      hideGroup('hospital_C') %>%
      addMarkers(
        layerId = ~NAME,
        group = 'hospitals_D',
        data = hospitals[hospitals$lead == 'D', ],
        lng = ~Longitude,
        lat = ~Latitude,
        popup = ~popup_content,
        icon = D_icon
      ) %>%
      hideGroup('hospital_D')
    
  })
  
  # Filter the priority groups and update the map
  observeEvent(input$lead_priority_input, ignoreNULL = FALSE, {
    
    # A Leads
    if('A' %in% input$lead_priority_input) {
      leafletProxy('mymap') %>%
        showGroup('hospitals_A')
    }
    else {
      leafletProxy('mymap') %>%
        hideGroup('hospitals_A')
    }
    
    # B Leads
    if('B' %in% input$lead_priority_input) {
      leafletProxy('mymap') %>%
        showGroup('hospitals_B')
    }
    else {
      leafletProxy('mymap') %>%
        hideGroup('hospitals_B')
    }
    
    # C Leads
    if('C' %in% input$lead_priority_input) {
      leafletProxy('mymap') %>%
        showGroup('hospitals_C')
    }
    else {
      leafletProxy('mymap') %>%
        hideGroup('hospitals_C')
    }
    
    # D Leads
    if('D' %in% input$lead_priority_input) {
      leafletProxy('mymap') %>%
        showGroup('hospitals_D')
    }
    else {
      leafletProxy('mymap') %>%
        hideGroup('hospitals_D')
    }
    
  })
  
  # Display what is searched on the map
 # observeEvent(input$search_input, ignoreNULL = FALSE, {
    
#    hos <- hospitals[hospitals$NAME %in% input$search_input, ]
    
#    leafletProxy('mymap') %>%
#      clearPopups() %>%
#      addPopups(
#        lng = hos$Longitude,
#        lat = hos$Latitude,
#        popup = hos$popup_content
#      )
    
#  })
  
  observeEvent(input$search_input,ignoreNULL = FALSE,{
    hos <- hospitals[hospitals$NAME %in% input$search_input, ]
    leafletProxy('mymap') %>%
      #  clearPopups() %>%
       # addPopups(
      #    lng = hos$Longitude,
      #    lat = hos$Latitude,
      #    popup = NULL
      #  ) %>%
      # leaflet(data = hos) %>% addTiles() %>%
      #if(count(input$search_input) == 1)
      addMarkers(
        layerId = ~input$search_input,
        group = 'searched',
        data = hos,
        lng = hos$Longitude,
        lat = hos$Latitude,
        popup = ~popup_content,
        icon = makeIcon(
          iconUrl = paste('hospital_',hos$lead,'.svg', sep = ''),
          iconWidth = 15,
          iconHeight = 15
        )
      ) 
    
   # hideGroup('searched')
    #else addPopups(
     #    lng = hos$Longitude,
      #    lat = hos$Latitude,
       #   popup = NULL
        #) 
  })

  observeEvent(input$mymap_marker_click, {
    
    hospital <- hospitals[hospitals$NAME == input$mymap_marker_click$id, ]
    
    # Update charts
    output$hc_income <- renderHighchart({make_chart('Income', count(unique(hospitals$income_bucket)), hospital$income_bin, income_labs)})
    output$hc_beds <- renderHighchart({make_chart('Beds', count(unique(hospitals$beds_bucket)), hospital$beds_bin, bed_labs)})
    output$text_beds <- renderText(hospital$beds)
    output$text_name <- renderText(hospital$NAME)
  })
  

}



