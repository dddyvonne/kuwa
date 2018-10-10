server <- function(input, output, session) {
  # Remove the useless <li> created by navbar header
  shinyjs::runjs("$('.navbar-nav li').first().remove();")
  
  output$hc_income <- renderHighchart({make_chart('Income', length(income_labs), 0, seq(1,length(income_labs)))}) #length(unique(hospitals$income_bucket)), 0, income_labs)})
  output$hc_beds <- renderHighchart({make_chart('Beds', length(bed_labs), 0, seq(1,length(bed_labs)))}) #length(unique(hospitals$beds_bucket)), 0, bed_labs)})
  output$hc_ops <- renderHighchart({make_chart('Ops Claimed', length(opclaimed_labs), 0, seq(1,length(opclaimed_labs)))}) #length(unique(hospitals$Num_Of_OP_Claims_Total_bucket)), 0, opclaimed_labs)})
  output$hc_costs <- renderHighchart({make_chart('Total Costs', length(ipcosts_labs), 0, seq(1,length(ipcosts_labs)))}) #length(unique(hospitals$ip_total_costs_bucket)), 0, ipcosts_labs)})
  output$hc_cmi <- renderHighchart({make_chart('CMI Recent Value', length(cmi_labs), 0, seq(1,length(cmi_labs)))}) #length(unique(hospitals$cmi_recent_value_bucket)), 0, cmi_labs)})
  output$hc_avgstay <- renderHighchart({make_chart('Average Stay', length(avgstay_labs), 0, seq(1,length(avgstay_labs)))}) #length(unique(hospitals$ip_avg_stay_bucket)), 0, avgstay_labs)})
  output$hc_households <- renderHighchart({make_chart('Households', length(households_labs), 0, seq(1,length(households_labs)))}) #length(unique(hospitals$households_bucket)), 0, households_labs)})
  output$hc_safety <- renderHighchart({make_chart('Safety Domain Score', length(safety_labs), 0, seq(1,length(safety_labs)))}) #length(unique(hospitals$Weighted_Safety_Domain_Score_bucket)), 0, safety_labs)})
  output$hc_performance <- renderHighchart({make_chart('Performance Score', length(performance_labs), 0, seq(1,length(performance_labs)))}) #length(unique(hospitals$Total_Performance_Score_bucket)), 0, performance_labs)})
  output$hc_gpo <- renderHighchart({make_chart('GPO', length(gpo_labs),0, seq(1,length(gpo_labs)))})
  outVar = reactive({
    state_code = state_limits[state_limits$State==input$filterStates,Code]
    if(input$filterStates!='Whole US'){
      newvals = unique(hospitals[hospitals$State_y==state_code,'NAME'])
    }
    else{
      newvals = unique(hospitals$NAME)
    }
    newvals
  })
  observe({
    updateSelectInput(session,'search_input',
    choices = outVar()
  )})
  
  # US State Map
  output$mymap <- renderLeaflet({
    leaflet() %>%
      fitBounds(state_limits[state_limits$State==input$filterStates,W],state_limits[state_limits$State==input$filterStates,S],state_limits[state_limits$State==input$filterStates,E],state_limits[state_limits$State==input$filterStates,N]) %>%
      #fitBounds(-127.68,24.43199,-64.39877,51.14134) %>%
      addTiles(options = providerTileOptions(minZoom = 4,maxZoom=20)) %>%
      setMaxBounds(lng1 = -180, lat1 =-10, lng2=40, lat2=80) %>%
      #addProviderTiles(providers$CartoDB.Positron) %>%
      #clusterOptions = markerClusterOptions() %>%
      addMarkers(
        layerId = ~NAME,
        group = 'hospitals_A',
        data = hospitals[hospitals$lead == 'A', ],
        lng = ~Longitude,
        lat = ~Latitude,
        popup = ~popup_content,
        icon = A_icon(15),
        clusterOptions = markerClusterOptions({maxClusterRadius = 4})
          
      ) %>%
      #hideGroup('hospitals_A') %>%
      addMarkers(
        layerId = ~NAME,
        group = 'hospitals_B',
        data = hospitals[hospitals$lead == 'B', ],
        lng = ~Longitude,
        lat = ~Latitude,
        popup = ~popup_content,
        icon = B_icon(15),
        clusterOptions = markerClusterOptions()
      ) %>%
      #hideGroup('hospitals_B') %>%
      addMarkers(
        layerId = ~NAME,
        group = 'hospitals_C',
        data = hospitals[hospitals$lead == 'C', ],
        lng = ~Longitude,
        lat = ~Latitude,
        popup = ~popup_content,
        icon = C_icon(15),
        clusterOptions = markerClusterOptions()
      ) %>%
      #hideGroup('hospital_C') %>%
      addMarkers(
        layerId = ~NAME,
        group = 'hospitals_D',
        data = hospitals[hospitals$lead == 'D', ],
        lng = ~Longitude,
        lat = ~Latitude,
        popup = ~popup_content,
        icon = D_icon(15),
        clusterOptions = markerClusterOptions()
      ) #%>%
      #hideGroup('hospital_D')
  })
  
  # Filter the priority groups and update the map
  observeEvent(input$lead_priority_input, ignoreNULL = FALSE, {
    # A Leads
    if('A' %in% input$lead_priority_input) {
      leafletProxy('mymap') %>%
        showGroup('hospitals_A') #%>%
        #showGroup('hospitals_zoom_markers_A')
    }
    else {
      leafletProxy('mymap') %>%
        hideGroup('hospitals_A') %>%
        hideGroup('hospitals_zoom_markers_A')
    }
    # B Leads
    if('B' %in% input$lead_priority_input) {
      leafletProxy('mymap') %>%
        showGroup('hospitals_B') #%>%
        #showGroup('hospitals_zoom_markers_B')
    }
    else {
      leafletProxy('mymap') %>%
        hideGroup('hospitals_B') %>%
        hideGroup('hospitals_zoom_markers_B')
    }
    # C Leads
    if('C' %in% input$lead_priority_input) {
      leafletProxy('mymap') %>%
        showGroup('hospitals_C') #%>%
        #showGroup('hospitals_zoom_markers_C')
    }
    else {
      leafletProxy('mymap') %>%
        hideGroup('hospitals_C') %>%
        hideGroup('hospitals_zoom_markers_C')
    }
    # D Leads
    if('D' %in% input$lead_priority_input) {
      leafletProxy('mymap') %>%
        showGroup('hospitals_D') #%>%
        #showGroup('hospitals_zoom_markers_D')
    }
    else {
      leafletProxy('mymap') %>%
        hideGroup('hospitals_D') %>%
        hideGroup('hospitals_zoom_markers_D')
    }
    features <- c('Income','Beds','Ops Claimed','Total Costs','CMI Recent Value','Average Stay','Households','Safety Domain Score','Performance Score','GPO','Total Score')
    session$sendCustomMessage('initializeFeatureNames',features)
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
    
     #message <- 'SEARCH'
     #session$sendCustomMessage('handler1',message)
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
    
    message <- 'SEARCH'
    session$sendCustomMessage('handler1',message)
   # hideGroup('searched')
    #else addPopups(
     #    lng = hos$Longitude,
      #    lat = hos$Latitude,
       #   popup = NULL
        #) 
  })

  observeEvent(input$mymap_marker_click,{
    
    hospital <- hospitals[hospitals$NAME == input$mymap_marker_click$id, ]
    
    # Update charts
    output$hc_income <- renderHighchart({make_chart('Income', length(income_labs), hospital$income_bin, seq(1,length(income_labs)))})
    output$hc_beds <- renderHighchart({make_chart('Beds', length(bed_labs), hospital$beds_bin, seq(1,length(bed_labs)))})
    output$hc_ops <- renderHighchart({make_chart('Ops Claimed', length(opclaimed_labs), hospital$ops_bin, seq(1,length(opclaimed_labs)))})
    output$hc_costs <- renderHighchart({make_chart('Total Costs', length(ipcosts_labs), hospital$ipcosts_bin, seq(1,length(ipcosts_labs)))})
    output$hc_cmi <- renderHighchart({make_chart('CMI Recent Value', length(cmi_labs), hospital$cmi_bin, seq(1,length(cmi_labs)))})
    output$hc_avgstay <- renderHighchart({make_chart('Average Stay', length(avgstay_labs), hospital$avgstay_bin, seq(1,length(avgstay_labs)))})
    output$hc_households <- renderHighchart({make_chart('Households', length(households_labs), hospital$households_bin, seq(1,length(households_labs)))})
    output$hc_safety <- renderHighchart({make_chart('Safety Domain Score', length(safety_labs), hospital$safety_bin, seq(1,length(safety_labs)))})
    output$hc_performance <- renderHighchart({make_chart('Performance Score', length(performance_labs), hospital$performance_bin, seq(1,length(performance_labs)))})
    output$hc_gpo <- renderHighchart({make_chart('GPO',length(gpo_labs), hospital$GPO_Text__c_Vizient_score, seq(1,length(gpo_labs)))})
    output$text_beds <- renderText({with(hospitals, sum(hospitals[GPO_Affiliations == hospital$GPO_Affiliations, "beds"]))})
    output$text_name <- renderText({hospital$GPO_Affiliations})
    output$text_households <- renderText({with(hospitals, colSums(hospitals[GPO_Affiliations == hospital$GPO_Affiliations, "households"]))})
    output$text_costs <- renderText({with(hospitals, colSums(hospitals[GPO_Affiliations == hospital$GPO_Affiliations, "ip_total_costs"]))})
    output$text_cmi <- renderText({round(with(hospitals,colMeans(hospitals[GPO_Affiliations == hospital$GPO_Affiliations, 'cmi_recent_value'])),2)})
    output$text_avgstay <- renderText({round(with(hospitals,colMeans(hospitals[GPO_Affiliations == hospital$GPO_Affiliations, 'ip_avg_stay'])),2)})
    output$text_ops <- renderText({with(hospitals,colSums(hospitals[GPO_Affiliations == hospital$GPO_Affiliations, 'Num_Of_OP_Claims_Total']))})
    output$text_safety <- renderText({round(with(hospitals,colMeans(hospitals[GPO_Affiliations == hospital$GPO_Affiliations, 'Weighted_Safety_Domain_Score'])),2)})
    output$text_performance <- renderText({round(with(hospitals,colMeans(hospitals[GPO_Affiliations == hospital$GPO_Affiliations, 'Total_Performance_Score'])),2)})
    output$text_income <- renderText({with(hospitals,colSums(hospitals[GPO_Affiliations == hospital$GPO_Affiliations, 'income']))})
    
    output$score_income <- renderText({paste(round(pct_income(hospital$income),1)*10,'/10')})  #real scores go here :)
    output$score_beds <- renderText({paste(round(pct_beds(hospital$beds),1)*10,'/10')})
    output$score_ops <- renderText({paste(round(pct_ops(hospital$Num_Of_OP_Claims_Total),1)*10,'/10')})
    output$score_costs <- renderText({paste(round(pct_ipcosts(hospital$ip_total_costs),1)*10,'/10')})
    output$score_cmi <- renderText({paste(round(pct_cmi(hospital$cmi_recent_value),1)*10,'/10')})
    output$score_avgstay <- renderText({paste(round(pct_avgstay(hospital$ip_avg_stay),1)*10,'/10')})
    output$score_households <-renderText({paste(round(pct_households(hospital$households),1)*10,'/10')})
    output$score_safety <- renderText({paste(round(pct_safety(hospital$Weighted_Safety_Domain_Score),1)*10,'/10')})
    output$score_performance <- renderText({paste(round(pct_performance(hospital$Total_Performance_Score),1)*10,'/10')})
    output$score_gpo <- renderText({paste(hospital$GPO_Text__c_Vizient_score*10,'/10')})
    
    #income_score
    income_score <- round(pct_income(hospital$income),1)*10
    beds_score <- round(pct_beds(hospital$beds),1)*10
    ops_score <- round(pct_ops(hospital$Num_Of_OP_Claims_Total),1)*10
    costs_score <- round(pct_ipcosts(hospital$ip_total_costs),1)*10
    cmi_score <- round(pct_cmi(hospital$cmi_recent_value),1)*10
    avgstay_score <- round(pct_avgstay(hospital$ip_avg_stay),1)*10
    households_score <- round(pct_households(hospital$households),1)*10
    safety_score <- round(pct_safety(hospital$Weighted_Safety_Domain_Score),1)*10
    performance_score <- round(pct_performance(hospital$Total_Performance_Score),1)*10
    gpo_score <- hospital$GPO_Text__c_Vizient_score*10
    
    output$total_score <- renderText({paste((round((income_score*0.1 + beds_score*0.1 + ops_score*0.15 
                                     +costs_score*0.05 + cmi_score*0.05 + avgstay_score*0.05
                                     + households_score*0.05 + safety_score*0.10
                                     + performance_score*0.05 + gpo_score*0.3)*10)),'/100')})
    
    
    
    output$value_income <- renderText({hospital$income})  #real values go here :)
    output$value_beds <- renderText({hospital$beds})
    output$value_ops <- renderText({hospital$Num_Of_OP_Claims_Total})
    output$value_costs <- renderText({hospital$ip_total_costs})
    output$value_cmi <- renderText({hospital$cmi_recent_value})
    output$value_avgstay <- renderText({hospital$ip_avg_stay})
    output$value_households <-renderText({hospital$households})
    output$value_safety <- renderText({hospital$Weighted_Safety_Domain_Score})
    output$value_performance <- renderText({hospital$Total_Performance_Score})
    output$value_gpo <- renderText({hospital$GPO_Text__c_Vizient_score})
    message <- input$filterStates #'MARKER CLICK'
    session$sendCustomMessage('markerClick',message)
  })
  
  observeEvent(input$mymap_bounds,{ #input$mymap_zoom,{
    mybounds <- unlist(input$mymap_bounds,use.names=FALSE)

    if(input$mymap_zoom > 9){zoomFlag <<- 30}
    else if(input$mymap_zoom <= 9){zoomFlag <<- 15} # & zoomFlag == 30
    message <- paste0('ZOOM: ',input$mymap_zoom,'  -  ',zoomFlag)
    session$sendCustomMessage('handler1',message)
    
    #if(zoomFlag > 0){
        leafletProxy('mymap') %>%
        addMarkers(
          layerId = ~NAME, #paste0(~NAME,'zoom'),
          group = 'hospitals_zoom_markers_A',
          data = hospitals[hospitals$lead == 'A' & hospitals$Latitude<=mybounds[1] & hospitals$Latitude>=mybounds[3] & hospitals$Longitude<=mybounds[2] & hospitals$Longitude>=mybounds[4],],
          lng = ~Longitude,
          lat = ~Latitude,
          popup = ~popup_content,
          icon = A_icon(zoomFlag)
        ) %>%
        addMarkers(
          layerId = ~NAME, #paste0(~NAME,'zoom'),
          group = 'hospitals_zoom_markers_B',
          data = hospitals[hospitals$lead == 'B' & hospitals$Latitude<=mybounds[1] & hospitals$Latitude>=mybounds[3] & hospitals$Longitude<=mybounds[2] & hospitals$Longitude>=mybounds[4],],
          lng = ~Longitude,
          lat = ~Latitude,
          popup = ~popup_content,
          icon = B_icon(zoomFlag)
        ) %>%
        addMarkers(
          layerId = ~NAME, #paste0(~NAME,'zoom'),
          group = 'hospitals_zoom_markers_C',
          data = hospitals[hospitals$lead == 'C' & hospitals$Latitude<=mybounds[1] & hospitals$Latitude>=mybounds[3] & hospitals$Longitude<=mybounds[2] & hospitals$Longitude>=mybounds[4],],
          lng = ~Longitude,
          lat = ~Latitude,
          popup = ~popup_content,
          icon = C_icon(zoomFlag)
        ) %>%
        addMarkers(
          layerId = ~NAME, #paste0(~NAME,'zoom'),
          group = 'hospitals_zoom_markers_D',
          data = hospitals[hospitals$lead == 'D' & hospitals$Latitude<=mybounds[1] & hospitals$Latitude>=mybounds[3] & hospitals$Longitude<=mybounds[2] & hospitals$Longitude>=mybounds[4],],
          lng = ~Longitude,
          lat = ~Latitude,
          popup = ~popup_content,
          icon = D_icon(zoomFlag)
        )
      
        if(!('A' %in% input$lead_priority_input)){
          leafletProxy('mymap') %>%
          hideGroup('hospitals_zoom_markers_A') %>%
          hideGroup('hospitals_A')
        }
        else{
          leafletProxy('mymap') %>%
          #showGroup('hospitals_zoom_markers_A') %>%
          showGroup('hospitals_A')            
        }
      
        if(!('B' %in% input$lead_priority_input)){
          leafletProxy('mymap') %>%
          hideGroup('hospitals_zoom_markers_B') %>%
          hideGroup('hospitals_B')
        }
        else{
          leafletProxy('mymap') %>%
          #showGroup('hospitals_zoom_markers_B') %>%
          showGroup('hospitals_B')            
        }
      
        if(!('C' %in% input$lead_priority_input)){
          leafletProxy('mymap') %>%
          hideGroup('hospitals_zoom_markers_C') %>%
          hideGroup('hospitals_C')
        }
        else{
          leafletProxy('mymap') %>%
          #showGroup('hospitals_zoom_markers_C') %>%
          showGroup('hospitals_C')            
        }
      
        if(!('D' %in% input$lead_priority_input)){
          leafletProxy('mymap') %>%
          hideGroup('hospitals_zoom_markers_D') %>%
          hideGroup('hospitals_D')  
        }
        else{
          leafletProxy('mymap') %>%
          #showGroup('hospitals_zoom_markers_D') %>%
          showGroup('hospitals_D')       
        }
    #}  
    inputvals <<- input$mymap_bounds
  })
}