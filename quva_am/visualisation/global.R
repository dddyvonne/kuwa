library(shiny)
library(shinyjs)
library(leaflet)
library(data.table)
library(htmltools)
library(highcharter)
library(dplyr)
library(tools)

setwd('/Users/harveymanhood/Documents/- github/kuwa-Y/quva_am/visualisation')
hospitals <- fread('data/quva_dashboardv2.csv') #, data.table = F
bins <- fread('data/bin_number_groups.csv',data.table = F)
income_bins <- fread('data/income_bins.csv',data.table = F)
beds_bins <- fread('data/beds_bin.csv', data.table = F)
ops_bins <- fread('data/ops_bin.csv', data.table = F)
performance_bins <- fread('data/performance_bin.csv', data.table = F)
ipcosts_bins <- fread('data/ipcosts_bin.csv',data.table = F)
cmi_bins <- fread('data/cmi_bin.csv', data.table = F)
avgstay_bins <- fread('data/avgstay_bin.csv', data.table = F)
households_bins <- fread('data/households_bin.csv', data.table = F)
safety_bins <- fread('data/safety_bin.csv',data.table = F)
state_limits <- fread('data/state_limits.csv')

create_labs <- function(data){
  data <- as.character(round(data))
  data <- paste(data, lead(data, default = ''), sep = '-')
  data
}

create_list <- function(data) {
  label <- gsub("[\\(\\)]", "", data[,1])
  label_fin <- gsub("\\[|\\]", "", label)
  label_fin <- gsub(",","",label_fin)
  label_fin <- strsplit(label_fin,' ')
  label_fin <- as.numeric(unique(unlist(label_fin)))
  label_fin
}

income_labs <- create_labs(create_list(income_bins))
bed_labs <- create_labs(create_list(beds_bins))
opclaimed_labs <- create_labs(create_list(ops_bins))
performance_labs <- create_labs(create_list(performance_bins))
ipcosts_labs <- create_labs(create_list(ipcosts_bins))
cmi_labs <- create_labs(create_list(cmi_bins))
avgstay_labs <- create_labs(create_list(avgstay_bins))
households_labs <- create_labs(create_list(households_bins))
safety_labs <- create_labs(create_list(safety_bins))

#income_labs <- create_labs(c(0, 48.525, 80.564, 141.836, 176.893))
#bed_labs <- create_labs(c(3, 50.0, 100, 150, 254.456, 1532.489, 2409.971, 2635.0, 4000.0))
#opclaimed_labs <- create_labs(c(0, 1001, 5000, 9140.479, 35608.014, 44765.146, 58625.375))
#ipcosts_labs <- create_labs(c(0, 164.197, 15070.302, 6021665.45, 313446375.0))
#cmi_labs <- create_labs(c(0, 1.185, 1.897, 2.489, 2.921))
#avgstay_labs <- create_labs(c(-0.474, 6.546, 10.75))
#households_labs <- create_labs(c(0,5323.096, 15075.413, 20805.224, 21687.004, 23381.0))
#safety_labs <- create_labs(c(0.0, 5.0, 10.0, 12.618, 14.957, 16.546, 20.0, 22.507, 128.8))
#performance_labs <- create_labs(c(0, 29.828, 63.414, 82.273))

all_labs <- paste0("'",paste0(c(income_labs,bed_labs,opclaimed_labs,ipcosts_labs,cmi_labs,avgstay_labs,households_labs,safety_labs,performance_labs),collapse="','"),"'")
all_labs_counts <- paste0(c(length(income_labs),length(bed_labs),length(opclaimed_labs),length(ipcosts_labs),length(cmi_labs),length(avgstay_labs),length(households_labs),length(safety_labs),length(performance_labs)),collapse=",")
jsCode <- paste0("shinyjs.init = function(){(function(w,d,s,l,i){w[l]=w[l]||[];w[l].push({'gtm.start':new Date().getTime(),event:'gtm.js'});var f=d.getElementsByTagName(s)[0],
                 j=d.createElement(s),dl=l!='dataLayer'?'&l='+l:'';j.async=true;j.src='https://www.googletagmanager.com/gtm.js?id='+i+dl;
                 f.parentNode.insertBefore(j,f);})(window,document,'script','dataLayer','GTM-W6CQG8K');dataLayer.unshift({all_labs:[",all_labs,"],all_labs_counts:[",all_labs_counts,"]});}")

# Leads
hospitals$lead <- sample(c('A', 'B', 'C', 'D'), 7329, replace = T, rep(0.25, 4))
hospitals <- merge(x = hospitals, y = beds_bins[ , c("beds_bucket", "beds_bin")], by = "beds_bucket", all.x=TRUE)
hospitals <- merge(x = hospitals, y = income_bins[, c("income_bucket","income_bin")], by = "income_bucket", all.x=TRUE)
hospitals <- merge(x = hospitals, y = performance_bins[, c("Total_Performance_Score_bucket", "performance_bin")], by = "Total_Performance_Score_bucket", all.x=TRUE)
hospitals <- merge(x = hospitals, y = ipcosts_bins[, c("ip_total_costs_bucket", "ipcosts_bin")], by = "ip_total_costs_bucket", all.x=TRUE)
hospitals <- merge(x = hospitals, y = cmi_bins[, c("cmi_recent_value_bucket", "cmi_bin")], by = "cmi_recent_value_bucket", all.x=TRUE)
hospitals <- merge(x = hospitals, y = avgstay_bins[, c("ip_avg_stay_bucket", "avgstay_bin")], by = "ip_avg_stay_bucket", all.x=TRUE)
hospitals <- merge(x = hospitals, y = households_bins[, c("households_bucket", "households_bin")], by = "households_bucket", all.x=TRUE)
hospitals <- merge(x = hospitals, y = safety_bins[, c("Weighted_Safety_Domain_Score_bucket", "safety_bin")], by = "Weighted_Safety_Domain_Score_bucket", all.x=TRUE)
hospitals <- merge(x = hospitals, y = ops_bins[, c("Num_Of_OP_Claims_Total_bucket", "ops_bin")], by = "Num_Of_OP_Claims_Total_bucket", all.x=TRUE)

# FAKE NEWS
#hospitals$income <- sample(seq(3), 5958, replace = T)
#hospitals$beds <- sample(seq(3), 5958, replace = T)

# Adjust Hospital Names
hospitals$NAME <- hospitals$Hospital_Name

# Hospital Map Icons
A_icon <- function(s){
  makeIcon(
    iconUrl = 'hospital_A.svg',
    iconWidth = s,
    iconHeight = s
  )
}
B_icon <- function(s){
  makeIcon(
    iconUrl = 'hospital_B.svg',
    iconWidth = s,
    iconHeight = s
  )
}
C_icon <- function(s){
  makeIcon(
    iconUrl = 'hospital_C.svg',
    iconWidth = s,
    iconHeight = s
  )
}
D_icon <- function(s){
  makeIcon(
    iconUrl = 'hospital_D.svg',
    iconWidth = s,
    iconHeight = s
  )
}

# Popup content
# NAME ADDRESS AND SALES TEAM
hospitals$popup_content <- paste0('<h6 class="popup_name">', hospitals$NAME, '</h6>', toTitleCase(tolower(hospitals$City)), ', ', hospitals$State_y, '</br>', 'Assigned Sales: ', hospitals$Assigned_Name)

make_chart <- function(chart_name, bin_number, singled_out = 0, bin_labels) {
  
  x <- seq(bin_number)
  
  # Get the correct colors
  colors <- colorBin('RdYlGn', domain = seq(0, length(x)), bins = bin_number)
  
  hc <- highchart(type = 'chart') %>%
    hc_chart(
      backgroundColor = NULL) %>%
    hc_xAxis(visible = FALSE) %>% #TRUE,
             #title = list(text = chart_name,
            #              rotation = 0),
            # labels = FALSE,
            # tickLength = 0) %>%
    hc_yAxis(visible = FALSE,
             min = 0,
             max = bin_number) %>%
    hc_plotOptions(series = list (stacking = 'normal'),
                   bar = list(pointWidth = 30, animation = FALSE, 
                              dataLabels = list(enabled = T, 
                                                formatter = JS("function(){return this.point.label}"), 
                                                color = 'black', 
                                                #size = '15px',
                                                inside = T))) %>%
    hc_tooltip(enabled = FALSE) %>%
    hc_legend(enabled = FALSE)
  
  for (i in seq(length(x), 1)) {
    
    d <- data.frame(y = 1,
                    label = bin_labels[i])
    
    # Color all bins grey unless it is singled out or all are shown
    if (i == singled_out || singled_out == 0) {
      
      # The dark red and green on 2 and 3 bins isn't dark enough so do it manually
      if(bin_number == 2 || bin_number == 3) {
        
        # Dark Green
        if (i ==  bin_number)
          d$color <- "#1A9850"
        # Dark Red
        else if (i == 1)
          d$color <- "#D73027"
        else
          d$color <- colors(i -0.5)
      }
      else
        d$color <- colors(i-0.5)
    }
    # Grey
    else
      d$color <- '#D1D1D1'
    
    
    hc <- hc %>% hc_add_series(d, type = 'bar')
  }
  
  hc
  
}

sch_marker <- function(hospitals, search_input) {
  observeEvent(input$search_input, ignoreNULL = FALSE, {
    
    hos <- hospitals[hospitals$NAME %in% search_input, ]
    
    leafletProxy('mymap') %>%
      clearPopups() %>%
      addPopups(
        lng = hos$Longitude,
        lat = hos$Latitude,
        popup = hos$popup_content
      )
    addMarkers(
      layerId = input$search_input,
      group = paste('hospitals_', (hos[ which(hos$NAME==search_input),])$lead),
      data = hos[hos$NAME == search_input, ],
      lng = ~Longitude,
      lat = ~Latitude,
      # popup = ~popup_content,
      icon = paste((hospitals[ which(hos$NAME==search_input),])$lead,'_icon')
    ) %>%
      output$text_name_sch<- renderText({(hos$NAME)})
    output$text_beds_sch<- renderText({hos$beds})
  })
}