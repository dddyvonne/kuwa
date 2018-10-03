library(shiny)
library(shinyjs)
library(leaflet)
library(data.table)
library(htmltools)
library(highcharter)
library(dplyr)
library(tools)

hospitals <- fread('data/quva_dashboardv1.csv', data.table = F)
bins <- fread('data/bin_number_groups.csv',data.table = F)
income_bins <- fread('data/income_bins.csv',data.table = F)
beds_bins <- fread('data/beds_bin.csv', data.table = F)
ops_bins <- fread('data/ops_bin.csv', data.table = F)

create_labs <- function(data) {
  
  data <- as.character(round(data))
  data <- paste(data, lead(data, default = ''), sep = '-')
  data
  
}

income_labs <- create_labs(c(0, 48.525, 80.564, 141.836,176.893))
bed_labs <- create_labs(c(3, 100, 150, 254.456, 1532.489, 2409.971))
opclaimed_labs <- create_labs(c(0,1001, 5000, 9140.479, 35608.014, 44765.146, 58625.375))

# Leads
hospitals$lead <- sample(c('A', 'B', 'C', 'D'), 5958, replace = T, rep(0.25, 4))
hospitals <- merge(x = hospitals, y = beds_bins[ , c("beds_bucket", "beds_bin")], by = "beds_bucket", all.x=TRUE)
hospitals <- merge(x = hospitals, y = income_bins[, c("income_bucket","income_bin")], by = "income_bucket", all.x=TRUE)
hospitals <- merge(x = hospitals, y = ops_bins[, c("Num_Of_OP_Claims_Total_bucket", "ops_bin")], by = "Num_Of_OP_Claims_Total_bucket", all.x=TRUE)

# FAKE NEWS
#hospitals$income <- sample(seq(3), 5958, replace = T)
#hospitals$beds <- sample(seq(3), 5958, replace = T)

# Adjust Hospital Names
hospitals$NAME <- hospitals$Hospital_Name_x

# Hospital Map Icons
A_icon <- makeIcon(
  iconUrl = 'hospital_A.svg',
  iconWidth = 15,
  iconHeight = 15
)

B_icon <- makeIcon(
  iconUrl = 'hospital_B.svg',
  iconWidth = 15,
  iconHeight = 15
)

C_icon <- makeIcon(
  iconUrl = 'hospital_C.svg',
  iconWidth = 15,
  iconHeight = 15
)

D_icon <- makeIcon(
  iconUrl = 'hospital_D.svg',
  iconWidth = 15,
  iconHeight = 15
)

# Popup content
# NAME ADDRESS AND SALES TEAM
hospitals$popup_content <- paste0('<b>', hospitals$NAME, '</b><br/>', toTitleCase(tolower(hospitals$City)), ', ', hospitals$State_y, '</br>', 'Sales Team ALPHA')

make_chart <- function(chart_name, bin_number, singled_out = 0, bin_labels) {
  
  x <- seq(bin_number)
  
  # Get the correct colors
  colors <- colorBin('RdYlGn', domain = seq(0, length(x)), bins = bin_number)
  
  hc <- highchart(type = 'chart') %>%
    hc_chart(
      backgroundColor = NULL) %>%
    hc_xAxis(visible = TRUE,
             title = list(text = chart_name,
                          rotation = 0),
             labels = FALSE,
             tickLength = 0) %>%
    hc_yAxis(visible = FALSE,
             min = 0,
             max = bin_number) %>%
    hc_plotOptions(series = list (stacking = 'normal'),
                   bar = list(pointWidth = 30, animation = FALSE, 
                              dataLabels = list(enabled = T, 
                                                formatter = JS("function() {return this.point.label}"), 
                                                color = 'black', 
                                                size = '15px',
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
