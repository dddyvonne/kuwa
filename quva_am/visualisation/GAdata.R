library(googleAnalyticsR)
#authenticate
ga_auth()
#get your accounts
account_list <- google_analytics_account_list()

#pick a profile with data to query - Chipotle
ga_id <- account_list[30,'viewId']
ga_id



###
#Advanced pulling with filters
###

## create filters on metrics
mf <- met_filter("bounces", "GREATER_THAN", 0)
mf2 <- met_filter("sessions", "GREATER", 2)
## create filters on dimensions
df <- dim_filter("source","BEGINS_WITH","1",not = TRUE)
df2 <- dim_filter("source","BEGINS_WITH","a",not = TRUE)
## construct filter objects
fc2 <- filter_clause_ga4(list(df, df2), operator = "AND")
fc <- filter_clause_ga4(list(mf, mf2), operator = "AND")
## make v4 request
ga_data1 <- google_analytics_4(ga_id,
                               date_range = c("2015-07-30","2015-10-01"),
                               dimensions=c('source','medium'),
                               metrics = c('sessions','bounces'),
                               met_filters = fc,
                               dim_filters = fc2,
                               filtersExpression = "ga:source!=(direct)")
