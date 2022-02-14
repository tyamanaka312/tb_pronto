# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Build the dataframes for 30 HBSx to display annual and provisional monthly or quarterly
# notifications using the echarts4r package (based on ECharts)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# ----------------  plot title  ---------------------#

output$hbc_heading <- renderText({
  if(input$indicator == "provisional") {
    paste0("Provisional* number of people with new or relapse episodes of TB notified per month")
    
    } else {
      paste0("Number of people with new or relapse episodes of TB notified per year in 30 TB high burden countries, 2016-2020")
   
    }
    })

# Generate a dataframe for provisional monthly data
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

lapply(y, function(i) {  
  output[[paste0("hbc_plot_",i)]] <- renderEcharts4r({

  if(input$indicator == "provisional") {
  json_url <- "https://extranet.who.int/tme/generateJSON.asp"
  country_list <- country_list_json()$countries %>%
    select(iso2, country) %>%
    arrange(country)

  # for (i in y) {
    url <- paste0(json_url, "?ds=c_newinc&iso2=", i)
    json <- fromJSON(readLines(url, warn = FALSE, encoding = 'UTF-8'))
    
    # store provisional monthly data
    c_newinc_prov <- json[[1]]
    c_newinc_prov <- c_newinc_prov %>%
      mutate(iso2=paste(i)) %>%
      inner_join(country_list,by=c('iso2'))
      
  # Put the provisional data into a variable to make subsequent code easier to read
  data_to_plot <- c_newinc_prov
  
  # Find out whether we have monthly or quarterly data
  frequency <- as.numeric(min(data_to_plot$report_frequency))
  period_prefix <- ifelse(frequency == 71, "q_", "m_")
  period_name  <- ifelse(frequency == 71, "Quarter", "Month")
  
  # Check if some years have a different frequency
  if (min(data_to_plot$report_frequency) != max(data_to_plot$report_frequency)) {
    
    # Convert any years with quarterly data to monthly equivalent
    data_to_plot <- data_to_plot %>%
      mutate(m_01 = ifelse(report_frequency == 71, round(q_1/3), m_01),
             m_02 = ifelse(report_frequency == 71, round(q_1/3), m_02),
             m_03 = ifelse(report_frequency == 71, round(q_1/3), m_03),
             
             m_04 = ifelse(report_frequency == 71, round(q_2/3), m_04),
             m_05 = ifelse(report_frequency == 71, round(q_2/3), m_05),
             m_06 = ifelse(report_frequency == 71, round(q_2/3), m_06),
             
             m_07 = ifelse(report_frequency == 71, round(q_3/3), m_07),
             m_08 = ifelse(report_frequency == 71, round(q_3/3), m_08),
             m_09 = ifelse(report_frequency == 71, round(q_3/3), m_09),
             
             m_10 = ifelse(report_frequency == 71, round(q_4/3), m_10),
             m_11 = ifelse(report_frequency == 71, round(q_4/3), m_11),
             m_12 = ifelse(report_frequency == 71, round(q_4/3), m_12),
             
             year = ifelse(report_frequency == 71, paste(year, "(quarterly averaged as monthly)"), year)
      )
    
  }
  
  # Get the total notification for 2019 (final pre-pandemic year) and calculate the average monthly or quarterly
  url <- paste0(json_url, "?ds=c_newinc&iso2=", i)
  
  json <- fromJSON(readLines(url, warn = FALSE, encoding = 'UTF-8'))
  
  prepandemic_year <- json$c_newinc_year %>%
    filter(year == 2019) %>%
    select(c_newinc)
  
  prepandemic_year_avge <- ifelse(frequency == 71, prepandemic_year/4, prepandemic_year/12) %>% as.numeric()
  
  # Build the chart
  # Refer to https://echarts.apache.org/en/option.html for additional properties
  # noting that instead of using Javascript dot notation to set properties such as yAxis.axisTick.show = true
  # have to use an R list such as axisTick = list(show = FALSE)
  
  prov_chart <-  data_to_plot %>%
    
    # Flip to long format
    pivot_longer(cols = starts_with(period_prefix),
                 names_to = "period",
                 # Add "0?" to the names_prefix regex to remove any leading zeros
                 names_prefix = paste0(period_prefix, "0?"),
                 values_to = "c_newinc") %>%
    
    select(year, period, c_newinc) %>%
    
    # Grouping by year makes Echarts show each year as a separate line (named data series)
    group_by(year) %>%
    
    # Build the chart object
    e_charts(x = period) %>%
    
    e_line(serie = c_newinc,
           symbolSize = 12) %>%
    
    e_title(text = paste(data_to_plot[1,21]))   %>%
    
    # params.seriesName is the year because the dataframe is grouped by year
    e_tooltip(formatter = JS(paste0("
                                  function(params){
                                    return(
                                    params.seriesName + ' ' + '", tolower(period_name), " ' + params.value[0]",
                                    "+ '<br />Number notified: ' + params.value[1] )
                                  }
                                "))) %>%
    
    e_legend(TRUE, right = '15%',top = '7%') %>%
    
    # Adjust x axis properties
    e_x_axis(name = period_name,
             nameLocation = 'middle',
             nameTextStyle = list(fontSize  = 14,
                                  padding = 14) ) %>%
    
    # Make sure large numbers are not truncated in exes labels
    e_grid(containLabel = TRUE) %>%
    
    # Add a horizontal line to show pre-pandemic year's average notification by quarter/month
    e_mark_line(data = list(yAxis = prepandemic_year_avge),
                title = "2019\nAverage",
                
                # suppress mouseover tooltip:
                silent = TRUE)
  
  # Before defining the y-axis properties
  # Calculate the maximum value of all the points and the pre-pandemic year average
  max_value <- data_to_plot %>%
    select(starts_with(period_prefix)) %>%
    max(na.rm = TRUE) %>%
    max(c(prepandemic_year_avge), na.rm = TRUE)
  
  if (max_value == prepandemic_year_avge) {
    
    # The automatic y-axis scale will not show the pre-pandemic year average, so
    # extend the scale manually
    
    # Want to calculate the next next convenient highest multiple of 10
    # one order of magnitude below that of the previous year's average
    # e.g for 115 we want 120 (next multiple of 10), for 1568 we want 1600 (next multiple of 100), etc.
    
    order_of_magnitude <- 10^floor(log10(max_value) - 1)
    
    max_value <- ceiling(max_value/order_of_magnitude) * order_of_magnitude
    
    prov_chart <- prov_chart %>%
      
      e_y_axis(axisLine = list(show = FALSE),
               axisTick = list(show = FALSE),
               # over-ride the scales generated automatically because the
               # previous year average doesn't appear if it is higher than any of the
               # monthly or quarterly data points
               min = 0,
               max = max_value)
    
  } else {
    
    # The maximum value comes from the data points so let eCharts figure out the scale
    prov_chart <- prov_chart %>%
      
      e_y_axis(axisLine = list(show = FALSE),
               axisTick = list(show = FALSE))
    
  }
  
  } else {
    
# Generate a dataframe for annual data
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# lapply(y, function(i) {  
  # output[[paste0("annual_plot_",i)]] <- renderEcharts4r({
    # output$annual_plot <- renderEcharts4r({
  json_url <- "https://extranet.who.int/tme/generateJSON.asp"
  country_list <- country_list_json()$countries %>%
    select(iso2, country) %>%
    arrange(country)
  
  # for (i in y()) {
    url <- paste0(json_url, "?ds=c_newinc&iso2=", i)
    json <- fromJSON(readLines(url, warn = FALSE, encoding = 'UTF-8'))
    
    # store provisional monthly data
    c_newinc_annual <- json[[2]]
    c_newinc_annual <- c_newinc_annual %>%
      mutate(iso2=paste(i)) %>%
      mutate(year = as.character(year)) %>%
      inner_join(country_list,by=c('iso2'))
    
    p <- c_newinc_annual %>%
      subset(!is.na(c_newinc)) %>%
      e_charts(x = year) %>%
      
      e_line(serie = c_newinc,
             symbolSize = 12) %>%
      
      e_title(text = paste(c_newinc_annual[1,4]))   %>%
      
      e_legend(FALSE) %>%
      
      # Adjust x and y axis properties
      e_y_axis(axisLine = list(show = FALSE),
               axisTick = list(show = FALSE)) %>%
      
      e_x_axis(name = "Year",
               nameLocation = 'middle',
               nameTextStyle = list(fontSize  = 14,
                                    padding = 14)) %>%
      
      # Make sure large numbers are not truncated in exes labels
      e_grid(containLabel = TRUE) %>%
      
      e_tooltip(formatter = JS("
                                  function(params){
                                    return(
                                    params.value[0] + '<br />Number notified: ' + params.value[1])
                                  }
                                ")) %>%
      e_group("grp") %>%  # assign group
      e_connect_group("grp")
    p
    
  }
  })
  })  

# #---------------- for downloading plot ---------------------#
# 
# lapply(y, function(i) {  
#   output[[paste0("dl_hbc_plot_",i)]] <- reactive({
#     
#     if(input$indicator == "provisional") {
#       json_url <- "https://extranet.who.int/tme/generateJSON.asp"
#       country_list <- country_list_json()$countries %>%
#         select(iso2, country) %>%
#         arrange(country)
#       
#       # for (i in y) {
#       url <- paste0(json_url, "?ds=c_newinc&iso2=", i)
#       json <- fromJSON(readLines(url, warn = FALSE, encoding = 'UTF-8'))
#       
#       # store provisional monthly data
#       c_newinc_prov <- json[[1]]
#       c_newinc_prov <- c_newinc_prov %>%
#         mutate(iso2=paste(i)) %>%
#         inner_join(country_list,by=c('iso2'))
#       
#       # Put the provisional data into a variable to make subsequent code easier to read
#       data_to_plot <- c_newinc_prov
#       
#       # Find out whether we have monthly or quarterly data
#       frequency <- as.numeric(min(data_to_plot$report_frequency))
#       period_prefix <- ifelse(frequency == 71, "q_", "m_")
#       period_name  <- ifelse(frequency == 71, "Quarter", "Month")
#       
#       # Check if some years have a different frequency
#       if (min(data_to_plot$report_frequency) != max(data_to_plot$report_frequency)) {
#         
#         # Convert any years with quarterly data to monthly equivalent
#         data_to_plot <- data_to_plot %>%
#           mutate(m_01 = ifelse(report_frequency == 71, round(q_1/3), m_01),
#                  m_02 = ifelse(report_frequency == 71, round(q_1/3), m_02),
#                  m_03 = ifelse(report_frequency == 71, round(q_1/3), m_03),
#                  
#                  m_04 = ifelse(report_frequency == 71, round(q_2/3), m_04),
#                  m_05 = ifelse(report_frequency == 71, round(q_2/3), m_05),
#                  m_06 = ifelse(report_frequency == 71, round(q_2/3), m_06),
#                  
#                  m_07 = ifelse(report_frequency == 71, round(q_3/3), m_07),
#                  m_08 = ifelse(report_frequency == 71, round(q_3/3), m_08),
#                  m_09 = ifelse(report_frequency == 71, round(q_3/3), m_09),
#                  
#                  m_10 = ifelse(report_frequency == 71, round(q_4/3), m_10),
#                  m_11 = ifelse(report_frequency == 71, round(q_4/3), m_11),
#                  m_12 = ifelse(report_frequency == 71, round(q_4/3), m_12),
#                  
#                  year = ifelse(report_frequency == 71, paste(year, "(quarterly averaged as monthly)"), year)
#           )
#         
#       }
#       
#       # Get the total notification for 2019 (final pre-pandemic year) and calculate the average monthly or quarterly
#       url <- paste0(json_url, "?ds=c_newinc&iso2=", i)
#       
#       json <- fromJSON(readLines(url, warn = FALSE, encoding = 'UTF-8'))
#       
#       prepandemic_year <- json$c_newinc_year %>%
#         filter(year == 2019) %>%
#         select(c_newinc)
#       
#       prepandemic_year_avge <- ifelse(frequency == 71, prepandemic_year/4, prepandemic_year/12) %>% as.numeric()
#       
#       # Build the chart
#       # Refer to https://echarts.apache.org/en/option.html for additional properties
#       # noting that instead of using Javascript dot notation to set properties such as yAxis.axisTick.show = true
#       # have to use an R list such as axisTick = list(show = FALSE)
#       
#       prov_chart <-  data_to_plot %>%
#         
#         # Flip to long format
#         pivot_longer(cols = starts_with(period_prefix),
#                      names_to = "period",
#                      # Add "0?" to the names_prefix regex to remove any leading zeros
#                      names_prefix = paste0(period_prefix, "0?"),
#                      values_to = "c_newinc") %>%
#         
#         select(year, period, c_newinc) %>%
#         
#         # Grouping by year makes Echarts show each year as a separate line (named data series)
#         group_by(year) %>%
#         
#         # Build the chart object
#         e_charts(x = period) %>%
#         
#         e_line(serie = c_newinc,
#                symbolSize = 12) %>%
#         
#         e_title(text = paste(data_to_plot[1,21]))   %>%
#         
#         # params.seriesName is the year because the dataframe is grouped by year
#         e_tooltip(formatter = JS(paste0("
#                                   function(params){
#                                     return(
#                                     params.seriesName + ' ' + '", tolower(period_name), " ' + params.value[0]",
#                                         "+ '<br />Number notified: ' + params.value[1] )
#                                   }
#                                 "))) %>%
#         
#         e_legend(TRUE, right = '15%',top = '7%') %>%
#         
#         # Adjust x axis properties
#         e_x_axis(name = period_name,
#                  nameLocation = 'middle',
#                  nameTextStyle = list(fontSize  = 14,
#                                       padding = 14) ) %>%
#         
#         # Make sure large numbers are not truncated in exes labels
#         e_grid(containLabel = TRUE) %>%
#         
#         # Add a horizontal line to show pre-pandemic year's average notification by quarter/month
#         e_mark_line(data = list(yAxis = prepandemic_year_avge),
#                     title = "2019\nAverage",
#                     
#                     # suppress mouseover tooltip:
#                     silent = TRUE)
#       
#       # Before defining the y-axis properties
#       # Calculate the maximum value of all the points and the pre-pandemic year average
#       max_value <- data_to_plot %>%
#         select(starts_with(period_prefix)) %>%
#         max(na.rm = TRUE) %>%
#         max(c(prepandemic_year_avge), na.rm = TRUE)
#       
#       if (max_value == prepandemic_year_avge) {
#         
#         # The automatic y-axis scale will not show the pre-pandemic year average, so
#         # extend the scale manually
#         
#         # Want to calculate the next next convenient highest multiple of 10
#         # one order of magnitude below that of the previous year's average
#         # e.g for 115 we want 120 (next multiple of 10), for 1568 we want 1600 (next multiple of 100), etc.
#         
#         order_of_magnitude <- 10^floor(log10(max_value) - 1)
#         
#         max_value <- ceiling(max_value/order_of_magnitude) * order_of_magnitude
#         
#         prov_chart <- prov_chart %>%
#           
#           e_y_axis(axisLine = list(show = FALSE),
#                    axisTick = list(show = FALSE),
#                    # over-ride the scales generated automatically because the
#                    # previous year average doesn't appear if it is higher than any of the
#                    # monthly or quarterly data points
#                    min = 0,
#                    max = max_value)
#         
#       } else {
#         
#         # The maximum value comes from the data points so let eCharts figure out the scale
#         prov_chart <- prov_chart %>%
#           
#           e_y_axis(axisLine = list(show = FALSE),
#                    axisTick = list(show = FALSE))
#         
#       }
#       
#     } else {
#       
#       # Generate a dataframe for annual data
#       # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#       # lapply(y, function(i) {  
#       # output[[paste0("annual_plot_",i)]] <- renderEcharts4r({
#       # output$annual_plot <- renderEcharts4r({
#       json_url <- "https://extranet.who.int/tme/generateJSON.asp"
#       country_list <- country_list_json()$countries %>%
#         select(iso2, country) %>%
#         arrange(country)
#       
#       # for (i in y()) {
#       url <- paste0(json_url, "?ds=c_newinc&iso2=", i)
#       json <- fromJSON(readLines(url, warn = FALSE, encoding = 'UTF-8'))
#       
#       # store provisional monthly data
#       c_newinc_annual <- json[[2]]
#       c_newinc_annual <- c_newinc_annual %>%
#         mutate(iso2=paste(i)) %>%
#         mutate(year = as.character(year)) %>%
#         inner_join(country_list,by=c('iso2'))
#       
#       p <- c_newinc_annual %>%
#         subset(!is.na(c_newinc)) %>%
#         e_charts(x = year) %>%
#         
#         e_line(serie = c_newinc,
#                symbolSize = 12) %>%
#         
#         e_title(text = paste(c_newinc_annual[1,4]))   %>%
#         
#         e_legend(FALSE) %>%
#         
#         # Adjust x and y axis properties
#         e_y_axis(axisLine = list(show = FALSE),
#                  axisTick = list(show = FALSE)) %>%
#         
#         e_x_axis(name = "Year",
#                  nameLocation = 'middle',
#                  nameTextStyle = list(fontSize  = 14,
#                                       padding = 14)) %>%
#         
#         # Make sure large numbers are not truncated in exes labels
#         e_grid(containLabel = TRUE) %>%
#         
#         e_tooltip(formatter = JS("
#                                   function(params){
#                                     return(
#                                     params.value[0] + '<br />Number notified: ' + params.value[1])
#                                   }
#                                 ")) %>%
#         e_group("grp") %>%  # assign group
#         e_connect_group("grp")
#       p
#       
#     }
#   })
# })  
# 
# plotInput <- reactive({
#   e_arrange(dl_hbc_plot_PH(), dl_hbc_plot_AO(), dl_hbc_plot_PG(), rows = 2, cols = 2)
#     # p <- do.call("e_arrange",mget(ls(pattern = "hbc_))plot_*")))
# })
# 
# output$dl_30hbc <- downloadHandler(
#   filename = function() { paste("30hbc1_",Sys.Date(),".png",sep="") },
#   content = function(file) {
#     png(file)
#     dl_hbc_plot_AO()
#     dev.off()
#   }#,
#   # contentType = 'image/png'
# )
# 
