# !diagnostics off

function(input, output, session) {
  observe({
    #filter dataframe by input year
    dist_crime = reactive({
      if (input$inspect == T) {
        dist %>%
          filter(year == input$year) %>%
          group_by(district) %>%
          summarise(crime_count = n()) %>%
          mutate(
            total = sum(crime_count),
            crime_ratio = crime_count / total,
            crime_count = ifelse(district %in% input$district_map,
                                 crime_count, 0)
          )
      } else{
        dist %>%
          filter(year == input$year) %>%
          group_by(district) %>%
          summarise(crime_count = n()) %>%
          mutate(
            total = sum(crime_count),
            crime_ratio = crime_count / total,
            crime_count = crime_count
          )
      }
    })
    
    #filter crime with shooting
    dist_shooting = reactive({
      dist %>%
        filter(year == input$year &
                 shooting == 1)
    })
    
    #match order of district with geojson file
    dist_ordered = reactive({
      dist_crime()[order(match(dist_crime()$district, bostonDist$DISTRICT)),]
    })
    
    #filter dataframe for map
    df_select = reactive({
      date_trend %>%
        filter(date >= input$date[1] &
                 date <= input$date[2] &
                 district %in% input$district)
    })
    
    #filter dataframe for ratio graph
    dist_ratio = reactive({
      date_trend %>%
        filter(date >= input$date[1] &
                 date <= input$date[2]) %>%
        group_by(year, district) %>%
        summarise(crime_count = n()) %>%
        mutate(total = sum(crime_count),
               crime_ratio = crime_count / total)
    })
    
    #get numbers of crime for different type of input time
    df_date = reactive({
      if (input$Byother) {
        if (input$SelectBy == 'year') {
          df_select() %>%
            group_by(district, year) %>%
            summarise(date_crime_count = n())
        } else if (input$SelectBy == 'month') {
          df_select() %>%
            group_by(district, month) %>%
            summarise(date_crime_count = n())
        } else if (input$SelectBy == 'day_of_week') {
          df_select() %>%
            group_by(district, day_of_week) %>%
            summarise(date_crime_count = n())
        } else if (input$SelectBy == 'hour') {
          df_select() %>%
            group_by(district, hour) %>%
            summarise(date_crime_count = n())
        } else{
          dist_ratio()
        }
      } else{
        df_select() %>%
          group_by(date, district) %>%
          summarise(date_crime_count = n())
      }
    })
    
    #output table for time series tab
    output$date_table = DT::renderDataTable({
      df_date()
    })
    
    #plot Time series
    output$date_line = renderPlotly({
      if (input$Byother) {
        if (input$SelectBy == 'year') {
          df_date() %>%
            ggplot(aes(x = year, y = date_crime_count)) +
            geom_line(aes(color = district))
        } else if (input$SelectBy == 'month') {
          df_date() %>%
            ggplot(aes(x = month, y = date_crime_count)) +
            geom_line(aes(color = district))
        } else if (input$SelectBy == 'day_of_week') {
          df_date() %>%
            ungroup(district) %>%
            ggplot(aes(x = day_of_week, y = date_crime_count)) +
            geom_line(aes(colour = district, group = district))
        } else if (input$SelectBy == 'hour') {
          df_date() %>%
            ungroup(district) %>%
            ggplot(aes(x = hour, y = date_crime_count)) +
            geom_line(aes(color = district, group = district))
        } else{
          df_date() %>%
            ggplot(aes(x = year, y = crime_ratio)) +
            geom_line(aes(color = district))
        }
      } else{
        df_date() %>%
          ggplot(aes(x = date, y = date_crime_count)) +
          geom_line(aes(color = district), se = F)
      }
    })
    
    #set label for each district in map
    labels = reactive({
      paste(
        "<h4>",
        paste('District: ', dist_ordered()$district),
        "</h4>",
        "<b>",
        "Crime Ratio: ",
        round(dist_ordered()$crime_ratio, digits = 3),
        '</b>',
        '<br>',
        "<b>",
        "Crime Count: ",
        dist_ordered()$crime_count,
        '</b>',
        sep = ''
      )
    })
    
    #render map
    
    #set reactive function for map
    dist_map = reactive({
      bins = c(1000, 2000, 3000, 4000, 5000, 6000, 7000, 10000, 13000, Inf)
      
      pal = colorBin('YlOrRd',
                     bins = bins)
      
      leaflet() %>%
        setView(lng = -71.0589, lat = 42.3601, 10.1) %>%
        addProviderTiles(providers$Stamen.Toner) %>%
        addPolygons(
          data = bostonDist,
          weight = 1,
          smoothFactor = 0.5,
          fillOpacity = 0.8,
          color = "white",
          dashArray = "3",
          fillColor = pal(dist_ordered()$crime_count),
          highlightOptions = highlightOptions(
            color = "#FF0000",
            weight = 5,
            bringToFront = TRUE
          ),
          label = lapply(labels(), HTML)
        ) %>%
        addLegend(
          pal = pal,
          title = 'Numbers of Crime',
          values = round(dist_ordered()$crime_count),
          opacity = 1,
          position = 'topright'
        )
    })
    
    output$mymap1 = renderLeaflet({
      if (input$shoot) {
        dist_map() %>%
          addMarkers(
            data = dist_shooting(),
            lng = dist_shooting()$long,
            lat = dist_shooting()$lat,
            icon = icon('thumbs-down'),
            popup = paste(
              dist_shooting()$occurred_on_date,
              dist_shooting()$offense_code_group,
              sep = ' : '
            )
          )
      } else{
        dist_map()
      }
    })
    
    #map tab table output
    output$data_table = DT::renderDataTable({
      dist_ordered() %>%
        mutate(crime_ratio = round(crime_ratio, digits = 3)) %>%
        arrange(desc(crime_ratio))
    })
    
    #infobox for max crime ratio
    output$maxBox = renderInfoBox({
      max_ratio = max(dist_ordered()$crime_ratio)
      max_district = dist_ordered() %>%
        filter(crime_ratio == max_ratio) %>%
        select(district)
      infoBox(
        strong(max_district),
        round(max_ratio, digits = 3),
        color = 'red',
        icon = icon("thumbs-down")
      )
    })
    
    #infobox for min crime ratio
    output$minBox = renderInfoBox({
      min_ratio = min(dist_ordered()$crime_ratio)
      min_district = dist_ordered() %>%
        filter(crime_ratio == min_ratio) %>%
        select(district)
      infoBox(
        strong(min_district),
        round(min_ratio, digits = 3),
        color = 'blue',
        icon = icon("thumbs-up")
      )
    })
    
    #infobox for avg. crime count
    output$avgBox = renderInfoBox({
      avg_count = mean(dist_ordered()$crime_count)
      infoBox(strong(
        'Avg. Numbers of Crime',
        round(avg_count, digits = 0),
        color = 'blue',
        icon = icon("calculator")
      ))
    })
    
    offense_word = reactive({
      dist_word %>%
        filter(district == input$district_word) %>%
        group_by(district, offense_code_group) %>%
        summarise(count = n()) %>%
        arrange(desc(count)) %>%
        head(input$max)
    })
    
    output$word = renderPlot({
      wordcloud(
        words = offense_word()$offense_code_group,
        freq = offense_word()$count,
        colors = colorRampPalette(brewer.pal(9, "YlOrRd"))(32),
        random.order = F,
        rot.per = 0.3,
        scale = c(2.8, .3),
        random.color = F,
        max.words = 70
      )
      # wordcloud2 will also be useful
      # wordcloud2(offense_word()[,c(2, 3)],
      #            minRotation = -pi/6, maxRotation = -pi/6,
      #            color = "random-light", backgroundColor = "red")
    })
    
    output$hist = renderPlotly({
      offense_word() %>%
        config(displayModeBar = FALSE) %>%
        config(showLink = FALSE) %>%
        mutate(Offense = reorder(offense_code_group, count)) %>%
        ggplot(aes(Offense, count)) +
        geom_bar(stat = "identity",
                 fill = "white",
                 color = 'red') +
        ggtitle("Numbers of Crime Type") +
        xlab("Crime Type") +
        ylab("Numbers") +
        guides(fill = guide_legend(title = "Numbers")) +
        coord_flip()
    })
  })
}