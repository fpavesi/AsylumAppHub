server <- function(input, output, session) {
  
#-------------------------------------------------------------------------------
  # first reactive dataset
  df_sub_1 = reactiveVal(NULL)
  observe({
    
    df_sub_1(
     asylum %>% 
        filter(geo == input$geol & sex == input$sex & age == input$age &
                 time == input$timefra) %>% 
       filter(!citizen %in% c("Total", "European Union - 27 countries (from 2020)",                        
                              "European Union - 28 countries (2013-2020)", "Extra-EU27 (from 2020)",
                              "Extra-EU28 (2013-2020)"))
    )
    
  })
  
  # first slider update
  observe({
    updateSliderInput(
      session = session,
      inputId = "threshold",
      min = min(df_sub_1()$Applications, na.rm = T)+1,
      max = max(df_sub_1()$Applications, na.rm = T),
      value = c(min(df_sub_1()$Applications, na.rm = T)+0.05*max(df_sub_1()$Applications, na.rm = T),
                max(df_sub_1()$Applications, na.rm = T))
    )
  })
  
  
  #update first reactive dataset
  df_sub_11 = reactiveVal(NULL)
  observe({
    df_sub_11(
      df_sub_1() %>% 
        filter(Total_first >= input$threshold[[1]],
               Total_first <= input$threshold[[2]]) %>% 
        mutate(RR_f = Total_pos_first/Total_first,
               RR_d = Total_pos_def/Total_first)
    )
  })
  
  # first istance barplot
  output$histogram1 = renderPlotly({
    
    plot_ly() %>%
      add_trace(data = df_sub_11(),
                name = "Number of decisions",
                text = "Number of decisions",
                y = ~Total_first,
                x = ~citizen,
                type = "bar",
                marker = list(color = "darkorange",
                              line = list(color = "black",
                                          width = 0.5)),
                hovertemplate = paste("Number of decisions on %{x}: <br> %{y}")) %>% 
      add_trace(data = df_sub_11(),
                name = "Total asylums granted at first istance",
                y = ~ Total_pos_first,
                x = ~ citizen,
                type = "bar",
                color = I("green"),
                hovertemplate = paste("Total number of asylums granted to %{x}: %{y}<br>",
                                      "Of which for: <br>", 
                                      " - Geneve convention: ", df_sub_11()$Genconv_first, "(", round(df_sub_11()$Genconv_first/df_sub_11()$Total_pos_first, digits = 4)*100, "%)<br>",
                                      " - Humanitarian status: ", df_sub_11()$Humstat_first, "(", round(df_sub_11()$Humstat_first/df_sub_11()$Total_pos_first, digits = 4)*100, "%)<br>",
                                      " - Subsidiary protection: ", df_sub_11()$Sub_prot_first, "(", round(df_sub_11()$Sub_prot_first/df_sub_11()$Total_pos_first, digits = 4)*100, "%)<br>")) %>%
      add_trace(data = df_sub_11(),
                name = "Recognition rate at first istance",
                y = ~ RR_f,
                x = ~ citizen,
                type = "scatter",
                mode = "markers",
                color = I("darkred"),
                yaxis = "y2",
                size = 5) %>% 
      layout(xaxis = list(categoryorder = "array",
                          categoryarray = df_sub_11()$citizen[order(df_sub_11()$Total_first, decreasing = T)])) %>% 
      layout(plot_bgcolor='white', 
             barmode = "overlay",
             title = paste("Recognition rates at first istance of", input$geol, "in", input$timefra),
             legend = list(title = list(text = "<b> Legend </b>"),
                           x = 1.1,
                           y = 0.7),
             xaxis = list( 
               title = list(title = "<b> Asylum seekers citizenship </b>"),
               zerolinecolor = 'black', 
               zerolinewidth = 2, 
               gridcolor = 'ffff'), 
             yaxis = list( 
               title = "",
               zerolinecolor = 'black', 
               zerolinewidth = 2, 
               gridcolor = 'grey'),
             yaxis2 = list(
               zerolinecolor = "#B22222",
               gridcolor = "#B22222",
               tickvals = list(0, 0.5, 1),
               gridwidth = 2,
               zerolinewidth = 2,
               overlaying = "y",
               side = "right",
               title = "<b> Recognition rate </b>",
               tickfont = list(color = "#B22222", size = 14),
               tickangle = 270
               ),
             margin = list(
               l = 0,
               r = 0,
               b = 150,
               t = 30,
               pad = 20)
             ) 
    
    
    
    
  })
  
  output$rrfirst = renderText({
    paste("Overall RR by first istance of", input$geol, "in", input$timefra, ": ", round(sum(df_sub_11()$Total_pos_first)/sum(df_sub_11()$Total_first), digits = 4)*100, "%")
  })
  
  
  
  # definitive decision barplot
  output$histogram2 = renderPlotly({

    plot_ly() %>%
      add_trace(data = df_sub_11(),
                name = "Number of decisions",
                text = "Number of decisions",
                y = ~Total_def,
                x = ~citizen,
                type = "bar",
                marker = list(color = "darkorange",
                              line = list(color = "black",
                                          width = 0.5)),
                hovertemplate = paste("Number of decisions on %{x}: <br> %{y}")) %>% 
      add_trace(data = df_sub_11(),
                name = "Total asylum granted by definitive decision",
                y = ~ Total_pos_def,
                x = ~ citizen,
                type = "bar",
                color = I("green"),
                hovertemplate = paste("Total number of asylums granted to %{x}: %{y}<br>",
                                      "Of which for: <br>", 
                                      " - Geneve convention: ", df_sub_11()$Genconv_def, "(", round(df_sub_11()$Genconv_def/df_sub_11()$Total_pos_def, digits = 4)*100, "%)<br>",
                                      " - Humanitarian status: ", df_sub_11()$Humstat_def, "(", round(df_sub_11()$Humstat_def/df_sub_11()$Total_pos_def, digits = 4)*100, "%)<br>",
                                      " - Subsidiary protection: ", df_sub_11()$Sub_prot_def, "(", round(df_sub_11()$Sub_prot_def/df_sub_11()$Total_pos_def, digits = 4)*100, "%)<br>")) %>%
      add_trace(data = df_sub_11(),
                name = "Recognition rate by definitive decision",
                y = ~ RR_d,
                x = ~ citizen,
                type = "scatter",
                mode = "markers",
                color = I("darkred"),
                yaxis = "y2",
                size = 5) %>% 
      layout(xaxis = list(categoryorder = "array",
                          categoryarray = df_sub_11()$citizen[order(df_sub_11()$Total_def, decreasing = T)])) %>% 
      layout(plot_bgcolor='white', 
             barmode = "overlay",
             title = paste("Recognition rates by definitive decision of", input$geol, "in", input$timefra),
             legend = list(title = list(text = "<b> Legend </b>"),
                           x = 1.1,
                           y = 0.7),
             xaxis = list( 
               title = list(title = "<b> Asylum seekers citizenship </b>"),
               zerolinecolor = 'black', 
               zerolinewidth = 2, 
               gridcolor = 'ffff'), 
             yaxis = list( 
               title = "",
               zerolinecolor = 'black', 
               zerolinewidth = 2, 
               gridcolor = 'grey'),
             yaxis2 = list(
               zerolinecolor = "#B22222",
               gridcolor = "#B22222",
               tickvals = list(0, 0.5, 1),
               gridwidth = 2,
               zerolinewidth = 2,
               overlaying = "y",
               side = "right",
               title = "<b> Recognition rate </b>",
               tickfont = list(color = "#B22222", size = 14),
               tickangle = 270
             ),
             margin = list(
               l = 0,
               r = 0,
               b = 150,
               t = 30,
               pad = 20)
      )
  })
  
  output$rrdef = renderText({
    paste("Overall RR by definitive decision of", input$geol, "in", input$timefra, ": ", round(sum(df_sub_11()$Total_pos_def)/sum(df_sub_11()$Total_def), digits = 4)*100, "%")
  })

#-------------------------------------------------------------------------------  
  # second dataset creation
  df_sub_2 = reactiveVal(NULL)
  observe({
    
    df_sub_2(
      asylum %>% 
        filter(geo == input$geol_1 & sex == input$sex_1 & age == input$age_1) %>% 
        filter(time >= input$range[[1]],
               time <= input$range[[2]]) %>% 
        filter(!citizen %in% c("Total", "European Union - 27 countries (from 2020)",                        
                               "European Union - 28 countries (2013-2020)", "Extra-EU27 (from 2020)",
                               "Extra-EU28 (2013-2020)")) %>% 
        group_by(geo, citizen) %>% 
        summarise(Applications = sum(Applications, na.rm = T),
                  Genconv_first = sum(Genconv_first, na.rm = T),
                  Humstat_first = sum(Humstat_first, na.rm = T),
                  Rejected_first = sum(Rejected_first, na.rm = T),
                  Sub_prot_first = sum(Sub_prot_first, na.rm = T),
                  Temp_prot_first = sum(Temp_prot_first, na.rm = T),
                  Total_first = sum(Total_first, na.rm = T),
                  Total_pos_first = sum(Total_pos_first, na.rm = T),
                  Genconv_def = sum(Genconv_def, na.rm = T),
                  Humstat_def = sum(Humstat_def, na.rm = T),
                  Rejected_def = sum(Rejected_def, na.rm = T),
                  Sub_prot_def = sum(Sub_prot_def, na.rm = T),
                  Temp_prot_def = sum(Temp_prot_def, na.rm = T),
                  Total_def = sum(Total_def, na.rm = T),
                  Total_pos_def = sum(Total_pos_def, na.rm = T))
    )
    
  })
  
  # second slider update
  observe({
    updateSliderInput(
      session = session,
      inputId = "threshold_1",
      min = min(df_sub_2()$Applications, na.rm = T)+1,
      max = max(df_sub_2()$Applications, na.rm = T),
      value = c(min(df_sub_2()$Applications, na.rm = T)+0.15*max(df_sub_2()$Applications, na.rm = T),
                max(df_sub_2()$Applications, na.rm = T))
    )
  })
  
  # update second dataset
  df_sub_22 = reactiveVal(NULL)
  observe({
    df_sub_22(
      df_sub_2() %>% 
        mutate(dum = ifelse(Applications >= input$threshold_1[[1]] & Applications <= input$threshold_1[[2]],
                            1,
                            0),
               citizen = ifelse(dum == 1, citizen, "Other"))
    )
  })
  
  # piechart on applications
  output$applpie = renderPlotly({
    
    plot_ly() %>% 
      add_trace(data = df_sub_22(),
                labels = ~citizen,
                values = ~Applications,
                textposition = 'inside',
                textinfo = 'label+percent',
                type = "pie",
                showlegend = F) %>% 
      layout(title = "Total asylum applications",
             xaxis = list(showgrid = F,
                          zeroline = F),
             yaxis = list(showgrid = F,
                          zeroline = F))
  })
  
  
  output$appltext = renderText({
    paste("Total asylum applications in", input$geol_1, "in the period", input$range[[1]], "-",input$range[[2]], ": ", sum(df_sub_22()$Applications))
  })
  
  
  # piechart on first istances
  output$firstpie = renderPlotly({
    
    plot_ly() %>% 
      add_trace(data = df_sub_22(),
                labels = ~citizen,
                values = ~Total_pos_first,
                textposition = 'inside',
                textinfo = 'label+percent',
                type = "pie",
                showlegend = F) %>% 
      layout(title = "Asylums granted by first istance",
             xaxis = list(showgrid = F,
                          zeroline = F),
             yaxis = list(showgrid = F,
                          zeroline = F))
  })
  
  output$firsttext = renderText({
    a = paste("Total asylums granted by", input$geol_1, "by first istance in the period", input$range[[1]], "-",input$range[[2]], ": ", sum(df_sub_22()$Total_pos_first))
    b = "Of which by: "
    c = paste("  - Geneve convention:", sum(df_sub_22()$Genconv_first))
    d = paste("  - Humanitaran status:", sum(df_sub_22()$Humstat_first))
    e = paste("  - Subsidiary protection:", sum(df_sub_22()$Sub_prot_first))
    HTML(paste(a, b, c, d, e, sep = '<br/>'))

  })
  
  
  # piechart on definitive decision
  output$defpie = renderPlotly({
    
    plot_ly() %>% 
      add_trace(data = df_sub_22(),
                labels = ~citizen,
                values = ~Total_pos_def,
                textposition = 'inside',
                textinfo = 'label+percent',
                type = "pie",
                showlegend = F) %>% 
      layout(title = "Asylums granted by definitive decision",
             xaxis = list(showgrid = F,
                          zeroline = F),
             yaxis = list(showgrid = F,
                          zeroline = F))
  })
  
  output$deftext = renderText({
    a = paste("Total asylums granted by", input$geol_1, "by definitive decision in the period", input$range[[1]], "-",input$range[[2]], ": ", sum(df_sub_22()$Total_pos_def))
    b = "Of which by: "
    c = paste("  - Geneve convention:", sum(df_sub_22()$Genconv_def))
    d = paste("  - Humanitaran status:", sum(df_sub_22()$Humstat_def))
    e = paste("  - Subsidiary protection:", sum(df_sub_22()$Sub_prot_def))
    HTML(paste(a, b, c, d, e, sep = '<br/>'))
  })
  
#-------------------------------------------------------------------------------  
  # third dataset creation
  df_sub_3 = reactiveVal(NULL)
  observe({
    
    df_sub_3(
      asylum %>% 
        filter(citizen == input$citizen_2 & sex == input$sex_2 & age == input$age_2) %>% 
        filter(time >= input$range_2[[1]],
               time <= input$range_2[[2]]) %>% 
        filter(!geo %in% c("Total", "European Union - 27 countries (from 2020)",                        
                               "European Union - 28 countries (2013-2020)", "Extra-EU27 (from 2020)")) %>% 
        group_by(citizen, geo) %>% 
        summarise(Applications = sum(Applications, na.rm = T),
                  Genconv_first = sum(Genconv_first, na.rm = T),
                  Humstat_first = sum(Humstat_first, na.rm = T),
                  Rejected_first = sum(Rejected_first, na.rm = T),
                  Sub_prot_first = sum(Sub_prot_first, na.rm = T),
                  Temp_prot_first = sum(Temp_prot_first, na.rm = T),
                  Total_first = sum(Total_first, na.rm = T),
                  Total_pos_first = sum(Total_pos_first, na.rm = T),
                  Genconv_def = sum(Genconv_def, na.rm = T),
                  Humstat_def = sum(Humstat_def, na.rm = T),
                  Rejected_def = sum(Rejected_def, na.rm = T),
                  Sub_prot_def = sum(Sub_prot_def, na.rm = T),
                  Temp_prot_def = sum(Temp_prot_def, na.rm = T),
                  Total_def = sum(Total_def, na.rm = T),
                  Total_pos_def = sum(Total_pos_def, na.rm = T))
    )
    
  })
  
  # third slider update
  observe({
    updateSliderInput(
      session = session,
      inputId = "threshold_2",
      min = min(df_sub_3()$Applications, na.rm = T)+1,
      max = max(df_sub_3()$Applications, na.rm = T),
      value = c(min(df_sub_3()$Applications, na.rm = T)+0.05*max(df_sub_3()$Applications, na.rm = T),
                max(df_sub_3()$Applications, na.rm = T))
    )
  })
  
  # update third dataset
  df_sub_33 = reactiveVal(NULL)
  observe({
    df_sub_33(
      df_sub_3() %>% 
        mutate(dum = ifelse(Applications >= input$threshold_2[[1]] & Applications <= input$threshold_2[[2]],
                            1,
                            0),
               geo = ifelse(dum == 1, geo, "Other"))
    )
  })
  
  
  
  # piechart on applications
  output$applpie_co = renderPlotly({
    
    plot_ly() %>% 
      add_trace(data = df_sub_33(),
                labels = ~geo,
                values = ~Applications,
                textposition = 'inside',
                textinfo = 'label+percent',
                type = "pie",
                showlegend = F) %>% 
      layout(title = "Asylum applications",
             xaxis = list(showgrid = F,
                          zeroline = F),
             yaxis = list(showgrid = F,
                          zeroline = F))
  })
  
  output$appltext_co = renderText({
    paste("Total asylum applications from", input$citizen_2, "in the period", input$range_2[[1]], "-",input$range_2[[2]], ": ", sum(df_sub_33()$Applications))
  })
  
  
  # piechart on first istances
  output$firstpie_co = renderPlotly({
    
    plot_ly() %>% 
      add_trace(data = df_sub_33(),
                labels = ~geo,
                values = ~Total_pos_first,
                textposition = 'inside',
                textinfo = 'label+percent',
                type = "pie",
                showlegend = F) %>% 
      layout(title = "Asylums granted by first istance",
             xaxis = list(showgrid = F,
                          zeroline = F),
             yaxis = list(showgrid = F,
                          zeroline = F))
  })
  
  output$firsttext_co = renderText({
    a = paste("Total asylums granted to", input$citizen_2, "by definitive decision in the period", input$range_2[[1]], "-",input$range_2[[2]], ": ", sum(df_sub_33()$Total_pos_first))
    b = "Of which by: "
    c = paste("  - Geneve convention:", sum(df_sub_33()$Genconv_first))
    d = paste("  - Humanitaran status:", sum(df_sub_33()$Humstat_first))
    e = paste("  - Subsidiary protection:", sum(df_sub_33()$Sub_prot_first))
    HTML(paste(a, b, c, d, e, sep = '<br/>'))
  })
  
  
  # piechart on definitive decision
  output$defpie_co = renderPlotly({

    plot_ly() %>% 
      add_trace(data = df_sub_33(),
                labels = ~geo,
                values = ~Total_pos_def,
                textposition = 'inside',
                textinfo = 'label+percent',
                type = "pie",
                showlegend = F) %>% 
      layout(title = "Asylums granted by definitive decision",
             xaxis = list(showgrid = F,
                          zeroline = F),
             yaxis = list(showgrid = F,
                          zeroline = F))
  })
  
  
  output$deftext_co = renderText({
    a = paste("Total asylums granted to", input$citizen_2, "by definitive decision in the period", input$range_2[[1]], "-",input$range_2[[2]], ": ", sum(df_sub_33()$Total_pos_def))
    b = "Of which by: "
    c = paste("  - Geneve convention:", sum(df_sub_33()$Genconv_def))
    d = paste("  - Humanitaran status:", sum(df_sub_33()$Humstat_def))
    e = paste("  - Subsidiary protection:", sum(df_sub_33()$Sub_prot_def))
    HTML(paste(a, b, c, d, e, sep = '<br/>'))
  })
  
  
  
}

