function(input, output, session) {

  observeEvent(input$btn,
               introjs(session))
  
  d <- reactive(dataset[dataset$epoch == as.numeric(input$i), ])
  
  output$weight_display <- renderUI({
    b <- d()[1,] |> round(2)
    i <- ifelse(is.null(input$select), 0, input$select)
    coef_list <- list(
      list(coef = b$b1, 
           style = case_when(i == 0 ~ "", i == 2 ~ "", TRUE ~ "color:lightgrey"), 
           name = "b<sub>1</sub>", icon = if (! i %in% c(0, 2)) {""} else if (b$b1 > 0) {icon("arrow-up", class = "text-success")} else{icon("arrow-down", class = "text-danger")}),
      list(coef = b$w1, 
           style = case_when(i == 0 ~ "", i == 2 ~ "", TRUE ~ "color:lightgrey"),
           name = "w<sub>1</sub>", icon = if (! i %in% c(0, 2)) {""} else if (b$w1 > 0) {icon("arrow-up", class = "text-success")} else{icon("arrow-down", class = "text-danger")}), 
      
      list(coef = b$b2, 
           style = case_when(i == 0 ~ "", i == 3 ~ "", TRUE ~ "color:lightgrey"),
           name = "b<sub>2</sub>", icon = if (! i %in% c(0, 3)) {""} else if (b$b2 > 0) {icon("arrow-up", class = "text-success")} else{icon("arrow-down", class = "text-danger")}),
      list(coef = b$w2, 
           style = case_when(i == 0 ~ "", i == 3 ~ "", TRUE ~ "color:lightgrey"),
           name = "w<sub>2</sub>", icon = if (! i %in% c(0, 3)) {""} else if (b$w2 > 0) {icon("arrow-up", class = "text-success")} else{icon("arrow-down", class = "text-danger")}), 

      list(coef = b$b3, 
           style = case_when(i == 0 ~ "", i == 4 ~ "", TRUE ~ "color:lightgrey"),
           name = "b<sub>3</sub>", icon = if (! i %in% c(0, 4)) {""} else if (b$b3 > 0) {icon("arrow-up", class = "text-success")} else{icon("arrow-down", class = "text-danger")}),
      list(coef = b$w3, 
           style = case_when(i == 0 ~ "", i == 4 ~ "", TRUE ~ "color:lightgrey"),
           name = "w<sub>3</sub>", icon = if (! i %in% c(0, 4)) {""} else if (b$w3 > 0) {icon("arrow-up", class = "text-success")} else{icon("arrow-down", class = "text-danger")})
       )
    fluidRow(
      lapply(coef_list, function(coef) {
        column(
          width = 2, 
          align = "center",
          h5(HTML(coef$name), style = coef$style),
          h4(coef$coef, style = coef$style),
          coef$icon
      )
      })
    )
  })
  
  output$coef_display <- renderUI({
    b <- d()[1,] |> round(2)
    i <- ifelse(is.null(input$select), 0, input$select)
    coef_list <- list(
      list(coef = b$beta0, 
           style = case_when(i == 0 ~ "", TRUE ~ "color:lightgrey"),
           name = "&#x3B2;<sub>0</sub>", 
           icon = if (! i %in% c(0)) {""} else if (b$beta0 > 0) {icon("arrow-up", class = "text-success")} else{icon("arrow-down", class = "text-danger")}), 
      list(coef = b$beta1, 
           style = case_when(i == 0 ~ "", i == 2 ~ "", TRUE ~ "color:lightgrey"),
           name = "&#x3B2;<sub>1</sub>", 
           icon = if (! i %in% c(0, 2)) {""} else if (b$beta1 > 0) {icon("arrow-up", class = "text-success")} else{icon("arrow-down", class = "text-danger")}), 
      list(coef = b$beta2, 
           style = case_when(i == 0 ~ "", i == 3 ~ "", TRUE ~ "color:lightgrey"),
           name = "&#x3B2;<sub>2</sub>", 
           icon = if (! i %in% c(0, 3)) {""} else if (b$beta2 > 0) {icon("arrow-up", class = "text-success")} else{icon("arrow-down", class = "text-danger")}),
      list(coef = b$beta3, 
           style = case_when(i == 0 ~ "", i == 4 ~ "", TRUE ~ "color:lightgrey"),
           name = "&#x3B2;<sub>3</sub>", 
           icon = if (! i %in% c(0, 4)) {""} else if (b$beta3 > 0) {icon("arrow-up", class = "text-success")} else{icon("arrow-down", class = "text-danger")})
    )
    fluidRow(
      lapply(coef_list, function(coef) {
        column(
          width = 3, 
          align = "center",
          h4(HTML(coef$name), style = coef$style),
          h3(coef$coef, style = coef$style),
          coef$icon
        )
      })
    )
  })
  
  output$diag <- renderVisNetwork({
    visNetwork(nodes, edges, height = "100%", width = "100%",
               main = "") %>%
      visIgraphLayout(layout = "layout.norm", 
                      layoutMatrix = coords) %>%
      visOptions(highlightNearest = FALSE) %>%
      visInteraction(dragNodes = FALSE, 
                     dragView = FALSE, 
                     zoomView = FALSE) %>%
      visEdges(arrows = 'to')  %>%
      visEvents(select = "function(properties){
                  if (properties.nodes[0] == null){p = 0} else {p = properties.nodes[0]};
                  Shiny.setInputValue('select', p);
                  ;}"
      ) 
  })
  
  
  output$plot <- renderPlotly({
    i <- ifelse(is.null(input$select), 0, input$select)
    
    plot_ly(d(), x = ~ x) %>%
      add_markers(
        y = ~ y,
        hovertemplate = "x: %{x:.2f}<br>y: %{y:.2f}",
        name = "observed data",
        marker = list(size = 10, color = "grey")
      ) %>%
      add_trace(
        y = ~ beta0,
        type = "scatter",
        mode = "lines",
        line = list(color = fade_color("#648FFF", i, 0), dash = "dash"),
        hovertemplate = "x: %{x:.2f}<br>&#x3B2;<sub>0</sub>: %{y:.2f}",
        name = "&#x3B2;<sub>0"
      ) %>%
      add_trace(
        y = ~ y1,
        type = "scatter",
        mode = "lines",
        line = list(color = fade_color("#785EF0", i, 2), dash = "dash",
                    width = bold_line(i, 2)),
        hovertemplate = "x: %{x:.2f}<br>&#x3B2;<sub>1</sub>A<sub>1</sub>: %{y:.2f}",
        name = "&#x3B2;<sub>1</sub>A<sub>1"
      ) %>%
      add_trace(
        y = ~ y2,
        type = "scatter",
        mode = "lines",
        line = list(color = fade_color("#DC267F", i, 3), dash = "dash",
                    width = bold_line(i, 3)),
        hovertemplate = "x: %{x:.2f}<br>&#x3B2;<sub>2</sub>A<sub>2</sub>: %{y:.2f}",
        name = "&#x3B2;<sub>2</sub>A<sub>2"
      ) %>%
      add_trace(
        y = ~ y3,
        type = "scatter",
        mode = "lines",
        line = list(color = fade_color("#FE6100", i, 4), dash = "dash",
                    width = bold_line(i, 4)),
        hovertemplate = "x: %{x:.2f}<br>&#x3B2;<sub>3</sub>A<sub>3</sub>: %{y:.2f}",
        name = "&#x3B2;<sub>3</sub>A<sub>3"
      ) %>%
      add_trace(
        y = ~ predictions,
        type = "scatter",
        mode = "lines",
        line = list(color = fade_color("#FFB000", i, 0), width = 5),
        hovertemplate = "x: %{x:.2f}<br>predicted y: %{y:.2f}",
        name = "prediction"
      ) %>%
      layout(yaxis = list(range = range(dataset$predictions),
                          zeroline = FALSE),
             showlegend = FALSE,
             xaxis = list(zeroline = FALSE),
             plot_bgcolor = "#f7f7f7",
             paper_bgcolor = "#f7f7f7")
  })
  
  output$plot_beta0 <- renderPlotly({
    i <- ifelse(is.null(input$select), 0, input$select)
    
    p0 <- ggplot(d(), aes(x = x, y = beta0)) +
      geom_line(col = fade_color("#648FFF", i, 0), lwd = 2) +
      theme_minimal() +
      coord_cartesian(xlim = range(dataset$x), ylim = range(dataset$beta0)) +
      ylab("&#x3B2;<sub>0</sub>")
    
    ggplotly(p0) %>%
      style(hovertext = glue::glue(
        "x: {round(d()$x, 1)} <br><br>&#x3B2;<sub>0:{round(d()$beta0,1)}"
      ))
  })
  
  output$plot_beta1 <- renderPlotly({
    i <- ifelse(is.null(input$select), 0, input$select)
    
    ggplot(d(), aes(x = x, y = y1)) +
      geom_line(aes(x = x, y = y_dashed1),
                color = "gray",
                lty = 2) +
      geom_line(col = fade_color("#785EF0", i, 2), lwd = 2) +
      theme_minimal() +
      coord_cartesian(xlim = range(dataset$x), ylim = range(dataset$y_dashed1)) +
      ylab("&#x3B2;<sub>1</sub> A<sub>1")  -> p1
    p1 <- ggplotly(p1) %>%
      style(
        hovertext = glue::glue(
          "x: {round(d()$x, 1)} <br><br>&#x3B2;<sub>1</sub>A<sub>1</sub>:{round(d()$y1,1)}"
        )
      )
    p1$x$data[[1]]$hoverinfo <- "none"
    p1
  })
  
  output$plot_beta2 <- renderPlotly({
    i <- ifelse(is.null(input$select), 0, input$select)
    
    ggplot(d(), aes(x = x, y = y2)) +
      geom_line(aes(x = x, y = y_dashed2),
                color = "gray",
                lty = 2) +
      geom_line(col = fade_color("#DC267F", i, 3), lwd = 2) +
      theme_minimal() +
      coord_cartesian(xlim = range(dataset$x), ylim = range(dataset$y_dashed2)) +
      ylab("&#x3B2;<sub>2</sub> A<sub>2")  -> p2
    
    p2 <- ggplotly(p2) %>%
      style(
        hovertext = glue::glue(
          "x: {round(d()$x, 1)} <br><br>&#x3B2;<sub>2</sub>A<sub>2</sub>:{round(d()$y2,1)}"
        )
      )
    p2$x$data[[1]]$hoverinfo <- "none"
    p2
  })
  
  output$plot_beta3 <- renderPlotly({
    i <- ifelse(is.null(input$select), 0, input$select)
    ggplot(d(), aes(x = x, y = y3)) +
      geom_line(aes(x = x, y = y_dashed3),
                color = "gray",
                lty = 2) +
      geom_line(col = fade_color("#FE6100", i, 4), lwd = 2) +
      theme_minimal() +
      coord_cartesian(xlim = range(dataset$x), ylim = range(dataset$y_dashed3)) +
      ylab("&#x3B2;<sub>3</sub> A<sub>3") -> p3
    
    p3 <- ggplotly(p3) %>%
      style(
        hovertext = glue::glue(
          "x: {round(d()$x, 1)} <br><br>&#x3B2;<sub>3</sub>A<sub>3</sub>:{round(d()$y3,1)}"
        )
      )
    p3$x$data[[1]]$hoverinfo <- "none"
    p3
  })
  
  # Define the animation
  i <- reactiveVal(1)
  interval <- reactiveTimer(50)
  
  observeEvent(input$play, {
    i(1)
    interval()
  })
  
  observeEvent(interval(), {
    if (input$play %% 2 && i() <= 300) {
      updateSliderInput(session, "i", value = i())
      i(i() + 10)
    }
  })
  
  
  
}

