function(input, output, session) {
  # Create diagram
  
  d <- reactive(dataset[dataset$epoch == as.numeric(input$i), ])
  
  output$eq <- renderUI({
    b <- d()[1,] |> round(2)
    withMathJax(
      glue::glue(
        "$$\\hat{y}=?b$beta0| ?ifelse(b$beta1<0, '','+')|  ?b$beta1|\\times \\max\\{0,?b$b1|?ifelse(b$w1<0, '','+')|?b$w1|x\\} ?ifelse(b$beta2<0, '','+')| \\\\ ?b$beta2|\\times \\max\\{0,?b$b2|?ifelse(b$w2<0, '','+')|?b$w2|x\\} ?ifelse(b$beta3<0, '','+')|\\\\ ?b$beta3|\\times \\max\\{0,?b$b3|?ifelse(b$w3<0, '','+')|?b$w3|x\\}$$",
        .open = "?",
        .close = "|"
      )
    )
  })
  output$diag <- renderGrViz({
    render_graph(graph)
  })
  
  output$diag <- renderGrViz({
    render_graph(graph)
  })
  
  output$plot <- renderPlotly({
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
        line = list(color = "#A3A500", dash = "dash"),
        hovertemplate = "x: %{x:.2f}<br>beta<sub>0</sub>: %{y:.2f}",
        name = "beta<sub>0"
      ) %>%
      add_trace(
        y = ~ y1,
        type = "scatter",
        mode = "lines",
        line = list(color = "#00BF7D", dash = "dash"),
        hovertemplate = "x: %{x:.2f}<br>beta<sub>1</sub>A<sub>1</sub>: %{y:.2f}",
        name = "beta<sub>1</sub>A<sub>1"
      ) %>%
      add_trace(
        y = ~ y2,
        type = "scatter",
        mode = "lines",
        line = list(color = "#00B0F6", dash = "dash"),
        hovertemplate = "x: %{x:.2f}<br>beta<sub>2</sub>A<sub>2</sub>: %{y:.2f}",
        name = "beta<sub>2</sub>A<sub>2"
      ) %>%
      add_trace(
        y = ~ y3,
        type = "scatter",
        mode = "lines",
        line = list(color = "#E76BF3", dash = "dash"),
        hovertemplate = "x: %{x:.2f}<br>beta<sub>3</sub>A<sub>3</sub>: %{y:.2f}",
        name = "beta<sub>3</sub>A<sub>3"
      ) %>%
      add_trace(
        y = ~ predictions,
        type = "scatter",
        mode = "lines",
        line = list(color = "#F8766D", width = 5),
        hovertemplate = "x: %{x:.2f}<br>predicted y: %{y:.2f}",
        name = "prediction"
      ) %>%
      layout(yaxis = list(range = range(dataset$predictions)),
             showlegend = FALSE)
  })
  
  output$plot_beta0 <- renderPlotly({
    p0 <- ggplot(d(), aes(x = x, y = beta0)) +
      geom_line(col = "#A3A500", lwd = 2) +
      theme_minimal() +
      coord_cartesian(xlim = range(x), ylim = range(dataset$beta0)) +
      ylab("beta<sub>0</sub>")
    
    ggplotly(p0) %>%
      style(hovertext = glue::glue(
        "x: {round(d()$x, 1)} <br><br>beta<sub>0:{round(d()$beta0,1)}"
      ))
  })
  
  output$plot_beta1 <- renderPlotly({
    ggplot(d(), aes(x = x, y = y1)) +
      geom_line(aes(x = x, y = y_dashed1),
                color = "gray",
                lty = 2) +
      geom_line(col = "#00BF7D", lwd = 2) +
      theme_minimal() +
      coord_cartesian(xlim = range(x), ylim = range(dataset$y_dashed1)) +
      ylab("beta<sub>1</sub> A<sub>1")  -> p1
    p1 <- ggplotly(p1) %>%
      style(
        hovertext = glue::glue(
          "x: {round(d()$x, 1)} <br><br>beta<sub>1</sub>A<sub>1</sub>:{round(d()$y1,1)}"
        )
      )
    p1$x$data[[1]]$hoverinfo <- "none"
    p1
  })
  
  output$plot_beta2 <- renderPlotly({
    ggplot(d(), aes(x = x, y = y2)) +
      geom_line(aes(x = x, y = y_dashed2),
                color = "gray",
                lty = 2) +
      geom_line(col = "#00B0F6", lwd = 2) +
      theme_minimal() +
      coord_cartesian(xlim = range(x), ylim = range(dataset$y_dashed2)) +
      ylab("beta<sub>2</sub> A<sub>2")  -> p2
    
    p2 <- ggplotly(p2) %>%
      style(
        hovertext = glue::glue(
          "x: {round(d()$x, 1)} <br><br>beta<sub>2</sub>A<sub>2</sub>:{round(d()$y2,1)}"
        )
      )
    p2$x$data[[1]]$hoverinfo <- "none"
    p2
  })
  
  output$plot_beta3 <- renderPlotly({
    ggplot(d(), aes(x = x, y = y3)) +
      geom_line(aes(x = x, y = y_dashed3),
                color = "gray",
                lty = 2) +
      geom_line(col = "#E76BF3", lwd = 2) +
      theme_minimal() +
      coord_cartesian(xlim = range(x), ylim = range(dataset$y_dashed3)) +
      ylab("beta<sub>3</sub> A<sub>3") -> p3
    
    p3 <- ggplotly(p3) %>%
      style(
        hovertext = glue::glue(
          "x: {round(d()$x, 1)} <br><br>beta<sub>3</sub>A<sub>3</sub>:{round(d()$y3,1)}"
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

