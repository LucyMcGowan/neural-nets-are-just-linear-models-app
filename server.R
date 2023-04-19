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
  
  # output$eq2 <- renderUI({
  #   b <- dataset2[dataset2$epoch == as.numeric(input$i2),][1, ] |> round(2)
  #   withMathJax(glue::glue("$$\\hat{y}=?b$beta0| ?ifelse(b$beta1<0, '','+')|  ?b$beta1|\\times \\max\\{0,?b$b21|?ifelse(b$w211<0, '','+')|?b$w211|\\times\\max\\{0, ?b$b11|\\\\?ifelse(b$w11<0, '','+')|?b$w11|x\\}?ifelse(b$w212<0, '','+')|?b$w212|\\times\\max\\{0,?b$b12|?ifelse(b$w12<0, '','+')|?b$w12|x\\}\\} ?ifelse(b$beta2<0, '','+')| \\\\ ?b$beta2|\\times \\max\\{0,?b$b22|?ifelse(b$w221<0, '','+')|?b$w221|\\times\\max\\{0, ?b$b11|\\\\?ifelse(b$w11<0, '','+')|?b$w11|x\\}?ifelse(b$w222<0, '','+')|?b$w222|\\times\\max\\{0,?b$b12|?ifelse(b$w12<0, '','+')|?b$w12|x\\}\\}$$",
  #                          .open = "?", .close = "|"))
  # })
  output$diag <- renderGrViz({
    render_graph(graph)
  })
  
  # output$diag2 <- renderGrViz({
  #   render_graph(graph2)
  # })
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
  
  # output$plot2 <- renderPlot({
  #   d <- dataset2[dataset2$epoch == as.numeric(input$i2),]
  #
  #   ggplot(d, aes(x = x, y = y), size = 2) +
  #     geom_point(color = "grey", size = 2) +
  #     geom_line(col = 1, aes(y = beta0), lty = 2) +
  #     geom_line(col = 2, aes(y = y21), lty = 2) +
  #     geom_line(col = 3, aes(y = y22), lty = 2) +
  #     geom_line(col = "cornflower blue", aes(y = predictions), lwd = 2, alpha = 0.5) +
  #     theme_minimal() +
  #     coord_cartesian(xlim = range(x), ylim = range(y))
  # })
  
  
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
  
  # output$plot_split2 <- renderPlot({
  #   d <- dataset2[dataset2$epoch == as.numeric(input$i2),]
  #
  #   ggplot(d, aes(x = x, y = beta0)) +
  #     geom_line(col = 1, lwd = 2) +
  #     theme_minimal() +
  #     coord_cartesian(xlim = range(x), ylim = range(dataset2$beta0)) +
  #     labs(title = glue::glue("Intercept: {round(d()$beta0[1], 2)}"),
  #          y = expression(beta[0])) -> p0
  #
  #   ggplot(d, aes(x = x, y = y1)) +
  #     geom_line(col = 2, lwd = 2) +
  #     geom_line(aes(x = x, y = y_dashed1), color = "gray", lty = 2) +
  #     theme_minimal() +
  #     coord_cartesian(xlim = range(x), ylim = range(dataset2$y_dashed1)) +
  #     labs(title = "Hidden Layer 1",
  #          y = expression(A[11])) -> p1
  #
  #   ggplot(d, aes(x = x, y = y2)) +
  #     geom_line(col = 3, lwd = 2) +
  #     geom_line(aes(x = x, y = y_dashed2), color = "gray", lty = 2) +
  #     theme_minimal() +
  #     coord_cartesian(xlim = range(x), ylim = range(dataset2$y_dashed2)) +
  #     labs(title = "Hidden Layer 1",
  #          y = expression(A[12])) -> p2
  #
  #   ggplot(d, aes(x = x, y = y3)) +
  #     geom_line(col = 3, lwd = 2) +
  #     geom_line(aes(x = x, y = y_dashed3), color = "gray", lty = 2) +
  #     theme_minimal() +
  #     coord_cartesian(xlim = range(x), ylim = range(dataset2$y_dashed3)) +
  #     labs(title = "Hidden Layer 1",
  #          y = expression(A[13])) -> p3
  #
  #   ggplot(d, aes(x = y1, y = y21)) +
  #     geom_line(col = 4, lwd = 2) +
  #     theme_minimal() +
  #     coord_cartesian(xlim = range(dataset2$y1), ylim = range(dataset2$y21)) +
  #     labs(title = glue::glue("Hidden Layer 2\n A1 Activation weight: {round(d()$beta1[1], 2)}"),
  #          y = expression(beta[1]*A[21])) -> p4
  #
  #   ggplot(d, aes(x = y2, y = y22)) +
  #     geom_line(col = 4, lwd = 2) +
  #     theme_minimal() +
  #     coord_cartesian(xlim = range(dataset2$y2), ylim = range(dataset2$y22)) +
  #     labs(title = glue::glue("Hidden Layer 2\nA2 Activation weight: {round(d()$beta2[1], 2)}"),
  #          y = expression(beta[2]*A[22])) -> p5
  #
  #
  #   wrap_plots(p1, p2, p3, p0, p4, p5, ncol = 5)
  # })
  
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
  
  # Define the animation
  # i2 <- reactiveVal(1)
  # interval2 <- reactiveTimer(50)
  #
  # observeEvent(input$play2, {
  #   i2(1)
  #   interval2()
  # })
  #
  # observeEvent(interval2(), {
  #   if (input$play2 %% 2 && i2() <= 500) {
  #     updateSliderInput(session, "i2", value = i2())
  #     i2(i2() + 10)
  #   }
  # })
  
  
}

