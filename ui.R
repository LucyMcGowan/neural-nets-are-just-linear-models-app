dashboardPage(
  dashboardHeader(title = "Neural Networks", disable = TRUE),
  dashboardSidebar(
    disable = TRUE,
    sidebarMenu(
      menuItem("Single Layer Network", tabName = "single_layer", icon = icon("dashboard"))
    )),
  dashboardBody(
    includeCSS("www/custom.css"),
    introjsUI(),
    h1("\"it's just a linear model\"", align = "center", style = "padding: 75px; margin-bottom: -20px"),
    h1("NEURAL NETWORKS", align = "center", style = "margin-top: -120px; margin-bottom: 100px;"),
    tabItems(
      tabItem(tabName = "single_layer",
              fluidRow(
                column(width = 2),
                column(width = 8,
                       fluidRow(
                         column(width = 4,
                                fluidRow(
                                  box(width = 12, title = "DIAGRAM OF THE NEURAL NETWORK", 
                                      h4("Here we have fit a neural network to a dataset with one predictor, x, and one outcome, y. The network has one hidden layer with three activations. Click the ", strong("Play"), " button to watch how the neural network fits across 300 epochs."),
                                      actionButton("btn","Click here for a walk through"),
                                      introBox(
                                        visNetworkOutput("diag", height = "300px"),
                                        data.step = 1,
                                        data.intro = "This is a visualization of the neural network that we fit. It has one input parameter, x, and one hidden layer with three activations: A1, A2, and A3. Click on one of the activation nodes to see it highlighted elsewhere in the dashboard."
                                      ),
                                      h4("Click on the nodes above to highlight them in the rest of the dashboard."))
                                )
                         ),
                         column(width = 8,
                                fluidRow(
                                  box(width = 12, status = "warning",
                                      introBox(
                                        plotlyOutput("plot"),
                                        data.step = 2,
                                        data.intro = "This figure shows the observed data (grey dots), the prediction from the neural network (orange line) as well as the coefficients multiplied by their activations (dashed lines). The orange line is equal to the sum of all the dashed lines (hence, it's just a linear model...woohoo!)"
                                      )
                                  ),
                                  box(width = 12, 
                                      introBox(sliderInput("i", "Iteration:", min = 1, max = 300, value = 1),
                                               data.step = 3,
                                               data.intro = "Slide this to see how the neural network updates as the number of epochs (iterations) increases. Try sliding it all the way to 300 to see how it ends.",
                                               data.position = "bottom-right-aligned"
                                      ),
                                      introBox(actionButton("play", "Play"),
                                               data.step = 4,
                                               data.intro = "Try pressing play to see how the model changes across epochs (iterations)."
                                      )
                                  )
                                )
                         )
                       ),
                       fluidRow(
                         column(width = 4,
                                box(width = 12, title = "EQUATION",
                                    withMathJax(),
                                    h4("$$\\hat{y}=\\beta_0 + \\beta_1\\max\\{0, b_1 + w_1x\\}+\\\\\\beta_2  \\max\\{0, b_2 + w_2x\\} + \\\\\\beta_3\\max\\{0, b_3+w_3x\\}$$"),
                                )),
                         column(width = 8,
                                box(width = 12, status = "info",
                                    title = "COEFFICIENTS",
                                    uiOutput("coef_display"),
                                    uiOutput("weight_display"))
                         )
                       ),
                       fluidRow(
                         introBox(
                           box(width = 12, 
                               box(width = 3, status = "warning",
                                   plotlyOutput("plot_beta0")),
                               box(width = 3, status = "warning",
                                   plotlyOutput("plot_beta1")),
                               box(width = 3, status = "warning",
                                   plotlyOutput("plot_beta2")),
                               box(width = 3, status = "warning",
                                   plotlyOutput("plot_beta3"))
                           ),
                           data.step = 5,
                           data.intro = "These plots show the activations multiplied by their coefficients. Here we used the <b>ReLU</b> transformation. That means we took the linear model within each activation and transformed it by setting it to 0 if it is negative, and its value otherwise. If you click the corresponding node in the diagram above you can see which of these corresponds to it." 
                         )
                       )
                ),
                column(width = 2)
              ),
              fluidRow(
                tags$footer(tagList(
                  br(),
                  br(),
                  p(
                    "Created by",
                    a(href = "https://lucymcgowan.com", "Lucy D'Agostino McGowan", style = "color: #f1c400"),
                    HTML("&#169;"), format(Sys.Date(), "%Y"),
                    class = "footer"
                  )
                ),
                align = "center")
              )
      )
    )
  ),
  skin = "green"
)