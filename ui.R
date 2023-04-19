dashboardPage(
  dashboardHeader(title = "Neural Networks"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Single Layer Network", tabName = "single_layer", icon = icon("dashboard"))#,
      #menuItem("Two Layer Network", tabName = "two_layers", icon = icon("th")
      #)
    )),
  dashboardBody(
    includeCSS("www/custom.css"),
      tags$footer(tagList(
        br(),
        br(),
        p(
          "Created by",
          a(href = "https://lucymcgowan.com", "Lucy D'Agostino McGowan", style = "color: #f1c400"),
          "Copyright", format(Sys.Date(), "%Y"),
          class = "footer"
        )
      ),
      align = "center"),
    tabItems(
      tabItem(tabName = "single_layer",
              h2("Single Hidden Layer Network"),
              fluidRow(
              column(width = 4,
                     fluidRow(
                       box(width = 12, title = "Diagram of Neural Network", grVizOutput("diag")),
                       withMathJax(),
                       box(width = 12, title = "Equation", uiOutput("eq"))
                     )
              ),
              column(width = 8,
                     fluidRow(
                       box(width = 12, plotlyOutput("plot")),
                       box(width = 12, sliderInput("i", "Iteration:", min = 1, max = 300, value = 1),
                           actionButton("play", "Play")))
                     )
              ),
              fluidRow(
                       box(width = 12, 
                           box(width = 3, plotlyOutput("plot_beta0")),
                           box(width = 3, plotlyOutput("plot_beta1")),
                           box(width = 3, plotlyOutput("plot_beta2")),
                           box(width = 3, plotlyOutput("plot_beta3"))
                     )
              )
              
      )#,
      # tabItem(tabName = "two_layers",
      #         h2("Two Hidden Layers Network"),
      #         column(width = 4,
      #                fluidRow(
      #                  box(width = 12, title = "Diagram of Neural Network", grVizOutput("diag2")),
      #                  withMathJax(),
      #                  box(width = 12, title = "Equation", uiOutput("eq2"))
      #                )
      #         ),
      #         column(width = 8,
      #                fluidRow(
      #                  box(width = 12, plotOutput("plot2")),
      #                  box(width = 12, sliderInput("i2", "Iteration:", min = 1, max = 500, value = 1),
      #                      actionButton("play2", "Play")),
      #                  box(width = 12, plotOutput("plot_split2"))
      #                )
      #          )
      #)
    )
  ),
  skin = "green"
)