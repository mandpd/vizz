library(miniUI)
library(shiny)
library(shinythemes)
library(tidyverse)
library(shinyjs)

#' @export
gamma_viz <- function() {
ui <- fluidPage(
  useShinyjs(),
  theme = shinytheme('united'),
  gadgetTitleBar('Gamma Parameter Chooser'),
  fluidRow(column(6,
                  wellPanel(
                    sliderInput(
                      inputId = 'shape',
                      label = 'Shape',
                      min = 1,
                      max = 3,
                      value = 2,
                      sep = '',
                      step = 0.001
                    )
                  )),
           column(6,
                  wellPanel(
                    sliderInput(
                      inputId = 'rate',
                      label = 'Rate',
                      min = 1,
                      max = 30,
                      value = 15,
                      sep = '',
                      step = 0.0001

                    )
                  ))),
  fluidRow(
    column(3, numericInput('targetMean', 'Target Mean', value = 60)),
    column(3, numericInput('targetQuantile', 'Target Quantile', 0.85)),
    column(3, numericInput('targetQuantileValue', 'Target Quantile Value', 100))
  ),
  fluidRow(column(12, actionButton('recalculate', label = 'Run Simulation'))
  ),
  uiOutput('plotPlus')
)

server <- function(input, output) {
  output$plotDisplay <-
    renderPlot({
      ggplot(df(), aes(x = data)) + geom_histogram(binwidth = 1) + geom_vline(xintercept =
                                                                                input$targetMean)
    })
  df <- eventReactive(input$recalculate, {
    show('plotPlus')
    sample = rgamma(100000, input$shape, 1 / input$rate)
    data.frame(data = sample)
  }, ignoreNULL = FALSE)
  output$vals <- renderText({
    paste(
      paste('Shape:', input$shape, sep = ' '),
      paste('Rate:', input$rate, sep = ' '),
      paste('Shape/Rate:', round(input$shape / input$rate, 2), sep = ' '),
      paste('Mean:',  round(mean(df(
      )$data), 2), sep = ' '),
      paste(
        input$targetQuantile,
        'quantile is',
        round(quantile(df()$data, probs = input$targetQuantile), 2),
        sep = ' '
      ),
      sep = '\n'
    )
  })
  output$plotPlus <- renderUI({fluidRow(column(8,
                                     plotOutput('plotDisplay')),
                              column(4,
                                     # verbatimTextOutput('summary'),
                                     verbatimTextOutput('vals')))})
  observeEvent(input$shape, {
    hide('plotPlus')
  })
  observeEvent(input$rate, {
    hide('plotPlus')
  })
  observeEvent(input$recalculate, {
    show('plotPlus')
  })
  observeEvent(input$done, {
    stopApp()
  })

}

runGadget(ui, server, viewer = paneViewer)
}
