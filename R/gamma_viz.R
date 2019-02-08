library(miniUI)
library(shiny)
library(shinythemes)
library(tidyverse)
library(shinyjs)

#' @export
gamma_viz <- function() {
ui <- shiny::fluidPage(
  shinyjs::useShinyjs(),
  theme = shinythemes::shinytheme('united'),
  miniUI::gadgetTitleBar('Gamma Parameter Chooser'),
  shiny::fluidRow(shiny::column(6,
                  shiny::wellPanel(
                    shiny::sliderInput(
                      inputId = 'shape',
                      label = 'Shape',
                      min = 1,
                      max = 3,
                      value = 2,
                      sep = '',
                      step = 0.001
                    )
                  )),
                  shiny::column(6,
                                shiny::wellPanel(
                                  shiny::sliderInput(
                      inputId = 'rate',
                      label = 'Scale',
                      min = 1,
                      max = 30,
                      value = 15,
                      sep = '',
                      step = 0.0001

                    )
                  ))),
  shiny::fluidRow(
    shiny::column(3, shiny::numericInput('targetMean', 'Target Mean', value = 60)),
    shiny::column(3, shiny::numericInput('targetQuantile', 'Target Quantile', 0.85)),
    shiny::column(3, shiny::numericInput('targetQuantileValue', 'Target Quantile Value', 100))
  ),
  shiny::fluidRow(shiny::column(12, shiny::actionButton('recalculate', label = 'Run Simulation'))
  ),
  shiny::uiOutput('plotPlus')
)

server <- function(input, output) {
  output$plotDisplay <-
    shiny::renderPlot({
      ggplot2::ggplot(df(), ggplot2::aes(x = data)) + ggplot2::geom_histogram(binwidth = 1) + ggplot2::geom_vline(xintercept =
                                                                                input$targetMean)
    })
  df <- shiny::eventReactive(input$recalculate, {
    show('plotPlus')
    sample = rgamma(100000, shape = input$shape, scale = input$rate)
    data.frame(data = sample)
  }, ignoreNULL = FALSE)
  output$vals <- shiny::renderText({
    paste(
      paste('Shape:', input$shape, sep = ' '),
      paste('Scale:', input$rate, sep = ' '),
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
  output$plotPlus <- shiny::renderUI({fluidRow(column(8,
                                                      shiny::plotOutput('plotDisplay')),
                                               shiny::column(4,
                                     # verbatimTextOutput('summary'),
                                     shiny::verbatimTextOutput('vals')))})
  shiny::observeEvent(input$shape, {
    shinyjs::hide('plotPlus')
  })
  shiny::observeEvent(input$rate, {
    shinyjs::hide('plotPlus')
  })
  shiny::observeEvent(input$recalculate, {
    shinyjs::show('plotPlus')
  })
  shiny::observeEvent(input$done, {
    shiny::stopApp()
  })

}

shiny::runGadget(ui, server)
}
