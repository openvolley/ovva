#' \pkg{ovva}
#'
#' A Shiny app and supporting functions for volleyball video and scouted match files.
#'
#' @name ovva
#' @docType package
#' @importFrom assertthat assert_that is.flag is.string
#' @importFrom datavolley read_dv
#' @importFrom dplyr .data bind_rows case_when distinct do group_by group_by_at lag lead left_join mutate n tribble ungroup
#' @importFrom htmltools tagList tags
#' @importFrom methods as
#' @importFrom rintrojs introBox introjsUI
#' @importFrom shiny actionButton column fluidPage fluidRow mainPanel observe observeEvent onSessionEnded onStop plotOutput reactive renderUI req renderPlot selectInput selectizeInput sidebarLayout sidebarPanel sliderInput tabPanel updateSelectInput uiOutput
#' @importFrom shinycssloaders withSpinner
#' @importFrom shinydashboard tabBox
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyWidgets pickerInput updatePickerInput
#' @importFrom stats na.omit
NULL
