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
#' @importFrom ovideo ov_video_control
#' @importFrom peranavolley pv_read
#' @importFrom shiny actionButton column fluidPage fluidRow isolate mainPanel modalDialog numericInput observe observeEvent onSessionEnded onStop plotOutput reactive reactiveVal reactiveValues removeModal renderUI renderPlot selectInput selectizeInput showModal sidebarLayout sidebarPanel sliderInput tabPanel tabsetPanel updateActionButton updateNumericInput updateSelectInput uiOutput
#' @importFrom shinyWidgets pickerInput updatePickerInput
#' @importFrom stats na.omit setNames
NULL
