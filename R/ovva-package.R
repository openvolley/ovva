#' \pkg{ovva}
#'
#' A Shiny app and supporting functions for volleyball video and scouted match files.
#'
#' @name ovva
#' @docType package
#' @importFrom assertthat assert_that is.flag is.string
#' @importFrom datavolley dv_read
#' @importFrom dplyr %>% across all_of bind_rows case_when distinct do group_by group_by_at if_else lag lead left_join mutate n tribble ungroup
#' @importFrom htmltools HTML tagList tags
#' @importFrom methods as
#' @importFrom ovideo ov_video_control ov_video_timing_df
#' @importFrom peranavolley pv_read
#' @importFrom rlang .data .env
#' @importFrom shiny actionButton column debounce downloadButton downloadHandler fluidPage fluidRow isolate mainPanel modalDialog numericInput observe observeEvent onSessionEnded onStop plotOutput reactive reactiveVal reactiveValues removeModal renderUI renderPlot selectInput selectizeInput showModal sidebarLayout sidebarPanel sliderInput tabPanel tabsetPanel updateActionButton updateNumericInput updateSelectInput updateSliderInput uiOutput
#' @importFrom shinyWidgets pickerInput updatePickerInput
#' @importFrom stats na.omit setNames
#' @importFrom utils capture.output str write.csv
"_PACKAGE"
