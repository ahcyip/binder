# Load libraries and functions ####
library(shiny)
library(readxl); library(tidyxl);
library(janitor); library(magrittr)
library(tidyverse)

# pkg list for photon
# shiny,readxl,tidyxl,janitor,magrittr,tidyverse
# NULL
# NULL

source("fns.R")

#___________________________________________________________________
# Define UI ####

ui <- fluidPage(
  titlePanel("TRUCK choice model"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Choose xlsx file to load Class 7&8 Sleep inputs)',
                accept = c(".xlsx",".xlsm")
      ),
      # generic shiny app import/export https://gist.github.com/SachaEpskamp/5796467
      sliderInput("disc_rate",
                  "Discount rate:",
                  min = 0.01,
                  max = 0.30,
                  value = 0.075),
      sliderInput("opdays",
                  "Operating days:",
                  min = 1,
                  max = 365,
                  value = 250)
      # eventual download button: https://stackoverflow.com/questions/49359097/upload-transform-xlsx-file-download-result-shiny
    ),
    mainPanel(
      textOutput(outputId = "discount_rate_desc"),
      h3("Technology Options table"),
      tableOutput('contents_TO'),
      h3("Final Market Shares"),
      tableOutput('final_mkt_shr')
    )
  )
)

#___________________________________________________________________
# Define server ####

server <- function(input, output){

  output$contents_TO <- renderTable({
    req(input$file1)
    names <- get_names(input$file1)
    read_excel_range("TECH_OPT_PARAMS_Cls1", FALSE, names, input$file1) %>%
      set_colnames(read_excel_range("TECH_OPT_COLS_Cls1", FALSE, names, input$file1) %>% as.character()) %>%
      clean_names() %>%
      select(-(2:3))
  })

  output$final_mkt_shr <- renderTable({
    req(input$file1)
    names <- get_names(input$file1)
    bind_cols(read_excel_range("Cls1MktShrA", FALSE, names, input$file1),
              read_excel_range("Cls1MktShrB", FALSE, names, input$file1),
              read_excel_range("Cls1MktShrC", FALSE, names, input$file1),
              read_excel_range("Cls1MktShrD", FALSE, names, input$file1),
              read_excel_range("Cls1MktShrE", FALSE, names, input$file1)) %>%
      set_colnames(c("Adv Conv", "ISG", "HEV", "PHEV", "FCHEV")) %>%
      mutate(`Conventional Diesel ICE` = 1 - `Adv Conv` - ISG - HEV - PHEV - FCHEV,
             year = 2007:2050) %>%
      select(year, `Conventional Diesel ICE`, everything()) %>%
      filter(year > 2014)
  })

  output$discount_rate_desc <- renderText({
    paste("Discount rate:", input$disc_rate * 100, "%")
  })



}

#___________________________________________________________________
# Run the application ####
shinyApp(ui = ui, server = server)
