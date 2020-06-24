# Load libraries ####
library(shiny)
library(readxl); library(tidyxl);
library(janitor); library(magrittr)
library(tidyverse)

# pkg list for photon
# shiny,readxl,tidyxl,janitor,magrittr,tidyverse
# NULL
# NULL

#___________________________________________________________________
# Functions from fns.R ####

# read inputs ####

get_names <- function(input_file) {
  input_file$datapath %>%
    xlsx_names() %>%
    mutate(location = str_extract(formula, ".*(?=!)")) %>%
    filter(name %>% str_detect("Print", negate = TRUE) & hidden == FALSE & is.na(sheet)) %>%
    mutate(location = case_when(str_detect(location, "OFFSET") ~ "OFFSET",
                                TRUE ~ location))
}

read_excel_range <- function(name, col_names = FALSE, names, input_file) {
  names %>%
    dplyr::filter(name == {{name}}) %>%
    pull(formula) %>%
    as.character() %>%
    read_excel(path = input_file$datapath,
               range = .,
               col_names = col_names) %>%
    return()
}

read_all_names_in_tab <- function(location, names, input_file) {
  names %>%
    dplyr::filter(location == {{location}}) %>%
    pull(name) %>%
    purrr::set_names() %>%
    map(read_excel_range, FALSE, names, input_file) %>%
    return()
}


convert_input_to_long <- function(input_table, tech_cols, values_to) {
  if (tech_cols == "base_and_alts")
    tech_col_names <- Inputs78Sleep$TECH_OPT_DESC_Cls1 %>% pull()
  else # i.e. alts_only
    tech_col_names <- Inputs78Sleep$TECH_OPT_DESC_Cls1 %>% pull() %>% magrittr::extract(seq(2, nrow(Inputs78Sleep$TECH_OPT_DESC_Cls1)))

  output1 <- input_table %>%
    bind_cols(Inputs78Sleep$Years,
              cls = rep("78Sleep", nrow(Inputs78Sleep$Years))) %>% # set class here
    set_colnames(c(tech_col_names, "yr", "cls")) %>%
    clean_names()

  if (values_to == "costs") {
    output1 %>%
      mutate_at(vars(-c("yr", "cls", "conventional_diesel_ice")), list("_tot_cost" = ~. + conventional_diesel_ice)) %>%
      mutate(conventional_diesel_ice__tot_cost = conventional_diesel_ice) %>%
      rename_at(vars(-c("yr", "cls") & !ends_with("__tot_cost")), ~paste(., "inc_cost", sep = "__")) %>%
      pivot_longer(cols = -c("yr", "cls"),
                   names_to = c("tech",".value"),
                   names_pattern = "(.*)__(.*)") %>% # base cost + named range: IncCosts
      group_by(yr, cls) %>%
      # mutate(tech_type = if_else(tech == "conventional_diesel_ice", "base", "alt")) %>%
      mutate(tech_type = if_else(tot_cost == min(tot_cost), "base", "alt"), # identify least-cost tech alt as "base"
             base_cost = min(tot_cost)) %>%
      ## need extra code/rule to handle if multiple AFVs have the same lowest cost
      ungroup() %>%
      return()
  } else {
    output1 %>%
      pivot_longer(cols = -c("yr", "cls"),
                   names_to = "tech",
                   values_to = values_to) %>%
      return()
  }
}

#___________________________________________________________________
# adoption ####

payback <- function(inc_cost, monthly_savings, monthly_disc_rate, max_pd = 1000) {
  # payback period (with consideration of discounted cash flows)
  # can be calculated via subtraction method like in TRUCK VBA

  # monthly_savings is constant (i.e. expected fuel cost stays constant from year of decision/purchase)
  # # i.e. no expectation of change in fuel cost through lifetime.

  return(if_else(
    # if monthly savings are 0 or negative, or if monthly savings are not enough to overcome incremental cost,
    #   then return max_pd
    monthly_savings <= rep(0, length(monthly_savings)) | (inc_cost * monthly_disc_rate)/monthly_savings >= 1, max_pd,
    # else, calculate discounted payback period via equation (works for equal cash flows)
    # https://financeformulas.net/Discounted-Payback-Period.html
    pmin(
      ceiling(log(1/(1-(inc_cost * monthly_disc_rate)/monthly_savings))/log(1+monthly_disc_rate)),
      max_pd)))
}

# VBA function for payback, subtraction method
# Function Payback(IncCost As Double, CashFlow As Double, IntRate As Double, Optional MaxPd As Integer = 1000) As Integer
#
# Dim iPeriod As Integer
# Dim bNotFound As Boolean
# Dim NPVal As Double
#
# Application.Volatile True
#
# bNotFound = True
# NPVal = -IncCost
# iPeriod = 0
#
# Do While bNotFound And iPeriod <= MaxPd
#
# iPeriod = iPeriod + 1
# NPVal = NPVal + CashFlow / (1 + IntRate) ^ iPeriod
# If NPVal >= 0 Then bNotFound = False
#
# Loop
#
# Payback = iPeriod
#
#
# End Function

#___________________________________________________________________
# S-curves inputs ####

#  Curve for indifference to first cost and fuel cost savings
# This curve applies when incremental cost is small and fuel costs are similar.  Thresholds are set by "indifference costs."
# Weibull: F(p) = 1-e^(-(p/k2)^k1)
# k1>1; k2>0
# k1 =	7		3	adjusts skew
# k2 =	0.8		0.57	adjusts saturation rate
# Pmax=	1		1	max probability
# first col for fraction of indiff first cost. second cdol for fraction of indiff fuel cost.
# Weibull function output

curve_for_indiff_to_first_cost <- function(frac_of_indiff_first_cost, k1 = 7, k2 = 0.8) {
  1-pweibull(frac_of_indiff_first_cost, shape = k1, scale = k2)
} # replaces S_curves$FirstCostInd
curve_for_indiff_to_fuel_cost_savings <- function(frac_of_indiff_fuel_cost, k1 = 3, k2 = 0.57) {
  1-pweibull(-1 * frac_of_indiff_fuel_cost, shape = k1, scale = k2)
} # replaces S_curves$FuelCostInd

#Logistic curve for evolution of experience/risk/preference curves
#This curve phases in user specified preferences representing factors other than costs.

# Phase in curve applied to difference between initial and final value of preference factor.

#Logistic
#f(t) = 1/(1+e^(-(t-k1)/k2))			note this is the logistic cumulative probability function.

#k1 =	4	years to half saturation
#k2 =	1	adjusts "steepness" at half saturation (width of probability function at 3 sigma)

# other options:
# - Gompertz
# - Hill (Hill is an alternative parameterization of log-logistic curve)
# -- ref: https://www.statforbiology.com/nonlinearregression/usefulequations#log-logistic_curve
# - Weibull

curve_for_preference_factor_phase_in <- function(t, k1 = 4, k2 = 1) {
  1/(1+exp(-(t-k1)/k2))
} # replaces S_curves$SCdata

# Adjustment factor for incremental cost
# Weibull: F(p) = 1-e^(-(p/k2)^k1)
# k1>1; k2>0
# k1 =	2	adjusts skew
# k2 =	0.6	adjusts saturation rate
# Pmax =	1	max probability

curve_for_adj_incr_cost <- function(incr_cost_div_by_base_cost, k1 = 2, k2 = 0.6) {
  1-pweibull(incr_cost_div_by_base_cost, shape = k1, scale = k2)
}


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
