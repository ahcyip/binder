# load libraries & fns ####
library(readxl); library(tidyxl); library(janitor); library(magrittr); library(tidyverse); library(here)
library(metR)

source(here::here("truck-app", "fns.R"))

#___________________________________________________________________
# read inputs from TRUCK.xlsm ####

# alterations from app.R
# -removed "reactive"
# -names instead of names()
# -added definition for input instead of user-selected

input <- list()
#input$file1$datapath <- #here::here("TRUCK78_20200710.xlsm")
  #here::here("truck-app", "workingCopy of TRUCK78_20200221.xlsm")
  #here::here("truck-app", "TRUCK78_20200221.xlsm")

input$file1$datapath <- #here::here("TRUCK78_20200710.xlsm")
  #here::here("truck-app", "workingCopy of TRUCK78_20200221.xlsm")
  here::here("truck-app", "TRUCK78_20200827_ab.xlsm")
  #here::here("truck-app", "TRUCK78_20200710_original.xlsm")

names <- get_names(input$file1)
#View(names)

#reading an individual name
#read_excel_range("TECH_OPT_PARAMS_Cls1", FALSE, names, input$file1)

# read yellow tab inputs (whole tabs at a time)
## missing:
## classes other than 7&8 sleeper
## actual selected market adoption curve in RunModel
## opdays now set here. could be in RunModel
## disc_rate should be set here but is currently being read in.
##
RunModel <- read_all_names_in_tab("'Run Model'", names, input$file1)
RunModel$adoption_curve <- tibble(adoption_curve = "Moderate")
# this needs to be replaced with either actual selection in runmodel OR be set in the app and removed from the worksheet as an option.
RunModel$opdays <- tibble(cls = c("78Sleep","78Day","78SU"), OPDAYS = c(250,250,250))


Inputs78Sleep <- read_all_names_in_tab("'Inputs 7&8 Sleep'", names, input$file1)
Inputs78Day <- read_all_names_in_tab("'Inputs 7&8 Day'", names, input$file1)
Inputs78SU <- read_all_names_in_tab("'Inputs 7&8 SU'", names, input$file1)
# Ignoring / Don't need the following Excel named ranges:
# (TECHOPTS_Cls1 and TECH_OPT_ROWS_Cls1 ("base a b c d e")) tech_opt_fuel_x ...
# and TechClsxx, FuelxClsx
# # TECH_OPT_DESC_Cls1 (alrdy included in params) (somewhat useful - used in convert input to long)

FuelPrices <- read_all_names_in_tab("'Fuel Prices'", names, input$file1)

# grey tab fixed data
MarketData <- read_all_names_in_tab("'Market Data'", names, input$file1)
AdoptionDecision <- read_all_names_in_tab("'Adoption Decision'", names, input$file1)
FuelAvail <- read_all_names_in_tab("FuelAvail", names, input$file1)
S_curves <- read_all_names_in_tab("'S-curves'", names, input$file1)

#___________________________________________________________________
# Load inputs ####

Years <- Inputs78Sleep$Years # this only exists in Cls 1 sheet - technically should be in runmodel and ensured consistent between all classes.

clsnames_in_78 <- list("78Sleep", "78Day", "78SU")

tech_opts <- list(params = list(Inputs78Sleep$TECH_OPT_PARAMS_Cls1,
                                Inputs78Day$TECH_OPT_PARAMS_Cls2,
                                Inputs78SU$TECH_OPT_PARAMS_Cls3),
                  clsnames = clsnames_in_78,
                  cols = list(Inputs78Sleep$TECH_OPT_COLS_Cls1, # these should be all the same
                              Inputs78Day$TECH_OPT_COLS_Cls2,
                              Inputs78SU$TECH_OPT_COLS_Cls3)) %>%
  pmap_dfr(~load_tech_options(..1, ..2, ..3))

# to do: input/calc mileage dep costs: monthly budget, monthly savings as per blue calculation sheets
# Alicia Birky:
# Additional (non-fuel) costs that depend on accumulated miles, such as engine repower, hybrid battery replacement, etc.

tech_opt_desc_list_of_3_in_78 <- list(Inputs78Sleep$TECH_OPT_DESC_Cls1,
                                      Inputs78Day$TECH_OPT_DESC_Cls2,
                                      Inputs78SU$TECH_OPT_DESC_Cls3)

costs <- list(input_tables = list(Inputs78Sleep$COST_Cls1,
                                  Inputs78Day$COST_Cls2,
                                  Inputs78SU$COST_Cls3),
              tech_opt_desc = tech_opt_desc_list_of_3_in_78,
              clsnames = clsnames_in_78) %>%
  pmap_dfr(~convert_input_to_long(..1, ..2, ..3, tech_cols = "base_and_alts", values_to = "costs"))

subsidy <- list(input_tables = list(Inputs78Sleep$SUBSIDY_Cls1,
                                    Inputs78Day$SUBSIDY_Cls2,
                                    Inputs78SU$SUBSIDY_Cls3),
                tech_opt_desc = tech_opt_desc_list_of_3_in_78,
                clsnames = clsnames_in_78) %>%
  pmap_dfr(~convert_input_to_long(..1, ..2, ..3, tech_cols = "alts_only", values_to = "subsidy"))
# should allow for subsidy / tax (negative subsidy) on base fuel vehicle - would resolve inconsistency of alts only

fuel1_economy <- list(input_tables = list(Inputs78Sleep$FUEL1MPG_Cls1,
                                          Inputs78Day$FUEL1MPG_Cls2,
                                          Inputs78SU$FUEL1MPG_Cls3),
                      tech_opt_desc = tech_opt_desc_list_of_3_in_78,
                      clsnames = clsnames_in_78) %>%
  pmap_dfr(~convert_input_to_long(..1, ..2, ..3, tech_cols = "base_and_alts", values_to = "f1_mpgde"))

fuel2_economy <- list(input_tables = list(Inputs78Sleep$FUEL2MPG_Cls1,
                                          Inputs78Day$FUEL2MPG_Cls2,
                                          Inputs78SU$FUEL2MPG_Cls3),
                      tech_opt_desc = tech_opt_desc_list_of_3_in_78,
                      clsnames = clsnames_in_78) %>%
  pmap_dfr(~convert_input_to_long(..1, ..2, ..3, tech_cols = "base_and_alts", values_to = "f2_mpgde"))

CD_range <- list(input_tables = list(Inputs78Sleep$CDRANGE_CLS1,
                                     Inputs78Day$CDRANGE_CLS2,
                                     Inputs78SU$CDRANGE_CLS3),
                 tech_opt_desc = tech_opt_desc_list_of_3_in_78,
                 clsnames = clsnames_in_78) %>%
  pmap_dfr(~convert_input_to_long(..1, ..2, ..3, tech_cols = "base_and_alts", values_to = "CD_range"))

#___________________________________________________________________
# general inputs ####

mktdata_tbl <- MarketData %>%
  bind_cols() %>%
  set_colnames(names(MarketData)) %>%
  pivot_longer(-Cohorts, names_to = c("cls", "flt", ".value"), names_pattern = "(Cls.)(.*Cent)(.*)") %>%
  select(-DailyRng) %>% # DailyRng is VMT/opdays (i.e. it depends on variable parameters)
  mutate(cls = case_when(cls == "Cls1" ~ "78Sleep",
                         cls == "Cls2" ~ "78Day",
                         cls == "Cls3" ~ "78SU"))

fuelprice_tbl <- FuelPrices$FuelPriceCent %>% set_colnames(paste(FuelPrices$FuelPriceCols, "Cent", sep = "_")) %>%
  bind_cols(FuelPrices$FuelPriceNCent %>% set_colnames(paste(FuelPrices$FuelPriceCols, "NCent", sep = "_"))) %>%
  select(-Year_NCent, yr = Year_Cent) %>%
  pivot_longer(-yr, names_to = c("fuel", "flt"), names_pattern = "(.*)_(.*)", values_to = "price_dge")

fuelavail_tbl <- FuelAvail$fuel_avail %>%
  set_colnames(FuelPrices$FuelPriceCols[2:length(FuelPrices$FuelPriceCols)]) %>%
  add_column(yr = Years %>% pull()) %>%
  pivot_longer(-yr, names_to = "fuel", values_to = "NCent") %>%
  mutate(Cent = 1) %>%
  pivot_longer(-(yr:fuel), names_to = "flt", values_to = "fuel_avail_pref_factor_multiplier")

# FUEL AVAILABILITY (eventually will replace FuelAvail$fuel_avail in the future)

# library(curl)
# tf <- tempfile(fileext = ".xlsx")
# curl::curl_download("https://afdc.energy.gov/files/u/data/data_source/10332/10332_alt_fueling_stations_fuel_1-8-20.xlsx", tf)
# readxl::read_excel(tf, skip = 2, col_names = TRUE, trim_ws = TRUE) %>%
#       View()

# DOE AFDC: US Alt Fuel Stations by Fuel Type

# https://afdc.energy.gov/data/10332

# This chart shows the trend of U.S. alternative fueling stations by fuel type from 1992 to 2018. Propane stations were the most numerous until 2011, when they were surpassed by electric vehicle supply equipment (EVSE), or charging units. The growth in EVSE units accelerated starting in 2011, following the 2010 increase of plug-in electric vehicles offered by major automakers. 2016 experienced the largest growth for EVSE to support the growing electric vehicle population, followed closely by 2017 and 2018. The number of EVSE units is expected to increase as the population of electric vehicles continues to grow. The number of E85 stations has been increasing steadily since 2004, as the number of flex-fuel vehicles available from major manufacturers has increased. The number of CNG stations decreased between 1996 and 2006 (despite the increase in CNG sales during this time) largely because the average station size was increasing.

# Source: Alternative Fuels Data Center (AFDC), either directly (afdc.energy.gov/stations/states) or from historical Transportation Energy Data Books (www.osti.gov)
# Notes: Starting in 2011, electric charging equipment was counted by the outlet rather than by the geographical location (i.e., station). This is different than other fuels, which only count the geographical location regardless of how many dispensers or nozzles are on site.

#___________________________________________________________________
# adoption inputs ####
# to be replaced by weibull model fitted to survey data
adoption_tbl <- AdoptionDecision$AdoptDec %>%
  set_colnames(AdoptionDecision$AdoptCols) %>%
  pivot_longer(-months, names_to = "adoption_curve", values_to = "cumulative_proportion_willing_to_adopt")



#___________________________________________________________________
# Run ####

shares_by_tech <- build_calc_sheet() %>%
  calc_pb_and_mktshrs() %>%
  calc_results_by_flt_and_tech() %>%
  calc_results_by_tech()

shares_by_tech %>%
  write_csv("csv_output/shares_by_tech.csv")


#___________________________________________________________________
# tests ####

# payback
payback_result <- build_calc_sheet() %>%
  calc_pb_and_mktshrs() %>%
  pivot_wider(names_from = "tech", values_from = "payback") %>%
  filter(yr > 2020 & flt == "NCent" & cohort == ">200") # tech_type != "base" &

payback_result %>%
  write_csv("csv_output/payback.csv")
# replicates 2020-02-21 results.
# not sure why results are different in Copy of truck 0221 (0426 results)


# final market share check
build_calc_sheet() %>%
  calc_pb_and_mktshrs() %>%
  pivot_wider(names_from = "tech", values_from = "final_mkt_shr") %>%
  filter(yr > 2020 & flt == "NCent" & cohort == ">200") %>% # tech_type != "base" &
  View()


# shares_by_flt %>%
#   filter(yr > 2020 & tech == "adv_conv") %>% # tech_type != "base" &
#   View()
#
# shares_by_tech %>%
#   filter(yr > 2020 & tech == "adv_conv") %>% # tech_type != "base" &
#   View()


#___________________________________________________________________
# Mkt Pen Veh-Mi graphs ####

plot_mktpen_vmt(shares_by_tech, "20200221_fchev_test3cls.png")

#___________________________________________________________________
