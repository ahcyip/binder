# load libraries ####
library(readxl); library(tidyxl); library(janitor); library(magrittr); library(tidyverse); library(here)

#source(here::here("truck-app", "fns.R"))
source(here::here("Documents", "GitHub","binder", "truck-app", "fns.R"))

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
  here::here("Documents", "GitHub","binder", "truck-app", "TRUCK78_20200221_mg.xlsm")

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
# ranges of interest:
# TECH_OPT_COLS_Cls1
# TECH_OPT_PARAMS_Cls1
# (don't need the following:)
# (TECHOPTS_Cls1 and TECH_OPT_ROWS_Cls1 ("base a b c d e")) tech_opt_fuel_x ...
# and TechClsxx, FuelxClsx
# # TECH_OPT_DESC_Cls1 (alrdy included in params)
# (maybe these are used in other references elsewhere?)
#
# COST_Cls1
# SUBSIDY_Cls1
# FUEL1MPG_Cls1
# FUEL2MPG_Cls1
# CDRANGE_CLS1
#
# Years # only in cls 1 input sheet... should be in runmodel since it's general to all classes

FuelPrices <- read_all_names_in_tab("'Fuel Prices'", names, input$file1)

# grey tab fixed data
MarketData <- read_all_names_in_tab("'Market Data'", names, input$file1)
AdoptionDecision <- read_all_names_in_tab("'Adoption Decision'", names, input$file1)
FuelAvail <- read_all_names_in_tab("FuelAvail", names, input$file1)
S_curves <- read_all_names_in_tab("'S-curves'", names, input$file1)

#___________________________________________________________________
# Cls1-specific inputs ####

tech_opts <- Inputs78Sleep$TECH_OPT_PARAMS_Cls1 %>%
  mutate(cls = "78Sleep") %>%
  set_colnames(c(Inputs78Sleep$TECH_OPT_COLS_Cls1 %>% as.character(), "cls")) %>%
  clean_names() %>%
  mutate(description = make_clean_names(description)) %>%
  select(-na, -na_2) %>% # 2nd & 3rd empty columns
  mutate(monthly_mi_dep_savings = 0)
# to do: calc mileage dep costs: monthly budget, monthly savings as per blue calculation sheets
# Alicia Birky:
# Additional (non-fuel) costs that depend on accumulated miles, such as engine repower, hybrid battery replacement, etc.
#View(tech_opts)

costs <- Inputs78Sleep$COST_Cls1 %>% convert_input_to_long(tech_cols = "base_and_alts", values_to = "costs")
subsidy <- Inputs78Sleep$SUBSIDY_Cls1 %>% convert_input_to_long(tech_cols = "alts_only", values_to = "subsidy") # should allow for subsidy / tax (negative subsidy) on base fuel vehicle - would resolve inconsistency of alts only
fuel1_economy <- Inputs78Sleep$FUEL1MPG_Cls1 %>% convert_input_to_long(tech_cols = "base_and_alts", values_to = "f1_mpgde")
fuel2_economy <- Inputs78Sleep$FUEL2MPG_Cls1 %>% convert_input_to_long(tech_cols = "base_and_alts", values_to = "f2_mpgde")
CD_range <- Inputs78Sleep$CDRANGE_CLS1 %>% convert_input_to_long(tech_cols = "base_and_alts", values_to = "CD_range")

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
  add_column(yr = Inputs78Sleep$Years %>% pull()) %>%
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
# prepare calculation sheet and build up fleets and cohorts ####

# variable dictionary
#
# fuel
# - defined in tech_options
#
# f1_vmtshr
# - tech-specific, calculated from vmt, cdrange (class), opdays
#
# f1_price
# - year/fleet/fuel-specific
#
# vmt
# - cohort indicator
#
# fe/mpg
# - class/year/fuel/tech-specific

build_calc_sheet <- function() {
  costs %>%
    left_join(tech_opts, by = c("cls", "tech" = "description")) %>%
    left_join(subsidy, by = c("cls", "yr", "tech")) %>%
    left_join(fuel1_economy, by = c("cls", "yr", "tech")) %>%
    left_join(fuel2_economy, by = c("cls", "yr", "tech")) %>%
    left_join(CD_range, by = c("cls", "yr", "tech")) %>%
    mutate(flt = "Cent/NCent") %>%
    separate_rows(flt) %>%
    mutate(cohort = paste0(MarketData$Cohorts %>% pull(), collapse = "/")) %>%
    separate_rows(cohort, sep = "/") %>%
    left_join(mktdata_tbl, by = c("cls", "flt", "cohort" = "Cohorts")) %>%
    left_join(fuelprice_tbl, by = c("yr", "flt", "fuel_1" = "fuel")) %>%
    left_join(fuelprice_tbl, by = c("yr", "flt", "fuel_2" = "fuel"), suffix = c("_f1", "_f2")) %>%
    left_join(fuelavail_tbl, by = c("yr", "flt", "fuel_1" = "fuel")) %>%
    left_join(RunModel$opdays, by = "cls")
}



# Calculate payback and market shares ####

calc_pb_and_mktshrs <- function(calc_sheet) {
  calc_sheet_completed <- calc_sheet %>%
  mutate(VMTperday = VMT/OPDAYS, # aka "DailyRng"
         fuel1_vmtshr = if_else(fuel_2 == "N/A", 1,
                                1-pmin(CD_range /VMTperday, 1)),
         monthly_fuel_cost = VMT/12 * (fuel1_vmtshr * price_dge_f1 /f1_mpgde +
                                         (1-fuel1_vmtshr) * pmax(price_dge_f2, 0, na.rm = T) /f2_mpgde),
         base_monthly_fuel_cost = if_else(tech_type == "base", monthly_fuel_cost, NA_real_)) %>%
  group_by(yr, cls, flt, cohort) %>%
  mutate(base_monthly_fuel_cost = max(base_monthly_fuel_cost, na.rm = T)) %>% # i.e. the monthly_fuel_cost that isn't NA
  ungroup() %>%
  mutate(monthly_savings = base_monthly_fuel_cost - monthly_fuel_cost + monthly_mi_dep_savings,
         payback = case_when(range_limited == T & CD_range < VMTperday ~ 86,
                             tech_type == "base" ~ NA_real_,
                             T ~ payback(inc_cost, monthly_savings,
                                         RunModel$disc_rate %>% as.numeric() %>% divide_by(12),
                                         # discount rate should be re-calculated properly instead of simple divide by 12
                                         max_pd = 86)),
         # Preference Factor
         # Alicia Birky:
         # The preference factor represents the fraction, in competition with the base, that consumers would purchase if purchase cost and fuel cost were the same as the base.  Set value =0.5 for no bias/preference.
         pref_factor = if_else(yr < intro_yr, 0,
                               intro + curve_for_preference_factor_phase_in(yr - intro_yr) * (final - intro) * fuel_avail_pref_factor_multiplier),
# fuel availability affects preference factor here (and fuel availability doesn't affect anywhere else.)
# preference factor applied as a multiplier to either indiff curve result and adoption curve result.
# preference factor also affects the final market share calculation, where

         # Indifference Costs
         # Alicia Birky:
         # These are threshold costs within which the buyer is relatively indifferent to cost differences.
         # Probability of purchase is an S-Curve based on ratio of actual cost to threshold.
         indifference_calc = if_else(inc_cost <= incr_cost & monthly_savings >= -fuel_mo,
# incr_cost is the indifference cost - incr cost from Tech Options table
# fuel_mo is the indifference cost - fuel from Tech Options table
                                     pref_factor * curve_for_indiff_to_first_cost(inc_cost/incr_cost) * curve_for_indiff_to_fuel_cost_savings(monthly_savings/fuel_mo),
                                     0),

         # Incremental Cost Factor (adjusts adoption rate at the end)
         inc_cost_adj = if_else(inc_cost < 0, 1, curve_for_adj_incr_cost(inc_cost/base_cost)),

         adoption_curve = RunModel$adoption_curve %>% pull()) %>%

  left_join(adoption_tbl, by = c("payback" = "months", "adoption_curve" = "adoption_curve")) %>%

  mutate(cumulative_proportion_willing_to_adopt = if_else(is.na(cumulative_proportion_willing_to_adopt), 0,
                                                          cumulative_proportion_willing_to_adopt),
         indiv_tech_adoption_rate = if_else(payback > 85 | is.na(payback), 0, # no adoption with high payback
                                            pmin(1, # capped adoption at 100%
  # the larger of
  # "indifference calc", which is pref_factor * "indifference curves", if costs are below threshold, and
                                                pmax(indifference_calc,
  # "adoption rate", which is pref_factor / 0.5 (max pref factor) * adoption % according to payback
                                                    pref_factor/0.5*cumulative_proportion_willing_to_adopt))),
# =MIN(1,MAX(AD24,IF(Y24>85,0,(AI24/0.5)*VLOOKUP(Y24,AdoptDec,$AU$18,0))))
# So this is either pref_factor * adoption curve %, or pref_factor * "indifference curves"

# final adjustment of individual tech adoption rate by incremental_cost_adjustment
adj_indiv_tech_adoption_rate = if_else(tech_type == "base", 0,
                                                pmin(indiv_tech_adoption_rate * inc_cost_adj, 1))) %>%


# final market share is calculated by
# max adj_indiv_tech_adp_rate (largest adjusted share of the non-baseline technologies)
# multiplied by (split by)
# (pref_factor * adj_indiv_tech_adp_rate) / sum(pref_factor * adj_indiv_tech_adp_rate)
# this basically puts all other vehicles in a nest. it also limits all non-baseline to largest non-baseline tech share prediction
  group_by(yr, cls, flt, cohort) %>%
  mutate(final_mkt_shr = case_when(tech_type == "base" ~ NA_real_,
                                   sum(adj_indiv_tech_adoption_rate) == 0 ~ 0,
                                   TRUE ~ max(adj_indiv_tech_adoption_rate) * pref_factor * adj_indiv_tech_adoption_rate / sum(pref_factor * adj_indiv_tech_adoption_rate, na.rm = T)),
         final_mkt_shr = if_else(tech_type == "base", 1-sum(final_mkt_shr, na.rm = T), final_mkt_shr)) %>%
# =IF(SUM($AX42:$BB42)=0,0,MAX($AX42:$BB42)*(AI42*AX42/SUMPRODUCT($AI42:$AM42,$AX42:$BB42)))
  ungroup() %>%
  mutate(tech_shr_of_vmt = VMTShr * final_mkt_shr,
         tech_shr_of_trk = TruckShr * final_mkt_shr)

return(calc_sheet_completed)
}

#___________________________________________________________________
# Post-processing ####
calc_results_by_flt_and_tech <- function(calc_sheet_completed) {
  calc_sheet_completed %>%
    group_by(yr, cls, flt, tech) %>%
    summarise(tech_shr_of_vmt = sum(tech_shr_of_vmt), tech_shr_of_trk = sum(tech_shr_of_trk))
}
calc_results_by_tech <- function(calculated_results_by_flt_and_tech) {
  calculated_results_by_flt_and_tech %>%
    group_by(yr, cls, tech) %>%
    summarise(tech_shr_of_vmt = sum(tech_shr_of_vmt), tech_shr_of_trk = sum(tech_shr_of_trk))
}

#___________________________________________________________________
# Run ####

shares_by_tech <- build_calc_sheet() %>%
  calc_pb_and_mktshrs() %>%
  calc_results_by_flt_and_tech() %>%
  calc_results_by_tech()


#___________________________________________________________________
# tests ####

# payback
results %>%
  pivot_wider(names_from = "tech", values_from = "payback") %>%
  filter(yr > 2020 & flt == "NCent" & cohort == ">200") %>% # tech_type != "base" &
  View()
# replicates 2020-02-21 results.
# not sure why results are different in Copy of truck 0221 (0426 results)

# final market share check
results %>%
  pivot_wider(names_from = "tech", values_from = "final_mkt_shr") %>%
  filter(yr > 2020 & flt == "NCent" & cohort == ">200") %>% # tech_type != "base" &
  View()



shares_by_flt %>%
  filter(yr > 2020 & tech == "adv_conv") %>% # tech_type != "base" &
  View()

shares_by_tech %>%
  filter(yr > 2020 & tech == "adv_conv") %>% # tech_type != "base" &
  View()


#___________________________________________________________________
# Mkt Pen Veh-Mi graphs ####

plot_mktpen_vmt <- function (shares_by_tech, filename) {
  gg1 <- shares_by_tech %>%
    mutate(tech = tech %>% fct_relevel(c("adv_conv", "isg", "hev", "phev", "fchev", "conventional_diesel_ice")) %>% fct_rev()) %>%
    ggplot() +
    geom_area(aes(x = yr, y = tech_shr_of_vmt, fill = tech)) +
    facet_wrap(vars(cls)) +
    scale_x_continuous(limits = c(2016,2050)) +
    scale_fill_hue() +
    theme_minimal() +
    theme(legend.position = "bottom") +
    labs(x ="Year", y = "Share of new vehicle miles in market", fill = "Technology")
  gg1
  ggsave(filename, gg1, width = 8, height = 6)
  return(gg1)
}

plot_mktpen_vmt(shares_by_tech, "20200221.png")

#___________________________________________________________________
# manual nesting ####

# # turn off phev and fchev - set their intro year to 2051
# tech_opts %<>%
#   mutate(intro_yr = c(NA, 2020, 2020, 2020, 2050, 2050))
# 
# # run model
# shares_by_tech_conv_only <- build_calc_sheet() %>%
#   calc_pb_and_mktshrs() %>%
#   calc_results_by_flt_and_tech() %>%
#   calc_results_by_tech()
# 
# plot_mktpen_vmt(shares_by_tech_conv_only, "20200221-conventional_only.png")
# 
# # calculate a composite baseline - wt avg cost & fe (weighted by vmt share)
# new_baseline_composite <- shares_by_tech_conv_only %>%
#   filter(cls %in% "78Sleep" &
#            tech %in% c("conventional_diesel_ice", "adv_conv", "isg", "hev")) %>%
#   left_join(costs, by = c("cls","yr","tech")) %>%
#   left_join(tech_opts, by = c("cls", "tech" = "description")) %>%
#   left_join(subsidy, by = c("cls", "yr", "tech")) %>%
#   left_join(fuel1_economy, by = c("cls", "yr", "tech")) %>%
#   left_join(fuel2_economy, by = c("cls", "yr", "tech")) %>%
#   left_join(CD_range, by = c("cls", "yr", "tech")) %>%
#   group_by(yr, cls) %>%
#   summarise(tot_cost = sum(tot_cost * tech_shr_of_vmt / sum(tech_shr_of_vmt)),
#             f1_mpgde = sum(f1_mpgde * tech_shr_of_vmt / sum(tech_shr_of_vmt)),
#             tech_type = "base",
#             tech = "conventional_diesel_ice")
# 
# # insert baseline composite by modifying costs & f1_mpgde, then turn off advconv, isg, hev
# costs %<>%
#   left_join(new_baseline_composite, by = c("cls", "yr", "tech_type", "tech")) %>%
#   mutate(tot_cost = if_else(tech_type == "base", tot_cost.y, tot_cost.x)) %>%
#   select(-tot_cost.x, -tot_cost.y, -f1_mpgde)
# 
# fuel1_economy %<>%
#   left_join(new_baseline_composite, by = c("cls", "yr", "tech")) %>%
#   mutate(f1_mpgde = if_else(tech == "conventional_diesel_ice", f1_mpgde.y, f1_mpgde.x)) %>%
#   select(-f1_mpgde.x, -f1_mpgde.y, -tot_cost, -tech_type)
# 
# tech_opts %<>%
#   mutate(intro_yr = c(NA, 2050, 2050, 2050, 2020, 2020))
# 
# # re-run model
# shares_by_tech_after_composite <- build_calc_sheet() %>%
#   calc_pb_and_mktshrs() %>%
#   calc_results_by_flt_and_tech() %>%
#   calc_results_by_tech()
# 
# plot_mktpen_vmt(shares_by_tech_after_composite, "20200221-composite-conventional.png")
# 
# redistributed_conventional_composite <- shares_by_tech_after_composite %>%
#   left_join(shares_by_tech_conv_only, by = c("cls", "yr", "tech")) %>%
#   group_by(yr,cls) %>%
#   mutate(composite_shr_of_vmt = if_else(tech == "conventional_diesel_ice",
#                                         tech_shr_of_vmt.x, NA_real_),
#          composite_shr_of_vmt = max(composite_shr_of_vmt, na.rm=T),
#          tech_shr_of_vmt = if_else(tech %in% c("conventional_diesel_ice", "adv_conv", "isg", "hev"), composite_shr_of_vmt * tech_shr_of_vmt.y, tech_shr_of_vmt.x))
# 
# plot_mktpen_vmt(redistributed_conventional_composite, "20200221-redist-composite.png")
# 
# # reload Cls1-specific inputs when done
# 







