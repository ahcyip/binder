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
input$file1$datapath <- #here::here("TRUCK78_20200710.xlsm")
  #here::here("truck-app", "workingCopy of TRUCK78_20200221.xlsm")
  #here::here("truck-app", "TRUCK78_20200221.xlsm")
  #here::here("truck-app", "TRUCK78_20200831.xlsm")
  here("truck-app", "TRUCK78_20201103.xlsm")
input$file2$datapath <- here("truck-app", "TRUCK46_20201103.xlsm")

names1 <- get_names(input$file1)
names2 <- get_names(input$file2)
# View(names1)
# View(names2)
#
# names_comparison_btwn_78and46 <- names1 %>%
#   select(-rId, -sheet, -comment, -hidden, -is_range) %>%
#   full_join(names2 %>% select(-rId, -sheet, -comment, -hidden, -is_range),
#             by = c("location", "name"),
#             suffix = c(".78", ".46")) %>%
#   select(location, name, everything())
# names_comparison_btwn_78and46 %>% View()

# read yellow tab inputs
# disc_rate is read in, but might want to set this in app.
## missing:
## actual selected market adoption curve in RunModel (currently assumed Moderate)
## opdays now set here (currently assumed 250). Should be in RunModel tab instead of at top of each tab.
## disc_rate should be set here/in app but is currently being read in.
RunModel1 <- read_all_names_in_tab("'Run Model'", names1, input$file1)
RunModel1$adoption_curve <- tibble(cls = c("78Sleep","78Day","78SU"), adoption_curve = "Moderate")
# this needs to be replaced with either actual selection in runmodel OR be set in the app and removed from the worksheet as an option.
RunModel1$opdays <- tibble(cls = c("78Sleep","78Day","78SU"), OPDAYS = c(250,250,250)) # this is manually entered right now.

RunModel2 <- read_all_names_in_tab("'Run Model'", names2, input$file2)
RunModel2$adoption_curve <- tibble(cls = c("46Diesel","46Gas"), adoption_curve = "Moderate")
# this needs to be replaced with either actual selection in runmodel OR be set in the app and removed from the worksheet as an option.
RunModel2$opdays <- tibble(cls = c("46Diesel","46Gas"), OPDAYS = c(250,250)) # this is manually entered right now.

RunModel_adoption_curve <- bind_rows(RunModel1$adoption_curve, RunModel2$adoption_curve)
RunModel_opdays <- bind_rows(RunModel1$opdays, RunModel2$opdays)

Inputs78Sleep <- read_all_names_in_tab("'Inputs 7&8 Sleep'", names1, input$file1)
Inputs78Day <- read_all_names_in_tab("'Inputs 7&8 Day'", names1, input$file1)
Inputs78SU <- read_all_names_in_tab("'Inputs 7&8 SU'", names1, input$file1)
# Ignoring / Don't need the following Excel named ranges:
# (TECHOPTS_Cls1 and TECH_OPT_ROWS_Cls1 ("base a b c d e")) tech_opt_fuel_x ...
# and TechClsxx, FuelxClsx
# # TECH_OPT_DESC_Cls1 (alrdy included in params) (somewhat useful - used in convert input to long)
Inputs46Gas <- read_all_names_in_tab("'Inputs 4-6 Gas'", names2, input$file2)
Inputs46Diesel <- read_all_names_in_tab("'Inputs 4-6 Diesel'", names2, input$file2)


FuelPrices1 <- read_all_names_in_tab("'Fuel Prices'", names1, input$file1)
FuelPrices2 <- read_all_names_in_tab("'Fuel Prices'", names2, input$file2)


# grey tab fixed data
MarketData1 <- read_all_names_in_tab("'Market Data'", names1, input$file1)
AdoptionDecision1 <- read_all_names_in_tab("'Adoption Decision'", names1, input$file1)
FuelAvail1 <- read_all_names_in_tab("FuelAvail", names1, input$file1)
#S_curves1 <- read_all_names_in_tab("'S-curves'", names1, input$file1) # currently not used. s-curves (indifference algorithm, incremental cost, preference factor) are in written in R in fns.R

MarketData2 <- read_all_names_in_tab("'Market Data'", names2, input$file2)
#AdoptionDecision2 <- read_all_names_in_tab("'Adoption Decision'", names2, input$file2) # not used (assume same as 78)
FuelAvail2 <- read_all_names_in_tab("FuelAvail", names2, input$file2)
#S_curves2 <- read_all_names_in_tab("'S-curves'", names2, input$file2) # not used


#___________________________________________________________________
# Load inputs ####

Years <- Inputs78Sleep$Years # this only exists in Cls 1 sheet - technically should be in runmodel and ensured consistent between all classes.
stopifnot(Years == Inputs78Gas$Years)
# also, only used to fill in years for fuel availability ...

clsnames <- list("78Sleep", "78Day", "78SU", "46Gas", "46Diesel")

tech_opts <- list(params = list(Inputs78Sleep$TECH_OPT_PARAMS_Cls1,
                                Inputs78Day$TECH_OPT_PARAMS_Cls2,
                                Inputs78SU$TECH_OPT_PARAMS_Cls3,
                                Inputs46Gas$TECH_OPT_PARAMS_Cls1 %>% select(-...1) %>% add_column(...1 = Inputs46Diesel$TECH_OPT_DESC_Cls2$...1) %>% select(...1, everything()),
                                Inputs46Diesel$TECH_OPT_PARAMS_Cls2
                                ),
                  clsnames = clsnames,
                  cols = list(Inputs78Sleep$TECH_OPT_COLS_Cls1, # these should be all the same
                              Inputs78Day$TECH_OPT_COLS_Cls2,
                              Inputs78SU$TECH_OPT_COLS_Cls3,
                              Inputs46Gas$TECH_OPT_COLS_Cls1,
                              Inputs46Diesel$TECH_OPT_COLS_Cls2
                              )) %>%
  pmap_dfr(~load_tech_options(..1, ..2, ..3))

# to do: input/calc mileage dep costs: monthly budget, monthly savings as per blue calculation sheets
# Alicia Birky:
# Additional (non-fuel) costs that depend on accumulated miles, such as engine repower, hybrid battery replacement, etc.

tech_opt_desc <- list(Inputs78Sleep$TECH_OPT_DESC_Cls1,
                      Inputs78Day$TECH_OPT_DESC_Cls2,
                      Inputs78SU$TECH_OPT_DESC_Cls3,
                      Inputs46Diesel$TECH_OPT_DESC_Cls2, # Inputs46Gas$TECH_OPT_DESC_Cls1, # skip bad names conv gasoline, not used, etc.
                      Inputs46Diesel$TECH_OPT_DESC_Cls2
                      )

costs <- list(input_tables = list(Inputs78Sleep$COST_Cls1,
                                  Inputs78Day$COST_Cls2,
                                  Inputs78SU$COST_Cls3,
                                  Inputs46Gas$COST_Cls1,
                                  Inputs46Diesel$COST_Cls2
                                  ),
              tech_opt_desc = tech_opt_desc,
              clsnames = clsnames) %>%
  pmap_dfr(~convert_input_to_long(..1, ..2, ..3, tech_cols = "base_and_alts", values_to = "costs"))

subsidy <- list(input_tables = list(Inputs78Sleep$SUBSIDY_Cls1,
                                    Inputs78Day$SUBSIDY_Cls2,
                                    Inputs78SU$SUBSIDY_Cls3,
                                    Inputs46Gas$SUBSIDY_Cls1,
                                    Inputs46Diesel$SUBSIDY_Cls2
                                    ),
                tech_opt_desc = tech_opt_desc,
                clsnames = clsnames) %>%
  pmap_dfr(~convert_input_to_long(..1, ..2, ..3, tech_cols = "alts_only", values_to = "subsidy"))
# should allow for subsidy / tax (negative subsidy) on base fuel vehicle - would resolve inconsistency of alts only

fuel1_economy <- list(input_tables = list(Inputs78Sleep$FUEL1MPG_Cls1,
                                          Inputs78Day$FUEL1MPG_Cls2,
                                          Inputs78SU$FUEL1MPG_Cls3,
                                          Inputs46Gas$FUEL1MPG_Cls1,
                                          Inputs46Diesel$FUEL1MPG_Cls2
                                          ),
                      tech_opt_desc = tech_opt_desc,
                      clsnames = clsnames) %>%
  pmap_dfr(~convert_input_to_long(..1, ..2, ..3, tech_cols = "base_and_alts", values_to = "f1_mpgde"))

fuel2_economy <- list(input_tables = list(Inputs78Sleep$FUEL2MPG_Cls1,
                                          Inputs78Day$FUEL2MPG_Cls2,
                                          Inputs78SU$FUEL2MPG_Cls3,
                                          Inputs46Gas$FUEL2MPG_Cls1,
                                          Inputs46Diesel$FUEL2MPG_Cls2
                                          ),
                      tech_opt_desc = tech_opt_desc,
                      clsnames = clsnames) %>%
  pmap_dfr(~convert_input_to_long(..1, ..2, ..3, tech_cols = "base_and_alts", values_to = "f2_mpgde"))

CD_range <- list(input_tables = list(Inputs78Sleep$CDRANGE_CLS1,
                                     Inputs78Day$CDRANGE_CLS2,
                                     Inputs78SU$CDRANGE_CLS3,
                                     Inputs46Gas$CDRANGE_CLS1,
                                     Inputs46Diesel$CDRANGE_CLS2),
                 tech_opt_desc = tech_opt_desc,
                 clsnames = clsnames) %>%
  pmap_dfr(~convert_input_to_long(..1, ..2, ..3, tech_cols = "base_and_alts", values_to = "CD_range"))

#___________________________________________________________________
# general inputs ####

# 2 missing columns in marketdata in cls4-6 workbook

mktdata_tbl <- MarketData1 %>% bind_cols() %>% set_colnames(names(MarketData1)) %>% rename_with(~paste0("C78", .)) %>%
  bind_cols(MarketData2 %>% bind_cols() %>% set_colnames(names(MarketData2)) %>% mutate(Cls3CentTruckShr = 0, Cls3NCentTruckShr = 0) %>% rename_with(~paste0("C46", .))) %>%
  select(-C46Cohorts, Cohorts = C78Cohorts) %>%
  pivot_longer(-Cohorts, names_to = c("cls", "flt", ".value"), names_pattern = "(...Cls.)(.*Cent)(.*)") %>%
  select(-DailyRng) %>% # DailyRng is VMT/opdays (i.e. it depends on variable parameters)
  mutate(cls = case_when(cls == "C78Cls1" ~ "78Sleep",
                         cls == "C78Cls2" ~ "78Day",
                         cls == "C78Cls3" ~ "78SU",
                         cls == "C46Cls1" ~ "46Gas",
                         cls == "C46Cls2" ~ "46Diesel",
                         cls == "C46Cls3" ~ NA_character_))

fuelprice78_tbl <- FuelPrices1$FuelPriceCent %>% set_colnames(paste(FuelPrices1$FuelPriceCols, "Cent", sep = "_")) %>%
  bind_cols(FuelPrices1$FuelPriceNCent %>% set_colnames(paste(FuelPrices1$FuelPriceCols, "NCent", sep = "_"))) %>%
  select(-Year_NCent, yr = Year_Cent) %>%
  pivot_longer(-yr, names_to = c("fuel", "flt"), names_pattern = "(.*)_(.*)", values_to = "price_dge")
fuelprice46_tbl <- FuelPrices2$FuelPriceCent %>% set_colnames(paste(FuelPrices2$FuelPriceCols, "Cent", sep = "_")) %>%
  bind_cols(FuelPrices2$FuelPriceNCent %>% set_colnames(paste(FuelPrices2$FuelPriceCols, "NCent", sep = "_"))) %>%
  select(-Year_NCent, yr = Year_Cent) %>%
  pivot_longer(-yr, names_to = c("fuel", "flt"), names_pattern = "(.*)_(.*)", values_to = "price_dge")
fuelprice_tbl <- bind_rows(fuelprice78_tbl %>% mutate(cls = "78Sleep"),
                           fuelprice78_tbl %>% mutate(cls = "78Day"),
                           fuelprice78_tbl %>% mutate(cls = "78SU"),
                           fuelprice46_tbl %>% mutate(cls = "46Gas"),
                           fuelprice46_tbl %>% mutate(cls = "46Diesel"))

fuelavail78_tbl <- FuelAvail1$fuel_avail %>%
  set_colnames(FuelPrices1$FuelPriceCols[2:length(FuelPrices1$FuelPriceCols)]) %>%
  add_column(yr = Years %>% pull()) %>%
  pivot_longer(-yr, names_to = "fuel", values_to = "NCent") %>%
  mutate(Cent = 1) %>%
  pivot_longer(-(yr:fuel), names_to = "flt", values_to = "fuel_avail_pref_factor_multiplier")
fuelavail46_tbl <- FuelAvail2$fuel_avail %>%
  set_colnames(FuelPrices2$FuelPriceCols[2:length(FuelPrices2$FuelPriceCols)]) %>%
  add_column(yr = Years %>% pull()) %>%
  pivot_longer(-yr, names_to = "fuel", values_to = "NCent") %>%
  mutate(Cent = 1) %>%
  pivot_longer(-(yr:fuel), names_to = "flt", values_to = "fuel_avail_pref_factor_multiplier")
fuelavail_tbl <- bind_rows(fuelavail78_tbl %>% mutate(cls = "78Sleep"),
                           fuelavail78_tbl %>% mutate(cls = "78Day"),
                           fuelavail78_tbl %>% mutate(cls = "78SU"),
                           fuelavail46_tbl %>% mutate(cls = "46Gas"),
                           fuelavail46_tbl %>% mutate(cls = "46Diesel"))


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
adoption_tbl <- AdoptionDecision1$AdoptDec %>%
  set_colnames(AdoptionDecision1$AdoptCols) %>%
  pivot_longer(-months, names_to = "adoption_curve", values_to = "cumulative_proportion_willing_to_adopt")

# assume same in 78 and 46


#___________________________________________________________________
# Run ####

completed_calc_sheet <- build_calc_sheet() %>% calc_pb_and_mktshrs()

shares_by_tech <- completed_calc_sheet %>% calc_results_by_tech()
# shares_by_tech %>% write_csv("csv_output/shares_by_tech.csv")


#___________________________________________________________________
# tests (compare against xls) ####

# payback
payback_results <- completed_calc_sheet %>%
  select(yr, tech, payback, flt, cohort, cls, tech_type) %>%
  pivot_wider(names_from = "tech", values_from = "payback") %>%
  filter(yr > 2020 & flt == "NCent" & cohort == ">200" & cls == "78Sleep" & tech_type != "base")
# these are the parameters for the last run typically left in the truck spreadsheet
payback_results %>% View("payback")
# TRUCK78_20200221 results successfully replicated.
# Payback results were different from those in Copy of truck 20200221 (last run in 20200426)

payback_results %>%
  write_csv("csv_output/payback.csv")

# final market share check
completed_calc_sheet %>%
  select(yr, tech, final_mkt_shr, flt, cohort, cls) %>%
  mutate(final_mkt_shr = round(final_mkt_shr * 100, 1)) %>%
  pivot_wider(names_from = "tech", values_from = "final_mkt_shr") %>%
  filter(yr > 2020 & flt == "NCent" & cohort == ">200" & cls == "78SU") %>% # tech_type != "base" &
    # these are the parameters for the last run typically left in the truck spreadsheet
  View("mktshr")

# completed_calc_sheet %>%
#   calc_results_by_flt_and_tech() %>%
#   filter(yr > 2020 & tech == "adv_conv") %>% # tech_type != "base" &
#   View()

# shares_by_tech %>%
#   filter(yr > 2020 & tech == "adv_conv") %>% # tech_type != "base" &
#   View()

Cls78SleepTrkShr <- 47.6/100
Cls78SleepVMTShr <- 66.84/100
Cls78DayTrkShr <- 25.6/100
Cls78DayVMTShr <- 24.19/100
Cls78SUTrkShr <- 26.8/100
Cls78SUVMTShr <- 8.97/100

# Market Penetration sheet in xls
shares_by_tech %>%
  filter(cls %in% clsnames_in_78) %>%
  select(-tech_shr_of_trk_v7, -tech_shr_of_vmt, -tech_shr_of_vmt_v7) %>%
  ungroup() %>%
  mutate(cls = fct_relevel(cls, c("78Sleep", "78Day", "78SU")),
         tech = fct_relevel(tech, c("conventional_diesel_ice", "adv_conv", "hev", "bev", "fcev", "phev")),
         tech_shr_of_trk = round(tech_shr_of_trk * 100, 1)) %>%
  arrange(cls, tech) %>%
  pivot_wider(names_from = c("cls", "tech"), names_prefix = "Cls",  values_from = "tech_shr_of_trk") %>%
  rowwise() %>%
  mutate(Cls78_base = round(sum(Cls78Sleep_conventional_diesel_ice * Cls78SleepTrkShr,
                                Cls78Day_conventional_diesel_ice * Cls78DayTrkShr,
                                Cls78SU_conventional_diesel_ice * Cls78SUTrkShr), 1),
         Cls78_alt = 100 - Cls78_base) %>%
         #sum(c_across(Cls78Sleep_conventional_diesel_ice:Cls78SU_phev)) - Cls78_base) %>%
  View("Mkt Pen")

# Mkt Pen Veh-Mi
shares_by_tech %>%
  filter(cls %in% clsnames_in_78) %>%
  select(-tech_shr_of_trk_v7, -tech_shr_of_trk, -tech_shr_of_vmt_v7) %>%
  ungroup() %>%
  mutate(cls = fct_relevel(cls, c("78Sleep", "78Day", "78SU")),
         tech = fct_relevel(tech, c("conventional_diesel_ice", "adv_conv", "hev", "bev", "fcev", "phev")),
         tech_shr_of_vmt = round(tech_shr_of_vmt * 100, 1)) %>%
  arrange(cls, tech) %>%
  pivot_wider(names_from = c("cls", "tech"), names_prefix = "Cls",  values_from = "tech_shr_of_vmt") %>%
  rowwise() %>%
  mutate(Cls78_base = round(sum(Cls78Sleep_conventional_diesel_ice * Cls78SleepVMTShr,
                                Cls78Day_conventional_diesel_ice * Cls78DayVMTShr,
                                Cls78SU_conventional_diesel_ice * Cls78SUVMTShr), 1),
         Cls78_alt = 100 - Cls78_base) %>%
  #sum(c_across(Cls78Sleep_conventional_diesel_ice:Cls78SU_phev)) - Cls78_base) %>%
  View("Mkt Pen Veh-Mi")




#___________________________________________________________________
# Mkt Pen Veh-Mi graphs ####

(plot_20200827 <- plot_mktpen_of_techs(shares_by_tech, "20200827.png", "tech_shr_of_vmt"))


# testing v7
#plot_20200827_v7 <- plot_mktpen_of_techs(shares_by_tech, "20200827_v7.png", "tech_shr_of_vmt_v7")
# library(patchwork)
# plot_20200827 / plot_20200827_v7



#___________________________________________________________________
# diagnose difference between v6 & v7 (pf^2 removed)
# shares_by_tech %>%
#   mutate(tech_shr_of_vmt_diff = tech_shr_of_vmt_v7 - tech_shr_of_vmt) %>%
#   pull(tech_shr_of_vmt_diff) %>%
#   summary()
# no diff in tech_shr_of_vmt!

# completed_calc_sheet %>% pull(pref_factor) %>% summary()
# completed_calc_sheet %>% filter(pref_factor>0) %>% View()
# completed_calc_sheet %>% filter(pref_factor>0) %>% pull(yr) %>% summary()
# not sure why some PF go to 2050


# to do:
# ///DONE - cls 4-6 read in ...
# nesting diesel and hev

#___________________________________________________________________
# Other analyses ####

# ANL request: class, year, powertrain, mileage bin, share_sales, share_mileage
costs %>%
  filter(yr %in% c(2019, 2020, 2021, 2022)) %>%
  pivot_wider(names_from = "tech", values_from = "inc_cost") %>%
  arrange(yr) %>%
  View()
# incremental cost of adv_conv = $0 seems to be causing problems

completed_calc_sheet %>%
  group_by(yr, cls) %>%
  summarize(n=n(), sum_salesshr = sum(tech_shr_of_trk), sum_vmtshr = sum(tech_shr_of_vmt)) %>%
  View()
# 4 classes in 2021 (except gasoline) have extra/duplicated share, 200% conventional + adv_conv

results_for_ANL_20201103 <- completed_calc_sheet %>%
  group_by(cls, yr, tech, cohort) %>%
  summarise(tech_shr_of_vmt = sum(tech_shr_of_vmt),
            tech_shr_of_trk = sum(tech_shr_of_trk), .groups = "drop") %>%
  mutate(cls = cls %>% fct_relevel(clsnames),
         tech = tech %>% fct_recode(conventional_ice = "conventional_diesel_ice") %>%
           fct_relevel(c("conventional_ice", "adv_conv", "hev", "phev", "bev", "fcev")),
         cohort = cohort %>% fct_relevel(c("0-20", "20-40", "40-60", "60-80", "80-100", "100-120", "120-140",
                                           "140-160", "160-180", "180-200", ">200"))) %>%
  arrange(cls, yr, tech, cohort) %>%
  select(class = cls, year = yr, powertrain = tech, mileage_bin = cohort, share_sales = tech_shr_of_trk, share_mileage = tech_shr_of_vmt)

results_for_ANL_20201103 %>%
  group_by(year) %>%
  summarize(n=n(), sum_salesshr = sum(share_sales), sum_vmtshr = sum(share_mileage)) %>%
  View()

results_for_ANL_20201103 %>%
  filter(year %in% c(2020,2021)) %>%
  group_by(year, class, powertrain) %>%
  summarize(n=n(), sum_salesshr = sum(share_sales), sum_vmtshr = sum(share_mileage)) %>%
  View()

write_csv(results_for_ANL_20201103, "results_for_ANL_20201103_v3.csv")

results_for_ANL_20201103 %>%
  filter(year %in% c(2020, 2021, 2022, 2030, 2040, 2050)) %>%
  ggplot() +
  geom_col(aes(x = mileage_bin, y = share_mileage, fill = powertrain %>% fct_rev()), position = "stack") +
  facet_grid(rows = vars(class), cols = vars(year), scales = "free_y") +
  scale_fill_manual(values = c("#0079C2", "#00A4E4", "#F7A11A", "#FFC423", "#5D9732",
                               "#8CC63F"
                               #, "#933C06", "#D9531E", "#5E6A71", "#D1D5D8"
                               )) +
  labs(x = "Mileage bin [mi/yr]",
       y = "VMT share of new sales within class",
       fill = "Powertrain") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
ggsave("results_for_ANL.png", width = 8, height = 6)
#___________________________________________________________________
# hdstock ####


# for HDStock compatibility, I could write to named ranges Cls1MktShrCentA etc.
# but no point if I'm expanding tech slots...
#


# sheet-specific named ranges
# vehprices, mkt pen - mi,
#
# save to same input file
# stamp it with date
#
# add named ranges?? if new techs
#
# directly write to hdstock
