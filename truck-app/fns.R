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

payback <- function(inc_cost, monthly_savings, monthly_disc_rate, max_pd = 1000, raw = FALSE) {
  # vectorized function calculating payback period (with consideration of discounted cash flows)
  # can alternatively be calculated via subtraction method like in TRUCK VBA

  # monthly_savings is constant (i.e. expected fuel cost stays constant from year of decision/purchase)
  # # i.e. no expectation of change in fuel cost through lifetime.

  if (raw == FALSE) {
    pb <- if_else(
      # if monthly savings are 0 or negative, or if monthly savings are not enough to overcome incremental cost,
      #   then return max_pd
      monthly_savings <= rep(0, length(monthly_savings)) | (inc_cost * monthly_disc_rate)/monthly_savings >= 1, max_pd,
      # else, calculate discounted payback period via equation (works for equal cash flows)
      # rounded up to nearest whole number, and with max of max_pd
      # https://financeformulas.net/Discounted-Payback-Period.html
      pmin(
        ceiling(log(1/(1-(inc_cost * monthly_disc_rate)/monthly_savings))/log(1+monthly_disc_rate)),
        max_pd))
  } else if (raw == TRUE) {
    pb <- log(1/(1-(inc_cost * monthly_disc_rate)/monthly_savings))/log(1+monthly_disc_rate)
  }

  return(pb)
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
# sheet calculations ####

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
                                 intro +
                                   curve_for_preference_factor_phase_in(yr - intro_yr) *
                                   (final - intro) * fuel_avail_pref_factor_multiplier),
           # fuel availability affects preference factor here (and fuel availability doesn't affect anywhere else.)
           # preference factor applied as a multiplier to either indiff curve result and adoption curve result.
           # preference factor also affects the final market share calculation, where

           # Indifference Costs
           # Alicia Birky:
           # These are threshold costs within which the buyer is relatively indifferent to cost differences.
           # Probability of purchase is an S-Curve based on ratio of actual cost to threshold.
           #
           # if it's similar cost and pays back quickly or loses a tiny amount of money (never pays back)

           indifference_calc = if_else(inc_cost <= incr_cost & monthly_savings >= -fuel_mo,
                                       # incr_cost is the indifference cost - incr cost from Tech Options table
                                       # fuel_mo is the indifference cost - fuel from Tech Options table
                                       pref_factor * curve_for_indiff_to_first_cost(inc_cost/incr_cost) * curve_for_indiff_to_fuel_cost_savings(monthly_savings/fuel_mo),
                                       0),

           # Incremental Cost Factor (adjusts adoption rate at the end)
           # if it pays back but costs 2x, cash flow / cash availability may be restricted
           # is this covered in adoption curve?
           # same payback but large incremental cost may dissuade

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
                                                        pref_factor /0.5 * cumulative_proportion_willing_to_adopt))),
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
                                     TRUE ~ max(adj_indiv_tech_adoption_rate) *
                                       pref_factor * adj_indiv_tech_adoption_rate / sum(pref_factor * adj_indiv_tech_adoption_rate, na.rm = T)),
           final_mkt_shr = if_else(tech_type == "base", 1-sum(final_mkt_shr, na.rm = T), final_mkt_shr)) %>%
    # =IF(SUM($AX42:$BB42)=0,0,MAX($AX42:$BB42)*(AI42*AX42/SUMPRODUCT($AI42:$AM42,$AX42:$BB42)))
    #
    # >>> TO DO: remove PF in this weighting function
    #
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
