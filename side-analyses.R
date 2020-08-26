# manual nesting ####

# turn off phev and fchev - set their intro year to 2051
tech_opts %<>%
  mutate(intro_yr = c(NA, 2020, 2020, 2020, 2050, 2050))

# run model
shares_by_tech_conv_only <- build_calc_sheet() %>%
  calc_pb_and_mktshrs() %>%
  calc_results_by_flt_and_tech() %>%
  calc_results_by_tech()

plot_mktpen_vmt(shares_by_tech_conv_only, "20200221-conventional_only.png")

# calculate a composite baseline - wt avg cost & fe (weighted by vmt share)
# ## harmonic mean of fe
new_baseline_composite <- shares_by_tech_conv_only %>%
  filter(cls %in% "78Sleep" &
           tech %in% c("conventional_diesel_ice", "adv_conv", "isg", "hev")) %>%
  left_join(costs, by = c("cls","yr","tech")) %>%
  left_join(tech_opts, by = c("cls", "tech" = "description")) %>%
  left_join(subsidy, by = c("cls", "yr", "tech")) %>%
  left_join(fuel1_economy, by = c("cls", "yr", "tech")) %>%
  left_join(fuel2_economy, by = c("cls", "yr", "tech")) %>%
  left_join(CD_range, by = c("cls", "yr", "tech")) %>%
  group_by(yr, cls) %>%
  summarise(tot_cost = sum(tot_cost * tech_shr_of_vmt / sum(tech_shr_of_vmt)),
            f1_mpgde = sum(f1_mpgde * tech_shr_of_vmt / sum(tech_shr_of_vmt)),
            tech_type = "base",
            tech = "conventional_diesel_ice")

# insert baseline composite by modifying costs & f1_mpgde, then turn off advconv, isg, hev
costs %<>%
  left_join(new_baseline_composite, by = c("cls", "yr", "tech_type", "tech")) %>%
  mutate(tot_cost = if_else(tech_type == "base", tot_cost.y, tot_cost.x)) %>%
  select(-tot_cost.x, -tot_cost.y, -f1_mpgde)

fuel1_economy %<>%
  left_join(new_baseline_composite, by = c("cls", "yr", "tech")) %>%
  mutate(f1_mpgde = if_else(tech == "conventional_diesel_ice", f1_mpgde.y, f1_mpgde.x)) %>%
  select(-f1_mpgde.x, -f1_mpgde.y, -tot_cost, -tech_type)

tech_opts %<>%
  mutate(intro_yr = c(NA, 2050, 2050, 2050, 2020, 2020))

# re-run model
shares_by_tech_after_composite <- build_calc_sheet() %>%
  calc_pb_and_mktshrs() %>%
  calc_results_by_flt_and_tech() %>%
  calc_results_by_tech()

plot_mktpen_vmt(shares_by_tech_after_composite, "20200221-composite-conventional.png")

redistributed_conventional_composite <- shares_by_tech_after_composite %>%
  left_join(shares_by_tech_conv_only, by = c("cls", "yr", "tech")) %>%
  group_by(yr,cls) %>%
  mutate(composite_shr_of_vmt = if_else(tech == "conventional_diesel_ice",
                                        tech_shr_of_vmt.x, NA_real_),
         composite_shr_of_vmt = max(composite_shr_of_vmt, na.rm=T),
         tech_shr_of_vmt = if_else(tech %in% c("conventional_diesel_ice", "adv_conv", "isg", "hev"), composite_shr_of_vmt * tech_shr_of_vmt.y, tech_shr_of_vmt.x))

plot_mktpen_vmt(redistributed_conventional_composite, "20200221-redist-composite.png")


# reload Cls1-specific inputs when done



# indicate nests in input tables


#___________________________________________________________________
# payback analysis ####

sweep <- expand_grid(inc_cost = c(100, seq(-2000,30000,500)),
                     monthly_savings = c(10, seq(-500,3000,50)),
                     monthly_disc_rate = c(0.07/12), #, 0.00000001, 0.3/12))
                     pf = c(2,4,6) %>% map(curve_for_preference_factor_phase_in) %>% unlist())
sweep

truck_map <- sweep %>%
  mutate(payback_raw = sweep %>% pmap(~payback(..1, ..2, ..3, raw = TRUE)) %>% unlist(),
         payback_truck = sweep %>% pmap(~payback(..1, ..2, ..3, raw = FALSE, max_pd = 85)) %>% unlist(),
         adoption_curve = "Moderate",
         indiff_inccost = sweep %>% pmap(~curve_for_indiff_to_first_cost(..1/5000)) %>% unlist(),
         indiff_fuelsavings = sweep %>% pmap(~curve_for_indiff_to_fuel_cost_savings(..2/100)) %>% unlist(),
         indiff = 0.5 * indiff_inccost * indiff_fuelsavings,
         incr_adj = sweep %>% pmap(~curve_for_adj_incr_cost(..1/100000)) %>% unlist()
         ) %>%
  #mutate(payback = if_else(payback > 999 | payback < 0.1 | inc_cost < 0 | monthly_savings < 0, NA_real_, payback)) %>%
  left_join(adoption_tbl, by = c("payback_truck" = "months", "adoption_curve" = "adoption_curve")) %>% #applies moderate ATA adoption result to payback
  mutate(adoption_prob = if_else(inc_cost < 5000 & monthly_savings > -100 & indiff > cumulative_proportion_willing_to_adopt, indiff, cumulative_proportion_willing_to_adopt),
         adoption_prob_adj = adoption_prob * incr_adj,
         adoption_prob_diff = adoption_prob - cumulative_proportion_willing_to_adopt,
         adoption_prob_adj_diff = adoption_prob_adj - cumulative_proportion_willing_to_adopt)

View(truck_map)
summary(truck_map)
truck_map %>%
  filter(monthly_savings < 50) %>%
  pull(indiff_fuelsavings) %>%
  summary()

# high-level TRUCK adoption algorithm, illustrated
(high_level_map <- truck_map %>%
  filter(monthly_disc_rate == 0.07/12 & payback_raw < 200 & payback_raw > 0.1 & inc_cost > 0 & monthly_savings > -101) %>%
  ggplot() +
  #facet_wrap(facets = vars(monthly_disc_rate)) +
  stat_contour(aes(x = inc_cost, y = monthly_savings, z = payback_raw, col = ..level..),
               breaks = c(1, seq(3,12,3), seq(18, 30, 6), 36, 48, 60, 72, 84, 120)) +
  geom_text_contour(aes(x = inc_cost, y = monthly_savings, z = payback_raw, col = ..level..),
                    breaks = c(1, seq(3,12,3), seq(18, 30, 6), 36, 48, 60, 72, 84, 120)) +
  labs(x = "Incremental cost [$]",
       y = "Monthly fuel savings [$/month]",
       col = "Discounted\npayback\n[months]",
       title = "TRUCK adoption algorithm for advanced & alt-fuel technologies",
       subtitle = "MAX of {adoption from payback OR indifference calc (x0.5) if in red region}\nTHEN apply incremental cost adjustment (6.2-100% for 100-0% inc/base cost)\nTHEN apply preference factor (~10 yr 0-100% phase-in * fuel availability adjustment)\nFINALLY assign all shares as fraction of predicted share of max non-base tech\n(nests all non-base techs and [arbitrarily/disproportionately] influenced on best non-base tech)") +
  scale_x_continuous(breaks = seq(0,30000,3000), minor_breaks = seq(0, 30000, 1000)) +
  scale_y_continuous(limits = c(-100,1000)) +
  theme_minimal() +
  theme(axis.text.x.bottom = element_text(angle = 90)) +
  annotate("rect", xmin = 0, xmax = 5000, ymin = -100, ymax = 100, alpha = .3, fill = "red") +
  annotate("rect", xmin = 0, xmax = 5000, ymin = 100, ymax = 1000, alpha = .3, fill = "pink") +
  geom_blank())
# library(directlabels) # direct.label(v2, method="bottom.pieces")

# add to axes
# indifference and incremental cost adjustment (30k = 25%) - just stick in scaled screenshots



# illustration of as-implemented
# Adoption probability from payback OR indifference algorithm

# payback moderate
(payback_map <- truck_map %>%
  filter(monthly_disc_rate == 0.07/12 & payback_raw < 999 & payback_raw > 0.1 & inc_cost > 0 & monthly_savings > -101) %>%
  ggplot() +
  #facet_wrap(facets = vars(monthly_disc_rate)) +
  geom_tile(aes(x = inc_cost, y = monthly_savings, col = cumulative_proportion_willing_to_adopt, fill = cumulative_proportion_willing_to_adopt), size = 8) +
  stat_contour(aes(x = inc_cost, y = monthly_savings, z = payback_raw),
               col = "white",
               breaks = c(1, seq(3,12,3), seq(18, 30, 6), 36, 48, 60, 72, 84, 120)) +
  geom_text_contour(aes(x = inc_cost, y = monthly_savings, z = payback_raw),
                    col = "white",
                    breaks = c(1, seq(3,12,3), seq(18, 30, 6), 36, 48, 60, 72, 84, 120)) +
  labs(x = "Incremental cost [$]",
       y = "Monthly fuel savings [$/month]",
       col = "Adoption\nprobability",
       fill = "Adoption\nprobability",
       title = "Adoption probability from payback",
       subtitle = "according to Moderate curve based on 1997 ATA survey") +
  scale_x_continuous(breaks = seq(0,30000,3000), minor_breaks = seq(0, 30000, 1000)) +
  scale_y_continuous(limits = c(-150,1000)) +
  scale_colour_viridis_c() +
  scale_fill_viridis_c() +
  theme_minimal() +
  theme(axis.text.x.bottom = element_text(angle = 90)) +
  geom_blank())


# indifference
(indiff_map <- truck_map %>%
  filter(monthly_disc_rate == 0.07/12 & inc_cost > 0) %>%
  #& payback_raw < 999 & payback_raw > 0.1 & & monthly_savings > -101) %>%
  ggplot() +
  #facet_wrap(facets = vars(monthly_disc_rate)) +
  geom_tile(aes(x = inc_cost, y = monthly_savings, col = indiff, fill = indiff), size = 8) +
  stat_contour(aes(x = inc_cost, y = monthly_savings, z = payback_raw),
               col = "white",
               breaks = c(1, seq(3,12,3), seq(18, 30, 6), 36, 48, 60, 72, 84, 120)) +
  geom_text_contour(aes(x = inc_cost, y = monthly_savings, z = payback_raw),
                    col = "white",
                    breaks = c(1, seq(3,12,3), seq(18, 30, 6), 36, 48, 60, 72, 84, 120)) +
  labs(x = "Incremental cost [$]",
       y = "Monthly fuel savings [$/month]",
       col = "Indifference result",
       fill = "Indifference result",
       title = "Adoption probability based on indifference algorithm (max 0.5)") +
  scale_x_continuous(breaks = seq(0,30000,3000), minor_breaks = seq(0, 30000, 1000)) +
  scale_y_continuous(limits = c(-150,1000)) +
  scale_colour_viridis_c(end = 0.5) +
  scale_fill_viridis_c(end = 0.5) +
  theme_minimal() +
  theme(axis.text.x.bottom = element_text(angle = 90)) +
  geom_blank())


(adoption_map <- truck_map %>%
  filter(monthly_disc_rate == 0.07/12 & inc_cost > 0) %>% # payback_raw < 999 & payback_raw > 0.1 & inc_cost > 0 & monthly_savings > -101) %>%
  ggplot() +
  #facet_wrap(facets = vars(monthly_disc_rate)) +
  geom_tile(aes(x = inc_cost, y = monthly_savings, col = adoption_prob_adj, fill = adoption_prob_adj), size = 8) +
  labs(x = "Incremental cost [$]",
       y = "Monthly fuel savings [$/month]",
       col = "Adoption\nprobability",
       fill = "Adoption\nprobability",
       title = "Adoption probability",
       subtitle = "MAX of {adoption from payback OR indifference result (x0.5) if in indiff region}\nAND with incremental cost adjustment applied") +
  scale_x_continuous(breaks = seq(0,30000,3000), minor_breaks = seq(0, 30000, 1000)) +
  scale_y_continuous(limits = c(-100,1000)) +
  scale_colour_viridis_c() +
  scale_fill_viridis_c() +
  theme_minimal() +
  theme(axis.text.x.bottom = element_text(angle = 90)) +
  geom_blank())



# where is indifference algorithm and incremental cost adjustment making a difference?
(difference_map <- truck_map %>%
  filter(monthly_disc_rate == 0.07/12 & inc_cost > 0) %>% # payback_raw < 999 & payback_raw > 0.1 & inc_cost > 0 & monthly_savings > -101) %>%
  filter(adoption_prob_adj_diff != 0) %>%
  ggplot() +
  #facet_wrap(facets = vars(monthly_disc_rate)) +
  geom_tile(aes(x = inc_cost, y = monthly_savings, col = adoption_prob_adj_diff, fill = adoption_prob_adj_diff), size = 8) +
  labs(x = "Incremental cost [$]",
       y = "Monthly fuel savings [$/month]",
       col = "Diff in\nadoption\nprobability",
       fill = "Diff in\nadoption\nprobability",
       title = "Difference in adoption probability from raw % based on payback\ndue to indifference algorithm and incremental cost adjustment")+
  scale_x_continuous(limits = c(0, 30000), breaks = seq(0,30000,3000), minor_breaks = seq(0, 30000, 1000)) +
  scale_y_continuous(limits = c(-100,1000)) +
  scale_colour_gradient2(low = "red", high = "green") +
  scale_fill_gradient2(low = "red", high = "green") +
  theme_minimal() +
  theme(axis.text.x.bottom = element_text(angle = 90)) +
  annotate("label", 8000, 100, label = "indifference\nalgorithm", colour = "green") +
  annotate("label", 22000, 700, label = "incremental cost\nadjustment", colour = "red") +
  geom_blank())




# compare payback map against NPV+logit (assume payback), logit provides heterogeneity



# library(FinancialMath)
# FinancialMath::NPV(0,rep(1,16),1:16,0.03)

