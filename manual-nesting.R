# manual nesting

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


# reload unmodified inputs when done



# allow indicate nests in input tables
