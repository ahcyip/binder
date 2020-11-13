# load libraries & fns ####
libs <- c("readxl", "tidyxl", "janitor", "magrittr", "here", "metR", "ggrepel", "tidyverse")
invisible(lapply(libs, library, character.only = TRUE))

source(here::here("truck-app", "fns.R"))

# payback analysis

# sweep ####
sweep <- expand_grid(inc_cost = c(100, seq(-2000,30000,500)),
                     monthly_savings = c(10, seq(-500,3000,50)),
                     disc_rate = c(0.07, 0.03, 0.14), #, 0.00000001, 0.3/12))
                     yr_entry = 99, #c(1:20, 99),
                     adoption_curve = c("Moderate", "Aggressive", "Conservative"))
#sweep

truck_map <- sweep %>%
  mutate(payback_raw = sweep %>% pmap(~payback(..1, ..2, ..3/12, raw = TRUE)) %>% unlist(),
         payback_truck = sweep %>% pmap(~payback(..1, ..2, ..3/12, raw = FALSE, max_pd = 85)) %>% unlist(),
         pf = 0.5*(yr_entry %>% map(curve_for_preference_factor_phase_in) %>% unlist()), #assumes intro 0, final 0.5, FA = 1
         indiff_inccost = sweep %>% pmap(~curve_for_indiff_to_first_cost(..1/5000)) %>% unlist(),
         indiff_fuelsavings = sweep %>% pmap(~curve_for_indiff_to_fuel_cost_savings(..2/100)) %>% unlist(),
         indiff = pf * indiff_inccost * indiff_fuelsavings,
         incr_adj = sweep %>% pmap(~curve_for_adj_incr_cost(..1/100000)) %>% unlist()
         ) %>%
  #mutate(payback = if_else(payback > 999 | payback < 0.1 | inc_cost < 0 | monthly_savings < 0, NA_real_, payback)) %>%
  left_join(adoption_tbl, by = c("payback_truck" = "months", "adoption_curve" = "adoption_curve")) %>% #applies ATA adoption result
  mutate(adoption_prob = if_else(inc_cost < 5000 & monthly_savings > -100 & indiff > pf/0.5*cumulative_proportion_willing_to_adopt, indiff, pf/0.5*cumulative_proportion_willing_to_adopt),
         adoption_prob_adj = adoption_prob * incr_adj,
         adoption_prob_diff = adoption_prob - cumulative_proportion_willing_to_adopt,
         adoption_prob_adj_diff = adoption_prob_adj - cumulative_proportion_willing_to_adopt,
         adoption_curve = adoption_curve %>% fct_relevel(c("Aggressive", "Moderate", "Conservative")))

# View(truck_map)
# summary(truck_map)
# truck_map %>%
#   filter(monthly_savings < 50) %>%
#   pull(indiff_fuelsavings) %>%
#   summary()

# high-level TRUCK adoption algorithm, illustrated ####
(high_level_map <- truck_map %>%
  filter(disc_rate == 0.07 & adoption_curve == "Moderate" & payback_raw < 200 & payback_raw > 0.1 & inc_cost > 0 & monthly_savings > -101 & yr_entry == 99) %>%
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



# illustration of as-implemented ####

# payback moderate ####
(payback_map <- truck_map %>%
  filter(disc_rate == 0.07 & adoption_curve == "Moderate" & yr_entry == 99 & inc_cost > 0 & monthly_savings >= 0) %>% # & payback_raw < 9999 & payback_raw > 0.01
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
       subtitle = "Calculated based on Moderate curve fit to 1997 ATA survey data, discount rate of 7%\n\nContours and labels represent discounted payback time in months") +
  scale_x_continuous(breaks = seq(0,30000,3000), minor_breaks = seq(0, 30000, 1000)) +
  scale_y_continuous(limits = c(0,1000)) +
  scale_colour_viridis_c(limits = c(0,1), breaks = c(0,0.25,0.5,0.75,1), labels = c("0%","25%","50%","75%","100%")) +
  scale_fill_viridis_c(limits = c(0,1), breaks = c(0,0.25,0.5,0.75,1), labels = c("0%","25%","50%","75%","100%")) +
  theme_minimal() +
  theme(axis.text.x.bottom = element_text(angle = 90)) +
  geom_blank())
ggsave("payback_map.png")

#payback faceted by discount rate and adoption curve
(payback_map_fct <- truck_map %>%
    filter(yr_entry == 99 & inc_cost > 0 & monthly_savings > 0) %>% # & payback_raw < 9999 & payback_raw > 0.01
    ggplot() +
    facet_grid(cols = vars(disc_rate), rows = vars(adoption_curve), labeller = label_both) +
    geom_tile(aes(x = inc_cost, y = monthly_savings, col = cumulative_proportion_willing_to_adopt, fill = cumulative_proportion_willing_to_adopt), size = 8) +
    stat_contour(aes(x = inc_cost, y = monthly_savings, z = payback_raw),
                 col = "white",
                 breaks = c(3, 6, 12, 18, 24, 36, 48, 72)) +
    geom_text_contour(aes(x = inc_cost, y = monthly_savings, z = payback_raw),
                      col = "white",
                      breaks = c(3, 6, 12, 18, 24, 36, 48, 72)) +
    labs(x = "Incremental cost [$]",
         y = "Monthly fuel savings [$/month]",
         col = "Adoption\nprobability",
         fill = "Adoption\nprobability",
         title = "Adoption probability based on discounted payback period and 1997 ATA survey responses",
         subtitle = "Contours and labels represent discounted payback time in months\nColumns show variations on discount rate, affecting both payback and adoption probability\nRows show variations on fitted adoption curve, affecting translation of payback to adoption probability") +
    scale_x_continuous(breaks = seq(0,30000,3000), minor_breaks = seq(0, 30000, 1000)) +
    scale_y_continuous(limits = c(0,1000)) +
    scale_colour_viridis_c(limits = c(0,1), breaks = c(0,0.25,0.5,0.75,1), labels = c("0%","25%","50%","75%","100%")) +
    scale_fill_viridis_c(limits = c(0,1), breaks = c(0,0.25,0.5,0.75,1), labels = c("0%","25%","50%","75%","100%")) +
    theme_minimal() +
    theme(axis.text.x.bottom = element_text(angle = 90)) +
    geom_blank())
ggsave("payback_map_fct.png", width = 8, height = 9)



# indifference ####
(indiff_map <- truck_map %>%
  filter(disc_rate == 0.07 & adoption_curve == "Moderate" & inc_cost > 0 & yr_entry == 99) %>%
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
ggsave("indiff_map.png")

# Adoption probability from payback OR indifference algorithm AND PF AND incremental cost adjustment ####
(adoption_map <- truck_map %>%
  filter(disc_rate == 0.07 & adoption_curve == "Moderate" & inc_cost > 0 & yr_entry == 99) %>% # payback_raw < 999 & payback_raw > 0.1 & inc_cost > 0 & monthly_savings > -101) %>%
  ggplot() +
  #facet_wrap(facets = vars(monthly_disc_rate)) +
  geom_tile(aes(x = inc_cost, y = monthly_savings, col = adoption_prob_adj, fill = adoption_prob_adj), size = 8) +
  labs(x = "Incremental cost [$]",
       y = "Monthly fuel savings [$/month]",
       col = "Adoption\nprobability",
       fill = "Adoption\nprobability",
       title = "Adoption probability",
       subtitle = "MAX of {adoption based on payback OR indifference algorithm}\nAND with preference factor AND incremental cost adjustment applied") +
  scale_x_continuous(breaks = seq(0,30000,3000), minor_breaks = seq(0, 30000, 1000)) +
  scale_y_continuous(limits = c(-150,1000)) +
  scale_colour_viridis_c() +
  scale_fill_viridis_c() +
  theme_minimal() +
  theme(axis.text.x.bottom = element_text(angle = 90)) +
  geom_blank())


(adoption_map_facet_yrentry <- truck_map %>%
    filter(disc_rate == 0.07 & adoption_curve == "Moderate" & inc_cost > 0 & yr_entry %in% c(2,5,8,11)) %>%
    # payback_raw < 999 & payback_raw > 0.1 & inc_cost > 0 & monthly_savings > -101) %>%
    ggplot() +
    facet_wrap(facets = vars(yr_entry), nrow = 1) +
    geom_tile(aes(x = inc_cost, y = monthly_savings, col = adoption_prob_adj, fill = adoption_prob_adj), size = 8) +
    labs(x = "Incremental cost [$]",
         y = "Monthly fuel savings [$/month]",
         col = "Adoption\nprobability",
         fill = "Adoption\nprobability",
         title = "Adoption probability after adjustments",
         subtitle = "MAX of {adoption based on payback OR indifference algorithm}\nAND with preference factor AND incremental cost adjustment applied\n\nColumns: Years after first market entry")+
         #caption = "Other assumptions:\n7% discount rate; base truck cost $120000") +
    scale_x_continuous(breaks = seq(0,30000,3000), minor_breaks = seq(0, 30000, 1000)) +
    scale_y_continuous(limits = c(-150,1000)) +
    scale_colour_viridis_c(limits = c(0,1), breaks = c(0,0.25,0.5,0.75,1), labels = c("0%","25%","50%","75%","100%")) +
    scale_fill_viridis_c(limits = c(0,1), breaks = c(0,0.25,0.5,0.75,1), labels = c("0%","25%","50%","75%","100%")) +
    theme_minimal() +
    theme(axis.text.x.bottom = element_text(angle = 90)) +
    geom_blank())
ggsave("adoption_facet_pf.png", width = 9, height = 6)


sample_path <- completed_calc_sheet %>%
  filter(tech != "conventional_diesel_ice" & cls == "78Sleep" & flt == "NCent" & cohort == ">200" & yr > 2020 & inc_cost < 30000)

(adoption_map +
  geom_point(data = sample_path,
             mapping = aes(x = inc_cost, y = monthly_savings, shape = tech), col = "red") +
  geom_line(data = sample_path,
             mapping = aes(x = inc_cost, y = monthly_savings, group = tech), col = "red") +
  geom_label_repel(data = sample_path,
             mapping = aes(x = inc_cost, y = monthly_savings, label = yr)))




# where is indifference algorithm, pf, and incremental cost adjustment making a difference?
(difference_map <- truck_map %>%
  filter(disc_rate == 0.07 & adoption_curve == "Moderate" & inc_cost > 0 & yr_entry %in% c(2,5,8,11)) %>% # payback_raw < 999 & payback_raw > 0.1 & inc_cost > 0 & monthly_savings > -101) %>%
  filter(adoption_prob_adj_diff != 0) %>%
  ggplot() +
  #facet_wrap(facets = vars(monthly_disc_rate)) +
  facet_wrap(facets = vars(yr_entry), nrow = 1) +
  geom_tile(aes(x = inc_cost, y = monthly_savings, col = adoption_prob_adj_diff, fill = adoption_prob_adj_diff), size = 8) +
  labs(x = "Incremental cost [$]",
       y = "Monthly fuel savings [$/month]",
       col = "Difference in\nadoption\nprobability",
       fill = "Difference in\nadoption\nprobability",
       title = "Adjustments to adoption probability",
       subtitle = "(Indifference algorithm, preference factor, and incremental cost adjustments to payback model)\n\nColumns: Years after first market entry")+
  scale_x_continuous(limits = c(0, 30000), breaks = seq(0,30000,3000), minor_breaks = seq(0, 30000, 1000)) +
  scale_y_continuous(limits = c(-150, 1000)) +
  scale_colour_gradient2(low = "red", high = "green", limits = c(-1,0.5), breaks = c(-1,-0.5,0,0.5), labels = c("-100%", "-50%", "0%", "+50%")) +
  scale_fill_gradient2(low = "red", high = "green", limits = c(-1,0.5), breaks = c(-1,-0.5,0,0.5), labels = c("-100%", "-50%", "0%", "+50%")) +
  theme_minimal() +
  theme(axis.text.x.bottom = element_text(angle = 90)) +
  #annotate("label", 8000, 100, label = "indifference\nalgorithm", colour = "green") +
  #annotate("label", 22000, 700, label = "incremental cost\nadjustment", colour = "red") +
  geom_blank())
ggsave("difference_facet_pf.png", width = 9, height = 6)


# next steps:
# compare payback map against NPV+logit (assume payback), logit provides heterogeneity

# library(FinancialMath)
# FinancialMath::NPV(0,rep(1,16),1:16,0.03)

