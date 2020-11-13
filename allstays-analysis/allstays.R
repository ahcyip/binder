# install packages ####
# if (!("rvest" %in% installed.packages())) {
#   install.packages("rvest")
# }
# if (!("tidyverse" %in% installed.packages())) {
#   install.packages("tidyverse")
# }
#___________________________________________________________________
# load packages (must run every time) ####
library(maps);
library(rvest); library(ggmap); library(ggsn); library(ggrepel);
library(janitor); library(snakecase); library(sf); library(progress);
library(magrittr); library(tidyverse); library(broom)

#___________________________________________________________________
# load truckstops from allstays ####
links_states <- "https://www.allstays.com/c/truck-stop-locations.htm" %>%
  read_html() %>%
  xml2::xml_find_all("//a[contains(@class, 'mapside button')]") %>%
  rvest::html_attr("href") %>%
  paste0("http:" , .) %>%
  .[!grepl("newfoundland|yukon|quebec|british|ontario|nova|manitoba|alberta|saskatchewan|brunswick|northwest", .)] # remove canadian pages. alt code: str_remove("http://www.allstays.com/c/truck-stops-newfoundland.htm")
links_states_3groups <- links_states %>%
  split(., cut(seq_along(.), 3, labels = FALSE))

#state_names <- links_states %>% str_match("stops-(.*).htm") %>% .[,2]

extract_stop_pg_links_from_state_pg <- function(state_pg_link) {
  state_pg_link %>%
    read_html() %>%
    xml_find_all("//html/body/div[2]/div/div[1]/a[@href]") %>%
    rvest::html_attr("href") %>%
    paste0("http:" , .) %>%
    subset(., str_detect(., "truckstops-details")) %>%
    unique()
}

safely_read_html <-  safely(read_html)
# safely_read_html("https://www.allstays.com/c/truck-stop-locations.htm")
# safely_read_html("https://www.allstays.com/c/xxxxxxxx.htm")$error$message

safely_extract_stopdata <- function(stop_pg_link) {
  id <- stop_pg_link %>% str_extract("[:digit:]+")
  pg <- stop_pg_link %>% safely_read_html()
  if (is.null(pg$error)) {
  name <- pg$result %>%
    xml_find_all("//span[contains(@itemprop, 'name')]") %>%
    rvest::html_text()
  chain <- pg$result %>%
    xml_find_all("/html/body/div[2]/div[1]/div/h1/span/img") %>%
    rvest::html_attr("src")
  address <- pg$result %>%
    xml_find_all("//span[contains(@itemprop, 'streetAddress')]") %>%
    rvest::html_text()
  city <- pg$result %>%
    xml_find_all("//span[contains(@itemprop, 'addressLocality')]") %>%
    rvest::html_text()
  postalCode <- pg$result %>%
    xml_find_all("//span[contains(@itemprop, 'postalCode')]") %>%
    rvest::html_text()
  state <- pg$result %>%
    xml_find_all("//span[contains(@itemprop, 'addressRegion')]") %>%
    rvest::html_text()
  desc <- pg$result %>%
    xml_find_all("//span[contains(@itemprop, 'description')]") %>%
    rvest::html_text()
  loc <- pg$result %>%
    xml_find_all("//a[contains(@title, 'Send to Navigation app')]") %>%
    rvest::html_attr("href")
  print(paste("extracted data from stop", id))
  pb$tick()
  return(list(id = id,
              raw_loc = loc,
              raw_desc = desc,
              address = address,
              stop_name = name,
              chain = chain,
              postalCode = postalCode,
              city = city,
              state = state))
  } else {
    # this didn't seem to work/activate
    print(pg$error$message)
    pb$tick()
    return(list(id = id,
                raw_loc = NA,
                raw_desc = pg$error$message,
                address = NA,
                stop_name = NA,
                chain = NA,
                postalCode = NA,
                city = NA,
                state = NA))
  }
}


stop_info_list <- list()

pb <- progress_bar$new(format = "  progress [:bar] :percent eta: :eta",
                       total = links_states_3groups[[1]] %>%
                         purrr::map(extract_stop_pg_links_from_state_pg) %>%
                         lengths() %>% sum(), clear = FALSE, show_after = 10)
# pb <- progress_bar$new(format = "  progress [:bar] :percent eta: :eta",
#                        total = 2, clear = FALSE, show_after = 0)
stop_info_list[[1]] <- links_states_3groups[[1]] %>%
#  c("http://www.allstays.com/c/truck-stops-rhode-island.htm") %>% # test RI's 2 stops
  map_depth(1, extract_stop_pg_links_from_state_pg) %>% map_depth(2, safely_extract_stopdata)

pb <- progress_bar$new(format = "  progress [:bar] :percent eta: :eta",
                       total = links_states_3groups[[2]] %>%
                         purrr::map(extract_stop_pg_links_from_state_pg) %>%
                         lengths() %>% sum(), clear = FALSE, show_after = 10)
stop_info_list[[2]] <- links_states_3groups[[2]] %>%
  map_depth(1, extract_stop_pg_links_from_state_pg) %>% map_depth(2, safely_extract_stopdata)

pb <- progress_bar$new(format = "  progress [:bar] :percent eta: :eta",
                       total = links_states_3groups[[3]] %>%
                         purrr::map(extract_stop_pg_links_from_state_pg) %>%
                         lengths() %>% sum(), clear = FALSE, show_after = 10)
stop_info_list[[3]] <- links_states_3groups[[3]] %>%
  map_depth(1, extract_stop_pg_links_from_state_pg) %>% map_depth(2, safely_extract_stopdata)


(stop_info_df <- stop_info_list %>%
    unlist(recursive = FALSE) %>%
    unlist(recursive = FALSE) %>%
    enframe() %>%
    unnest_wider(value))

#___________________________________________________________________
# mine all truck stop php pages !!! ####

stage_pg_link_from_id <- function(id) {
  paste0("https://www.allstays.com/truckstops-details/", id, ".php")
}

one_hundred_thousand_links <- 1:100000 %>% stage_pg_link_from_id()
links_100k_split_3 <- one_hundred_thousand_links %>%
  split(., cut(seq_along(.), 3, labels = FALSE))

stop_info_list_100k <- list()
pb <- progress_bar$new(format = "  progress [:bar] :percent eta: :eta",
                       total = links_100k_split_3[[1]] %>% length(),
                       clear = FALSE, show_after = 10)
stop_info_list_100k[[1]] <- links_100k_split_3[[1]] %>%
  purrr::map(safely_extract_stopdata)
pb <- progress_bar$new(format = "  progress [:bar] :percent eta: :eta",
                       total = links_100k_split_3[[2]] %>% length(),
                       clear = FALSE, show_after = 10)
stop_info_list_100k[[2]] <- links_100k_split_3[[2]] %>%
  purrr::map(safely_extract_stopdata)
pb <- progress_bar$new(format = "  progress [:bar] :percent eta: :eta",
                       total = links_100k_split_3[[3]] %>% length(),
                       clear = FALSE, show_after = 10)
stop_info_list_100k[[3]] <- links_100k_split_3[[3]] %>%
  purrr::map(safely_extract_stopdata)



# 33311 ??? unusedconnection
stop_info_list_100k %>%
  map_depth(2, "id") %>% #"raw_desc"
  enframe() %>%
  View()

  unnest(cols = value) %>%
  mutate(value = as.factor(value)) %>%
  summary()

#___________________________________________________________________
# save RDS ####
stop_info_df %>% saveRDS(here::here("binder", "allstays-analysis", "stop_info_df.RDS"))
stop_info_df %>% saveRDS(here::here("allstays-analysis", "stop_info_df.RDS"))

#___________________________________________________________________
# read/mutate allstays data (start here) ####
stop_info_df <- readRDS(here::here("allstays-analysis", "stop_info_df.RDS"))
View(stop_info_df)

#___________________________________________________________________
# clean out duplicates ####
stop_info_df %>% filter(region == "nv") %>% View()
# turns out there are stops and fuel on both sides in the NV example that looked like duplicate
# same with reno valero and nitenday (which had "pacific pride commercial fueling")
#https://www.google.com/maps/place/39%C2%B032'02.7%22N+119%C2%B047'07.3%22W/@39.5340885,-119.7854993,20z/data=!4m5!3m4!1s0x0:0x0!8m2!3d39.534076!4d-119.785368


# check 404 ####
stop_info_df %>% filter(is.na(raw_loc)) %>% View()
# known 404 problems that showed up as unused connections: 44124, 198722, 48681, 48881
# and an NA!
# https://www.allstays.com/c/truck-stops-colorado.htm
# Burlington
# Travel Shoppe I-70 Ex 437 (Hwy 385), 415 S. Lincoln St…
# 20 truck parking spaces - 4 Diesel lanes - 2 Showers - Restaurant - ATM - FedEx - UPS - Engine Repair - D…More
# Burlington Travel Shoppe -
#   I-70 Ex 437 (Hwy 385), 415 S. Lincoln St, Burlington CO
# 80807
# 20 truck parking spaces - 4 Diesel lanes - 2 Showers -
#   Restaurant - ATM - FedEx - UPS - Engine Repair - Dump -
#   RVs Welcome - Travel Store - TCH - Comdata - Trailer Drop -
#   Pay Phones (TS)
# View Map - Navigation - Add/Check Reviews
# 39.295815, -102.279517

# california
# 44124
# Patterson Flying J Travel Plaza -
#   I-5 Exit 434, 2275 Sperry Ave, Patterson CA 95387
# Fax: 209-892-2739 - 130 parking spaces - 9 diesel lanes - 9
# showers - Cinnabon - Wendys - PJ Fresh - Internet - 9 Bulk
# DEF - CAT Scales - Laundry - ATM - 2 RV Lanes - RV Dump
# Station($10, far right of RV fuel lanes) - Propane (difficult to
#                                                     access without backing up) - Trucker Lounge - Western Union
# - Check Cashing - Game Room - FedEx - UPS - DAT -
#   Fuelman (TS)
# View Map - Navigation - Add/Check Reviews
# 37.464774, -121.165269

stop_info_df %>% filter(name %in% c(3131, 3132, 3133, 3134, 3135)) %>% View()

# accidentally typed in 91213 and found a truck stop but it's not on the state list and so it's not in my data.
# Cougar Corner
# https://maps.google.com/maps?q=37.922069,-83.259815 # this shows a marathon... but it's also not on the list.
# Truck Stop Location:
#   US460, 1741 W Main St, West Liberty KY 41472 # this address shows a shell on gmaps...
# Truck Stop Details:
#   small stop (TS)

# hmm. maybe i should just loop through 1:100000



stop_info_df %>% filter(name %in% c(3938, 3939, 3940, 4913, 4914, 4915)) %>% View()
# 3939	48681	summer shade 36.877825 -85.665952
# in the pdf!
# Summer Shade Five Star Marathon -
# 4770 Summer Shade Rd, Summer Shade KY 42166
# no truck parking - 4 Diesel lanes - ATM (TS)
# View Map - Navigation - Add/Check Reviews
#
# 4914	48881 in PDF. 34.970393 -104.790945
# Santa Rosa Sinclair -
#   1009 Stuckeys Rd, I-40 Frontage Rd and County Rd 4 H,
# Santa Rosa NM 88435
# 25 truck parking spaces - Store - 3 Diesel lanes - Air Fill - Pay
# phone - ATM - Y - Propane tanks
# View Map - Navigation - Add/Check Reviews




# more processing ####
(stops_processed <- stop_info_df %>%
  filter(!is.na(raw_loc)) %>% # ignore 3x 404s
  mutate(loc = str_match(raw_loc, "daddr=(.*)")[,2]) %>% #extract2 if pipes desired
  separate(loc, c("lat","lon"), ",") %>%
  mutate(lat = as.numeric(lat),
         lon = as.numeric(lon),
         region = as.factor(to_any_case(state, "snake")),
         state = as.factor(state),
         chain2 = as.factor(str_match(chain, "_(.*)_")[,2]),
         chain3 = as.factor(if_else(chain2 == "indie", "indie", "chain")),
         diesel_lanes = as.numeric(str_match(raw_desc, "([:digit:]*)\\s*[dD]iesel")[,2]),
         propane = str_detect(raw_desc, "[pP]ropane"),
         biodiesel = str_detect(raw_desc, "[bB]io\\s*[dD]iesel"),
         diesel_lanes_NA = if_else(is.na(diesel_lanes), TRUE, FALSE),
         diesel_lanes_NAas1 = if_else(diesel_lanes_NA, 1, diesel_lanes),
         diesel_lanes_NAas0 = if_else(diesel_lanes_NA, 0, diesel_lanes),
         parking = as.numeric(str_match(raw_desc, "([:digit:]+)\\s*[pP]arking")[,2]),
         truckparking = as.numeric(str_match(raw_desc, "([:digit:]+)\\s*[tT]+ruck\\s*[pP]arking")[,2]),
         spaces = as.numeric(str_match(raw_desc, "([:digit:]+)\\s*[sS]paces")[,2]),
         noparking = str_match(raw_desc, "(no|No|NO|no [tT]ruck)\\s*[pP]arking")[,2],
         noparking = case_when(id %in% c(90104, 90160, 94866) ~ NA_character_,
                               TRUE ~ noparking) %>% as.factor(),
         smallstop = if_else(str_detect(raw_desc, "[sS]mall\\s*[sS]top"), TRUE, NA),
         #pksp_total = parking + truckparking + spaces,
         noparking_T = if_else(!is.na(noparking), TRUE, NA),
         noparking_as0 = if_else(noparking_T, 0, NA_real_)) %>%
   rowwise() %>% # also, sum diesel, but NA turn into 0
   mutate(pksp_total = sum(parking, truckparking, spaces, noparking_as0, na.rm = TRUE)) %>%
   ungroup() %>%
   mutate(pksp_total = if_else(is.na(parking) & is.na(truckparking) & is.na(spaces) & is.na(noparking_T), NA_real_, pksp_total),
          pksp_NA = is.na(pksp_total),
          k=2.3))



# check data problems ####

stops_processed %>%
  filter(diesel_lanes == 0) %>%
  View()
# 96412 Pioneer, TN "TA" stop shows up.
# google maps says it's pilot. nearby pilot across road is a 6.
# 2 other Shell stations are visible on map:
# one with raised roof, one across highway that says "no tractor trailers allowed".
# new? speedway diesel lanes? nvm that's the pilot of 6.
# OK this might be legit...
# may not be the only one? maybe intercept of model doesn't need to be raised to k=2.3.


stops_processed %>% filter(is.na(lon)) %>% View() # nrow()
# 16 stops without lat lons
# most don't have location links in the pdf either.
# could use google geocode to figure them out.


register_google(Sys.getenv("google_cloud_apikey"))
stops_with_missing_latlon <- stops_processed %>%
  filter(is.na(lon)) %>%
  mutate(address_full = paste(address,city,state,postalCode, sep = ", ")) %>%
  mutate_geocode(address_full, output = "latlona")

stops_with_missing_latlon %>% View()
stops_with_missing_latlon %>% saveRDS(here::here("allstays-analysis", "stops_w_missing_latlon.RDS"))

# to do:
# join these back in with lat/lon guesses
# manual edit those addresses to find a better location?
# find "10 parralel area truck parking space" and maybe erase certain words/phrases?

#___________________________________________________________________
# no parking analysis ####

# does "no parking" match zero parking spaces?
stops_processed %>%
  tabyl(pksp_total, noparking_T) %>%
  View()
# yes except for 3 stops, with 20/30/60 pksp and no parking = TRUE.
stops_processed %>%
  filter(noparking_T == TRUE & pksp_total > 0) %>%
  View()
# checked the 3. temp no parking, no parking signs, no parking on street.
# ids 90104 90160 94866
# manually adjusted in processing.

#___________________________________________________________________
# chain analysis ####

# lane summary by chain

# AM Best
# Flying J Travel Plaza
# Loves Travel Stop
# Pacific Pride Commercial
# Petro Center
# Pilot Travel Center
# Speedco commercial
# TA Travel Center
### [independent] Indie Truck Stop

# other irrelevant categories
## CAT Scales
## Low Clearance
## Mack Dealer
## Kenworth Dealer
## Peterbilt Dealer
## Truck Wash
## Volvo Dealer
## Weigh Station Scales
## Western Star Dealer
## thermo king
## utility dealer
stops_processed %>%
  group_by(chain2) %>%
  summarize(n = n(),
            med_raw = median(diesel_lanes, na.rm = T),
            min_raw = min(diesel_lanes, na.rm = T),
            max_raw = max(diesel_lanes, na.rm = T),
            mean_assuming_1for0 = round(mean(diesel_lanes_NAas1), 1), # not great assumption (see below)
            med_assuming_1for0 = median(diesel_lanes_NAas1)) %>% # not great assumption (see below)
  arrange(desc(n)) %>%
  View()


# chains in each state
stops_processed %>%
  tabyl(state, chain2) %>%
  View()

stops_processed %>%
  tabyl(state, chain3) %>%
  adorn_percentages() %>%
  bind_cols(stops_processed %>% tabyl(state, chain3)) %>%
  #adorn_pct_formatting(digits = 0) %>%
  mutate(across(c("chain...2","indie...3"), round, 1)) %>%
  arrange(desc(chain...2)) %>%
  select(-state...4) %>%
  adorn_totals("row") %>%
  View()


#___________________________________________________________________
# parking (proxy indicator of size) ####
## also, check out truck stops data emailed from margo's team

stops_processed %>%
  select(parking, truckparking, spaces, noparking, smallstop, pksp_total) %>%
  summary()

# do "small stops" tend to have fewer parking spaces?
stops_processed %>%
  tabyl(pksp_total, smallstop)

stops_processed %>%
  ggplot() +
  geom_boxplot(aes(x = smallstop, y = pksp_total, col = chain3))
# it appears so


# the 3 words before parking (if no number / number + truck)
stops_processed %>%
  mutate(preceding_parking = as.factor(str_match(raw_desc, "\\s+([:alnum:]*\\s+[:alnum:]*\\s*[:alnum:]+)\\s+[pP]arking")[,2])) %>%
  group_by(preceding_parking) %>%
  summarize(n=n()) %>%
  arrange(desc(n)) %>%
  filter(!str_detect(preceding_parking, "\\s*[:digit:]+\\s*")) %>%
  #filter(str_detect(preceding_parking, "\\s*[:alpha:]+\\s*")) %>%
  View()
# may do something about these.
# RV, overnight, free/paid, lots/difficult, very large, very little, no X parking


# do "small stops" tend to have fewer diesel lanes?
stops_processed %>%
  ggplot() +
  geom_boxplot(aes(x = smallstop, y = diesel_lanes, col = chain3))


# do diesel lane info correlate with parking spaces?
# visualize
(diesellanes_vs_pksp <- ggplot() +
  geom_point(data = stops_processed,
             mapping = aes(x = jitter(pksp_total), y = jitter(diesel_lanes), col = chain3)) +
  labs(x = "Parking spaces",
       y = "Diesel lanes",
       col = "Operator") +
  theme_minimal())

# build a model

# get info to inform k intercept
stops_processed %>%
  filter(pksp_total == 0) %>%
  group_by(chain3) %>%
  summarize(avg = mean(diesel_lanes, na.rm = T))

# build model
lm_fit_for_diesel_lanes <- lm(diesel_lanes ~ -1 + log(pksp_total+10) + chain3 + offset(k),
                              data = stops_processed %>%
                                filter(!is.na(diesel_lanes) & !is.na(pksp_total) &
                                         pksp_total < 500))
summary(lm_fit_for_diesel_lanes)
plot(lm_fit_for_diesel_lanes)

# save predictions of model in new data frame together with variable you want to plot against
stops_processed <- stops_processed %>%
  mutate(lane_pred = predict(lm_fit_for_diesel_lanes, stops_processed))
# se included
stops_with_preds <- lm_fit_for_diesel_lanes %>% augment(newdata = stops_processed)



diesellanes_vs_pksp +
  geom_point(data = stops_with_preds %>% filter(diesel_lanes_NA),
             mapping = aes(x = pksp_total, y = .fitted, col = chain3), shape = 4, size = 5) +
  ggtitle("Diesel lanes at truck stops (2763) compared to truck parking space count",
          subtitle = "X: predicted # lanes for stops without diesel lane info (3380), based on least squares fit to available data") + #on log(pksp+10)+chain+2.3") +
  scale_x_continuous(limits = c(-5, 499)) +
  scale_y_continuous(limits = c(0, 16.1))
ggsave("diesel_lane_prediction.png", width = 8, height = 6)

# data availability
stops_processed %>%
  tabyl(pksp_NA, diesel_lanes_NA)
#          diesel lane info
# pksp_NA FALSE TRUE
# FALSE  2763 3380
# TRUE    27   61

# 3380 stops with parking space count and no diesel lane info.
# can use the 2763 data points with both info to predict diesel lane count.
# 27 with diesel lane info without parking info (no prediction needed)
# 61 without either info.

stops_processed %>%
  tabyl(pksp_NA, diesel_lanes_NA, chain3)
# 2 "chain" out of the 61 without parking info and missing diesel lane info

stops_processed %>%
  tabyl(pksp_NA, diesel_lanes_NA, smallstop)
# 7 "smallstop" out of the 61 without parking info and missing diesel lane info

# # could predict parking and then diesel lane.
# # alternative, predict diesel lane directly.
(manual_diesel_lane_pred <- stops_processed %>%
  group_by(chain3, smallstop) %>%
  summarize(manual_diesel_lane_pred = mean(diesel_lanes, na.rm=T)))


# produce final result
stops_w_full_lanes <- stops_processed %>%
  left_join(manual_diesel_lane_pred, by = c("chain3", "smallstop")) %>%
  mutate(diesel_lanes_final = case_when(diesel_lanes_NA == FALSE ~ diesel_lanes,
                                        diesel_lanes_NA == TRUE & pksp_NA == FALSE ~ lane_pred,
                                        TRUE ~ manual_diesel_lane_pred),
         diesel_lanes_final_rounded = round(diesel_lanes_final))

stops_w_full_lanes %>% summary()

write_csv(stop_info_df, here::here("allstays-analysis", "truckstops_raw_ay.csv"))
write_csv(stops_w_full_lanes, here::here("allstays-analysis", "truckstops_processed_ay.csv"))

#___________________________________________________________________
# analyze truck stop numbers and diesel lanes ####

stops_processed %>% nrow()
# 6231 stops

stops_processed %>%
  tabyl(diesel_lanes) %>%
  mutate(total_diesel_lanes = diesel_lanes * n) %>%
  pull(total_diesel_lanes) %>%
  sum(na.rm = TRUE)
# 15728 lanes excluding NAs

# number and proportion of truck stops with no diesel lane info
stops_processed$diesel_lanes_NA %>% sum(na.rm = TRUE)
# 3441 stops without lane info
1 - (stops_processed$diesel_lanes_NA %>% sum() / (stops_processed %>% nrow()))
# 45% stops don't have diesel lane info

# with prediction based on parking/chain/small,
stops_w_full_lanes %>%
  tabyl(diesel_lanes_final) %>%
  mutate(total_diesel_lanes = diesel_lanes_final * n) %>%
  pull(total_diesel_lanes) %>%
  sum(na.rm = TRUE)
# 27204 diesel lanes


# diesel dispensing rate
# https://en.wikipedia.org/wiki/Fuel_dispenser
# Light passenger vehicle pump flow rate ranges up to about 50 litres (13 US gallons) per minute[7] (the United States limits this to 10 US gallons (38 litres) per minute[8]);
# https://www.govinfo.gov/app/details/FR-1996-06-26/96-16205
# refueling facilities are exempt from the 10gpm requirement if used exclusively to refuel heavy-duty vehicles, boats or airplanes.
#
# ...  pumps serving trucks and other large vehicles have a higher flow rate, up to 130 litres (34 US gallons) per minute in the UK,[7] and airline refueling can reach 1,000 US gallons (3,800 litres) per minute.[9] Higher flow rates may overload the vapor recovery system in vehicles equipped with enhanced evaporative emissions controls[10] (required since 1996 in the US), causing excess vapor emissions, and may present a safety hazard.
#  In the U.S. flow speed is limited to 10 gallons per minute for cars and 40 gallons per minute for trucks. This flow rate is based on the diameter of the vehicle's fuel filling pipe, which limits flow to these amounts.
# google "heavy duty diesel fuel dispensing speed"
# http://www.aboutrving.com/rv-topics/fueling-up-at-truck-pumps/
# If you drive one of those big diesel-pusher motorhomes, your fuel tank has a large capacity when compared with a normal vehicle. You can likely carry 100+ gallons of fuel—again, lots more than a normal vehicle.
# A big truck (a semi-tractor) may hold 300+ gallons of diesel fuel in what are called “saddle tanks” (150 gallons each, one on each side of the tractor). It would take them forever to fill at a normal fuel pump (for cars).
# Once a driver pulls into the pump, they are going to be there about 10–15 minutes. They will fill the tanks, clean the windshield, may check the oil, and whatever minor checking is necessary.
# Using the pump at the truck island is efficient. A fully functional truck pump is rated at 60 gallons per minute. That is six times faster than those at the typical gas station.
# https://www.gilbarco.com/la/en/products/fuel-dispensers/encore/ultra-high-flow
# * Assumes 30 psi inlet pressure. Flow rate for master and satellite used separately is about 32 gpm each. Actual results may vary.
# 2 estimates: 30 & 60 gpm = 0.5-1 gps
# 1 US gal diesel = 137381 Btu https://www.eia.gov/energyexplained/units-and-calculators/
# 1 Btu = 1055.06 J; 3412 Btu = 1 kWh (3600000 J) https://www.eia.gov/energyexplained/units-and-calculators/british-thermal-units.php
# so 1 US gal diesel = 144.945 MJ
# so 0.5-1 gal/sec * 144.945 MJ/gal = 72-145 MW
# about 7 min fueling, 7 min pulling up/inspecting/other -> 0.5
# assumes 100% 24/7/365 available operation
27204 * 100
# 1360200 MW
# Truck stops in the US cumulatively dispense diesel at a rate of 1360 GW


# a truck stop's capacity
# 100 MW * 18 lanes = 1.8 GW


library(readr)
alt_fuel_stations_Jul_20_2020_ <- read_csv("C:/Users/ayip/Downloads/alt_fuel_stations (Jul 20 2020).csv",
                                           col_types = cols(`BD Blends` = col_character(),
                                                            `Date Last Confirmed` = col_date(format = "%Y-%m-%d"),
                                                            `Expected Date` = col_character(),
                                                            `Federal Agency Code` = col_character(),
                                                            `Federal Agency ID` = col_character(),
                                                            `Federal Agency Name` = col_character(),
                                                            `Open Date` = col_date(format = "%Y-%m-%d"),
                                                            Plus4 = col_number()))
View(alt_fuel_stations_Jul_20_2020_)

alt_fuel_stations_Jul_20_2020_ %>% clean_names() %>% names()

alt_fuel_stations_Jul_20_2020_ %>%
  clean_names() %>%
  filter(ev_level2_evse_num > 0 | ev_dc_fast_count > 0) %>%
  #count(groups_with_access_code) %>% #new: access_code
  #View()
  filter(str_detect(groups_with_access_code, "Public") & !str_detect(groups_with_access_code, "PLANNED")) %>%
  count(facility_type) %>%
  View()


  filter(facility_type %in% c("TRUCK_STOP","REST_STOP","TRAVEL_CENTER")) %>% # many NA
  group_by(state) %>%
  summarize(lvl2total = sum(ev_level2_evse_num, na.rm=T),
            lvl3total = sum(ev_dc_fast_count, na.rm=T),
            station_count = n()) %>%
  arrange(desc(lvl3total)) %>%
  adorn_totals() %>%
  View()
# 47 EV charging stations with lvl 2+, public and not "planned" +
# with facility type "TRUCK_STOP","REST_STOP","TRAVEL_CENTER"
# total outlet count:
# 50 lvl 2
# 183 lvl 3


#https://chargehub.com/en/electric-car-charging-guide.html#charginglevels
#lvl 2: 3-20 kW, typically 6
#lvl 3: typically 50 kW, occasionally 20
# XFC: MW+ charging for trucks.
# top off
# team drivers, sleepers, double-shifts

# actual utilization?




# current estimate of EVSE at these truck stops/rest stops/travel centers:
50 * 20 + 183 * 50
# 10150 kW
# 10 MW


# total (any facility type incl NA): lvl2 67755, lvl3	14732
# total cumulative power rating of EVSE in US
67755 * 20 + 14732 * 50
# 2091700 kW
# 2.1 GW


# efficiency benefit means only 1/3 power needed for same propulsion.
# so... 30 MW equivalent of diesel fuel pumping?




#need: ev_vehicle_class
#a la cng_vehicle_class
#  Type: string
# For CNG stations, the maximum vehicle size that can physically access the fueling infrastructure, given as code values as described below:









#___________________________________________________________________
# states analysis (pre-prediction) ####

# state summary helps inform which state to focus on (fewest NAs for diesel lane information)
(state_summary <- stops_processed %>%
  group_by(region, diesel_lanes) %>%
  summarize(count = n()) %>%
    mutate(total_in_state = sum(count),
           proportion = count / total_in_state,
           avail = 1-proportion) %>%
  filter(is.na(diesel_lanes)) %>%
  arrange(desc(avail)))


# lanes histogram total
stops_processed %>%
  ggplot() +
  geom_histogram(aes(diesel_lanes_NAas0)) +
  ggtitle("diesel lane information for all stations in US")

# histograms per state (selected)
stops_processed %>%
  filter(state %in% c("nevada", "texas", "connecticut", "oregon", "ohio", "florida")) %>%
ggplot() +
  geom_histogram(aes(diesel_lanes_NAas0)) +
  facet_wrap(vars(state), scales = "free_y")

# histograms for non-zero-lane stations
stops_processed %>%
  filter(diesel_lanes != 0 | is.na(diesel_lanes)) %>%
  ggplot() +
  geom_histogram(aes(diesel_lanes)) +
  facet_wrap(vars(state))

# histogram for nevada
stops_processed %>%
  filter(state == "nevada") %>%
  ggplot() +
  geom_histogram(aes(diesel_lanes_NAas0)) +
  facet_wrap(vars(state))



# post-prediction
stops_w_full_lanes %>%
  ggplot() +
  geom_histogram(aes(x=diesel_lanes_final_rounded, fill = chain3), binwidth = 1) +
  ggtitle("Diesel lanes per location for all truck stops in the US",
          subtitle = "Data: 2763 actual counts from Allstays.com\n3380 predicted based on parking spaces/stop size and operator") +
  labs(x = "Number of diesel lanes", y = "Truck stop count", fill = "Operator")
ggsave("diesel_lanes_US.png", width = 5, height = 4)

library(geofacet)
stops_w_full_lanes %>%
  ggplot() +
  geom_histogram(aes(diesel_lanes_final_rounded, fill = chain3), binwidth = 1) +
  ggtitle("Diesel lanes per location for all truck stops in the US",
          subtitle = "Data: 2763 actual counts from Allstays.com, 3380 predicted based on parking spaces/stop size and ownership") +
  labs(x = "Number of diesel lanes", y = "Truck stop count") +
  facet_geo(vars(state), grid = us_state_grid1, scales = "fixed") + # fixed or free_y
  labs(x = "Number of diesel lanes", y = "Truck stop count", fill = "Operator")
ggsave("diesel_lanes_by_state.png", width = 8, height = 6)


#___________________________________________________________________
# alt fuels ?
# number of truck stops offering propane* (may not be for propulsion) or biodiesel?
# fuelman (probably also not for propulsion)
stops_processed %>%
  select(propane, biodiesel) %>%
  summary()
# cng? lng? electric?
# "fuel" ?

#___________________________________________________________________
# map of all states and truck stop numbers ####
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE)) %>%
  cbind(., st_coordinates(st_centroid(.))) %>%
  mutate(region = to_any_case(as.character(ID), "snake")) %>%
  left_join(state_summary, by = "region")
states %>% View()

map_data("state") %>% mutate(region = to_any_case(region, "snake")) %>% pull(region) %>% as.factor() %>% levels()

states_map <- map_data("state") %>%
  mutate(region = to_any_case(region, "snake")) %>%
  left_join(state_summary, by = "region")
states_map %>% View()

states_map %>%
  ggplot()+
  geom_polygon(aes(long, lat, group = group, fill = total_in_state), color = "white") +
  geom_label(data = states,
             aes(X, Y, label = total_in_state), size = 3) +
  scale_fill_viridis_c(option = "C")

#___________________________________________________________________
# mapping stations onto google satellite images ####

register_google(Sys.getenv("google_cloud_apikey"))

# close_up <- ggmap(nevada)
# x_lim <- close_up$data[c(1, 4), 1] * c(1, .9998)
# y_lim <- close_up$data[c(1, 4), 2] * c(1.003, 1)
map_with_stops <- function(gmap, state_name) {
  gg <- ggmap(gmap) +
    geom_point(data = stops_processed %>% filter(state == state_name),
               mapping = aes(x = lon, y = lat),
               color = "red",
               size = 5,
               alpha = 1,
               shape = 1) +
    geom_label_repel(data = stops_processed %>% filter(state == state_name),
                     mapping = aes(x = lon, y = lat,
                                   label = diesel_lanes,
                                   col = diesel_lanes_NA),
                     size = 3) +
    # blank() +
    # north2() +
    # scalebar(x.min = x_lim[1], x.max = x_lim[2],
    #          y.min = y_lim[1], y.max = y_lim[2],
    #          dist = 100, dist_unit = "mi", transform = TRUE,
    #          model = "WGS84") +
    scale_color_manual(values = c("dark green", "orange")) +
    theme_void() +
    ggtitle("truck stops and diesel lanes")

  return(gg)
}

# map of nevada with stations
nevada <- get_googlemap("Nevada", zoom = 6, maptype = "hybrid")
map_with_stops(nevada, "nevada")


# maps of each station in nevada
nevada_stops_maps <- stops_processed %>%
  filter(state == "nevada") %>%
  #slice_head(n=5) %>%
  select(lon, lat) %>%
  as.list() %>%
  pmap(~get_googlemap(c(..1, ..2), zoom = 17, maptype = "hybrid"))

nv_ggs <- nevada_stops_maps %>% purrr::map(map_with_stops,
                                           state_name = "nevada")
nv_ggs[[1]]
nv_ggs[[2]]
nv_ggs[[3]]
nv_ggs[[4]]
nv_ggs[[5]]

# map of a specific station
stops_processed %>%
  filter(id == 96412) %>%
  select(lon, lat) %>%
  as.list() %>%
  pmap(~get_googlemap(c(..1, ..2), zoom = 17, maptype = "hybrid")) %>%
  purrr::map(map_with_stops, state_name = "TN")


#___________________________________________________________________
# US map

states_map %>%
  ggplot()+
  geom_polygon(aes(long, lat, group = group), fill = "grey", color = "white") +
  geom_point(data = stops_processed %>% filter(region != "alaska"),
             mapping = aes(x = lon, y = lat),
             color = "red",
             size = 1,
             alpha = 1,
             shape = 16) +
  theme_void() +
  ggtitle("truck stops in the USA")


# get_googlemap("USA", zoom = 4,
#               maptype = "terrain",
#               size = c(1000, 600)) %>%
#   ggmap() +



#___________________________________________________________________
# population by county ####
if (! file.exists("PEP_2018_PEPANNRES.zip")) {
download.file("http://www.stat.uiowa.edu/~luke/data/PEP_2018_PEPANNRES.zip",
              "PEP_2018_PEPANNRES.zip")
unzip("PEP_2018_PEPANNRES.zip")
}

pep2018 <- read.csv("PEP_2018_PEPANNRES_with_ann.csv")
pepvars <- names(pep2018)
pep2018 <- read.csv("PEP_2018_PEPANNRES_with_ann.csv", stringsAsFactors = FALSE,
                    head = FALSE, skip = 2)
names(pep2018) <- pepvars
head(pep2018)



# to map lat lon to county fips
# use geocode (google api)
# or https://rdrr.io/cran/rSPARCS/man/FIPS.name.html
# or
# https://stackoverflow.com/questions/13316185/r-convert-zipcode-or-lat-long-to-county
library(sp)
library(maps)
library(maptools)

# The single argument to this function, pointsDF, is a data.frame in which:
#   - column 1 contains the longitude in degrees (negative in the US)
#   - column 2 contains the latitude in degrees

latlong2county <- function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per county
  counties <- map('county', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(counties$names, ":"), function(x) x[1])
  counties_sp <- map2SpatialPolygons(counties, IDs=IDs,
                                     proj4string=CRS("+proj=longlat +datum=WGS84"))

  # Convert pointsDF to a SpatialPoints object
  pointsSP <- SpatialPoints(pointsDF,
                            proj4string=CRS("+proj=longlat +datum=WGS84"))

  # Use 'over' to get _indices_ of the Polygons object containing each point
  indices <- over(pointsSP, counties_sp)

  # Return the county names of the Polygons object containing each point
  countyNames <- sapply(counties_sp@polygons, function(x) x@ID)
  countyNames[indices]
}

# Test the function using points in Wisconsin and Oregon.
testPoints <- data.frame(x = c(-90, -120), y = c(44, 44))

latlong2county(testPoints)
#[1] "wisconsin,juneau" "oregon,crook" # IT WORKS
