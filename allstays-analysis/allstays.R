# install packages ####
# if (!("rvest" %in% installed.packages())) {
#   install.packages("rvest")
# }
# if (!("tidyverse" %in% installed.packages())) {
#   install.packages("tidyverse")
# }
# devtools::install_github("dkahle/ggmap", ref = "tidyup")
# install.packages('ggrepel')
# install.packages("maps")
# install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel",
#                    "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))
#install.packages("progress")
#___________________________________________________________________
# load packages (must run every time) ####
library(maps);
library(rvest); library(ggmap); library(ggsn); library(ggrepel);
library(janitor); library(snakecase); library("sf"); library(progress);
library(magrittr); library(tidyverse);

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

stage_pg_link_from_id <- function(id) {
  paste0("https://www.allstays.com/truckstops-details/", id, ".php")
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


# not sure if 44124, 198722, 48681 were problems
# (unused connection closed)
# no error messages detected... known 404 problems...


stop_info_df <- stop_info_list %>%
  unlist(recursive = FALSE) %>%
  unlist(recursive = FALSE) %>%
  enframe() %>%
  unnest_wider(value) %>%
  filter(!is.na(raw_loc)) %>%
  mutate(loc = str_match(raw_loc, "daddr=(.*)")[,2], #extract2
         diesel = as.numeric(str_match(raw_desc, "([:digit:]*)\\s*diesel")[,2]),
         Diesel = as.numeric(str_match(raw_desc, "([:digit:]*)\\s*Diesel")[,2]),
         propane = str_detect(raw_desc, "ropane"),
         biodiesel = str_detect(raw_desc, "BioDiesel|biodiesel|Biodiesel|bioDiesel|Bio Diesel|bio diesel|Bio diesel|bio Diesel")) %>%
  rowwise() %>%
  mutate(diesel_lanes = sum(diesel, Diesel, na.rm = T)) %>%
  ungroup() %>%
  #mutate(coords = list(set_names(str_split(loc, ","), c("lat","lon")))) %>%
  ##unnest_wider(coords) %>%
  separate(loc, c("lat","lon"), ",") %>%
  mutate(lat = as.numeric(lat), lon = as.numeric(lon),
         diesel_lanes_0 = if_else(diesel_lanes == 0, TRUE, FALSE),
         region = to_any_case(state, "snake"))

stop_info_df %>% View()


#___________________________________________________________________
# save RDS ####
stop_info_df %>% saveRDS(here::here("stop_info.RDS"))

#___________________________________________________________________
# read/mutate allstays data (start here) ####
stop_info_df <- readRDS(here::here("stop_info.RDS"))

View(stop_info_df)


# __ truck parking spaces / Parking Spaces as indicator of size
# "small stop" also sometimes indicated
# chain as indicator of business

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



# clean out duplicates
# check 404


#___________________________________________________________________
# analyze truck stop numbers and diesel lanes ####
stop_info_df %>% nrow()

stop_info_df %>%
  tabyl(diesel_lanes) %>%
  mutate(total_diesel_lanes = diesel_lanes * n) %>%
  pull(total_diesel_lanes) %>%
  sum()


# number and proportion of truck stops with 0 diesel lanes
stop_info_df$diesel_lanes_0 %>% sum()

1 - (stop_info_df$diesel_lanes_0 %>% sum() / (stop_info_df %>% nrow()))

state_summary <- stop_info_df %>%
  group_by(region, diesel_lanes) %>%
  summarize(count = n()) %>%
  mutate(total_in_state = sum(count),
    proportion = count / total_in_state,
         avail = 1-proportion) %>%
  filter(diesel_lanes == 0) %>%
  arrange(desc(avail))


# number of truck stops offering propane?
stop_info_df$propane %>% sum()


# which state to focus on? look at state summary
View(state_summary)

# lanes histogram total
stop_info_df %>%
  ggplot() +
  geom_histogram(aes(diesel_lanes)) +
  ggtitle("diesel lane information for all stations in US")

# histograms per state (selected)
stop_info_df %>%
  filter(state %in% c("nevada", "texas", "connecticut", "oregon", "ohio", "florida")) %>%
ggplot() +
  geom_histogram(aes(diesel_lanes)) +
  facet_wrap(vars(state), scales = "free_y")

# histograms for non-zero-lane stations
stop_info_df %>%
  filter(diesel_lanes != 0) %>%
  ggplot() +
  geom_histogram(aes(diesel_lanes)) +
  facet_wrap(vars(state))

# histogram for nevada
stop_info_df %>%
  filter(state == "nevada") %>%
  ggplot() +
  geom_histogram(aes(diesel_lanes)) +
  facet_wrap(vars(state))



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
    geom_point(data = stop_info_df %>% filter(state == state_name),
               mapping = aes(x = lon, y = lat),
               color = "red",
               size = 5,
               alpha = 1,
               shape = 1) +
    geom_label_repel(data = stop_info_df %>% filter(state == state_name),
                     mapping = aes(x = lon, y = lat,
                                   label = diesel_lanes,
                                   col = diesel_lanes_0),
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
nevada_stops_maps <- stop_info_df %>%
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

#___________________________________________________________________
# US map

states_map %>%
  ggplot()+
  geom_polygon(aes(long, lat, group = group), fill = "grey", color = "white") +
  geom_point(data = stop_info_df %>% filter(region != "alaska"),
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



# to map lat lon to fips
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
