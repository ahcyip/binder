library(tidyverse)

# Parachoice's Fuel Availability Response
# implemented as coefficient to logit, equivalent to ASC in utility of logit
theta_infrasav <- function(FA) {
  1/(1+exp(-((FA-0.1)/0.02)))
}

FA <- seq(0,1,0.01)
ParachoiceFAR <- FA %>%
  bind_cols(FA %>% map(theta_infrasav) %>% unlist()) %>%
  rename(FA = ...1, theta_infrasav = ...2) %>%
  mutate(lnthetadivnegbeta = log(theta_infrasav)/(-25))

ParachoiceFAR %>% View()

ParachoiceFAR %>%
  ggplot() +
  geom_point(aes(x = FA, y = lnthetadivnegbeta)) +
  labs(x = "Fuel Availability (alt fuel station ratio to diesel)",
       y = "Impact on generalized cost\n(scaled to be approx equal to a fixed percentage increase\nin annual generalized cost relative to diesel)",
       title = "Parachoice Fuel Availability Response")

