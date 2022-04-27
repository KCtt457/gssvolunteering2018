#### Preamble ####
# Purpose: To simulate volunteering data for analysis
# Author: Kimlin Chin
# Date: 21 April 2022
# Contact: kimlin.chin@mail.utoronto.ca
# License: MIT


#### Workspace setup ####
library(janitor)
library(lubridate)
library(tidyverse)

#### Simulate data ####
set.seed(27422)

# Add main variables of interest - volunteer and life satisfaction,
# some basic demographic variables, and aspects of volunteering
simulated_volunteer_data <- tibble(
  id = 1:100,
  isvolunteer = c(rep("Volunteer", 55), rep("Non-volunteer", 45)),
  life_satisfaction = c(
    # Assume scale of 1-7 where 1 is least satisfied and 7 is most satisfied
    rep(7, 20),
    rep(6, 15),
    rep(5, 10),
    rep(4, 3),
    rep(3, 2),
    rep(2, 3),
    rep(1, 2),
    sample(
      c(1, 2, 3, 4, 5, 6, 7),
      size = 45,
      replace = TRUE,
      prob = c(0.05, 0.05, 0.1, 0.1, 0.3, 0.2, 0.2)
    )
  ), 
  # Demographic characteristics
  gender = sample(
    c("Male", "Female", "Other"),
    size = 100,
    replace = TRUE,
    prob = c(0.52, 0.43, 0.05)
  ),
  age = c(
    rep("15-24 years", 10),
    rep("25-34 years", 15),
    rep("35-44 years", 15),
    rep("45-54 years", 15),
    rep("55-64 years", 15),
    rep("65-74 years", 10),
    rep("75-84 years", 10),
    rep("85 years and over", 10)
  ),
  married = c(
    sample(
      c("Yes", "No"),
      size = 10,
      replace = TRUE,
      prob = c(0.5, 0.5)
    ),
    sample(
      c("Yes", "No"),
      size = 60,
      replace = TRUE,
      prob = c(0.48, 0.52)
    ),
    sample(
      c("Yes", "No"),
      size = 30,
      replace = TRUE,
      prob = c(0.2, 0.8)
    )
  ),
  # Volunteer aspects
  meaningful_contribution = c(rep("Yes", 35), rep("No", 20), rep(NA, 45)),
  num_hours_per_week = c(sample(
    seq(1, 30, by = 1), size = 55, replace = TRUE
  ), rep(NA, 45)),
  use_skills = c(sample(
    c("Yes", "No"),
    size = 55,
    replace = TRUE,
    prob = c(0.32, 0.68)
  ), rep(NA, 45))
)

#### Visualize simulated data ####
View(simulated_volunteer_data)

## Plot number of volunteers against non-volunteers
ggplot(simulated_volunteer_data) +
  geom_bar(aes(x = isvolunteer))

# There are slightly more volunteers than non-volunteers in the simulated data.

## Plot life satisfaction for volunteers vs non-volunteers
ggplot(simulated_volunteer_data) +
  geom_bar(aes(x = life_satisfaction, fill = isvolunteer), position = "dodge")

# There are more volunteers than non-volunteers for life satisfaction scores of 6 and 7.

#### Fit simple models to simulated data ####

## Life satisfaction vs volunteer flag
model1s <-
  lm(life_satisfaction ~ isvolunteer + age + gender + married, data = simulated_volunteer_data)
summary(model1s)

# Volunteering is positively correlated with life satisfaction.

## Life satisfaction vs volunteer characteristics
simulated_volunteers_only <- simulated_volunteer_data %>%
  filter(isvolunteer == "Volunteer")

model2s <-
  lm(life_satisfaction ~ meaningful_contribution + num_hours_per_week + use_skills,
     data = simulated_volunteers_only)
summary(model2s)

# Meaningful contribution is positively associated with life satisfaction among volunteers.
