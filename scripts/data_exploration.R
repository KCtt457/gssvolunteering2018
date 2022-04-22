
library(tidyverse)

gss1 <- read_csv("inputs/data/gss.csv")

# Number of volunteers vs non-volunteers
ggplot(gss1) +
  geom_bar(aes(x = fv1fvol))

# reasons for volunteering
reasons_v <- gss1 %>%
  select(rv_020, rv_025, rv_030, rv_040, rv_050, rv_060, rv_065,
         rv_070, rv_080, rv_090, rv_100, rv_110) %>%
  pivot_longer(everything(), names_to = "Reason", values_to = "Yes") %>%
  filter(Yes == "Yes") %>%
  group_by(Reason) %>%
  summarise(count = n())

reasons_described <- tibble(Reason = c("rv_020", 'rv_025', 'rv_030', 'rv_040', 'rv_050','rv_060', 'rv_065',
                  'rv_070', 'rv_080', 'rv_090', 'rv_100', 'rv_110'),
       description = c("Personally affected", "Family member volunteers",
                       "Friends volunteer", "To network", "Job opportunities",
                       "Religious reasons", "Spiritual or other beliefs", "To explore your own strengths",
                       "Community contribution", "To use your skills", "Support a cause",
                       "Improve health/well-being"))

reasons_v <- left_join(reasons_v, reasons_described, by = "Reason")

ggplot(reasons_v) +
  geom_bar(aes(x = reorder(description, count), y = count), stat = "identity") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank()) +
  labs(title = "Reasons for Volunteering in the past 12 months",
       x = "",
       y = "Number of responses") +
  coord_flip()

# reasons for not volunteering
reasons_nv <- gss1 %>%
  select(nv_020, nv_030, nv_040, nv_050, nv_060,
         nv_070, nv_080, nv_090, nv_100, nv_110, nv_120, nv_130) %>%
  pivot_longer(everything(), names_to = "Reason", values_to = "Yes") %>%
  filter(Yes == "Yes") %>%
  group_by(Reason) %>%
  summarise(count = n())

reasons_described <- tibble(Reason = c('nv_020', 'nv_030', 'nv_040', 'nv_050', 'nv_060',
                                       'nv_070', 'nv_080', 'nv_090', 'nv_100', 'nv_110', 'nv_120', 'nv_130'),
                            description = c("Gave enough time", "Previous experience", "No one asked",
                                            "Did not know how", "Health problems", "No time", "Financial cost",
                                            "Unable to make commitment", "Preferred to give money", "No interest",
                                            "No opportunity to use skills", "Not meaningful"))

reasons_nv <- left_join(reasons_nv, reasons_described, by = "Reason")

ggplot(reasons_nv) +
  geom_bar(aes(x = reorder(description, count), y = count), stat = "identity") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank()) +
  labs(title = "Reasons for Not Volunteering in the past 12 months",
       x = "",
       y = "Number of responses") +
  coord_flip()
