# Hospital-based smoking cessation offers
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)


# Load data and reshape into long format
data <- read_excel("data/hospital-outcomes.xlsx") %>%
  select(-Count_Referrals) %>%
  pivot_longer(
    cols = contains("Outcome"),
    names_to = "Outcome",
    values_to = "Count"
  ) %>%
  mutate(
    NHS_Trust = str_to_title(ODS_Name),
    NHS_Trust = str_replace(NHS_Trust, "Nhs", "NHS"),
    NHS_Trust = str_wrap(NHS_Trust, 30),
    Outcome = str_remove(Outcome, "Outcome - "),
    Outcome = str_replace(Outcome, "NULL", "Unknown")
  ) %>%
  select(-ODS_Name)

# Aggregate data to NHS trust and setting type
data_agg <- data %>%
  group_by(
    NHS_Trust, Setting_Type, Outcome
  ) %>%
  summarise(
    Count = sum(Count),
    .groups = "drop"
  )

plt <- ggplot(data_agg, aes(x = Setting_Type, y = Count, fill = Outcome)) +
  geom_col() +
  theme_bw() +
  facet_wrap(~NHS_Trust, scale = "free_y") +
  labs(
    x = "Setting Type",
    y = "Outcome Count",
    fill = ""
  ) + 
  theme(
    legend.position = "top"
  ) +
  scale_fill_brewer(palette="Set2")

plt

ggsave("output/hospital-outcomes-free-y.png", plt, 
       width = 7, height = 4)

plt2 <- plt +scale_y_continuous(
  limits = c(0, 4000),
  #expand = c(0, 0)
) 

ggsave("output/hospital-outcomes.png", plt2,
       width = 7, height = 4)