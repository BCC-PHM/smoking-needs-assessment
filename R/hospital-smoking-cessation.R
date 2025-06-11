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
  # Process data text
  mutate(
    NHS_Trust = str_to_title(ODS_Name),
    NHS_Trust = str_replace(NHS_Trust, "Nhs", "NHS"),
    NHS_Trust = str_wrap(NHS_Trust, 30),
    Outcome = str_remove(Outcome, "Outcome - "),
    Outcome = str_replace(Outcome, "NULL", "Unknown"),
    Referral_Type = str_remove(Referral_Type, " Commissioned Stop Smoking Service"),
    Referral_Type = str_remove(Referral_Type, " Community Pharmacy"),
    Setting_Type = paste0(Setting_Type, "\n(", Referral_Type, ")")
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
  scale_fill_brewer(palette="Set2")+
  theme(
    axis.text.x = element_text(size = 7)  # adjust size here
  )

plt

ggsave("output/hospital-outcomes.png", plt, 
       width = 8, height = 3.5)
