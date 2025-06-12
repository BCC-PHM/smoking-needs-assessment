library(BSol.mapR)
library(dplyr)
library(fingertipsR)
library(readxl)

# Load GP-PCN-Locality-LA lookup data
GP_lookup <- read_excel("data/External Megamap - April 2025.xlsx") %>%
  select(
    c(CODE, `PRAC NAME`, Type,`Constituency / Locality`, PCN, `Post Code`)
    ) %>%
  rename(
    Practice_Code = CODE,
    Practice_Name = `PRAC NAME`,
    Locality = `Constituency / Locality`,
    Postcode = `Post Code`
  )

# Load smoking cessation services data
smoking_services <- read_excel("data/List of providers and postcodes.xlsx") 

# Get smoking QOF prevalence from FingerTips
GP_data <- fingertips_data(
  AreaTypeID = 7,
  IndicatorID = 91280 
) %>%
  filter(
    Timeperiod == "2023/24"
  ) %>%
  rename(
    Practice_Code = AreaCode
  ) %>%
  inner_join(
    GP_lookup,
    by= join_by("Practice_Code")
  )

# Convert to ward-level estimate
ward_data <- convert_GP_data(
  data = GP_data,
  GP_code_header = "Practice_Code",
  value_header = "Count",
  norm_header = "Denominator",
  to = "Ward"
)

map <- plot_map(
  ward_data,
  value_header = "Count per 100 Denominator",
  area_name = "Birmingham",
  map_type = "Ward",
  map_title = "Smoking QOF Prevalence (2023/24)",
  style = "cont"
)


map <- add_points(
  map, 
  smoking_services,
  color = "Organisation.Type",
  size = 0.3
)
map

save_map(map, "output/smoking_prev_map.png")
save_map(map, "output/smoking_prev_map.html")