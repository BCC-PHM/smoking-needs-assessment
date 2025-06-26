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

writexl::write_xlsx(GP_data, "data/smoking-QOF.xlsx")

# Convert to ward-level estimate
ward_data <- convert_GP_data(
  data = GP_data,
  GP_code_header = "Practice_Code",
  value_header = "Count",
  norm_header = "Denominator",
  to = "Ward"
)

ward_shape <- Ward
ward_shape@data <- ward_shape@data %>%
  mutate(
    Ward = stringr::str_wrap(Ward, width = 15)
  )
ward_shape <- subset(ward_shape, ward_shape@data$Area == "Birmingham")

# Plot smoking QOF prevalence
map <- plot_map(
  ward_data,
  value_header = "Count per 100 Denominator",
  area_name = "Birmingham",
  map_type = "Ward",
  map_title = "Smoking QOF Prevalence (2023/24)",
  style = "cont"
) +
  tm_shape(ward_shape) +
  tm_text(text = "Ward", size = 0.25,
          xmod=c(0,0,0,0,0.1,                                                          # ie adjust text labels horizontally
                 0,0,-0.1,0.3,-0.4,                         
                 -0.09,0,0.3,-0.2,0,
                 0,0,0.2,0.3,0.1,                               
                 -1.0,0,-0.2,0,0,
                 0,0,-0.2,0,0,                             
                 0,-0.2,0.6,0,0.2,
                 0.1,0,-0.2,0,0.2,                                                
                 -0.3,0,0.3,0,0,
                 -0.2,0.15,0,0,0.2,                       
                 0,-0.1,-0.2,0,0,
                 0,0,-0.1,-0.2,0,                                              
                 0,-0.2,-0.2,0,-0.2,
                 0,-0.2,0.1,0),                                 
          ymod=c(0,-0.2,0,0,-0.1,                                                        # ie adjust text labels vertically
                 0,0,-0.2,0.3,-0.1,                         
                 0,0,-0.1,0.1,0,
                 0.1,0,0,0,-0.1,                            
                 0.3,-0.1,0,0,-0.2,
                 0,0,0.1,0,0,                               
                 0.2,-0.1,0.3,0,0.15,
                 0,0.1,0,0,0.3,                                                                     
                 -0.4,0,-0.3,0,-0.2,
                 0,0,0.1,0,0.2,                       
                 0,0.2,0,0,0.1,
                 0,0,0.3,0,0,                                              
                 0,0,0,0,0,
                 -0.1,0,0,0)
  ) 

# Add service points
map1 <- add_points(
  map, 
  smoking_services,
  color = "Organisation Type",
  size = 0.3
)

save_map(map1, "output/smoking_prev_map.png")

# Add service points
map2 <- add_points(
  map, 
  smoking_services,
  color = "Organisation Type",
  size = 0.7
)
save_map(map2, "output/smoking_prev_map.html")