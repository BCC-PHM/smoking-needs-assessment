library(readxl)
library(dplyr)
library(stringr)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

##############################################################################
#                      Load Postcode and Census Data                         #
##############################################################################

brum_postcodes <- read.csv(
  "data/West Midlands postcodes.csv"
  ) %>%
  filter(
    District == "Birmingham"
  ) %>%
  mutate(
    LSOA21 = LSOA21.Code,
    IMD_rank = Index.of.Multiple.Deprivation
  ) %>%
  select(
    Postcode, Longitude, Latitude, LSOA21, IMD_rank
  )

LSOA21_postcode_counts <- brum_postcodes %>%
  select(LSOA21, Postcode) %>%
  distinct() %>%
  count(LSOA21) %>%
  rename(Total_Postcodes = n)

brum_eths <- read.csv(
  "data/Census21_Brum_LSOA_all_ethnicity.csv"
) %>%
  rename(
    LSOA21 = Lower.layer.Super.Output.Areas.Code,
    LSOA_Name = Lower.layer.Super.Output.Areas,
    Ethnic_Group = Ethnic.group..20.categories.
  ) %>%
  mutate(
    Ethnic_Group = stringr::str_split_i(Ethnic_Group, ": ", i=2),
    Ethnic_Group = case_when(
      Ethnic_Group == "English, Welsh, Scottish, Northern Irish or British" ~ "White British",
      Ethnic_Group == "Other Mixed or Multiple ethnic groups" ~ "Mixed other",
      Ethnic_Group == "Irish" ~ "Other White",
      TRUE ~ Ethnic_Group
    )
    )%>%
  select(
    LSOA21, LSOA_Name, Ethnic_Group, Observation
  ) %>%
  # Remove `Does not apply` ethnic group since all zero
  filter(Ethnic_Group != "Does not apply" )

brum_eths <- brum_eths %>%
  # Calculate population total
  rbind(
    brum_eths %>%
      group_by(LSOA21, LSOA_Name) %>%
      summarise(Observation = sum(Observation)) %>%
      mutate(Ethnic_Group = "Total_Population")
  ) %>%
  arrange(LSOA21)

brum_age_sex <- read.csv(
    "data/Census21_Brum_LSOA_sex_age.csv"
  ) %>%
  rename(
    LSOA21 = Lower.layer.Super.Output.Areas.Code,
    LSOA_Name = Lower.layer.Super.Output.Areas,
    Age_cat = Age..D...8.categories.,
    Sex = Sex..2.categories.
  ) %>%
  mutate(
    Age_group = case_when(
      Age_cat == "Aged 15 years and under" ~ "0-15",
      Age_cat == "Aged 16 to 24 years" ~ "16-24",
      Age_cat == "Aged 25 to 34 years" ~ "25-34",
      Age_cat == "Aged 35 to 44 years" ~ "35-44",
      Age_cat == "Aged 45 to 54 years" ~ "45-54",
      Age_cat == "Aged 55 to 64 years" ~ "55-64",
      Age_cat == "Aged 65 to 74 years" ~ "65-74",
      Age_cat == "Aged 75 years and over" ~ "75+",
      TRUE ~ "Error in age renaming",
    )
  ) %>%
  arrange(LSOA21) %>%
  select(
    LSOA21, Sex, Age_group, Observation
  )

LSOA21_IMDs <- brum_postcodes %>%
  group_by(LSOA21) %>%
  summarise(
    IMD_rank = Mode(IMD_rank)
  )

##############################################################################
#                        Define Postcode Functions                           #
##############################################################################

get_LSOA_coverage <- function(
  # Data frame for all pharmacies 
  pharmacy_data, 
  NHS_Code,
  dist_m
) {
  
  postcodes_i <- purrr::map2_dfr(
    pharmacy_data$Latitude[pharmacy_data$NHS_Code == NHS_Code],
    pharmacy_data$Longitude[pharmacy_data$NHS_Code == NHS_Code],
    ~spatialrisk::points_in_circle(brum_postcodes, .y, .x,
                                   lon = Longitude,
                                   lat = Latitude,
                                   radius = dist_m))%>%
    select(Postcode, LSOA21)
  
  LSOA_coverage <- postcodes_i %>%
    # Remove double counted postcodes
    distinct() %>%
    # count number of postcodes within each LSOA
    count(LSOA21) %>%
    # Join to total number of postcodes in each LSOA
    left_join(
      LSOA21_postcode_counts,
      by = join_by("LSOA21")
    ) %>%
    # Calculate percentage of LSOA postcodes within 10 minutes walk
    mutate(
      overlap_frac = n / Total_Postcodes,
      overlap_perc = 100 * overlap_frac
    )
  
  # Check for unmatched rows
  if (any(is.na(LSOA_coverage$Total_Postcodes))) {
    stop("Error: Not all rows in the left table have a match in the right table.")
  }
  
  return(LSOA_coverage)
}


get_pharm_LSOA_eths <- function(
    LSOA_coverage
) {
  
  LSOA_eths <- LSOA_coverage %>%
    left_join(
      brum_eths,
      by = join_by("LSOA21"),
      relationship = "one-to-many"
    ) %>% mutate(
      Est_Num = overlap_frac * Observation
    ) %>%
    select(-Observation)
  
  # Check for unmatched rows
  if (any(is.na(LSOA_eths$Est_Num))) {
    stop("Error: Not all rows in the left table have a match in the right table.")
  }
  
  
  return(LSOA_eths)
}

get_pharm_pops <- function(
    LSOA_coverage
) {
  LSOA_eths <- get_pharm_LSOA_eths(LSOA_coverage)
  
  pharm_pops <- LSOA_eths %>%
    group_by(Ethnic_Group) %>%
    summarise(
      Est_Num = sum(Est_Num)
    )
  return(pharm_pops)
}


get_pharm_LSOA_eths <- function(
    LSOA_coverage
) {
  
  LSOA_eths <- LSOA_coverage %>%
    left_join(
      brum_eths,
      by = join_by("LSOA21"),
      relationship = "one-to-many"
    ) %>% mutate(
      Est_Num = overlap_frac * Observation
    ) %>%
    select(-Observation)
  
  # Check for unmatched rows
  if (any(is.na(LSOA_eths$Est_Num))) {
    stop("Error: Not all rows in the left table have a match in the right table.")
  }
  
  
  return(LSOA_eths)
}



get_pharm_imd <- function(
    LSOA_eths
    ) {
  pharm_LSOA_IMDs <- LSOA_eths %>%
    left_join(
      LSOA21_IMDs,
      by = join_by("LSOA21")
    ) %>%
    left_join(
      brum_eths,
      by = join_by("LSOA21"),
      relationship = "one-to-many"
    ) %>% 
    filter(Ethnic_Group == "Total_Population") %>%
    mutate(
      Est_Num = overlap_frac * Observation
    ) %>%
    select(-Observation)
  
  # Check for unmatched rows
  if (any(is.na(pharm_LSOA_IMDs$IMD_rank))) {
    stop("Error: Not all rows in the left table have a match in the right table.")
  }
  pharm_IMD <- pharm_LSOA_IMDs %>%
    mutate(
      IMD_quintile =  floor(5 * IMD_rank/32844 + 1),
    ) %>%
    group_by(IMD_quintile) %>%
    summarise(
      Est_Num = sum(Est_Num)
    ) 
  
  return(pharm_IMD)
}

get_pharm_age_sex <- function(
    LSOA_coverage
) {
  
  pharm_age_sex <- LSOA_coverage %>%
    left_join(
      brum_age_sex,
      by = join_by("LSOA21"),
      relationship = "one-to-many"
    ) %>% mutate(
      Est_Num = overlap_frac * Observation
    ) %>%
    select(-Observation) %>%
    group_by(
      Sex, Age_group
    ) %>%
    summarise(
      Est_Num = sum(Est_Num),
      .groups = "drop"
    )
  
  
  return(pharm_age_sex)
}


##############################################################################
#                         Load Pharmacy Postcodes                            #
##############################################################################

# Load smoking cessation services data
pharmacies <- read_excel("data/List of providers and postcodes.xlsx") %>%
  filter(
    `Organisation Type` == "Pharmacy"
  ) %>%
  left_join(
    brum_postcodes,
    by = join_by("Postcode")
  ) %>%
  rename(
    NHS_Code = `NHS Code`
  )

##############################################################################
#     Collect demographic info of Residents within 1km of each Pharmacy      #
##############################################################################

# Prepare data lists
pharmacy_eth_list <- list()
pharmacy_IMD_list <- list()
pharmacy_age_Sex_list <- list()

# Loop over all pharmacies
for (code_i in pharmacies$NHS_Code) {
  # Get estimated LSOA coverage
  LSOA_coverage_i <- get_LSOA_coverage(
    pharmacies,
    code_i,
    1000
  )
  
  # Get estimated ethnicity populations in 1km
  eths_i <- get_pharm_pops(LSOA_coverage_i) %>%
    mutate(
      NHS_Code = code_i,
      Name = pharmacies$Name[pharmacies$NHS_Code == code_i]
    )
  
  # Get estimated population IMDs in 1km
  imd_i <- get_pharm_imd(LSOA_coverage_i) %>%
    mutate(
      NHS_Code = code_i,
      Name = pharmacies$Name[pharmacies$NHS_Code == code_i]
    )
  
  # Get estimated population age and sex in 1km
  age_sex_i <- get_pharm_age_sex(LSOA_coverage_i) %>%
    mutate(
      NHS_Code = code_i,
      Name = pharmacies$Name[pharmacies$NHS_Code == code_i]
    )
  
  # Store data
  pharmacy_eth_list[[code_i]] <- eths_i
  pharmacy_IMD_list[[code_i]] <- imd_i
  pharmacy_age_Sex_list[[code_i]] <- age_sex_i
}

# Combine ethnicity data
pharmacy_eths <- data.table::rbindlist(pharmacy_eth_list) %>%
  select(
    c(Name, NHS_Code, Ethnic_Group, Est_Num)
  ) %>%
  mutate(
    Est_Num = round(Est_Num)
  )

# Combine IMD data
pharmacy_imds <- data.table::rbindlist(pharmacy_IMD_list) %>%
  select(
    c(Name, NHS_Code, IMD_quintile, Est_Num)
  ) %>%
  mutate(
    Est_Num = round(Est_Num)
  )

# Combine Age & Sex data
pharmacy_age_sex <- data.table::rbindlist(pharmacy_age_Sex_list) %>%
  select(
    c(Name, NHS_Code, Sex, Age_group, Est_Num)
  ) %>%
  mutate(
    Est_Num = round(Est_Num)
  )

output <- list(
  "Ethnicity" = pharmacy_eths,
  "Deprivation" = pharmacy_imds,
  "Age & Sex" = pharmacy_age_sex
)

writexl::write_xlsx(output, "data/pharmacy-demographic-estimates.xlsx")
