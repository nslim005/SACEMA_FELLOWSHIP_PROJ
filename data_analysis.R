#Part 1 Section 1 Assignment

#Load Libraries
library(readxl)
library(dplyr)
library(stringr)
library(lubridate)
library(skimr) 
library(ggplot2)
library(tidyr)
library(forcats)
library(sf)
library(readr)

#Part 1 Section 1 (Setup & Import)
#import the line list
linelist <- read_excel("./linelist_raw.xlsx")

#confirm Line list dimension
dim(linelist)

#confirm Line list column and data types types
str(linelist)

#summarize Line list
summary(linelist)


#Part 1 Section 2 (Data Cleaning)
##======string========###

linelist %>%
  count(hospital, sort = TRUE) #just to have a look at what the hospital column, looks like 

#From the above result, there are actually nine unique hospitals, 
#other five contain punctuation mismatch, spelling mismatch or typo errors

#creating a named vector to fix the issue highlighted above
hospital_lookup <- c(
  "Port Hospital"                      = "Port Hospital",
  "Military Hospital"                  = "Military Hospital",
  "Central Hospital"                   = "Central Hospital",
  "St. Mark's Maternity Hospital (SMMH)" = "St. Mark's Maternity Hospital (SMMH)",
  "Hospital A"                         = "Hospital A",
  "Hospital B"                         = "Hospital B",
  "Mitylira Hospital"                  = "Mitylira Hospital",
  "Port Hopital"                       = "Port Hospital",        # Fix typo
  "Military Hopital"                   = "Military Hospital",    # Fix typo
  "Central Hopital"                    = "Central Hospital",     # Fix typo
  "St. Marks Maternity Hopital (SMMH)" = "St. Mark's Maternity Hospital (SMMH)",  # Fix typo + punctuation
  "Mitylira Hopital"                   = "Mitylira Hospital",    # Fix typo
  "Other"                              = "Other",
  "NA"                                 = NA         # remains as-is
)

clean_linelist <- linelist %>% mutate( clean_hospital = hospital_lookup[hospital])
clean_linelist %>% count( clean_hospital, sort = TRUE) #confirming the new line list with clean hospital



##=========Types & ranges===========##

str(clean_linelist) # checking the datatypes
#The datatypes appearing for "date onset" and age variables were both wrong and have been corrected below.
#More so, factors have been used for categorical variables while integers for generation, 
#and row_num just incase they tend to increase


#Quick Summaries
 clean_linelist %>%
  select(age, wt_kg, ht_cm, temp) %>%
  skim()

clean_linelist <- clean_linelist %>%
  mutate(
    # Convert character dates to proper date-time
    `date onset` = ymd(`date onset`),
    
    # Convert age from character to numeric 
    age = as.numeric(age),
    
    # Convert categorical variables to factors
    gender = factor(gender),
    outcome = factor(outcome),
    source = factor(source),
    age_unit = factor(age_unit),
    fever = factor(fever),
    chills = factor(chills),
    cough = factor(cough),
    aches = factor(aches),
    vomit = factor(vomit),
    
    # Convert times to hm
    time_admission = hm(time_admission),
    
    # Integer where appropriate
    generation = as.integer(generation),
    row_num = as.integer(row_num),
    
    
    # Handling Continuous Variable from the skim output above
    # === AGE ===
    # Min = 0, Max = 84. 0 years is biologically possible (newborn).
    # No clear implausible values, but flag if age > 130 (although not present here).
    age = if_else(age < 0 | age > 130, NA_real_, age),
    
    # === WEIGHT ===
    # Min = -11 kg. NEGATIVE weight is physically impossible (mass cannot be < 0).
    # Likely data entry error or missing coded as -11. 
    # Set negative weights to NA.
    wt_kg = if_else(wt_kg < 0, NA_real_, wt_kg),
    
    # === HEIGHT ===
    # Min = 4 cm. Human height cannot be 4 cm (smallest viable newborn ~24 cm).
    ht_cm = if_else(ht_cm < 30, NA_real_, ht_cm),  # Below newborn minimum
    
    # Flag extreme heights for sensitivity analysis
    ht_extreme = ht_cm > 250,  # 295 cm warrants verification
    
    # === TEMPERATURE ===
    # Range 35.2–40.8°C. plausible (hypothermia to high fever).
    temp = if_else(temp < 30 | temp > 43, NA_real_, temp)
  )

#Missingness
colSums(is.na(linelist)) # Checked for missing values in the entire record
# How to fix missing records in 'fever', Chills,cough,aches and vomit: 
#I will create a convert the datatype to factor and create a new option Unknown, this will replace
#all the NAs

#for missing age, dropping rows will mean remove 107 rows (1.6% of 6,611). Its proportion is low, 
#It could be safe to remove, #However, not sure if missingness is ramdom or not to prevent bias
#Imputation could bring about bias. leaving as-is preserves uncertainty. 

#Duplicate Values


##=================Part 1 Section 3(Date Imputation)====================##

# ============================================================
# STEP 1: Obtain median gap from the available date records
# ============================================================

# Gap 1: incubation period which is date onset - infection date 
incubation_gap <- clean_linelist %>%
  filter(!is.na(`infection date`), !is.na(`date onset`)) %>%
  mutate(
    gap = as.numeric(
      as.Date(`date onset`) - as.Date(`infection date`)  
    )
  ) %>%
  pull(gap)

median_incubation <- median(incubation_gap, na.rm = TRUE) #calculate the median

# Gap 2: onset to admission which is: hosp date - date onset
admission_gap <- clean_linelist %>%
  filter(!is.na(`date onset`), !is.na(`hosp date`)) %>%
  mutate(gap = as.numeric(as.Date(`hosp date`) - as.Date(`date onset`))) %>%
  pull(gap)

median_admission <- median(admission_gap, na.rm = TRUE) #calculate median


# Gap 3: outcome which is: date_of_outcome - hosp date
outcome_gap <- clean_linelist %>%
  filter(!is.na(`hosp date`), !is.na(`date_of_outcome`)) %>%
  mutate(gap = as.numeric(as.Date(`date_of_outcome`) - as.Date(`hosp date`))) %>%
  pull(gap)

median_outcome <- median(outcome_gap, na.rm = TRUE)


cat("Median incubation (infection -> onset):", median_incubation, "days\n")
cat("Median admission (onset -> hospital):", median_admission, "days\n")
cat("Median outcome (hospital -> outcome):", median_outcome, "days\n")

# ============================================================
# STEP 2: Loop through each row and impute missing dates
# ============================================================

for (i in 1:nrow(clean_linelist)) {
  
  # --- Impute infection date (earliest; back-calculate from onset) ---
  if (is.na(clean_linelist$`infection date`[i]) && !is.na(clean_linelist$`date onset`[i])) {
    # infection date = onset date - typical incubation
    clean_linelist$`infection date`[i] <- clean_linelist$`date onset`[i] - days(median_incubation)
  }
  
  # --- Impute date onset (from infection date) ---
  if (is.na(clean_linelist$`date onset`[i]) && !is.na(clean_linelist$`infection date`[i])) {
    # onset = infection + incubation
    clean_linelist$`date onset`[i] <- clean_linelist$`infection date`[i] + days(median_incubation)
  }
  
  # --- Impute hosp date (from onset) ---
  if (is.na(clean_linelist$`hosp date`[i]) && !is.na(clean_linelist$`date onset`[i])) {
    # hospitalization = onset + admission delay
    clean_linelist$`hosp date`[i] <- clean_linelist$`date onset`[i] + days(median_admission)
  }
  
  # --- Back-impute date onset (from hosp date) ---
  if (is.na(clean_linelist$`date onset`[i]) && !is.na(clean_linelist$`hosp date`[i])) {
    # onset = hospitalization - admission delay
    clean_linelist$`date onset`[i] <- clean_linelist$`hosp date`[i] - days(median_admission)
  }
  
  # --- Impute date_of_outcome (from hosp date) ---
  if (is.na(clean_linelist$`date_of_outcome`[i]) && !is.na(clean_linelist$`hosp date`[i])) {
    # outcome = hospitalization + typical stay
    clean_linelist$`date_of_outcome`[i] <- clean_linelist$`hosp date`[i] + days(median_outcome)
  }
  
  # --- Back-impute hosp date (from outcome) ---
  if (is.na(clean_linelist$`hosp date`[i]) && !is.na(clean_linelist$`date_of_outcome`[i])) {
    # hospitalization = outcome - typical stay
    clean_linelist$`hosp date`[i] <- clean_linelist$`date_of_outcome`[i] - days(median_outcome)
  }
}


##=================Part 1 Section 4(Symptom Imputation)====================##

# Check temperature distribution by symptom status
clean_linelist %>%
  filter(!is.na(temp), !is.na(fever)) %>%
  group_by(fever) %>%
  summarise(
    n = n(),
    mean_temp = mean(temp, na.rm = TRUE),
    median_temp = median(temp, na.rm = TRUE),
    q25 = quantile(temp, 0.25),
    q75 = quantile(temp, 0.75),
    min_temp = min(temp),
    max_temp = max(temp)
  )

# Boxplot: Temperature distribution by symptom status
ggplot(clean_linelist %>% filter(!is.na(temp)), aes(x = fever, y = temp, fill = fever)) +
  geom_boxplot() +
  labs(title = "Temperature Distribution by Fever Status",
       x = "Fever Recorded", y = "Temperature (°C)") +
  theme_minimal()


thresholds <- clean_linelist %>%
  select(temp, fever, chills, cough, aches, vomit) %>%
  pivot_longer(cols = -temp, names_to = "symptom", values_to = "status") %>%
  filter(!is.na(temp), !is.na(status), status %in% c("yes", "no")) %>%
  group_by(symptom, status) %>%
  summarise(
    median_temp = median(temp, na.rm = TRUE),
    mean_temp = mean(temp, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = status, values_from = c(median_temp, mean_temp, n))

print(thresholds)


impute_symptom <- function(temp, symptom) {
  
  # Handle missing temperature
  if (is.na(temp)) {
    return(NA_character_)
  }
  
  # Set threshold based on symptom type
  threshold <- switch(symptom,
                      "fever"  = 38.0,   # Clinical fever threshold
                      "chills" = 38.0,   # Rigors accompany fever
                      "cough"  = 37.5,   # Slight association, lower threshold
                      "aches"  = 37.5,   # Slight association, lower threshold
                      "vomit"  = 37.5,   # Slight association, lower threshold
                      stop("Unknown symptom: ", symptom)
  )
  
  # Return yes/no based on threshold
  if (temp >= threshold) {
    return("yes")
  } else {
    return("no")
  }
}

##=================Part 1 Section 5(Visualization)===================##

#1. Ensure dates are properly formatted
clean_linelist <- clean_linelist %>%
  mutate(
    `date onset` = as.Date(`date onset`),
    `hosp date` = as.Date(`hosp date`)
  )

# Aggregate hospitalisations by week/month
hosp_trends <- clean_linelist %>%
  filter(!is.na(`hosp date`)) %>%
  mutate(
    year_month = floor_date(`hosp date`, "month"),
    year_week = floor_date(`hosp date`, "week")
  ) %>%
  group_by(year_month) %>%
  summarise(cases = n(), .groups = "drop")

# Plot
ggplot(hosp_trends, aes(x = year_month, y = cases)) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  geom_line(color = "darkred", linewidth = 1) +
  geom_point(color = "darkred", size = 2) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  labs(
    title = "Hospitalisation Cases Over Time",
    subtitle = "Monthly aggregation of admissions",
    x = "Month",
    y = "Number of Hospitalisations",
    caption = "Source: Clean linelist data"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save
ggsave("hospitalisation_trends.png", width = 10, height = 6, dpi = 300)

# 2. 

# 3. Count cases by hospital
hospital_cases <- clean_linelist %>%
  filter(!is.na(clean_hospital)) %>%
  count(clean_hospital, sort = TRUE) %>%
  mutate(
    clean_hospital = fct_reorder(clean_hospital, n)  # Order by count
  )

# Horizontal bar plot
ggplot(hospital_cases, aes(x = n, y = clean_hospital, fill = n)) +
  geom_col(show.legend = FALSE) +
  scale_fill_gradient(low = "lightblue", high = "darkred") +
  geom_text(aes(label = n), hjust = -0.3, size = 3.5) +
  labs(
    title = "Total Cases by Hospital",
    subtitle = "After standardising hospital names",
    x = "Number of Cases",
    y = NULL,
    caption = "Source: Cleaned linelist data"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10)) +
  xlim(0, max(hospital_cases$n) * 1.15)  # Make room for labels

ggsave("cases_by_hospital.png", width = 10, height = 7, dpi = 300)


##==Part 2 =====##
##Geospatial Visualisation

shapefile <- st_read("./nga_admin_boundaries.shp.zip")

immun_data <- read.csv("./immunization_subnational_nga.csv")

# Inspect both datasets
head(shapefile)
head(immun_data)
names(shapefile)
names(immun_data) 













