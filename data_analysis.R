#Part 1 Section 1 Assignment

#Load Libraries
library(readxl)
library(dplyr)
library(stringr)
library(lubridate)
library(skimr) 


#import the line list
linelist <- read_excel("./linelist_raw.xlsx")

#confirm Line list dimension
dim(linelist)

#confirm Line list column and data types types
str(linelist)

#summarize Line list
summary(linelist)


#Part 1 Section 2 (Data Cleaning)
#string

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



#Types & ranges

str(clean_linelist) # checking the datatypes
#The datatypes appearing for "date onset" and age variables were both wrong and have been corrected below.
#More so, factors have been used for categorical variables while integers for generation, 
#and row_num just incase they tend to increase


# #Quick Summaries
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





