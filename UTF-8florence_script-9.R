############################################################################################

#### Setup

#load libraries
library(tidyverse)
library(tidyr)
library(aurum)
rm(list = ls())

# set up aurum (package created to use mySQL through R)
cprd = CPRDData$new(cprdEnv = "diabetes-jun2024", cprdConf = "~/Downloads/.aurum.yaml")
#codesets = cprd$codesets()
#codes = codesets$getAllCodeSetVersion(v = "01/06/2024")

############################################################################################

#### Set up all_diabetes_cohort table

#set up table
analysis = cprd$analysis("all")
diabetes_cohort <- diabetes_cohort %>% analysis$cached("diabetes_cohort")
bmi <- bmi %>% analysis$cached("patid_clean_bmi_medcodes")

#Data dictionary is here: https://github.com/Exeter-Diabetes/CPRD-Cohort-scripts/tree/main (scroll down)

#obtain count for all diabetes with HES and diabetes diagnosis date available
diabetes_cohort %>% filter(!is.na(dm_diag_date_all) & with_hes==1) %>% count()
#1525118

############################################################################################

#### Create diabetes_transplant_cohort table

#set prefix for saving tables
analysis = cprd$analysis("florence")

#set up a table and filter all operation codes which begin with 'M01' (kidney transplant)
kidney_transplant_opcs <- cprd$tables$hesProceduresEpi %>% filter(substr(OPCS, 1, 3)=="M01") %>% analysis$cached("kidney_transplant_opcs", indexes="patid")

#see kidney_transplant OPC list used to select patients who have undergone kidney transplant
kidney_transplant_opcs %>% distinct(OPCS)

#join the two tables, filter out those without HES and diabetes diagnosis date available
diabetes_transplant_cohort <- diabetes_cohort %>% filter(!is.na(dm_diag_date_all) & with_hes==1) %>% inner_join(kidney_transplant_opcs, by="patid")
diabetes_transplant_cohort %>% count()
#5267

#obtain count for all diabetes with kidney tranplant, which have HES and diabetes diagnosis date available
diabetes_transplant_cohort %>% distinct(patid) %>% count()
#4891

#add a transplant date column to the diabetes_transplant_cohort table
diabetes_transplant_cohort <- diabetes_transplant_cohort %>%
  mutate(transplant_date = coalesce(evdate, epistart)) %>% #create new column which takes evdate or epistart as transplant date
  distinct(patid, transplant_date, .keep_all = TRUE) %>% #keep all columns for only unique pairs of patid and transplant_date
  analysis$cached("diabetes_transplant_cohort3", indexes = c("patid", "epistart"))
#obtain count for new diabetes_transplant_cohort table
diabetes_transplant_cohort %>% count()
#5217
#there are some patids which have more than one transplant date

#obtain count for unique patid in new diabetes_transplant_cohort table
diabetes_transplant_cohort %>% distinct(patid) %>% count()
#4891

############################################################################################

#### look at basic characteristics in diabetes_transplant_cohort table

#check column names for table with following code:
#colnames(diabetes_transplant_cohort)

### look at age at diagnosis
#obtain summary statistics for diabetes diagnosis age
diabetes_transplant_cohort %>%
  pull(dm_diag_age_all) %>%
  summary()
#there are cases where diabetes diagnosis age is 0

#see all data for those with diagnosis age of 0
zero_age_patids <- diabetes_transplant_cohort %>%
  filter(dm_diag_age_all == 0) %>%
  collect()
#there are 7 individuals with diagnosis age of 0

#set up table
analysis <- cprd$analysis("all")
diabetes_medcodes <- diabetes_medcodes %>% analysis$cached("patid_raw_diabetes_medcodes")
#set prefix for saving tables
analysis <- cprd$analysis("florence")

#see diagnosis dates, enter dates, and obsdates, for those with diagnosis age of 0
test <- diabetes_transplant_cohort %>%
  filter(dm_diag_age_all == 0) %>% #select those with diagnosis age of 0
  inner_join(diabetes_medcodes, by="patid") %>% #join these individuals with data from diabetes_medcodes data by patid
  filter(obsdate==dm_diag_date_all) %>% #only keep records where obdate is equal to diabetes diagnosis date
  select(patid, dm_diag_date_all, enterdate, obsdate) %>% #only keep relevant columns
  collect()
#1 individual has 3 duplicated rows, and 1 has same date for: dm_diag_date_all, enterdate and obsdate

#remove fully duplicated rows
test <- test %>% distinct()
#drop rows with the same date for: dm_diag_date_all, enterdate and obsdate
test <- test %>%
  filter(!(dm_diag_date_all == enterdate & enterdate == obsdate))


#create a flag column to indicate membership in zero_age_patids
patid_flag <- zero_age_patids %>%
  mutate(in_zero_age = TRUE) %>% #add a new column and make the value for each row true
  select(patid, in_zero_age) #select only patid and in_zero_age column
#alter dm_diag_age_all records using flag column
diabetes_transplant_cohort <- diabetes_transplant_cohort %>%
  left_join(patid_flag, by = "patid", copy = TRUE) %>% #join flag to diabetes_transplant_cohort
  mutate(
    in_zero_age = coalesce(in_zero_age, FALSE), #replace all NA values in in_zero_age with the value false
    dm_diag_age_all = if_else(coalesce(in_zero_age, FALSE), NA_real_, dm_diag_age_all)  #for rows where in_zero_age is true, set dm_diag_age_all to NA
  ) %>%
  analysis$cached("diabetes_transplant_cohort5")

#identify all patids that have same date for: dm_diag_date_all, enterdate and obsdate
patids_to_drop <- test %>%
  filter(dm_diag_date_all == enterdate & enterdate == obsdate) %>%
  select(patid)
#remove patids from diabetes_transplant_cohort using anti_join with patids_to_drop
diabetes_transplant_cohort <- diabetes_transplant_cohort %>%
  anti_join(patids_to_drop, by = "patid", copy = TRUE) %>%
  analysis$cached("diabetes_transplant_cohort6")


#obtain new count for unique patid in new diabetes_transplant_cohort table
diabetes_transplant_cohort %>% distinct(patid) %>% count()
#4890

#obtain new summary statistics for diabetes diagnosis age
diabetes_transplant_cohort %>%
  pull(dm_diag_age_all) %>%
  summary()


### look at age at transplant
#calculate date difference between evdate or epistart and dob, and record this in a new column
diabetes_transplant_cohort <- diabetes_transplant_cohort %>%
  mutate(age_at_transplant = datediff(if_else(is.na(evdate), epistart, evdate), dob) / 365.25)
#obtain summary statistics for age at transplant in diabetes_transplant_cohort table
diabetes_transplant_cohort %>%
  pull(age_at_transplant) %>%
  summary()


### look at sex (gender)
#obtain count of each gender in diabetes_transplant_cohort table
diabetes_transplant_cohort %>%
  group_by(gender) %>%
  summarise(unique_patients = n_distinct(patid))
#1=male, 2=female

### look at diabetes type (diabetes_type)
#obtain count of each diabetes type in diabetes_transplant_cohort table
diabetes_transplant_cohort %>%
  group_by(diabetes_type) %>%
  summarise(unique_patients = n_distinct(patid))

### look at ethnicity (ethnicity_5cat)
#obtain count of each ethnic category in diabetes_transplant_cohort table
diabetes_transplant_cohort %>%
  group_by(ethnicity_5cat) %>%
  summarise(unique_patients = n_distinct(patid))
#0=White, 1=South Asian, 2=Black, 3=Other, 4=Mixed

### look at deprivation (imd_decile/imd_quintile)
#collapse IMD scale (1-10) to 5 IMD quintiles, and record in new column
diabetes_transplant_cohort <- diabetes_transplant_cohort %>%
  mutate(
    imd_quintile = case_when(
      imd_decile %in% c(1, 2) ~ 1,
      imd_decile %in% c(3, 4) ~ 2,
      imd_decile %in% c(5, 6) ~ 3,
      imd_decile %in% c(7, 8) ~ 4,
      imd_decile %in% c(9, 10) ~ 5
      )
    )
#obtain count for each IMD quintile in diabetes_transplant_cohort table
diabetes_transplant_cohort %>%
  group_by(imd_quintile) %>%
  summarise(unique_patients = n_distinct(patid))

### look at BMI

##BMI at kidney transplant
#find BMI data closest to time of kidney transplant
diabetes_transplant_cohort_bmis <- diabetes_transplant_cohort %>%
  inner_join(bmi, by = "patid") %>% #join diabetes_transplant_cohort with all BMI data
  rename(bmi_date = date, bmi_value = testvalue) %>% #rename columns for clarity
  mutate(bmi_datediff = abs(datediff(transplant_date, bmi_date))) %>% #calculate absolute date difference between transplant date and BMI date
  group_by(patid, transplant_date) %>%
  filter(bmi_datediff == min(bmi_datediff)) %>% #keep only rows corresponding to closest BMI measurement
  slice_min(bmi_date, n = 1) %>% #keep only positive value (which is BMI measurement before transplant)
  ungroup() %>% #remove grouping
  select(patid, transplant_date, bmi_at_transplant_date = bmi_date, bmi_at_transplant_value = bmi_value) #keep only essential columns from BMI data and rename

#join closest BMI data with diabetes_transplant_cohort table
diabetes_transplant_cohort <- diabetes_transplant_cohort %>%
  left_join(diabetes_transplant_cohort_bmis, by = c("patid", "transplant_date")) %>%
  analysis$cached("diabetes_transplant_cohort_with_transplant_bmis7")

#obtain summary statistics for closest BMI to time of kidney transplant in updated diabetes_transplant_cohort table
diabetes_transplant_cohort %>%
  pull(bmi_at_transplant_value) %>%
  summary()

#check which patid has missing values
diabetes_transplant_cohort %>%
  filter(is.na(bmi_at_transplant_value)) %>%
  pull(patid)


##latest BMI
#find latest BMI data
latest_bmi <- diabetes_transplant_cohort %>%
  inner_join(bmi, by="patid") %>% #join diabetes_transplant_cohort with all BMI data
  rename(bmi_date = date, bmi_value = testvalue) %>% #rename columns for clarity
  group_by(patid, transplant_date) %>%
  mutate(max_bmi_date=max(bmi_date)) %>% #calculate most recent BMI date
  filter(bmi_date==max_bmi_date) %>% #keep only rows corresponding to most recent BMI measurement
  ungroup() %>% #remove grouping
  select(patid, transplant_date, latest_bmi_date = bmi_date, latest_bmi_value = bmi_value) #keep only essential columns from BMI data and rename

#join latest BMI data with diabetes_transplant_cohort table
diabetes_transplant_cohort <- diabetes_transplant_cohort %>%
  left_join(latest_bmi, by = c("patid", "transplant_date")) %>%
  analysis$cached("diabetes_transplant_cohort_with_latest_bmis8")
  
#obtain summary statistics for latest BMI in updated diabetes_transplant_cohort table
diabetes_transplant_cohort %>%
  pull(latest_bmi_value) %>%
  summary()

#check which patid has missing values
diabetes_transplant_cohort %>%
  filter(is.na(latest_bmi_value)) %>%
  pull(patid)


##BMI at diabetes diagnosis
#find BMI data closest to time of diagnosis
diabetes_diagnosis_bmis <- diabetes_transplant_cohort %>%
  inner_join(bmi, by = "patid") %>% #join diabetes_transplant_cohort with all BMI data
  rename(bmi_date = date, bmi_value = testvalue) %>% #rename columns for clarity
  mutate(bmi_datediff = abs(datediff(dm_diag_date_all, bmi_date))) %>% #calculate absolute date difference between diagnosis date and BMI date
  group_by(patid, dm_diag_date_all) %>%
  filter(bmi_datediff == min(bmi_datediff)) %>% #keep only rows corresponding to closest BMI measurement
  slice_min(bmi_date, n = 1) %>% #only keep positive values (which is BMI measurement before diagnosis)
  ungroup() %>% #remove grouping
  select(patid, dm_diag_date_all, bmi_at_diagnosis_date = bmi_date, bmi_at_diagnosis_value = bmi_value) #keep only essential columns from BMI data and rename

#join closest BMI data with diabetes_transplant_cohort table
diabetes_transplant_cohort <- diabetes_transplant_cohort %>%
  left_join(diabetes_diagnosis_bmis, by = c("patid", "dm_diag_date_all")) %>%
  analysis$cached("diabetes_transplant_cohort_with_diagnosis_bmis6")

#alter bmi_at_diagnosis_value records using flag column
diabetes_transplant_cohort <- diabetes_transplant_cohort %>%
  mutate(bmi_at_diagnosis_value = if_else(coalesce(in_zero_age, FALSE), NA_real_, bmi_at_diagnosis_value) #for rows where in_zero_age is true, set bmi_at_diagnosis_value to NA
  )

#obtain summary statistics for closest BMI to time of diabetes diagnosis in updated diabetes_transplant_cohort table
diabetes_transplant_cohort %>%
  pull(bmi_at_diagnosis_value) %>%
  summary()

#check which patid has missing values
diabetes_transplant_cohort %>%
  filter(is.na(bmi_at_diagnosis_value)) %>%
  pull(patid)

############################################################################################

#### remove other diabetes and clean outliers from data

#drop all patients with other diabetes type
diabetes_transplant_cohort <- diabetes_transplant_cohort %>%
  filter(!(diabetes_type == "other"))

#obtain count for unique patid in new diabetes_transplant_cohort table
diabetes_transplant_cohort %>% distinct(patid) %>% count()
#4711

#list all columns that require handling of extreme values for BMI
age_extr_cols <- c("dm_diag_age_all", "age_at_transplant")
bmi_extr_cols <- c("bmi_at_transplant_value", "bmi_at_diagnosis_value", "latest_bmi_value")
#calculate 1st and 99th percentiles required
percentiles <- diabetes_transplant_cohort %>%
  collect() %>%
  summarise(
    #2nd percentile for diagnosis age column
    dm_diag_age_all_p02 = quantile(dm_diag_age_all, 0.02, na.rm = TRUE),
    #1st percentile for transplant age column
    age_at_transplant_p01 = quantile(age_at_transplant, 0.01, na.rm = TRUE),
    #1st and 99th percentiles for all BMI columns  
    across(all_of(bmi_extr_cols), 
           list(p01 = ~ quantile(., 0.01, na.rm = TRUE),
                p99 = ~ quantile(., 0.99, na.rm = TRUE)))
  )
#select all extreme cases using pre-calculated percentiles
extreme_cases <- diabetes_transplant_cohort %>%
  collect() %>%
  filter(
    #1st percentile for age columns
    dm_diag_age_all <= percentiles$dm_diag_age_all_p02 |
      age_at_transplant <= percentiles$age_at_transplant_p01 |
      #1st and 99th percentiles for BMI columns
      bmi_at_transplant_value <= percentiles$bmi_at_transplant_value_p01 | bmi_at_transplant_value >= percentiles$bmi_at_transplant_value_p99 |
      bmi_at_diagnosis_value <= percentiles$bmi_at_diagnosis_value_p01 | bmi_at_diagnosis_value >= percentiles$bmi_at_diagnosis_value_p99 |
      latest_bmi_value <= percentiles$latest_bmi_value_p01 | latest_bmi_value >= percentiles$latest_bmi_value_p99
  )
#obtain count for unique patid in extreme_cases
extreme_cases %>% distinct(patid) %>% count()
#there are 337 extreme cases

#see data for extreme cases
ec <- extreme_cases %>% 
  select(patid, dm_diag_age_all, age_at_transplant, bmi_at_transplant_value, bmi_at_diagnosis_value, latest_bmi_value) %>%
  collect()
#remove all extreme values through anti-join
diabetes_transplant_cohort <- diabetes_transplant_cohort %>%
  anti_join(ec, by = "patid", copy = TRUE) %>%
  analysis$cached("diabetes_transplant_cohort_without_extrm_cases2")

#obtain count for unique patid in new diabetes_transplant_cohort table
diabetes_transplant_cohort %>% distinct(patid) %>% count()
#4374

################################################################################

#### look at new basic characteristics in diabetes_transplant_cohort table

### look at sex (gender)
#obtain count of each gender in diabetes_transplant_cohort table
diabetes_transplant_cohort %>%
  group_by(gender) %>%
  summarise(unique_patients = n_distinct(patid))
#1=male, 2=female

### look at diabetes type (diabetes_type)
#obtain count of each diabetes type in diabetes_transplant_cohort table
diabetes_transplant_cohort %>%
  group_by(diabetes_type) %>%
  summarise(unique_patients = n_distinct(patid))

### look at ethnicity (ethnicity_5cat)
#obtain count of each ethnic category in diabetes_transplant_cohort table
diabetes_transplant_cohort %>%
  group_by(ethnicity_5cat) %>%
  summarise(unique_patients = n_distinct(patid))
#0=White, 1=South Asian, 2=Black, 3=Other, 4=Mixed

### look at deprivation (imd_decile/imd_quintile)
#obtain count for each IMD quintile in diabetes_transplant_cohort table
diabetes_transplant_cohort %>%
  group_by(imd_quintile) %>%
  summarise(unique_patients = n_distinct(patid))


### look at age at diagnosis
#obtain summary statistics for diabetes diagnosis age
diabetes_transplant_cohort %>%
  pull(dm_diag_age_all) %>%
  summary()

### look at age at transplant
#obtain summary statistics for age at transplant in diabetes_transplant_cohort table
diabetes_transplant_cohort %>%
  pull(age_at_transplant) %>%
  summary()


### look at BMI

##BMI at kidney transplant
#obtain summary statistics for closest BMI to time of kidney transplant in updated diabetes_transplant_cohort table
diabetes_transplant_cohort %>%
  pull(bmi_at_transplant_value) %>%
  summary()


##latest BMI
#obtain summary statistics for latest BMI in updated diabetes_transplant_cohort table
diabetes_transplant_cohort %>%
  pull(latest_bmi_value) %>%
  summary()


##BMI at diabetes diagnosis
#obtain summary statistics for closest BMI to time of diabetes diagnosis in updated diabetes_transplant_cohort table
diabetes_transplant_cohort %>%
  pull(bmi_at_diagnosis_value) %>%
  summary()

################################################################################

#### look at basic characteristics split by diabetes type 1 and 2

### look at sex (gender)
#obtain count of each gender in diabetes_transplant_cohort table
diabetes_transplant_cohort %>%
  group_by(diabetes_type, gender) %>%
  summarise(unique_patients = n_distinct(patid))
#1=male, 2=female

### look at ethnicity (ethnicity_5cat)
#obtain count of each ethnic category in diabetes_transplant_cohort table
diabetes_transplant_cohort %>%
  group_by(diabetes_type, ethnicity_5cat) %>%
  summarise(unique_patients = n_distinct(patid))
#0=White, 1=South Asian, 2=Black, 3=Other, 4=Mixed

### look at deprivation (imd_quintile)
#obtain count of each IMD quintile in diabetes_transplant_cohort table
diabetes_transplant_cohort %>%
  group_by(diabetes_type, imd_quintile) %>%
  summarise(unique_patients = n_distinct(patid)) %>%
  print(n=12)

### look at age at diagnosis
#obtain new summary statistics for diabetes diagnosis age
diabetes_transplant_cohort %>%
  select(diabetes_type, dm_diag_age_all) %>% #only select data for diabetes type and diabetes diagnosis age
  collect() %>% #bring data from MySQL database as data frame in R
  group_by(diabetes_type) %>% #split by diabetes type
  summarise(
    min = min(dm_diag_age_all, na.rm = TRUE), #minimum
    q1 = quantile(dm_diag_age_all, 0.25, na.rm = TRUE), #lower quantile
    median = median(dm_diag_age_all, na.rm = TRUE), #median
    mean = mean(dm_diag_age_all, na.rm = TRUE), #mean
    q3 = quantile(dm_diag_age_all, 0.75, na.rm = TRUE), #upper quantile
    max = max(dm_diag_age_all, na.rm = TRUE), #maximum
    na_count = sum(is.na(dm_diag_age_all)) #number of NAs
  )

### look at age at transplant
#obtain summary statistics for age at transplant in diabetes_transplant_cohort table
diabetes_transplant_cohort %>%
  select(diabetes_type, age_at_transplant) %>% #only select data for diabetes type and tranplant age
  collect() %>% #bring data from MySQL database as data frame in R
  group_by(diabetes_type) %>% #split by diabetes type
  summarise(
    min = min(age_at_transplant, na.rm = TRUE), #minimum
    q1 = quantile(age_at_transplant, 0.25, na.rm = TRUE), #lower quantile
    median = median(age_at_transplant, na.rm = TRUE), #median
    mean = mean(age_at_transplant, na.rm = TRUE), #mean
    q3 = quantile(age_at_transplant, 0.75, na.rm = TRUE), #upper quantile
    max = max(age_at_transplant, na.rm = TRUE), #maximum
    na_count = sum(is.na(age_at_transplant)) #number of NAs
  )

### look at BMI

##BMI at kidney transplant
#obtain summary statistics for closest BMI to time of kidney transplant in updated diabetes_transplant_cohort table
diabetes_transplant_cohort %>%
  select(diabetes_type, bmi_at_transplant_value) %>% #only select data for diabetes type and BMI at transplant
  collect() %>% #bring data from MySQL database as data frame in R
  group_by(diabetes_type) %>% #split by diabetes type
  summarise(
    min = min(bmi_at_transplant_value, na.rm = TRUE), #minimum
    q1 = quantile(bmi_at_transplant_value, 0.25, na.rm = TRUE), #lower quantile
    median = median(bmi_at_transplant_value, na.rm = TRUE), #median
    mean = mean(bmi_at_transplant_value, na.rm = TRUE), #mean
    q3 = quantile(bmi_at_transplant_value, 0.75, na.rm = TRUE), #upper quantile
    max = max(bmi_at_transplant_value, na.rm = TRUE), #maximum
    na_count = sum(is.na(bmi_at_transplant_value)) #number of NAs
  )

##latest BMI
#obtain summary statistics for latest BMI in updated diabetes_transplant_cohort table
diabetes_transplant_cohort %>%
  select(diabetes_type, latest_bmi_value) %>% #only select data for diabetes type and latest BMI
  collect() %>% #bring data from MySQL database as data frame in R
  group_by(diabetes_type) %>% #split by diabetes type
  summarise(
    min = min(latest_bmi_value, na.rm = TRUE), #minimum
    q1 = quantile(latest_bmi_value, 0.25, na.rm = TRUE), #lower quantile
    median = median(latest_bmi_value, na.rm = TRUE), #median
    mean = mean(latest_bmi_value, na.rm = TRUE), #mean
    q3 = quantile(latest_bmi_value, 0.75, na.rm = TRUE), #upper quantile
    max = max(latest_bmi_value, na.rm = TRUE), #maximum
    na_count = sum(is.na(latest_bmi_value)) #number of NAs
  )

##BMI at diabetes diagnosis
#obtain summary statistics for closest BMI to time of diabetes diagnosis in updated diabetes_transplant_cohort table
diabetes_transplant_cohort %>%
  select(diabetes_type, bmi_at_diagnosis_value) %>% #only select data for diabetes type and BMI at diagnosis
  collect() %>% #bring data from MySQL database as data frame in R
  group_by(diabetes_type) %>% #split by diabetes type
  summarise(
    min = min(bmi_at_diagnosis_value, na.rm = TRUE), #minimum
    q1 = quantile(bmi_at_diagnosis_value, 0.25, na.rm = TRUE), #lower quantile
    median = median(bmi_at_diagnosis_value, na.rm = TRUE), #median
    mean = mean(bmi_at_diagnosis_value, na.rm = TRUE), #mean
    q3 = quantile(bmi_at_diagnosis_value, 0.75, na.rm = TRUE), #upper quantile
    max = max(bmi_at_diagnosis_value, na.rm = TRUE), #maximum
    na_count = sum(is.na(bmi_at_diagnosis_value)) #number of NAs
  )

############################################################################################

#### determine diabetes developing before and after transplant

#determine diagnosis-transplant timing for individuals in diabetes_transplant_cohort
diabetes_transplant_cohort <- diabetes_transplant_cohort %>%
  left_join(test %>% select(patid, enterdate), by = "patid", copy = TRUE) %>% #join enterdate from test with diabetes_transplant_cohort by patid
  mutate(
    effective_diag_date = coalesce(enterdate, dm_diag_date_all), #create new column which takes enterdate if present, otherwise takes dm_diag_date_all
    diag_transplant_timing = if_else( #create a new column to record diagnosis-transplant timing
      effective_diag_date < transplant_date, 1, #if diagnosis is before transplant record as 1
      if_else(effective_diag_date == transplant_date, 3, #if diagnosis is on same day as transplant record as 3
              2)))  #if diagnosis is after transplant record as 2

#obtain count for diabetes developing before and after in diabetes_transplant_cohort table
diabetes_transplant_cohort %>%
  group_by(diag_transplant_timing) %>%
  summarise(unique_patients = n_distinct(patid))
#1=diagnosis before transplant, 2=diagnosis after transplant, 3=diagnosis and transplant on same day

#see data for individuals who have diagnosis and transplant on same day
diag_transplant_same_day <- diabetes_transplant_cohort %>%
  filter(dm_diag_date_all == transplant_date) %>%
  collect()

### look at diabetes type (diabetes_type)
#obtain count of each diabetes type in diabetes_transplant_cohort table
diabetes_transplant_cohort %>%
  group_by(diag_transplant_timing, diabetes_type) %>%
  summarise(unique_patients = n_distinct(patid))
#type 1 diabetes post-transplant numbers are a lot higher than expected

############################################################################################

#look into patients with post-transplant type 1 diabetes

#select and see data for patients with post-transplant type 1 diabetes
post_trans_t1d <- diabetes_transplant_cohort %>% 
  filter(diabetes_type == 'type 1' & diag_transplant_timing == 2)

#check count for patients with post-transplant type 1 diabetes
post_trans_t1d %>% distinct(patid) %>% count()
#37

### look at sex (gender)
#obtain count of each gender
post_trans_t1d %>%
  group_by(gender) %>%
  summarise(unique_patients = n_distinct(patid))
#1=male, 2=female

### look at number of patients with prescription for insulin within last 6 months (current_oha)
#obtain count of patients with/without prescription for insulin within last 6 months
post_trans_t1d %>%
  group_by(current_oha) %>%
  summarise(unique_patients = n_distinct(patid))
#0=no, 1=yes

### look at ethnicity (ethnicity_5cat)
#obtain count of each ethnic category
post_trans_t1d %>%
  group_by(ethnicity_5cat) %>%
  summarise(unique_patients = n_distinct(patid))
#0=White, 1=South Asian, 2=Black, 3=Other, 4=Mixed

### look at deprivation (imd_quintile)
#obtain count for each IMD quintile
post_trans_t1d %>%
  group_by(imd_quintile) %>%
  summarise(unique_patients = n_distinct(patid))


### look at age at diagnosis
#obtain summary statistics for diabetes diagnosis age
post_trans_t1d %>%
  pull(dm_diag_age_all) %>%
  summary()

### look at age at transplant
#obtain summary statistics for age at transplant
post_trans_t1d %>%
  pull(age_at_transplant) %>%
  summary()


### look at BMI

##BMI at kidney transplant
#obtain summary statistics for closest BMI to time of kidney transplant
post_trans_t1d %>%
  pull(bmi_at_transplant_value) %>%
  summary()

##latest BMI
#obtain summary statistics for latest BMI
post_trans_t1d %>%
  pull(latest_bmi_value) %>%
  summary()

##BMI at diabetes diagnosis
#obtain summary statistics for closest BMI to time of diabetes diagnosis
post_trans_t1d %>%
  pull(bmi_at_diagnosis_value) %>%
  summary()

#see data for 37 individuals
pt_t1d <- post_trans_t1d %>% collect()

################################################################################

####data cleaning

#alter post-transplant t1d patients who have not been on insulin for past 6 months to t2d
patids_clean <- post_trans_t1d %>%
  filter(current_oha == 0) %>%
  mutate(diabetes_type = "type 2") %>%
  collect()
#select all post-transplant t1d patients who have been on insulin for past 6 months
patids_on_insulin <- post_trans_t1d %>%
  filter(current_oha == 1) %>%
  collect()
#update the diabetes_transplant_cohort with changes for post-transplant t1d patients
diabetes_transplant_cohort <- diabetes_transplant_cohort %>%
  left_join(patids_clean %>% select(patid, diabetes_type) %>%  #left join to alter diabetes types for those have not been on insulin
              rename(diabetes_type_new = diabetes_type),  #rename column
            by = "patid", copy = TRUE) %>%
  mutate(diabetes_type = coalesce(diabetes_type_new, diabetes_type)) %>%  #use new diabetes type if available, otherwise use original diabetes type
  select(-diabetes_type_new) %>%  #remove extra columns after update
  distinct() %>% #remove rows which are exact duplicates
  analysis$cached("diabetes_transplant_cohort13")

#find all patids which are recorded as having both diagnosis pre and post transplant
overlap_patids <- diabetes_transplant_cohort %>%
  filter(diag_transplant_timing == 2) %>%  #find all patients diagnosed post-transplant
  inner_join(diabetes_transplant_cohort %>%  #inner join with diabetes-transplant cohort
               filter(diag_transplant_timing == 1), by = "patid") %>%  #find all patients diagnosed pre-transplant
  distinct(patid) %>%  #only select distinct patids
  mutate(in_overlap = TRUE) %>%  #add a flag column to indicate which patids overlap
  select(patid, in_overlap)  #only keep neccessary columns

#count number of overlapping patids
overlap_patids %>% count()

#update diag_transplant_timing column in diabetes_transplant_cohort using the flag
diabetes_transplant_cohort <- diabetes_transplant_cohort %>%
  left_join(overlap_patids, by = "patid", copy = TRUE) %>%  #join overlap_patids to diabetes_transplant_cohort
  mutate(
    in_overlap = coalesce(in_overlap, FALSE),  ##replace all NA values in in_overlap with the value false
    diag_transplant_timing = if_else(in_overlap, 2, diag_transplant_timing)  #for rows where in_overlap is true, set diag_transplant_timing to 2
    ) 

#exclude post-transplant t1d patients who have been on insulin for past 6 months
diabetes_transplant_cohort2 <- diabetes_transplant_cohort %>%
  anti_join(patids_on_insulin, by = "patid", copy = TRUE) %>%
  analysis$cached("diabetes_transplant_without_t1dposttrans3")

#obtain count for unique patid in diabetes_transplant_cohort2 table
diabetes_transplant_cohort2 %>% distinct(patid) %>% count()
#4370

################################################################################

#### look at basic characteristics split by diabetes developing before and after transplant

#obtain count of each diabetes-transplant timing in diabetes_transplant_cohort table
diabetes_transplant_cohort %>%
  group_by(diag_transplant_timing) %>%
  summarise(unique_patients = n_distinct(patid))

### look at sex (gender)
#obtain count of each gender in diabetes_transplant_cohort table
diabetes_transplant_cohort %>%
  group_by(diag_transplant_timing, gender) %>%
  summarise(unique_patients = n_distinct(patid))
#1=male, 2=female

### look at diabetes type (diabetes_type)
#obtain count of each diabetes type in diabetes_transplant_cohort table
diabetes_transplant_cohort %>%
  group_by(diag_transplant_timing, diabetes_type) %>%
  summarise(unique_patients = n_distinct(patid))

### look at ethnicity (ethnicity_5cat)
#obtain count of each ethnic category in diabetes_transplant_cohort table
diabetes_transplant_cohort %>%
  group_by(diag_transplant_timing, ethnicity_5cat) %>%
  summarise(unique_patients = n_distinct(patid))
#0=White, 1=South Asian, 2=Black, 3=Other, 4=Mixed

### look at deprivation (imd_quintile)
#obtain count for each IMD quintile in diabetes_transplant_cohort table
diabetes_transplant_cohort %>%
  group_by(diag_transplant_timing, imd_quintile) %>%
  summarise(unique_patients = n_distinct(patid)) %>%
  print(n=17)


### look at age at diagnosis
#obtain new summary statistics for diabetes diagnosis age
diabetes_transplant_cohort %>%
  select(diag_transplant_timing, dm_diag_age_all) %>% #only select data for diabetes-transplant timing and diabetes diagnosis age
  collect() %>% #bring data from MySQL database as data frame in R
  group_by(diag_transplant_timing) %>% #split by diabetes-transplant timing
  summarise(
    min = min(dm_diag_age_all, na.rm = TRUE), #minimum
    q1 = quantile(dm_diag_age_all, 0.25, na.rm = TRUE), #lower quantile
    median = median(dm_diag_age_all, na.rm = TRUE), #median
    mean = mean(dm_diag_age_all, na.rm = TRUE), #mean
    q3 = quantile(dm_diag_age_all, 0.75, na.rm = TRUE), #upper quantile
    max = max(dm_diag_age_all, na.rm = TRUE), #maximum
    na_count = sum(is.na(dm_diag_age_all)) #number of NAs
  )

### look at age at transplant
#obtain summary statistics for age at transplant in diabetes_transplant_cohort table
diabetes_transplant_cohort %>%
  select(diag_transplant_timing, age_at_transplant) %>% #only select data for diabetes-transplant timing and tranplant age
  collect() %>% #bring data from MySQL database as data frame in R
  group_by(diag_transplant_timing) %>% #split by diabetes-transplant timing
  summarise(
    min = min(age_at_transplant, na.rm = TRUE), #minimum
    q1 = quantile(age_at_transplant, 0.25, na.rm = TRUE), #lower quantile
    median = median(age_at_transplant, na.rm = TRUE), #median
    mean = mean(age_at_transplant, na.rm = TRUE), #mean
    q3 = quantile(age_at_transplant, 0.75, na.rm = TRUE), #upper quantile
    max = max(age_at_transplant, na.rm = TRUE), #maximum
    na_count = sum(is.na(age_at_transplant)) #number of NAs
  )


### look at BMI

##BMI at kidney transplant
#obtain summary statistics for closest BMI to time of kidney transplant in updated diabetes_transplant_cohort table
diabetes_transplant_cohort %>%
  select(diag_transplant_timing, bmi_at_transplant_value) %>% #only select data for diabetes-transplant timing and BMI at transplant
  collect() %>% #bring data from MySQL database as data frame in R
  group_by(diag_transplant_timing) %>% #split by diabetes-transplant timing
  summarise(
    min = min(bmi_at_transplant_value, na.rm = TRUE), #minimum
    q1 = quantile(bmi_at_transplant_value, 0.25, na.rm = TRUE), #lower quantile
    median = median(bmi_at_transplant_value, na.rm = TRUE), #median
    mean = mean(bmi_at_transplant_value, na.rm = TRUE), #mean
    q3 = quantile(bmi_at_transplant_value, 0.75, na.rm = TRUE), #upper quantile
    max = max(bmi_at_transplant_value, na.rm = TRUE), #maximum
    na_count = sum(is.na(bmi_at_transplant_value)) #number of NAs
  )

##latest BMI
#obtain summary statistics for latest BMI in updated diabetes_transplant_cohort table
diabetes_transplant_cohort %>%
  select(diag_transplant_timing, latest_bmi_value) %>% #only select data for diabetes-transplant timing and latest BMI
  collect() %>% #bring data from MySQL database as data frame in R
  group_by(diag_transplant_timing) %>% #split by diabetes-transplant timing
  summarise(
    min = min(latest_bmi_value, na.rm = TRUE), #minimum
    q1 = quantile(latest_bmi_value, 0.25, na.rm = TRUE), #lower quantile
    median = median(latest_bmi_value, na.rm = TRUE), #median
    mean = mean(latest_bmi_value, na.rm = TRUE), #mean
    q3 = quantile(latest_bmi_value, 0.75, na.rm = TRUE), #upper quantile
    max = max(latest_bmi_value, na.rm = TRUE), #maximum
    na_count = sum(is.na(latest_bmi_value)) #number of NAs
  )

##BMI at diabetes diagnosis
#obtain summary statistics for closest BMI to time of diabetes diagnosis in updated diabetes_transplant_cohort table
diabetes_transplant_cohort %>%
  select(diag_transplant_timing, bmi_at_diagnosis_value) %>% #only select data for diabetes-transplant timing and BMI at diagnosis
  collect() %>% #bring data from MySQL database as data frame in R
  group_by(diag_transplant_timing) %>% #split by diabetes-transplant timing
  summarise(
    min = min(bmi_at_diagnosis_value, na.rm = TRUE), #minimum
    q1 = quantile(bmi_at_diagnosis_value, 0.25, na.rm = TRUE), #lower quantile
    median = median(bmi_at_diagnosis_value, na.rm = TRUE), #median
    mean = mean(bmi_at_diagnosis_value, na.rm = TRUE), #mean
    q3 = quantile(bmi_at_diagnosis_value, 0.75, na.rm = TRUE), #upper quantile
    max = max(bmi_at_diagnosis_value, na.rm = TRUE), #maximum
    na_count = sum(is.na(bmi_at_diagnosis_value)) #number of NAs
  )

################################################################################

#### add in treatment data and death status for diabetes_transplant_cohort table

#set up tables
analysis = cprd$analysis("mm")
drugd <- diabetes_cohort %>% analysis$cached("20250602_all_1stinstance_interim_5")
analysis = cprd$analysis("all_patid")
clean_hba1c <- clean_hba1c %>% analysis$cached("clean_hba1c_medcodes")

#set prefix for saving tables
analysis = cprd$analysis("florence")

##look at latest treatment
#make 2024-01-01 the index date
index_date <- as.Date("2024-01-01")
#find difference between transplant_date and index date
cohort_drugs <- drugd %>%
  inner_join(diabetes_transplant_cohort2 %>% select(patid), by = "patid") %>%  #keep only patids in diabetes-transplant cohort
  group_by(patid, drug_class) %>%  #group data by patid and drug class
  mutate(drug_datediff = datediff(index_date, dstartdate)) %>%  #calculate date difference
  analysis$cached("diabetes_transplant_cohort_drugs3")
#find data for latest drug date per patient
latest_drugs <- cohort_drugs %>%
  group_by(patid) %>%
  filter(drug_datediff == min(drug_datediff)) %>%  #select row with date difference farthest to 0
  ungroup()

#calculate length of latest treatment in days
latest_drugs <- latest_drugs %>%
  mutate(latest_drug_length = datediff(dstopdate_class, dstartdate)) %>%  #calculate length of latest treatment in days
  select(patid, dstartdate, drug_class, drug_substance, drug_datediff, dstopdate_class, dstopdate_substance, latest_drug_length, prehba1c, prehba1cdate)  #keep only essential columns
#dstartdate in latest_drugs should be date of latest treatment started
#prehba1c=Hba1c level closest to latest treatment in 6 month window

#obtain summary statistics for diabetes diagnosis age
latest_drugs %>%
  pull(latest_drug_length) %>%
  summary()
#there are cases where treatment duration is 0 days, meaning treatment is started and ended on same date

#see the data for those with a treatment duration of 0
zero_drug_length <- latest_drugs %>%
  filter(latest_drug_length == 0) %>%
  distinct() %>%
  collect()
#remove rows with treatment duration of 0
latest_drugs <- latest_drugs %>%
  anti_join(zero_drug_length, by = c("patid", "dstartdate"), copy = TRUE) %>%
  analysis$cached("diabetes_transplant_cohort_drugs4")
#count amount of patient ids with latest drug data available
latest_drugs %>% distinct(patid) %>% count() #2543
#see data for latest treatments
ld <- latest_drugs %>% collect()


##Hba1c 6 months to latest treatment
#find all Hba1c records within 6 months to latest treatment
latest_hba1c_6m <- latest_drugs %>%
  inner_join(clean_hba1c, by = "patid", copy = TRUE) %>%  #join latest_drugs with all hba1c data
  rename(hba1c_date = date, hba1c_value = testvalue) %>%  #rename columns for clarity
  filter(hba1c_date >= dstartdate - 183, hba1c_date <= dstartdate) %>%  #select all hba1c records 6 months before dstartdate
  select(patid, dstartdate, hba1c_date, hba1c_value)  #keep only essential columns from hba1c data
#count amount of patient ids with hba1c records within 6 months to latest treatment available
latest_hba1c_6m %>% distinct(patid) %>% count() #800
#join Hba1c records within 6 months to latest treatment with latest_drugs
latest_drugs <- latest_drugs %>%
  left_join(latest_hba1c_6m, by = c("patid", "dstartdate")) %>%
  analysis$cached("diabetes_transplant_latest_drugs_with_lt_hba1c_6m5")


##latest Hba1c
#find latest Hba1c records for cohort
latest_hba1c <- latest_drugs %>% 
  inner_join(clean_hba1c, by = "patid", copy = TRUE) %>%  #join latest_drugs with all hba1c data
  rename(hba1c_dates = date, hba1c_values = testvalue) %>%  #rename columns for clarity
  group_by(patid) %>% 
  slice_max(order_by = hba1c_dates, n = 1, with_ties = FALSE) %>%  #keep only rows corresponding to most recent hba1c measurement
  ungroup() %>%  #remove grouping
  select(patid, dstartdate, latest_hba1c_date = hba1c_dates, latest_hba1c_value = hba1c_values)  #keep only essential columns from hba1c data and rename
#join latest hba1c data with latest_drugs
latest_drugs <- latest_drugs %>%
  left_join(latest_hba1c, by = c("patid", "dstartdate")) %>%
  analysis$cached("diabetes_transplant_latest_drugs_with_latest_hba1c7")
#ensure there are no duplicate rows
latest_drugs <- latest_drugs %>% distinct()

#join latest_drugs with diabetes_transplant_cohort
diabetes_transplant_cohort2 <- diabetes_transplant_cohort2 %>%
  left_join(latest_drugs, by = "patid") %>%
  analysis$cached("diabetes_transplant_with_latest_treatments6")

#determine whether patients are dead
diabetes_transplant_cohort2 <- diabetes_transplant_cohort2 %>%
  mutate(death_status = if_else(!is.na(death_date), TRUE, FALSE)) %>%  #if death date is present record as true, otherwise record as false
  analysis$cached("diabetes_transplant_with_death_status")
#0=alive, 1=deceased

################################################################################

####look at characteristics by treatment class and create plot visualisations

#load libraries
library(gridExtra)

#obtain count of each drug class in diabetes_transplant_cohort table
drug_class_count <- diabetes_transplant_cohort2 %>%
  group_by(drug_class) %>%
  summarise(unique_patients = n_distinct(patid)) %>%
  collect()

### look at sex (gender)
#obtain count of each gender in diabetes_transplant_cohort table
gender_count <- diabetes_transplant_cohort2 %>%
  filter(!is.na(drug_class)) %>%  #remove rows where drug_class is NA
  group_by(drug_class, gender) %>%
  summarise(unique_patients = n_distinct(patid)) %>%
  collect()
#1=male, 2=female
#ensure that gender is a factor
gender_count$gender <- factor(gender_count$gender, 
                              levels = c(1, 2), 
                              labels = c("Male", "Female"))

### look at diabetes type (diabetes_type)
#obtain count of each diabetes type in diabetes_transplant_cohort table
diabetes_types_count <- diabetes_transplant_cohort2 %>%
  filter(!is.na(drug_class)) %>%  #remove rows where drug_class is NA
  group_by(drug_class, diabetes_type) %>%
  summarise(unique_patients = n_distinct(patid)) %>%
  collect()

### look at ethnicity (ethnicity_5cat)
#obtain count of each ethnic category in diabetes_transplant_cohort table
ethnicity_count <- diabetes_transplant_cohort2 %>%
  filter(!is.na(drug_class)) %>%  #remove rows where drug_class is NA
  group_by(drug_class, ethnicity_5cat) %>%
  summarise(unique_patients = n_distinct(patid)) %>%
  collect()
#0=White, 1=South Asian, 2=Black, 3=Other, 4=Mixed
#ensure that ethnicity is a factor
ethnicity_count$ethnicity_5cat <- factor(ethnicity_count$ethnicity_5cat, 
                                         levels = c(0, 1, 2, 3, 4), 
                                         labels = c("White", "South_asian", "Black", "Other", "Mixed"))

### look at deprivation (imd_quintile)
#obtain count for each IMD quintile in diabetes_transplant_cohort table
deprivation_count <- diabetes_transplant_cohort2 %>%
  filter(!is.na(drug_class)) %>%  #remove rows where drug_class is NA
  group_by(drug_class, imd_quintile) %>%
  summarise(unique_patients = n_distinct(patid)) %>%
  collect()
#ensure that deprivation is a factor
deprivation_count$imd_quintile <- factor(deprivation_count$imd_quintile, 
                                         levels = c(1, 2, 3, 4, 5), 
                                         labels = c("1st_quintile", "2nd_quintile", "3rd_quintile", "4th_quintile", "5th_quintile"))

### look at death status
#obtain count for death in diabetes_transplant_cohort table
death_count <- diabetes_transplant_cohort2 %>%
  filter(!is.na(drug_class)) %>%  #remove rows where drug_class is NA
  group_by(drug_class, death_status) %>%
  summarise(unique_patients = n_distinct(patid)) %>%
  collect()
#ensure that death status is a factor
death_count$death_status <- factor(death_count$death_status, 
                                   levels = c(0, 1), 
                                   labels = c("Alive", "Deceased"))

### look at diabetes diagnosis-transplant timings
#obtain count for diagnosis-transplant timings in diabetes_transplant_cohort table
diag_trans_timings_count <- diabetes_transplant_cohort2 %>%
  filter(!is.na(drug_class)) %>%  #remove rows where drug_class is NA
  group_by(drug_class, diag_transplant_timing) %>%
  summarise(unique_patients = n_distinct(patid)) %>%
  collect()
#ensure that diabetes diagnosis-transplant timing is a factor
diag_trans_timings_count$diag_transplant_timing <- factor(diag_trans_timings_count$diag_transplant_timing, 
                                   levels = c(1, 2, 3), 
                                   labels = c("Diagnosis_before_transplant", "Diagnosis_after_transplant", "Same_day"))

### look at age at diagnosis
#obtain summary statistics for diabetes diagnosis age
diabetes_transplant_cohort2 %>%
  select(drug_class, dm_diag_age_all) %>% #only select data for drug class and diabetes diagnosis age
  collect() %>% #bring data from MySQL database as data frame in R
  group_by(drug_class) %>% #split by drug class
  summarise(
    min = min(dm_diag_age_all, na.rm = TRUE), #minimum
    q1 = quantile(dm_diag_age_all, 0.25, na.rm = TRUE), #lower quantile
    median = median(dm_diag_age_all, na.rm = TRUE), #median
    mean = mean(dm_diag_age_all, na.rm = TRUE), #mean
    q3 = quantile(dm_diag_age_all, 0.75, na.rm = TRUE), #upper quantile
    max = max(dm_diag_age_all, na.rm = TRUE), #maximum
    na_count = sum(is.na(dm_diag_age_all)) #number of NAs
  )

### look at age at transplant
#obtain summary statistics for age at transplant in diabetes_transplant_cohort table
diabetes_transplant_cohort2 %>%
  select(drug_class, age_at_transplant) %>% #only select data for drug class and transplant age
  collect() %>% #bring data from MySQL database as data frame in R
  group_by(drug_class) %>% #split by drug class
  summarise(
    min = min(age_at_transplant, na.rm = TRUE), #minimum
    q1 = quantile(age_at_transplant, 0.25, na.rm = TRUE), #lower quantile
    median = median(age_at_transplant, na.rm = TRUE), #median
    mean = mean(age_at_transplant, na.rm = TRUE), #mean
    q3 = quantile(age_at_transplant, 0.75, na.rm = TRUE), #upper quantile
    max = max(age_at_transplant, na.rm = TRUE), #maximum
    na_count = sum(is.na(age_at_transplant)) #number of NAs
  )


### look at BMI

##BMI at kidney transplant
#obtain summary statistics for closest BMI to time of kidney transplant in diabetes_transplant_cohort table
diabetes_transplant_cohort2 %>%
  select(drug_class, bmi_at_transplant_value) %>% #only select data for drug class and BMI at transplant
  collect() %>% #bring data from MySQL database as data frame in R
  group_by(drug_class) %>% #split by drug class
  summarise(
    min = min(bmi_at_transplant_value, na.rm = TRUE), #minimum
    q1 = quantile(bmi_at_transplant_value, 0.25, na.rm = TRUE), #lower quantile
    median = median(bmi_at_transplant_value, na.rm = TRUE), #median
    mean = mean(bmi_at_transplant_value, na.rm = TRUE), #mean
    q3 = quantile(bmi_at_transplant_value, 0.75, na.rm = TRUE), #upper quantile
    max = max(bmi_at_transplant_value, na.rm = TRUE), #maximum
    na_count = sum(is.na(bmi_at_transplant_value)) #number of NAs
  )

##latest BMI
#obtain summary statistics for latest BMI in diabetes_transplant_cohort table
diabetes_transplant_cohort2 %>%
  select(drug_class, latest_bmi_value) %>% #only select data for drug class and latest BMI
  collect() %>% #bring data from MySQL database as data frame in R
  group_by(drug_class) %>% #split by drug class
  summarise(
    min = min(latest_bmi_value, na.rm = TRUE), #minimum
    q1 = quantile(latest_bmi_value, 0.25, na.rm = TRUE), #lower quantile
    median = median(latest_bmi_value, na.rm = TRUE), #median
    mean = mean(latest_bmi_value, na.rm = TRUE), #mean
    q3 = quantile(latest_bmi_value, 0.75, na.rm = TRUE), #upper quantile
    max = max(latest_bmi_value, na.rm = TRUE), #maximum
    na_count = sum(is.na(latest_bmi_value)) #number of NAs
  )

##BMI at diabetes diagnosis
#obtain summary statistics for closest BMI to time of diabetes diagnosis in diabetes_transplant_cohort table
diabetes_transplant_cohort2 %>%
  select(drug_class, bmi_at_diagnosis_value) %>% #only select data for drug class and BMI at diagnosis
  collect() %>% #bring data from MySQL database as data frame in R
  group_by(drug_class) %>% #split by drug class
  summarise(
    min = min(bmi_at_diagnosis_value, na.rm = TRUE), #minimum
    q1 = quantile(bmi_at_diagnosis_value, 0.25, na.rm = TRUE), #lower quantile
    median = median(bmi_at_diagnosis_value, na.rm = TRUE), #median
    mean = mean(bmi_at_diagnosis_value, na.rm = TRUE), #mean
    q3 = quantile(bmi_at_diagnosis_value, 0.75, na.rm = TRUE), #upper quantile
    max = max(bmi_at_diagnosis_value, na.rm = TRUE), #maximum
    na_count = sum(is.na(bmi_at_diagnosis_value)) #number of NAs
  )


### look at length of latest treatment
#obtain summary statistics for latest treatment length
diabetes_transplant_cohort2 %>%
  select(drug_class, latest_drug_length) %>% #only select data for drug class and latest treatment length
  collect() %>% #bring data from MySQL database as data frame in R
  group_by(drug_class) %>% #split by drug class
  summarise(
    min = min(latest_drug_length, na.rm = FALSE), #minimum
    q1 = quantile(latest_drug_length, 0.25, na.rm = TRUE), #lower quantile
    median = median(latest_drug_length, na.rm = TRUE), #median
    mean = mean(latest_drug_length, na.rm = TRUE), #mean
    q3 = quantile(latest_drug_length, 0.75, na.rm = TRUE), #upper quantile
    max = max(latest_drug_length, na.rm = FALSE), #maximum
    na_count = sum(is.na(latest_drug_length)) #number of NAs
  )


### look at latest hba1c
#obtain summary statistics for latest hba1c
diabetes_transplant_cohort2 %>%
  select(drug_class, latest_hba1c_value) %>% #only select data for drug class and latest hba1c
  collect() %>% #bring data from MySQL database as data frame in R
  group_by(drug_class) %>% #split by drug class
  summarise(
    min = min(latest_hba1c_value, na.rm = TRUE), #minimum
    q1 = quantile(latest_hba1c_value, 0.25, na.rm = TRUE), #lower quantile
    median = median(latest_hba1c_value, na.rm = TRUE), #median
    mean = mean(latest_hba1c_value, na.rm = TRUE), #mean
    q3 = quantile(latest_hba1c_value, 0.75, na.rm = TRUE), #upper quantile
    max = max(latest_hba1c_value, na.rm = TRUE), #maximum
    na_count = sum(is.na(latest_hba1c_value)) #number of NAs
  )


##bar plots
#create bar plot for drug class counts
ggplot(drug_class_count, aes(
  x = drug_class,
  y = unique_patients,
  colour = drug_class,
  fill = drug_class
)) +
  geom_col(position = "dodge") +
  geom_text(
    aes(label = unique_patients),  #add count to bars
    vjust = -0.3,
    size = 3
  ) +
  labs(
    title = "Number of Patients Per Drug Class",
    x = "Drug Class",
    y = "Number of Patients"
  ) +
  theme_minimal()
#create a bar plot for gender counts
ggplot(gender_count, aes(
  x = drug_class,
  y = unique_patients,
  fill = gender,
  group = gender
)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_text(
    aes(label = unique_patients),  #add count to bars
    position = position_dodge(width = 0.9),
    vjust = -0.3,
    size = 3
  ) +
  labs(
    title = "Number of Patients by Drug Class and Gender",
    x = "Drug Class",
    y = "Number of Patients"
  ) +
  theme_minimal()
#create a bar plot for diabetes types counts
ggplot(diabetes_types_count, aes(
  x = drug_class,
  y = unique_patients,
  fill = diabetes_type,
  group = diabetes_type
)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_text(
    aes(label = unique_patients),  #add count to bars
    position = position_dodge(width = 0.9),
    vjust = -0.3,
    size = 3
  ) +
  labs(
    title = "Number of Patients by Drug Class and Diabetes Type",
    x = "Drug Class",
    y = "Number of Patients"
  ) +
  theme_minimal()
#create a bar plot for ethnicity counts
ggplot(ethnicity_count, aes(
  x = drug_class,
  y = unique_patients,
  fill = ethnicity_5cat,
  group = ethnicity_5cat
)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_text(
    aes(label = unique_patients),  #add count to bars
    position = position_dodge(width = 0.9),
    vjust = -0.3,
    size = 1.5
  ) +
  labs(
    title = "Number of Patients by Drug Class and Ethnicity",
    x = "Drug Class",
    y = "Number of Patients"
  ) +
  theme_minimal()
#create a bar plot for deprivation counts
ggplot(deprivation_count, aes(
  x = drug_class,
  y = unique_patients,
  fill = imd_quintile,
  group = imd_quintile
)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_text(
    aes(label = unique_patients),  #add count to bars
    position = position_dodge(width = 0.9),
    vjust = -0.3,
    size = 1.5
  ) +
  labs(
    title = "Number of Patients by Drug Class and Deprivation (according to IMD quintiles)",
    x = "Drug Class",
    y = "Number of Patients"
  ) +
  theme_minimal()
#create a bar plot for death counts
ggplot(death_count, aes(
  x = drug_class,
  y = unique_patients,
  fill = death_status,
  group = death_status
)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_text(
    aes(label = unique_patients),  #add count to bars
    position = position_dodge(width = 0.9),
    vjust = -0.3,
    size = 1.5
  ) +
  labs(
    title = "Number of Patients by Drug Class and Death Status",
    x = "Drug Class",
    y = "Number of Patients"
  ) +
  theme_minimal()
#create a bar plot for diabetes diagnosis-transplant timing counts
ggplot(diag_trans_timings_count, aes(
  x = drug_class,
  y = unique_patients,
  fill = diag_transplant_timing,
  group = diag_transplant_timing
)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_text(
    aes(label = unique_patients),  #add count to bars
    position = position_dodge(width = 0.9),
    vjust = -0.3,
    size = 1.5
  ) +
  labs(
    title = "Number of Patients by Drug Class and Diabetes Diagnosis-transplant Timing",
    x = "Drug Class",
    y = "Number of Patients"
  ) +
  theme_minimal()


##box plots
#bring into R all column needed for diagnosis age box plot
diag_age_data <- diabetes_transplant_cohort2 %>%
  filter(!is.na(drug_class)) %>%  #remove rows where drug_class is NA
  select(drug_class, dm_diag_age_all) %>%
  collect()
#create box plot for diabetes diagnosis age
diag_age_box <- ggplot(diag_age_data, aes(x = drug_class, y = dm_diag_age_all)) +
  geom_boxplot() +
  labs(
    title = "(a) Age at Diabetes Diagnosis by Drug Class",
    x = "Drug Class",
    y = "Age at Diagnosis (years)"
  ) +
  theme_minimal()
#bring into R all column needed for transplant age box plot
trans_age_data <- diabetes_transplant_cohort2 %>%
  filter(!is.na(drug_class)) %>%  #remove rows where drug_class is NA
  select(drug_class, age_at_transplant) %>%
  collect()
#create box plot for transplant age
trans_age_box <- ggplot(trans_age_data, aes(x = drug_class, y = age_at_transplant)) + 
  geom_boxplot() +
  labs(
    title = "(b) Age at Transplant by Drug Class",
    x = "Drug Class",
    y = "Age at Transplant (years)"
  ) +
  theme_minimal()
#create a subplot for age related box plots
grid.arrange(diag_age_box, trans_age_box, nrow = 2) #stacked vertically


#bring into R all column needed for transplant BMI box plot
trans_bmi_data <- diabetes_transplant_cohort2 %>%
  filter(!is.na(drug_class)) %>%  #remove rows where drug_class is NA
  select(drug_class, bmi_at_transplant_value) %>%
  collect()
#create box plot for BMI at transplant
trans_bmi_box <- ggplot(trans_bmi_data, aes(x = drug_class, y = bmi_at_transplant_value)) +
  geom_boxplot() +
  labs(
    title = "(b) BMI at Transplant by Drug Class",
    x = "Drug Class",
    y = "BMI at Transplant"
  ) +
  theme_minimal()
#bring into R all column needed for latest BMI box plot
latest_bmi_data <- diabetes_transplant_cohort2 %>%
  filter(!is.na(drug_class)) %>%  #remove rows where drug_class is NA
  select(drug_class, latest_bmi_value) %>%
  collect()
#create box plot for latest BMI
latest_bmi_box <- ggplot(latest_bmi_data, aes(x = drug_class, y = latest_bmi_value)) +
  geom_boxplot() +
  labs(
    title = "(c) Latest BMI by Drug Class",
    x = "Drug Class",
    y = "Latest BMI"
  ) +
  theme_minimal()
#bring into R all column needed for diagnosis BMI box plot
diag_bmi_data <- diabetes_transplant_cohort2 %>%
  filter(!is.na(drug_class)) %>%  #remove rows where drug_class is NA
  select(drug_class, bmi_at_diagnosis_value) %>%
  collect()
#create box plot for BMI at diagnosis
diag_bmi_box <- ggplot(diag_bmi_data, aes(x = drug_class, y = bmi_at_diagnosis_value)) +
  geom_boxplot() +
  labs(
    title = "(a) BMI at Diabetes Diagnosis by Drug Class",
    x = "Drug Class",
    y = "BMI at Diagnosis"
  ) +
  theme_minimal()
#create a subplot for BMI related box plots
grid.arrange(diag_bmi_box, trans_bmi_box, latest_bmi_box, nrow = 3) #stacked vertically


#bring into R all column needed for latest treatment length box plot
drug_length_data <- diabetes_transplant_cohort2 %>%
  filter(!is.na(drug_class)) %>%  #remove rows where drug_class is NA
  select(drug_class, latest_drug_length) %>%
  collect()
#create box plot for length of latest treatment
ggplot(drug_length_data, aes(x = drug_class, y = latest_drug_length)) +
  geom_boxplot() +
  labs(
    title = "Length of Latest Treatment by Drug Class",
    x = "Drug Class",
    y = "Length of Latest Treatment (days)"
  ) +
  theme_minimal()
#bring into R all column needed for latest hba1c box plot
latest_hba1c_data <- diabetes_transplant_cohort2 %>%
  filter(!is.na(drug_class)) %>%  #remove rows where drug_class is NA
  select(drug_class, latest_hba1c_value) %>%
  collect()
#create box plot for length of latest treatment
ggplot(latest_hba1c_data, aes(x = drug_class, y = latest_hba1c_value)) +
  geom_boxplot() +
  labs(
    title = "Latest hba1c by Drug Class",
    x = "Drug Class",
    y = "Latest hba1c"
  ) +
  theme_minimal()




