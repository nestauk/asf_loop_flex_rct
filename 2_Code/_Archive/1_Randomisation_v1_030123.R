#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Title: "Loop randomisation code"
# Author: "Oli Berry"
# Date: "03/01/2023"

# Load packages
library(tidyverse) #----- Manipulating data (`case_when`)
library(janitor) #------- Useful for cleaning
library(stringr) #------- String operations (`str_wrap`)
library(data.table) #---- For transposing
library(readxl) #-------- Reading Excel files
library(blockrand) #----- Stratified randomisation  

# Data pathways
bas_dir <- "C:/Users/oli.berry/Documents/GitHub/asf-boiler-optimisation-rct-loop"
data_path <- file.path(bas_dir, "1_Data")
out_dir <- file.path(bas_dir, "3_Outputs")

# Set seed
set.seed(20230103)

# Functions
export_table <- function(dat, varname1, varname2){
  dat |>  
    tabyl(.data[[varname1]], .data[[varname2]], show_na = FALSE) |>
    adorn_totals(c("row", "col")) |>
    adorn_percentages("col") |> 
    adorn_pct_formatting(digits = 0) |> 
    adorn_ns() |>
    as.data.frame() |>
    write.csv(paste(paste(out_dir,paste(varname1, varname2, sep="_"), sep="/"), "csv", sep="."))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
### ~~~ Summary of code ~~~ ###

## This code is used to randomise Loop's customers into one of three experimental
## arms: Treatment 1 (T1), Treatment 2 (T2), or Control group (C)

## We used stratified randomisation to achieve this, stratifying on region. This
## is because region correlates with gas consumption because of weather and other
## regional factors.

## Additionally, we allocate customers in Treatment 1 or Treatment 2 to one of 
## four email variants, as follows:
 ## 1) Standard email
 ## 2) Thermostat advice variant
 ## 3) TRV advice variant
 ## 4) Social norms variant

## This will be achieved using simple randomisation.



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
### ~~~ Part 0: Importing data ~~~ ###

# Description of dataset required for code

# We need one observation for each customer and their individual-level covariates
# (as outlined below):

  # user_id ~ unique customer identifier
  # region_id ~ region identified for each customer (as defined by Nomenclature of Territorial Units for Statistics 1)
  # time_with_loop ~ Length of time the participant has used Loop (categorical as follows: (a) 1 month or less; (b) 1 to 3 months; (c) 3 to 6 months); and (d) 6 or more months)
  # proportion_of_emails_read ~ % of opening rates of up to 5 previous Loop emails (categorical, as follows: (a) Participant has opened each of the emails; (b) Participant has opened some of the emails; (c) Participants has not opened any of the emails)
  # annual_consumption_band ~ Annual energy consumption bands (categorical, 500 kWh buckets)


# Import dummy data
data <- read_excel(paste(data_path, "DummyAllocation.xlsx", sep="/")) |>
  janitor::clean_names()
  
## This can be ignored when using the correct dataset.



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
### ~~~ Part 1: Cleaning & merging ~~~ ###

# Cleaning
data <- data |>
  mutate(across(c("user_id", "region_id", "time_with_loop", "proportion_of_emails_read", "annual_consumption_band"), as.factor))

# Creating band for annual consumption
## These are based on pre-established bands
data <- data |>
  mutate(annual_consumption = as.integer(annual_consumption_band) * 500) |> ## Each band in 500 kWh
    mutate(annual_band = case_when(
      annual_consumption < 8000 ~ 1,
      annual_consumption < 12000 ~ 2,
      annual_consumption < 17000 ~ 3,
      annual_consumption >= 17000 ~ 4,
      TRUE ~ 99
    )) 



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
### ~~~ Part 2: Randomising ~~~ ###

# ~~ I. Generating random number ~~ #
## We generate a random number for each individual to randomise the order. This
## is performed so the stratified randomisation can be double-checked and also
## facilitates a random start to allocation for each region.

# Extracting unique user_ids (in case of repeats)
userid_df <- data |>
  select(user_id) |>
    as.data.frame(unique())

# Counting number of userids
n <- nrow(userid_df)

# Creating allocation dataframe
allocation <- as.data.frame(runif(n)) |>
  rename(rand_num = "runif(n)")

# Binding columns
userid_df <- cbind(
  userid_df,
  allocation)

# Left joining onto data
data <- left_join(
  data,
  userid_df,
  by = "user_id"
)

# Generating random number for region
## This is to randomly allocate which of the three experimental arms to start with
## to help ensure that groups are balanced in cases where the total number of
## individuals in a region is not a multiple of three

# Creating one random number per region
data <- data |>
  group_by(region_id) |>
    mutate(region_ran = rand_num[1] * 3) |>
      ungroup()

# Ordering
data <- data[order(data$rand_num), ]

# Removing duplicates (this randomly removes duplicates)
data <- data[!duplicated(data$user_id), ]
  
## Not expecting any duplicates, but worth removing in case as it will interfere
## with allocation.


# ~~ II. Stratifying ~~ #
## We allocate each row in sequence to either T1, T2, or C, starting on each
## randomly due to the random region_num

# Creating treatment assignment
data <- data |>
  group_by(region_id) |>
    mutate(treatment_num = ((row_number() + floor(region_ran)) %% 3)) |>
      mutate(treatment = case_when(
        treatment_num == 0 ~ "T1",
        treatment_num == 1 ~ "T2",
        treatment_num == 2 ~ "C",
        TRUE ~ "Missing"
      )) |>
        ungroup()


# ~~ III. Creating assignment for email variations ~~ #
## Using simple randomisation

# Extracting unique user_ids in T1 or T2 (in case of repeats)
userid_T_df <- data |>
  filter(treatment_num == 0 | treatment_num == 1) |>
  select(user_id) |>
  as.data.frame(unique())

# Counting number of userids in T1 or T2
n_T <- nrow(userid_T_df)

# Creating allocation dataframe
allocation_email <- floor(runif(n_T, min = 0, max = 4))

# Binding columns
userid_T_df <- cbind(
  userid_T_df,
  allocation_email)

# Left joining onto data
data <- left_join(
  data,
  userid_T_df,
  by = "user_id"
)

# Creating variable name
data <- data |>
  mutate(email = case_when(
    allocation_email == 0 ~ "Standard",
    allocation_email == 1 ~ "Thermostat",
    allocation_email == 2 ~ "TRV",
    allocation_email == 3 ~ "Social norm",
    TRUE ~ "Control"
  ))


# ~~ IV. Checking ~~ #

# Treatment arm
tabyl(data, region_id, treatment) ## Note that all are within one integer

# Email variant
tabyl(data[data$email !="Control", ], treatment, email)
## Note, will be more balanced with full sample (expected to be 100,000 customers)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
### ~~~ Part 3: Balance checks ~~~ ###
## Exporting balance checks

# Create list of variables
var_list <- c("region_id", "annual_consumption_band", "annual_band", "time_with_loop", "proportion_of_emails_read")

# Export - variable by treatment
map(var_list, ~export_table(dat = data, varname1 = .x, varname2 = "treatment"))

# Export allocation data
data |>
  select(user_id, region_id, annual_consumption_band, annual_band, time_with_loop, proportion_of_emails_read) |>
   write.csv(paste(paste(out_dir,paste("allocation_data"), sep="/"), "csv", sep="."))
  


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ END ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#