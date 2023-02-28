#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Title: "Loop Flex power code"
# Author: "Oli Berry"
# Date: "03/01/2023"

# Load packages
library(tidyverse) #----- Manipulating data (`case_when`)
library(janitor) #------- Useful for cleaning
library(stringr) #------- String operations (`str_wrap`)
library(purrr) #--------- Vectorization
library(data.table) #---- For transposing
library(readr) #--------- Reading csv files
library(summarytools) #-- Descriptive statistics
library(jtools) #-------- Regression outputs
library(lmtest) #-------- Linear regression tests
library(sandwich) #------ Robust SEs

# Data pathways
bas_dir <- "C:/Users/oli.berry/Documents/GitHub/asf_loop_flex_rct/"
data_path <- file.path(bas_dir, "1_Data")
out_dir <- file.path(bas_dir, "3_Outputs")

# Set seed
set.seed(20230103)
start_time <- Sys.time()



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
### ~~~ Summary of code ~~~ ###

## This code conducts simulated power calculations for the Loop Flexibility RCT.
## The outcome is electricity consumption during a Demand Flexibility Service event.
## The data comprise electricity consumption the most recent event (Wh) for 
## customers of Loop who have opted into the scheme overall.

## The code randomly allocates each customer to either a Treatment group or a 
## Control group. It then uses the standard error of the Treatment estimate to
## determine the minimum detectable effect.



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
### ~~~ Part 0: Importing data ~~~ ###

# Import dummy data
data <- read_csv(paste(data_path, "EventData.csv", sep="/")) |>
  janitor::clean_names()

#~~~ Cleaning and exploring
head(data)
str(data)

# Creating opt-out variable
data <- data |>
  mutate(opt_in = case_when(
    took_part == "TRUE" ~ 1,
    took_part == "FALSE" ~ 0,
    TRUE ~ 99
  ))

# Transforming to half-hour (current data is for full hour)
data <- data |>
  mutate(usage = usage / 2)

## Note, opting in to an event is our secondary outcome measure. We don't use
## this variable for the power calculations, but it is helpful to see.



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
### ~~~ Part 1: Exploring data ~~~ ###

# ~~ I. Usage ~~ #

# Summary statistics
summary(data$usage)
descr(data$usage)
nrow(data) ## 14,616 rows
length(data$usage[data$usage == 0]) ## 348 zeros (roughly 2%)

# Graphing
hist(data$usage, breaks = 1e3) ## Looks very skewed, with a lot of zeros

hist(log(data$usage[data$usage != 0]), breaks = 1e3) ## Looks more normal


# ~~ II. Opt ins ~~ #

summary(data$opt_in)
tabyl(data$opt_in) # 61% opt-in



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
### ~~~ Part 2: Power calculations ~~~ ###
## This is achieved using simulations

# ~~ I. Setting up loop ~~ #

#~~~ Parameters
sample_seq <- seq(1e3, 14e3, 500)

## NOTE TO REVIEWER - running all simulations takes under 5 minutes. You can
## reduce the number of simulations if you'd like.

sims <- length(sample_seq) * 30 ## Number of simulations
##sims <- length(sample_seq) * 3 ## Number of simulations

sim <- 0

# Looping
for (i in 1:sims) {
  
  # Sim count
  sim <- sim + 1
  
  #~~~ Selecting sample size
  ## We create a new dataframe so we don't need to import the data each time.
  
  # Creating random number and ordering
  data_to_use <- data |>
    filter(!duplicated(id)) |> ## Removing duplicates (there aren't any fortunately)
      group_by(id) |>
        mutate(rand = runif(1)) |> ## Generating random number
          ungroup() |>  
            arrange(rand) ## Sorting by random number
  
  # Selecting sample for this simulation
  sample <- sample_seq[(sim %% (length(sample_seq)) +1)] ## This is a way to subsequently sample from sample_seq
  
  # Subsetting to correct sample size
  data_to_use <- data_to_use |>
    filter(row_number() <= sample)
  
  #~~~ Random treatment assignment
  ## We are using simple randomisation
  
  data_to_use <- data_to_use |>
    group_by(id) |>
    mutate(treat_num = sample(2, 1, replace = TRUE)) |>
    ungroup() |>
    mutate(treatment = case_when(
      treat_num == 1 ~ "Control",
      treat_num == 2 ~ "Treatment",
      TRUE ~ "Missing"
    ))
  
  
  # ~~ II. Running regression ~~ #
  ## We want to assess power for three different regression specifications
  ## This uses a different model for each third of simulations
  
  if (sim <= length(sample_seq) * 10) {
    
    type <- "OLS"
    
    # Fit model
    fit <- lm(usage ~ treatment, data = data_to_use)
    
    # Robust SEs
    fit_coef <- coeftest(fit, vcov. = vcovHC, type = "HC3")
    
    
  } else if (sim <= length(sample_seq) * 20) {
    
    type <- "Logged"
    
    data_to_use <- data_to_use |>
      mutate(usage = log(usage + 1))
    
    # Fit model
    fit <- lm(usage ~ treatment, data = data_to_use)
    
    # Robust SEs
    fit_coef <- coeftest(fit, vcov. = vcovHC, type = "HC3")
    
    
  } else {
    
    type <- "Poisson"
    
    # Fit model
    fit <- glm(usage ~ treatment, family = "poisson", data = data_to_use)
    
    # Robust SEs
    fit_coef <- coeftest(fit, vcov. = vcovHC, type = "HC3")
    
  }
  
  # Converting to data frame
  coef_df <- transpose(as.data.frame(fit_coef[2, ])) |>
    mutate(beta = V1, se = V2, t_stat = V3, pval = V4) |>
    select(-c(V1, V2, V3, V4))
  
  # Control statistics
  control_mean <- mean(data_to_use$usage[data_to_use$treatment == "Control"], na.rm = TRUE)
  control_sd <- sd(data_to_use$usage[data_to_use$treatment == "Control"], na.rm = TRUE)
  
  
  # ~~ IV. Aggregating ~~ #
  
  # Binding columns
  sim_data <- cbind(sim, sample, type, control_mean, control_sd, coef_df)  
  
  # Binding rows
  if (sim == 1) {
    
    output_data <- sim_data
    
    # Exporting first regression to double check
    sink(paste(out_dir,"first_regression_flex.txt", sep="/"))
    
    # No covariates
    fit_simple <- lm(usage ~ treatment, data = data_to_use)
    print(summ(fit_simple))
    print(summ(fit_simple, robust = "HC3"))
    
    sink()
    
  } else {
    
    output_data <- rbind(output_data, sim_data)
    
  }
}

#~~~ Export
write.csv(output_data, file = paste(out_dir,"flex_power.csv", sep = "/"))


# ~~ III. Summarising for specific numbers of Treatment arms ~~ #
## This is for a pretty graph - it could be done manually in Excel, but this is more fun.

#~~~ Setting up arms
output_data <- output_data |>
  mutate(arms = case_when(
    sample == 14000 ~ 2, ## 7,000 in each arm, so 14,000 for T/C comparison
    sample == 9500 ~ 3, ## ~4,500 in each arm, so 9,000 for T/C comparison
    sample == 7000 ~ 4, ## ~3,500 in each arm, so 7,000 for T/C comparison
    sample == 5500 ~ 5, ##  ~2,8000 in each arm, so ~5,500 for T/C comparison
    TRUE ~ 99
  )
  )

# Summarising
output_table <- output_data |>
  group_by(type, sample) |>
  summarise(
    control_mean = mean(control_mean, na.rm = TRUE),
    control_sd = mean(control_sd, na.rm = TRUE),
    beta = mean(beta, na.rm = TRUE),
    se = mean(se, na.rm = TRUE),
    arms = mean(arms, na.rm = TRUE)
  )

# Filtering for experimental arms
output_table <- output_table |>
  filter(arms != 99)

# Adjusting alpha for multiple comparisons (Bonferroni)
output_table$alpha_adj <-  0.05 / (output_table$arms - 1) 

# Rule of 2.8, two-tailed
output_table$mde <-  output_table$se * (qnorm(0.8) + qnorm (1 - output_table$alpha_adj /2 ))

# Calculating % change
## Log and Poisson MDE are already expressed as % changes - so this is just for OLS

output_table$mde_prop <- 0
output_table$mde_prop[output_table$type == "OLS"] <- output_table$mde[output_table$type == "OLS"] / output_table$control_mean[output_table$type == "OLS"]
output_table$mde_prop[output_table$type != "OLS"] <- output_table$mde[output_table$type != "OLS"]


#~~~ Export
write.csv(output_table, file = paste(out_dir,"flex_power_arms.csv", sep = "/"))

# Checking time to run code
end_time <- Sys.time()
end_time - start_time



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ END ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#