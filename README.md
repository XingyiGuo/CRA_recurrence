# CRA_recurrence
  # Load necessary libraries
  library(readxl)
  library(dplyr)
  library(survival)
  library(survminer)
  library(ggplot2)
  library(gt)
  library(gtsummary)
  
  # Read the data
  data <- read_excel("Path")
  
  
  print(colnames(data))
  
  # Adding calculation for recurrence time and updating Recurrence Status with blank cells considered
  df <- data %>%
    select(person_id, `Gender`, `ethnicity`, `obesity`, `Polyp Onset Stratification`,
           `Adenoma Type`, `Recurrence Status`, `Time to Recurrence`, 
           birth_datetime, polypectomy_date_1, polypectomy_date_2, `Polyp Type`, `Dysplasia`, 
           `Number of Polyp`, `Size of Polyp (mm)`, `First Recurrence Date`, `Number_of_Recurrence`) %>%
    mutate(
      # Calculate recurrence time in months, handling missing polypectomy_date_2
      recurrence_time_months = ifelse(
        !is.na(polypectomy_date_2), 
        as.numeric(as.Date(polypectomy_date_2) - as.Date(polypectomy_date_1)) / 30.44, 
        NA
      ),
      # Update Recurrence Status based on the 6-month threshold or missing polypectomy_date_2
      `Recurrence Status` = ifelse(
        is.na(recurrence_time_months) | recurrence_time_months < 6, 
        "Not Recurrence", 
        "Recurrence"
      ),
      event = ifelse(`Recurrence Status` == "Recurrence", 1, 0),
      age_in = as.numeric(as.Date(polypectomy_date_1) - as.Date(birth_datetime)) / 365.25,
      age_out = as.numeric(as.Date(`First Recurrence Date`) - as.Date(birth_datetime)) / 365.25,
      age_grp = cut(age_in, breaks = c(0, 50, 60, 70, 96), labels = 1:4)
    ) %>%
    filter(age_out > age_in & age_in >= 18 & ethnicity != "Other" & !is.na(`Adenoma Type`))
 
  
  dim(df) 
  df %>% count(`Recurrence Status`)
  df %>% count(`Polyp Type`)
  # Exclude rows with blank (NA) cells in "First Recurrence Date"
  # 1. Basic descriptive statistics with one decimal place for both continuous and categorical variables
  table1 <- df %>%
    select(`Gender`, `ethnicity`, `obesity`, `Polyp Onset Stratification`, 
           `Polyp Type`, `Dysplasia`, `Number of Polyp`, `Size of Polyp (mm)`, 
           `Adenoma Type`, `Recurrence Status`) %>%
    tbl_summary(
      missing = "ifany",  # Show missing values if any
      statistic = list(
        all_continuous() ~ "{mean:.1f} ± {sd:.1f}",  # Mean ± SD with 1 decimal place
        all_categorical() ~ "{n} ({p}%)"            # Count and percentage
      ),
      digits = list(
        all_categorical() ~ c(0, 1),                # Specify 1 decimal place for percentages
        all_continuous() ~ c(1, 1)                  # Specify 1 decimal place for mean and SD
      )
    ) %>%
    modify_header(label = "**Variable**") %>%  # Modify header
    bold_labels()
  
  
  table1
  
  
  ################ Descriptive Statistics by single Variable#################
  
  # 2. Descriptive statistics by Gender with one decimal place
  table2_row_wise <- df %>%
    select(
      Gender, ethnicity, obesity, `Polyp Onset Stratification`, 
      `Polyp Type`, Dysplasia, `Number of Polyp`, `Size of Polyp (mm)`,
      `Adenoma Type`, `Recurrence Status`, Number_of_Recurrence
    ) %>%
    tbl_summary(
      by = "Recurrence Status",              # Grouped by ethnicity
      missing = "no",                # Do not show missing values
      statistic = list(
        all_continuous() ~ "{mean:.1f} ± {sd:.1f}",  # Mean ± SD with 1 decimal place
        all_categorical() ~ "{n} ({p}%)"            # Count and percentage
      ),
      digits = list(
        all_continuous() ~ c(1, 1),                 # 1 decimal place for mean and SD
        all_categorical() ~ c(0, 1)                # 1 decimal place for percentages
      ),
      percent = "row"                              # Row-wise percentages
    ) %>%
    add_p() %>%                                    # Add p-values
    modify_header(label = "**Variable**")          # Modify header
  
  # Print the resulting table
  table2_row_wise
  
  
############# Recurrence  ##################
  
  # Changing reference levels for categorical variables
  df <- df %>%
    mutate(
      Gender = relevel(factor(Gender), ref = "Female"), # Set reference level for Gender
      ethnicity = relevel(factor(ethnicity), ref = "Non-Hispanic White"), # Set reference level for ethnicity
      `Polyp Onset Stratification` = relevel(factor(`Polyp Onset Stratification`), ref = "51-75"), # Set reference level for Polyp Onset Stratification
      Dysplasia = relevel(factor(Dysplasia), ref = "Low grade"), # Set reference level for Dysplasia
      `Number of Polyp` = relevel(factor(`Number of Polyp`), ref = "<3"), # Set reference level for Number of Polyp
      `Size of Polyp (mm)` = relevel(factor(`Size of Polyp (mm)`), ref = "<10"), # Set reference level for Size of Polyp
      `Adenoma Type` = relevel(factor(`Adenoma Type`), ref = "Low Risk"),
      `Polyp Type` = relevel(factor(`Polyp Type`), ref = "Tubular") # Set reference level for Polyp Type
    )
  
  
  # Fit the Cox proportional hazards model using age intervals
  adjusted_model <- coxph(Surv(age_out - age_in, event) ~ `Gender` + `ethnicity` + `obesity` +  `Polyp Onset Stratification` 
                          + `Polyp Type` + `Dysplasia` + `Number of Polyp` + 
                            `Size of Polyp (mm)` + strata(age_grp), data = df)
  
  
  
  cox.zph(adjusted_model)
  
  # Generate a summary table for the adjusted model
  adjusted_table <- tbl_regression(
    adjusted_model, 
    exponentiate = TRUE
  )
  
  # Print the summary table
  print(adjusted_table)
  
  
  
  
  
  
  
