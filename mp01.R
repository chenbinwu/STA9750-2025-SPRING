if(!file.exists("data/mp01/nyc_payroll_export.csv")){
  dir.create("data/mp01", showWarnings=FALSE, recursive=TRUE)
  
  ENDPOINT <- "https://data.cityofnewyork.us/resource/k397-673e.json"
  
  if(!require("httr2")) install.packages("httr2")
  library(httr2)
  
  if(!require("jsonlite")) install.packages("jsonlite")
  library(jsonlite)
  
  if(!require("dplyr")) install.packages("dplyr")
  library(dplyr)
  
  if(!require("readr")) install.packages("readr")
  library(readr)
  
  BATCH_SIZE <- 50000
  OFFSET     <- 0
  END_OF_EXPORT <- FALSE
  ALL_DATA <- list()
  
  while(!END_OF_EXPORT){
    cat("Requesting items", OFFSET, "to", BATCH_SIZE + OFFSET, "\n")
    
    req <- request(ENDPOINT) |>
      req_url_query(`$limit`  = BATCH_SIZE, 
                    `$offset` = OFFSET)
    
    resp <- req_perform(req)
    
    batch_data <- fromJSON(resp_body_string(resp))
    
    ALL_DATA <- c(ALL_DATA, list(batch_data))
    
    if(NROW(batch_data) != BATCH_SIZE){
      END_OF_EXPORT <- TRUE
      
      cat("End of Data Export Reached\n")
    } else {
      OFFSET <- OFFSET + BATCH_SIZE
    }
  }
  
  ALL_DATA <- bind_rows(ALL_DATA)
  
  cat("Data export complete:", NROW(ALL_DATA), "rows and", NCOL(ALL_DATA), "columns.")
  
  write_csv(ALL_DATA, "data/mp01/nyc_payroll_export.csv")
}
library(dplyr)
library(readr)
library(stringr)

payroll_data <- read_csv("data/mp01/nyc_payroll_export.csv")

payroll_data <- payroll_data |>
  mutate(
    agency_name = str_to_title(agency_name),
    last_name = str_to_title(last_name),
    first_name = str_to_title(first_name),
    work_location_borough = str_to_title(work_location_borough),
    title_description = str_to_title(title_description),
    leave_status_as_of_june_30 = str_to_title(leave_status_as_of_june_30)
  )

glimpse(payroll_data)

eric_adams_data <- payroll_data |>
  filter(first_name == "Eric" & last_name == "Adams") |>
  select(fiscal_year, title_description, agency_name, regular_gross_paid, total_ot_paid, total_other_pay) |>
  mutate(total_salary = regular_gross_paid + total_ot_paid + total_other_pay)

library(DT)
eric_adams_data |>
  mutate(total_salary = scales::dollar(total_salary)) |>
  datatable(options = list(searching = FALSE, paging = FALSE, info = FALSE))

library(dplyr)

eric_adams_data <- payroll_data |>
  filter(first_name == "Eric" & last_name == "Adams")
eric_adams_data <- eric_adams_data |>
  mutate(

    total_compensation = case_when(
      !is.na(base_salary) ~ base_salary,
      
      !is.na(regular_gross_paid) & !is.na(total_ot_paid) ~
        regular_gross_paid + total_ot_paid,  
      
      !is.na(regular_gross_paid) ~
        regular_gross_paid * (regular_hours / 7.5), 
      
      TRUE ~ NA_real_ 
    )
  )

library(DT)
library(scales)
eric_adams_data |>
  select(fiscal_year, title_description, agency_name, total_compensation) |>
  mutate(total_compensation = dollar(total_compensation)) |>
  datatable(options = list(
    searching = FALSE, 
    paging = FALSE, 
    info = FALSE
  ))

payroll_data <- payroll_data |>
  mutate(
    total_compensation = case_when(
      !is.na(base_salary) ~ base_salary,  
      !is.na(regular_gross_paid) & !is.na(total_ot_paid) ~
        regular_gross_paid + total_ot_paid,  
      !is.na(regular_gross_paid) ~
        regular_gross_paid * (regular_hours / 7.5),  
      TRUE ~ NA_real_  
    )
  )

aggregate_salaries <- payroll_data |>
  group_by(first_name, last_name) |>
  summarize(
    total_compensation = sum(total_compensation, na.rm = TRUE)
  ) |>
  arrange(desc(total_compensation))

total_salary_summary <- payroll_data |>
  group_by(first_name, last_name) |>
  summarize(
    total_compensation = sum(total_compensation, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(total_compensation))

glimpse(total_salary_summary)

#Which job title has the highest base rate of pay?
highest_base_salary_job <- payroll_data |>
  group_by(title_description) |>
  summarize(avg_base_salary = mean(base_salary, na.rm = TRUE)) |>
  arrange(desc(avg_base_salary)) |>
  slice(1)

highest_base_salary_job

#which individual & in what year had the single highest city total payroll (regular and ot combined)?
highest_payroll_individual <- payroll_data |>
  filter(!is.na(first_name), !is.na(last_name))|>
  mutate(total_payroll = regular_gross_paid + total_ot_paid) |>
  group_by(first_name, last_name, fiscal_year) |>
  summarize(total_payroll = sum(total_payroll, na.rm = TRUE), .groups = 'drop') |>
  arrange(desc(total_payroll)) |>
  slice(1)

highest_payroll_individual

#Which individual worked the most overtime hours in this data set?
most_overtime_individual <- payroll_data |>
  filter(!is.na(first_name), !is.na(last_name))|>
  group_by(first_name, last_name) |>
  summarize(total_overtime_hours = sum(ot_hours, na.rm = TRUE), .groups = 'drop') |>
  arrange(desc(total_overtime_hours)) |>
  slice(1)  # Get the individual with the most overtime hours

most_overtime_individual

#Which agency has the highest average total annual payroll (base and overtime pay per employee)?
highest_avg_payroll_agency <- payroll_data |>
  mutate(total_payroll = regular_gross_paid + total_ot_paid) |>
  group_by(agency_name) |>
  summarize(avg_total_payroll = mean(total_payroll, na.rm = TRUE), .group = 'drop') |>
  arrange(desc(avg_total_payroll)) |>
  slice(1)

highest_avg_payroll_agency

#Which agency has the most employees on payroll in each year?
most_employee_agency <- payroll_data |>
  filter(!is.na(first_name), !is.na(last_name)) |>
  group_by(agency_name, fiscal_year) |>
  summarise(employee_count = n(), .groups = 'drop') |>
  arrange(desc(employee_count))

most_employee_agency

#Which agency has the highest overtime usage (compared to regular hours)?
highest_overtime_usage_agency <- payroll_data |>
  group_by(agency_name) |>
  summarize(overtime_ratio = sum(ot_hours, na.rm = TRUE) / sum(regular_gross_paid, na.rm = TRUE)) |>
  arrange(desc(overtime_ratio)) |>
  slice(1)  # Get the agency with the highest overtime usage

highest_overtime_usage_agency

#What is the average salary of employees who work outside the five boroughs? (That is, whose work_location_borough is not one of the five counties.)
avg_salary_outside_boroughs <- payroll_data |>
  filter(!work_location_borough %in% c("Manhattan", "Brooklyn", "Queens", "Bronx", "Staten Island")) |>
  summarize(avg_salary = mean(base_salary, na.rm = TRUE))

avg_salary_outside_boroughs

#How much has the city’s aggregate payroll grown over the past 10 years?
payroll_growth <- payroll_data |>
  mutate(total_payroll = regular_gross_paid + total_ot_paid) |>
  group_by(fiscal_year) |>
  summarize(aggregate_payroll = sum(total_payroll, na.rm = TRUE)) |>
  filter(fiscal_year >= (max(fiscal_year) - 10)) |>
  arrange(fiscal_year)

payroll_growth

#Policy 1: Capping Salaries at Mayoral Level
#Task 1: Compute Mayor's Total Pay and Identify Employees with Higher Pay
mayor_salary <- payroll_data |>
  filter(first_name == 'Eric', last_name == 'Adams') |>
  group_by(fiscal_year) |>
  summarize(mayor_salary = sum(regular_gross_paid + total_ot_paid, na.rm = True), .groups = 'drop')

if(!file.exists("data/mp01/nyc_payroll_export.csv")){
  dir.create("data/mp01", showWarnings=FALSE, recursive=TRUE)
  
  ENDPOINT <- "https://data.cityofnewyork.us/resource/k397-673e.json"
  
  if(!require("httr2")) install.packages("httr2")
  library(httr2)
  
  if(!require("jsonlite")) install.packages("jsonlite")
  library(jsonlite)
  
  if(!require("dplyr")) install.packages("dplyr")
  library(dplyr)
  
  if(!require("readr")) install.packages("readr")
  library(readr)
  
  BATCH_SIZE <- 50000
  OFFSET     <- 0
  END_OF_EXPORT <- FALSE
  ALL_DATA <- list()
  
  while(!END_OF_EXPORT){
    cat("Requesting items", OFFSET, "to", BATCH_SIZE + OFFSET, "\n")
    
    req <- request(ENDPOINT) |>
      req_url_query(`$limit`  = BATCH_SIZE, 
                    `$offset` = OFFSET)
    
    resp <- req_perform(req)
    
    batch_data <- fromJSON(resp_body_string(resp))
    
    ALL_DATA <- c(ALL_DATA, list(batch_data))
    
    if(NROW(batch_data) != BATCH_SIZE){
      END_OF_EXPORT <- TRUE
      
      cat("End of Data Export Reached\n")
    } else {
      OFFSET <- OFFSET + BATCH_SIZE
    }
  }
  
  ALL_DATA <- bind_rows(ALL_DATA)
  
  cat("Data export complete:", NROW(ALL_DATA), "rows and", NCOL(ALL_DATA), "columns.")
  
  write_csv(ALL_DATA, "data/mp01/nyc_payroll_export.csv")
}
library(dplyr)
library(readr)
library(stringr)

payroll_data <- read_csv("data/mp01/nyc_payroll_export.csv")

payroll_data <- payroll_data |>
  mutate(
    agency_name = str_to_title(agency_name),
    last_name = str_to_title(last_name),
    first_name = str_to_title(first_name),
    work_location_borough = str_to_title(work_location_borough),
    title_description = str_to_title(title_description),
    leave_status_as_of_june_30 = str_to_title(leave_status_as_of_june_30)
  )

glimpse(payroll_data)

eric_adams_data <- payroll_data |>
  filter(first_name == "Eric" & last_name == "Adams") |>
  select(fiscal_year, title_description, agency_name, regular_gross_paid, total_ot_paid, total_other_pay) |>
  mutate(total_salary = regular_gross_paid + total_ot_paid + total_other_pay)

library(DT)
eric_adams_data |>
  mutate(total_salary = scales::dollar(total_salary)) |>
  datatable(options = list(searching = FALSE, paging = FALSE, info = FALSE))

library(dplyr)

eric_adams_data <- payroll_data |>
  filter(first_name == "Eric" & last_name == "Adams")
eric_adams_data <- eric_adams_data |>
  mutate(
    
    total_compensation = case_when(
      !is.na(base_salary) ~ base_salary,
      
      !is.na(regular_gross_paid) & !is.na(total_ot_paid) ~
        regular_gross_paid + total_ot_paid,  
      
      !is.na(regular_gross_paid) ~
        regular_gross_paid * (regular_hours / 7.5), 
      
      TRUE ~ NA_real_ 
    )
  )

library(DT)
library(scales)
eric_adams_data |>
  select(fiscal_year, title_description, agency_name, total_compensation) |>
  mutate(total_compensation = dollar(total_compensation)) |>
  datatable(options = list(
    searching = FALSE, 
    paging = FALSE, 
    info = FALSE
  ))

payroll_data <- payroll_data |>
  mutate(
    total_compensation = case_when(
      !is.na(base_salary) ~ base_salary,  
      !is.na(regular_gross_paid) & !is.na(total_ot_paid) ~
        regular_gross_paid + total_ot_paid,  
      !is.na(regular_gross_paid) ~
        regular_gross_paid * (regular_hours / 7.5),  
      TRUE ~ NA_real_  
    )
  )

aggregate_salaries <- payroll_data |>
  group_by(first_name, last_name) |>
  summarize(
    total_compensation = sum(total_compensation, na.rm = TRUE)
  ) |>
  arrange(desc(total_compensation))

total_salary_summary <- payroll_data |>
  group_by(first_name, last_name) |>
  summarize(
    total_compensation = sum(total_compensation, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(total_compensation))

glimpse(total_salary_summary)

#Which job title has the highest base rate of pay?
highest_base_salary_job <- payroll_data |>
  group_by(title_description) |>
  summarize(avg_base_salary = mean(base_salary, na.rm = TRUE)) |>
  arrange(desc(avg_base_salary)) |>
  slice(1)

highest_base_salary_job

#which individual & in what year had the single highest city total payroll (regular and ot combined)?
highest_payroll_individual <- payroll_data |>
  filter(!is.na(first_name), !is.na(last_name))|>
  mutate(total_payroll = regular_gross_paid + total_ot_paid) |>
  group_by(first_name, last_name, fiscal_year) |>
  summarize(total_payroll = sum(total_payroll, na.rm = TRUE), .groups = 'drop') |>
  arrange(desc(total_payroll)) |>
  slice(1)

highest_payroll_individual

#Which individual worked the most overtime hours in this data set?
most_overtime_individual <- payroll_data |>
  filter(!is.na(first_name), !is.na(last_name))|>
  group_by(first_name, last_name) |>
  summarize(total_overtime_hours = sum(ot_hours, na.rm = TRUE), .groups = 'drop') |>
  arrange(desc(total_overtime_hours)) |>
  slice(1)  # Get the individual with the most overtime hours

most_overtime_individual

#Which agency has the highest average total annual payroll (base and overtime pay per employee)?
highest_avg_payroll_agency <- payroll_data |>
  mutate(total_payroll = regular_gross_paid + total_ot_paid) |>
  group_by(agency_name) |>
  summarize(avg_total_payroll = mean(total_payroll, na.rm = TRUE), .group = 'drop') |>
  arrange(desc(avg_total_payroll)) |>
  slice(1)

highest_avg_payroll_agency

#Which agency has the most employees on payroll in each year?
most_employee_agency <- payroll_data |>
  filter(!is.na(first_name), !is.na(last_name)) |>
  group_by(agency_name, fiscal_year) |>
  summarise(employee_count = n(), .groups = 'drop') |>
  arrange(desc(employee_count))

most_employee_agency

#Which agency has the highest overtime usage (compared to regular hours)?
highest_overtime_usage_agency <- payroll_data |>
  group_by(agency_name) |>
  summarize(overtime_ratio = sum(ot_hours, na.rm = TRUE) / sum(regular_gross_paid, na.rm = TRUE)) |>
  arrange(desc(overtime_ratio)) |>
  slice(1)  # Get the agency with the highest overtime usage

highest_overtime_usage_agency

#What is the average salary of employees who work outside the five boroughs? (That is, whose work_location_borough is not one of the five counties.)
avg_salary_outside_boroughs <- payroll_data |>
  filter(!work_location_borough %in% c("Manhattan", "Brooklyn", "Queens", "Bronx", "Staten Island")) |>
  summarize(avg_salary = mean(base_salary, na.rm = TRUE))

avg_salary_outside_boroughs

#How much has the city’s aggregate payroll grown over the past 10 years?
payroll_growth <- payroll_data |>
  mutate(total_payroll = regular_gross_paid + total_ot_paid) |>
  group_by(fiscal_year) |>
  summarize(aggregate_payroll = sum(total_payroll, na.rm = TRUE)) |>
  filter(fiscal_year >= (max(fiscal_year) - 10)) |>
  arrange(fiscal_year)

payroll_growth

#Policy 1: Capping Salaries at Mayoral Level
#Task 1: Compute Mayor's Total Pay and Identify Employees with Higher Pay
mayor_salary <- payroll_data |>
  filter(first_name == 'Eric', last_name == 'Adams') |>
  group_by(fiscal_year) |>
  summarize(mayor_salary = sum(regular_gross_paid + total_ot_paid, na.rm = TRUE), .groups = 'drop')

payroll_data <- payroll_data |>
  mutate(total_payroll = regular_gross_paid + total_ot_paid)

employees_above_mayor <- payroll_data |>
  filter(total_payroll > mayor_salary$mayor_salary[match(fiscal_year, mayor_salary$fiscal_year)])

nrow(employees_above_mayor)  

head(employees_above_mayor)  

#Task 2: Determine total savings if these employees’ compensation were capped at the mayor’s salary.
employees_above_mayor <- employees_above_mayor |>
  mutate(salary_cut = total_payroll - mayor_salary$mayor_salary[match(fiscal_year, mayor_salary$fiscal_year)],
         savings = salary_cut)

total_savings <- sum(employees_above_mayor$savings, na.rm = TRUE)

savings_by_agency <- employees_above_mayor |>
  group_by(agency_name, title_description) |>
  summarize(total_savings = sum(savings, na.rm = TRUE), .groups = 'drop') |>
  arrange(desc(total_savings))

total_savings
head(savings_by_agency)

#Task 3: Identify which agencies and job titles (if any) would bear the brunt of this policy.
summary_savings <- data.frame(
  Total_Savings = total_savings,
  Affected_Agencies = nrow(savings_by_agency),
  Agencies_Impact = head(savings_by_agency, 10)
)

if(total_savings > 10000000) {
  recommendation <- "The policy to cap salaries at the mayor's level could lead to significant savings and may be worth considering. However, care should be taken to mitigate potential impacts on moral in high-level positions."
} else {
  recommendation <- "The policy might not yield enough savings to justify its implementation. Other policies, such as targeting overtime redution, might provide better outcomes."
}
summary_savings
recommendation

#Policy 2: Increasing staffing to reduce overtime expenses
overtime_data <- payroll_data |>
  filter(!is.na(ot_hours) & ot_hours > 0) |>
  group_by(agency_name, title_description) |>
  summarize(
    total_overtime_hours = sum(ot_hours, na.rm = TRUE),
    total_ot_paid = sum(total_ot_paid, na.rm = TRUE),
    .groups = 'drop'
  )
head(overtime_data)

full_time_hours_per_week <- 40
overtime_data <- overtime_data |>
  mutate(
    employees_needed = total_overtime_hours / full_time_hours_per_week
  )
head(overtime_data)

# Calculate how many full-time employees are needed to replace overtime hours
overtime_data <- overtime_data |>
  mutate(
    total_overtime_hours = total_overtime_cost / (1.5 * avg_regular_salary),  # Convert cost into hours
    employees_needed = ceiling(total_overtime_hours / full_time_hours_per_week)  # Round up to the nearest employee
  ) |>
  mutate(
    cost_of_overtime = total_overtime_cost,
    cost_of_replacement = employees_needed * full_time_hours_per_week * avg_regular_salary,
    savings = cost_of_overtime - cost_of_replacement
  )
head(overtime_data)
