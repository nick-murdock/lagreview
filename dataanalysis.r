# THIS FILE IS FOR ALL DATA ANALYSIS PORTIONS OF THE COVIDENCE REVIEW

# Loading in the data

data <- read.csv("LAg_sys_review_data.csv")

# Viewing the beginning of the data
head(data)

# Load packages
library(tidyr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(broom)

# Test Analyses 
hist(data$year)
data %>%
  group_by(eval_field) %>%
  count()

count(data[which(data$eval_field == 'Evaluation')])

table(data$eval_field)

data %>% 
  group_by(eval_field) %>% 
  count(unknown:polynesia)

data %>%
  gather()
  group_by(eval_field) %>%
  summarise(no = n()) %>%
  spread(eval_field, no)
# END TEST ANALYSES

# Tables
## Journal
journal.table <- table(data$journal)

## Year
year.table <- table(data$year)

## Evaluation / Field Use
eval.table <- table(data$eval_field)

## Population Descriptions
population.table <- table(data$pop_descript)

## Cohort Descriptions
cohort.table <- table(data$cohort_descript)

## Sample Type
sample.table <- table(data$sample_type)

## Assay Manufacturer
manufac.table <- data %>%
                  group_by(eval_field) %>%
                  count(assay_manufact)


