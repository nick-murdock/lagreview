# THIS FILE IS FOR ALL data ANALYSIS PORTIONS OF THE COVIDENCE REVIEW

# Load packages
library(tidyr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(broom)

# Loading in the data

data <- read.csv("LAg_sys_review_data.csv")

# Viewing the beginning of the data.v2
head(data)
str(data)

# data clean-up
str(data[,41:58])

data %>% 
  count(unknown)

data %>% 
  count(northern_africa)

data %>% 
  count(sub_saharan_africa)

data %>% 
  count(latin_america_caribbean)

data %>% 
  count(northern_america)

data %>% 
  count(central_asia )

data %>% 
  count(eastern_asia)

data %>% 
  count(south_eastern_asia)

data %>% 
  count(southern_asia)

data %>% 
  count(western_asia)

data %>% 
  count(eastern_europe)

data %>% 
  count(northern_europe)

data %>% 
  count(southern_europe)

data %>% 
  count(western_europe)

data %>% 
  count(australia_new_zealand)

data %>% 
  count(melanesia)

data %>% 
  count(micronesia)

data %>% 
  count(polynesia)

data.v2 <- data %>% 
  mutate("unknown" = ifelse(unknown == "Yes" &
                              !is.na(unknown), "Yes", "No")) %>%
  mutate("northern_africa" = ifelse(northern_africa == "Yes" &
                              !is.na(northern_africa), "Yes","No")) %>%
  mutate("sub_saharan_africa" = ifelse(sub_saharan_africa == "Yes" &
                              !is.na(sub_saharan_africa), "Yes","No")) %>%
  mutate("latin_america_caribbean" = ifelse(latin_america_caribbean == "Yes" &
                              !is.na(latin_america_caribbean), "Yes","No")) %>%
  mutate("northern_america" = ifelse(northern_america == "Yes" &
                              !is.na(northern_america), "Yes","No")) %>%
  mutate("central_asia" = ifelse(central_asia == "Yes" &
                              !is.na(central_asia), "Yes","No")) %>%
  mutate("eastern_asia" = ifelse(eastern_asia == "Yes" & 
                              !is.na(eastern_asia), "Yes","No")) %>%
  mutate("south_eastern_asia" = ifelse(south_eastern_asia == "Yes" & 
                              !is.na(south_eastern_asia), "Yes","No")) %>%
  mutate("southern_asia" = ifelse(southern_asia == "Yes" & 
                              !is.na(southern_asia), "Yes","No")) %>%
  mutate("western_asia" = ifelse(western_asia == "Yes" &
                              !is.na(western_asia), "Yes","No")) %>%
  mutate("eastern_europe" = ifelse(eastern_europe == "Yes" & 
                              !is.na(eastern_europe), "Yes","No")) %>%
  mutate("northern_europe" = ifelse(northern_europe == "Yes" & 
                              !is.na(northern_europe), "Yes","No")) %>%
  mutate("southern_europe" = ifelse(southern_europe == "Yes" & 
                              !is.na(southern_europe), "Yes","No")) %>%
  mutate("western_europe" = ifelse(western_europe == "Yes" & 
                              !is.na(western_europe), "Yes","No")) %>%
  mutate("australia_new_zealand" = ifelse(australia_new_zealand == "Yes" & 
                              !is.na(australia_new_zealand), "Yes","No")) %>%
  mutate("melanesia" = ifelse(melanesia == "Yes" & 
                              !is.na(melanesia), "Yes","No")) %>%
  mutate("micronesia" = ifelse(micronesia == "Yes" & 
                              !is.na(micronesia), "Yes","No")) %>%
  mutate("polynesia" = ifelse(polynesia == "Yes" & 
                                 !is.na(polynesia), "Yes","No"))

data.v2$unknown <- as.factor(data.v2$unknown)
data.v2$northern_africa <- as.factor(data.v2$northern_africa)
data.v2$sub_saharan_africa <- as.factor(data.v2$sub_saharan_africa)
data.v2$latin_america_caribbean <- as.factor(data.v2$latin_america_caribbean)
data.v2$northern_america <- as.factor(data.v2$northern_america)
data.v2$central_asia <- as.factor(data.v2$central_asia)
data.v2$eastern_asia <- as.factor(data.v2$eastern_asia)
data.v2$south_eastern_asia <- as.factor(data.v2$south_eastern_asia)
data.v2$southern_asia <- as.factor(data.v2$southern_asia)
data.v2$western_asia <- as.factor(data.v2$western_asia)
data.v2$eastern_europe <- as.factor(data.v2$eastern_europe)
data.v2$northern_europe <- as.factor(data.v2$northern_europe)
data.v2$southern_europe <- as.factor(data.v2$southern_europe)
data.v2$western_europe <- as.factor(data.v2$western_europe)
data.v2$australia_new_zealand <- as.factor(data.v2$australia_new_zealand)
data.v2$melanesia <- as.factor(data.v2$melanesia)
data.v2$micronesia <- as.factor(data.v2$micronesia)
data.v2$polynesia <- as.factor(data.v2$polynesia)

str(data.v2[,41:58])

# Test Analyses 
hist(data.v2$year)
data.v2 %>%
  group_by(eval_field) %>%
  count()

count(data.v2[which(data.v2$eval_field == 'Evaluation')])

table(data.v2$eval_field)

data.v2 %>% 
  group_by(eval_field) %>% 
  count(unknown:polynesia)

data.v2 %>%
  gather()
  group_by(eval_field) %>%
  summarise(no = n()) %>%
  spread(eval_field, no)
# END TEST ANALYSES

# Tables
## Journal
journal.table <- table(data.v2$journal)

## Year
year.table <- table(data.v2$year)

## Evaluation / Field Use
eval.table <- table(data.v2$eval_field)

## Population Descriptions
population.table <- table(data.v2$pop_descript)

## Cohort Descriptions
cohort.table <- table(data.v2$cohort_descript)

## Sample Type
sample.table <- table(data.v2$sample_type)

## Assay Manufacturer
manufac.table <- data.v2.v2 %>%
                  group_by(eval_field) %>%
                  count(assay_manufact)


