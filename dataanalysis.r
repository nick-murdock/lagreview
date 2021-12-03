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

# Data clean-up
str(data[,41:58])

## Viewing counts of each region for clean-up
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
  count(central_asia)

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

## Create new dataframe from original and make all regions into Yes (1) or No (0)
data.v2 <- data %>% 
  mutate("unknown" = ifelse(unknown == "Yes" &
                              !is.na(unknown), 1, 0)) %>%
  mutate("northern_africa" = ifelse(northern_africa == "Yes" &
                              !is.na(northern_africa), 1, 0)) %>%
  mutate("sub_saharan_africa" = ifelse(sub_saharan_africa == "Yes" &
                              !is.na(sub_saharan_africa), 1, 0)) %>%
  mutate("latin_america_caribbean" = ifelse(latin_america_caribbean == "Yes" &
                              !is.na(latin_america_caribbean), 1, 0)) %>%
  mutate("northern_america" = ifelse(northern_america == "Yes" &
                              !is.na(northern_america), 1,0)) %>%
  mutate("central_asia" = ifelse(central_asia == "Yes" &
                              !is.na(central_asia), 1, 0)) %>%
  mutate("eastern_asia" = ifelse(eastern_asia == "Yes" & 
                              !is.na(eastern_asia), 1, 0)) %>%
  mutate("south_eastern_asia" = ifelse(south_eastern_asia == "Yes" & 
                              !is.na(south_eastern_asia), 1, 0)) %>%
  mutate("southern_asia" = ifelse(southern_asia == "Yes" & 
                              !is.na(southern_asia), 1, 0)) %>%
  mutate("western_asia" = ifelse(western_asia == "Yes" &
                              !is.na(western_asia), 1,0)) %>%
  mutate("eastern_europe" = ifelse(eastern_europe == "Yes" & 
                              !is.na(eastern_europe), 1, 0)) %>%
  mutate("northern_europe" = ifelse(northern_europe == "Yes" & 
                              !is.na(northern_europe), 1, 0)) %>%
  mutate("southern_europe" = ifelse(southern_europe == "Yes" & 
                              !is.na(southern_europe), 1, 0)) %>%
  mutate("western_europe" = ifelse(western_europe == "Yes" & 
                              !is.na(western_europe), 1, 0)) %>%
  mutate("australia_new_zealand" = ifelse(australia_new_zealand == "Yes" & 
                              !is.na(australia_new_zealand), 1, 0)) %>%
  mutate("melanesia" = ifelse(melanesia == "Yes" & 
                              !is.na(melanesia), 1, 0)) %>%
  mutate("micronesia" = ifelse(micronesia == "Yes" & 
                              !is.na(micronesia), 1, 0)) %>%
  mutate("polynesia" = ifelse(polynesia == "Yes" & 
                                 !is.na(polynesia), 1, 0))

str(data.v2[,41:58])

## Make sure that the numbers are the same as the original
data.v2 %>% 
  count(unknown)

data.v2 %>% 
  count(northern_africa)

data.v2 %>% 
  count(sub_saharan_africa)

data.v2 %>% 
  count(latin_america_caribbean)

data.v2 %>% 
  count(northern_america)

data.v2 %>% 
  count(central_asia)

data.v2 %>% 
  count(eastern_asia)

data.v2 %>% 
  count(south_eastern_asia)

data.v2 %>% 
  count(southern_asia)

data.v2 %>% 
  count(western_asia)

data.v2 %>% 
  count(eastern_europe)

data.v2 %>% 
  count(northern_europe)

data.v2 %>% 
  count(southern_europe)

data.v2 %>% 
  count(western_europe)

data.v2 %>% 
  count(australia_new_zealand)

data.v2 %>% 
  count(melanesia)

data.v2 %>% 
  count(micronesia)

data.v2 %>% 
  count(polynesia)

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

  
## Create new df of just sub-geo regions to create plot of how many studies 
## were conducted where samples were collected
eval_regions <- data.v2 %>% group_by(eval_field) %>%
  select(eval_field, unknown, northern_africa, sub_saharan_africa, latin_america_caribbean,
         northern_america, central_asia, eastern_asia, south_eastern_asia, southern_asia, western_asia,
         eastern_europe, northern_europe, southern_europe, western_europe, australia_new_zealand, melanesia,
         micronesia, polynesia) 

eval_region_sum <- eval_regions %>% group_by(eval_field) %>%
  summarize(unknown = sum(unknown),northern_africa = sum(northern_africa), 
            sub_saharan_africa = sum(sub_saharan_africa), latin_america_caribbean = sum(latin_america_caribbean),
            northern_america = sum(northern_america), central_asia = sum(central_asia), 
            eastern_asia = sum(eastern_asia), south_eastern_asia = sum(south_eastern_asia), 
            southern_asia = sum(southern_asia), western_asia = sum(western_asia),
            eastern_europe = sum(eastern_europe), northern_europe = sum(northern_europe), 
            southern_europe = sum(southern_europe), western_europe = sum(western_europe),
            australia_new_zealand = sum(australia_new_zealand), melanesia = sum(melanesia),
            micronesia = sum(micronesia), polynesia = sum(polynesia))

gathered_eval_region <- eval_region_sum %>% 
  gather(key = "sub_geo", value = "count", 2:19)

ggplot(data = gathered_eval_region, aes(x = sub_geo, 
                                        y = count, 
                                        fill = eval_field)) +
  geom_bar(stat = "identity", position = "dodge", col = "white") +  
  labs(title = "Number of studies where samples were collected to conduct LAg studies",
       x = "Sub-Geographic Region", y = "Count (n)") +
  scale_fill_discrete(name = "Type of study", labels = c("Evaluation", "Field Use")) +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1),
        plot.title = element_text(hjust = 0.5))

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


