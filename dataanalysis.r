# THIS FILE IS FOR ALL DATA ANALYSIS PORTIONS OF THE COVIDENCE REVIEW

# Load packages
library(tidyr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(broom)

# Loading in the data

data <- read.csv("LAg_sys_review_data.csv")

# Viewing the beginning of the data
head(data)
str(data)

# Data clean-up
str(data[,41:58])

## Viewing counts of each region for clean-up
data %>% count(unknown)

data %>% count(northern_africa)

data %>% count(sub_saharan_africa)

data %>% count(latin_america_caribbean)

data %>% count(northern_america)

data %>% count(central_asia)

data %>% count(eastern_asia)

data %>% count(south_eastern_asia)

data %>% count(southern_asia)

data %>% count(western_asia)

data %>% count(eastern_europe)

data %>% count(northern_europe)

data %>% count(southern_europe)

data %>% count(western_europe)

data %>% count(australia_new_zealand)

data %>% count(melanesia)

data %>% count(micronesia)

data %>% count(polynesia)

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
data.v2 %>% count(unknown)

data.v2 %>% count(northern_africa)

data.v2 %>% count(sub_saharan_africa)

data.v2 %>% count(latin_america_caribbean)

data.v2 %>% count(northern_america)

data.v2 %>% count(central_asia)

data.v2 %>% count(eastern_asia)

data.v2 %>% count(south_eastern_asia)

data.v2 %>% count(southern_asia)

data.v2 %>% count(western_asia)

data.v2 %>% count(eastern_europe)

data.v2 %>% count(northern_europe)

data.v2 %>% count(southern_europe)

data.v2 %>% count(western_europe)

data.v2 %>% count(australia_new_zealand)

data.v2 %>% count(melanesia)

data.v2 %>% count(micronesia)

data.v2 %>% count(polynesia)

## Cleaning up subtype_1 fields where it was blank instead of not defined
## All rows in subtype_1 column should have a value
data.v2 %>% count(subtype_1)
data.v2 %>% count(subtype_2)
data.v2 %>% count(subtype_3)
data.v2 %>% count(subtype_4)
data.v2 %>% count(subtype_5)
data.v2$subtype_1[(data.v2$subtype_1 == "")]<- "Not defined"

data.v2 %>% count(subtype_1)
# Test Analyses 
hist(data.v2$year)
data.v2 %>% group_by(eval_field) %>%
  count()

count(data.v2[which(data.v2$eval_field == 'Evaluation')])

table(data.v2$eval_field)

data.v2 %>% group_by(eval_field) %>% 
  count(unknown:polynesia)

data.v2 %>%
  gather()
  group_by(eval_field) %>%
  summarise(no = n()) %>%
  spread(eval_field, no)
# END TEST ANALYSES

# Tables & Plots
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
manufac.table <- data.v2 %>%
                  group_by(eval_field) %>%
                  count(assay_manufact)

## Sub-geographic region where samples were collected from

### Create new df containing regions and eval/field use columns
regions <- data.v2 %>% group_by(eval_field) %>%
  select(eval_field, unknown, northern_africa, sub_saharan_africa, latin_america_caribbean,
         northern_america, central_asia, eastern_asia, south_eastern_asia, southern_asia, western_asia,
         eastern_europe, northern_europe, southern_europe, western_europe, australia_new_zealand, melanesia,
         micronesia, polynesia) 

### Count the number of specific region per evaluation or field use study
region.sum <- regions %>% group_by(eval_field) %>%
  summarize(unknown = sum(unknown),northern_africa = sum(northern_africa), 
            sub_saharan_africa = sum(sub_saharan_africa), latin_america_caribbean = sum(latin_america_caribbean),
            northern_america = sum(northern_america), central_asia = sum(central_asia), 
            eastern_asia = sum(eastern_asia), south_eastern_asia = sum(south_eastern_asia), 
            southern_asia = sum(southern_asia), western_asia = sum(western_asia),
            eastern_europe = sum(eastern_europe), northern_europe = sum(northern_europe), 
            southern_europe = sum(southern_europe), western_europe = sum(western_europe),
            australia_new_zealand = sum(australia_new_zealand), melanesia = sum(melanesia),
            micronesia = sum(micronesia), polynesia = sum(polynesia))

### Reorganize table and plot
gathered.region <- region.sum %>% 
  gather(key = "sub_geo", value = "count", 2:19)

ggplot(data = gathered.region, aes(x = reorder(sub_geo, count), 
                                        y = count, 
                                        fill = eval_field)) +
  geom_bar(stat = "identity", position = "dodge", col = "white") +  
  labs(title = "Number of studies where samples were collected to conduct LAg studies",
       x = "Sub-Geographic Region", y = "Count (n)") +
  scale_fill_discrete(name = "Type of study", labels = c("Evaluation", "Field Use")) +
  scale_x_discrete(labels = c("australia_new_zealand" = "Australia & New Zealand",
                              "central_asia" = "Central Asia",
                              "eastern_asia" = "Eastern Asia",
                              "eastern_europe" = "Eastern Europe",
                              "latin_america_caribbean" = "Latin American & Caribbean",
                              "melanesia" = "Melanesia",
                              "micronesia" = "Micronesia",
                              "northern_africa" = "Northern Africa",
                              "northern_america" = "Northern America",
                              "northern_europe" = "Northern Europe",
                              "polynesia" = "Polynesia",
                              "south_eastern_asia" = "South Eastern Asia",
                              "southern_asia" = "Southern Asia",
                              "southern_europe" = "Southern Europe",
                              "sub_saharan_africa" = "Sub-Saharan Africa",
                              "unknown" = "Unknown",
                              "western_asia" = "Western Asia",
                              "western_europe" = "Western Europe")) +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1),
        plot.title = element_text(hjust = 0.5))

## Subtype

### Create neew df containing subtype and eval/field use columns
subtype <- data.v2 %>% group_by(eval_field) %>%
  select(eval_field, subtype_1, subtype_2, subtype_3, subtype_4, subtype_5)

### Reorganize df, group same subtypes together that were entered differently, remove missings
gathered.subtype <- subtype %>% gather("column", "subtype", 2:6) %>% as.data.frame(table())

str(gathered.subtype)

gathered.subtype$subtype <- as.factor(gathered.subtype$subtype)

table(gathered.subtype$subtype)

gathered.subtype.v2 <- gathered.subtype %>% 
  mutate("subtype" = ifelse(subtype == "A" | subtype == "A1" &
                              (!(is.na(subtype) | subtype == "")), "A", 
                     ifelse(subtype == "A & D" & 
                              (!(is.na(subtype) | subtype == "")), "A & D",
                     ifelse(subtype == "AE" | subtype == "CRF01_AE" &
                              (!(is.na(subtype) | subtype == "")), "CRF01_AE",
                     ifelse(subtype == "B" &
                              (!(is.na(subtype) | subtype == "")), "B",
                     ifelse(subtype == "C" &
                              (!(is.na(subtype) | subtype == "")), "C",
                     ifelse(subtype == "C/BC" &
                              (!(is.na(subtype) | subtype == "")), "C/BC",
                     ifelse(subtype == "CRF35_AD" &
                              (!(is.na(subtype) | subtype == "")), "CRF35_AD",
                     ifelse(subtype == "D" &
                              (!(is.na(subtype) | subtype == "")), "D",
                     ifelse(subtype == "Multiple" &
                              (!(is.na(subtype) | subtype == "")), "Multiple",
                     ifelse(subtype == "Non-B" &
                              (!(is.na(subtype) | subtype == "")), "Non-B",
                     ifelse(subtype == "Not defined" &
                             (!(is.na(subtype) | subtype == "")), "Not defined",
                     "NA"))))))))))))

table(gathered.subtype.v2$subtype)

gathered.subtype.v2 <- gathered.subtype.v2[!(gathered.subtype.v2$subtype=="NA"), ]

gathered.subtype.v2 <- gathered.subtype.v2 %>% group_by(eval_field) %>%
  select(eval_field, subtype)

subtype.table <- as.data.frame(table(gathered.subtype.v2))

ggplot(data = subtype.table, aes(x = reorder(subtype, Freq), 
                                   y = Freq, 
                                   fill = eval_field)) +
  geom_bar(stat = "identity", position = "dodge", col = "white") +  
  labs(title = "Number of studies based on HIV-1 subtypes",
       x = "HIV-1 Subtypes", y = "Count (n)") +
  scale_fill_discrete(name = "Type of study", labels = c("Evaluation", "Field Use")) +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1),
        plot.title = element_text(hjust = 0.5))

## MDRI (JL working on separate branch)
