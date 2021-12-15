# THIS FILE IS FOR ALL DATA ANALYSIS PORTIONS OF THE COVIDENCE REVIEW

##############################################################################
# List of analysis and plots to complete           
# Tables and plots should be grouped by evaluation or field use when appropriate
# - Other tables and plots you think may be interesting or important     
##############################################################################

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

## Get rid of \n in study purposes -- likely from hitting 'enter' key during covidance
data.v2$study_purpose <- gsub(pattern = "\n", replacement = "", 
                                           x = data.v2$study_purpose)

## Make mdri and frr variables into yes (1) or no (0)
data.v2 <- data.v2 %>% 
  mutate("mdri_1" = ifelse(mdri_1 == "Yes" &
                              !is.na(mdri_1), 1, 0)) %>%
  mutate("mdri_1_5" = ifelse(mdri_1_5 == "Yes" &
                                      !is.na(mdri_1_5), 1, 0)) %>%
  mutate("mdri_2" = ifelse(mdri_2 == "Yes" &
                                         !is.na(mdri_2), 1, 0)) %>%
  mutate("mdri_other" = ifelse(mdri_other == "Yes" &
                                              !is.na(mdri_other), 1, 0)) %>%
  mutate("mdri_1_vl_1000" = ifelse(mdri_1_vl_1000 == "Yes" &
                                       !is.na(mdri_1_vl_1000), 1,0)) %>%
  mutate("mdri_1_5_vl_1000" = ifelse(mdri_1_5_vl_1000 == "Yes" &
                                   !is.na(mdri_1_5_vl_1000), 1, 0)) %>%
  mutate("mdri_2_vl_1000" = ifelse(mdri_2_vl_1000 == "Yes" & 
                                   !is.na(mdri_2_vl_1000), 1, 0)) %>%
  mutate("mdri_vl_other" = ifelse(mdri_vl_other == "Yes" & 
                                         !is.na(mdri_vl_other), 1, 0)) %>%
  mutate("mdri_algorithm_other" = ifelse(mdri_algorithm_other == "Yes" & 
                                    !is.na(mdri_algorithm_other), 1, 0)) %>%
  mutate("frr_1" = ifelse(frr_1 == "Yes" &
                             !is.na(frr_1), 1, 0)) %>%
  mutate("frr_1_5" = ifelse(frr_1_5 == "Yes" &
                               !is.na(frr_1_5), 1, 0)) %>%
  mutate("frr_2" = ifelse(frr_2 == "Yes" &
                             !is.na(frr_2), 1, 0)) %>%
  mutate("frr_other" = ifelse(frr_other == "Yes" &
                                 !is.na(frr_other), 1, 0)) %>%
  mutate("frr_1_vl_1000" = ifelse(frr_1_vl_1000 == "Yes" &
                                     !is.na(frr_1_vl_1000), 1,0)) %>%
  mutate("frr_1_5_vl_1000" = ifelse(frr_1_5_vl_1000 == "Yes" &
                                       !is.na(frr_1_5_vl_1000), 1, 0)) %>%
  mutate("frr_2_vl_1000" = ifelse(frr_2_vl_1000 == "Yes" & 
                                     !is.na(frr_2_vl_1000), 1, 0)) %>%
  mutate("frr_vl_other" = ifelse(frr_vl_other == "Yes" & 
                                    !is.na(frr_vl_other), 1, 0)) %>%
  mutate("frr_algorithm_other" = ifelse(frr_algorithm_other == "Yes" & 
                                           !is.na(frr_algorithm_other), 1, 0)) 

## Rename assay manufacturer variables and group similar ones together
data.v2 <- data.v2 %>%
  mutate("assay_manufact" = ifelse(assay_manufact == "CDC", "CDC",
                            ifelse(assay_manufact == "Sedia", "Sedia",
                            ifelse(assay_manufact == "Maxim", "Maxim",
                            ifelse(assay_manufact == "Other: Not defined", "Not defined",
                            ifelse(assay_manufact == "Sedia vs. Maxim", "Sedia vs. Maxim",
                            ifelse(assay_manufact == "Other: Sedia (serum/plasma) and Maxim (DBS)" |
                                   assay_manufact == "Other: Sedia and Maxim", "Sedia and Maxim",
                            ifelse(assay_manufact == "Other: Sedia or Maxim", "Sedia or Maxim",
                            "NA"))))))))
table(data.v2$assay_manufact)

## Cleaning up subtype_1 fields where it was blank instead of not defined
## All rows in subtype_1 column should have a value
data.v2 %>% count(subtype_1)
data.v2 %>% count(subtype_2)
data.v2 %>% count(subtype_3)
data.v2 %>% count(subtype_4)
data.v2 %>% count(subtype_5)
data.v2$subtype_1[(data.v2$subtype_1 == "")]<- "Not defined"

data.v2 %>% count(subtype_1)

## Rename and group similar subtypes for each subtype column
### NOTE: Grouped subtypes A and A1 (A1 is a sub-subtype)
data.v2 <- data.v2 %>% 
  mutate("subtype_1" = ifelse(subtype_1 == "A" | subtype_1 == "A1" &
                            (!(is.na(subtype_1) | subtype_1 == "")), "A", 
                       ifelse(subtype_1 == "A & D" & 
                            (!(is.na(subtype_1) | subtype_1 == "")), "A & D",
                       ifelse(subtype_1 == "AE" | subtype_1 == "CRF01_AE" &
                            (!(is.na(subtype_1) | subtype_1 == "")), "CRF01_AE",
                       ifelse(subtype_1 == "B" &
                            (!(is.na(subtype_1) | subtype_1 == "")), "B",
                       ifelse(subtype_1 == "C" &
                            (!(is.na(subtype_1) | subtype_1 == "")), "C",
                       ifelse(subtype_1 == "C/BC" &
                            (!(is.na(subtype_1) | subtype_1 == "")), "C/BC",
                       ifelse(subtype_1 == "CRF35_AD" &
                            (!(is.na(subtype_1) | subtype_1 == "")), "CRF35_AD",
                       ifelse(subtype_1 == "D" &
                            (!(is.na(subtype_1) | subtype_1 == "")), "D",
                       ifelse(subtype_1 == "Multiple" &
                            (!(is.na(subtype_1) | subtype_1 == "")), "Multiple",
                       ifelse(subtype_1 == "Non-B" &
                            (!(is.na(subtype_1) | subtype_1 == "")), "Non-B",
                       ifelse(subtype_1 == "Not defined" &
                            (!(is.na(subtype_1) | subtype_1 == "")), "Not defined",
                       "NA")))))))))))) %>% 
  mutate("subtype_2" = ifelse(subtype_2 == "A" | subtype_2 == "A1" &
                                (!(is.na(subtype_2) | subtype_2 == "")), "A", 
                       ifelse(subtype_2 == "A & D" & 
                                (!(is.na(subtype_2) | subtype_2 == "")), "A & D",
                       ifelse(subtype_2 == "AE" | subtype_2 == "CRF01_AE" &
                                (!(is.na(subtype_2) | subtype_2 == "")), "CRF01_AE",
                       ifelse(subtype_2 == "B" &
                                (!(is.na(subtype_2) | subtype_2 == "")), "B",
                       ifelse(subtype_2 == "C" &
                                (!(is.na(subtype_2) | subtype_2 == "")), "C",
                       ifelse(subtype_2 == "C/BC" &
                                (!(is.na(subtype_2) | subtype_2 == "")), "C/BC",
                       ifelse(subtype_2 == "CRF35_AD" &
                                (!(is.na(subtype_2) | subtype_2 == "")), "CRF35_AD",
                       ifelse(subtype_2 == "D" &
                                (!(is.na(subtype_2) | subtype_2 == "")), "D",
                       ifelse(subtype_2 == "Multiple" &
                                (!(is.na(subtype_2) | subtype_2 == "")), "Multiple",
                       ifelse(subtype_2 == "Non-B" &
                                (!(is.na(subtype_2) | subtype_2 == "")), "Non-B",
                       ifelse(subtype_2 == "Not defined" &
                                (!(is.na(subtype_2) | subtype_2 == "")), "Not defined",
                       "NA")))))))))))) %>% 
  mutate("subtype_3" = ifelse(subtype_3 == "A" | subtype_3 == "A1" &
                                (!(is.na(subtype_3) | subtype_3 == "")), "A", 
                       ifelse(subtype_3 == "A & D" & 
                                (!(is.na(subtype_3) | subtype_3 == "")), "A & D",
                       ifelse(subtype_3 == "AE" | subtype_3 == "CRF01_AE" &
                                (!(is.na(subtype_3) | subtype_3 == "")), "CRF01_AE",
                       ifelse(subtype_3 == "B" &
                                (!(is.na(subtype_3) | subtype_3 == "")), "B",
                       ifelse(subtype_3 == "C" &
                                (!(is.na(subtype_3) | subtype_3 == "")), "C",
                       ifelse(subtype_3 == "C/BC" &
                                (!(is.na(subtype_3) | subtype_3 == "")), "C/BC",
                       ifelse(subtype_3 == "CRF35_AD" &
                                 (!(is.na(subtype_3) | subtype_3 == "")), "CRF35_AD",
                       ifelse(subtype_3 == "D" &
                                (!(is.na(subtype_3) | subtype_3 == "")), "D",
                       ifelse(subtype_3 == "Multiple" &
                                (!(is.na(subtype_3) | subtype_3 == "")), "Multiple",
                       ifelse(subtype_3 == "Non-B" &
                                (!(is.na(subtype_3) | subtype_3 == "")), "Non-B",
                       ifelse(subtype_3 == "Not defined" &
                                (!(is.na(subtype_3) | subtype_3 == "")), "Not defined",
                       "NA")))))))))))) %>% 
  mutate("subtype_4" = ifelse(subtype_4 == "A" | subtype_4 == "A1" &
                                (!(is.na(subtype_4) | subtype_4 == "")), "A", 
                       ifelse(subtype_4 == "A & D" & 
                                (!(is.na(subtype_4) | subtype_4 == "")), "A & D",
                       ifelse(subtype_4 == "AE" | subtype_4 == "CRF01_AE" &
                                (!(is.na(subtype_4) | subtype_4 == "")), "CRF01_AE",
                       ifelse(subtype_4 == "B" &
                                (!(is.na(subtype_4) | subtype_4 == "")), "B",
                       ifelse(subtype_4 == "C" &
                                (!(is.na(subtype_4) | subtype_4 == "")), "C",
                       ifelse(subtype_4 == "C/BC" &
                                (!(is.na(subtype_4) | subtype_4 == "")), "C/BC",
                       ifelse(subtype_4 == "CRF35_AD" &
                                (!(is.na(subtype_4) | subtype_4 == "")), "CRF35_AD",
                       ifelse(subtype_4 == "D" &
                                (!(is.na(subtype_4) | subtype_4 == "")), "D",
                       ifelse(subtype_4 == "Multiple" &
                                (!(is.na(subtype_4) | subtype_4 == "")), "Multiple",
                       ifelse(subtype_4 == "Non-B" &
                                (!(is.na(subtype_4) | subtype_4 == "")), "Non-B",
                       ifelse(subtype_4 == "Not defined" &
                                (!(is.na(subtype_4) | subtype_4 == "")), "Not defined",
                       "NA")))))))))))) %>% 
  mutate("subtype_5" = ifelse(subtype_5 == "A" | subtype_5 == "A1" &
                                (!(is.na(subtype_5) | subtype_5 == "")), "A", 
                       ifelse(subtype_5 == "A & D" & 
                                (!(is.na(subtype_5) | subtype_5 == "")), "A & D",
                       ifelse(subtype_5 == "AE" | subtype_5 == "CRF01_AE" &
                                (!(is.na(subtype_5) | subtype_5 == "")), "CRF01_AE",
                       ifelse(subtype_5 == "B" &
                                (!(is.na(subtype_5) | subtype_5 == "")), "B",
                       ifelse(subtype_5 == "C" &
                                (!(is.na(subtype_5) | subtype_5 == "")), "C",
                       ifelse(subtype_5 == "C/BC" &
                                (!(is.na(subtype_5) | subtype_5 == "")), "C/BC",
                       ifelse(subtype_5 == "CRF35_AD" &
                                (!(is.na(subtype_5) | subtype_5 == "")), "CRF35_AD",
                       ifelse(subtype_5 == "D" &
                                (!(is.na(subtype_5) | subtype_5 == "")), "D",
                       ifelse(subtype_5 == "Multiple" &
                                (!(is.na(subtype_5) | subtype_5 == "")), "Multiple",
                       ifelse(subtype_5 == "Non-B" &
                                (!(is.na(subtype_5) | subtype_5 == "")), "Non-B",
                       ifelse(subtype_5 == "Not defined" &
                                (!(is.na(subtype_5) | subtype_5 == "")), "Not defined",
                       "NA"))))))))))))

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
sample.table <- data.v2 %>%
  group_by(eval_field) %>% count(sample_type)

ggplot(data = sample.table, aes(x = reorder(sample_type, n), 
                                 y = n,
                                 fill = eval_field)) +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve = "single"), size = 0) +
  labs(title = "Number of studies by sample type",
       x = "Sample type", y = "Count (n)") +
  scale_fill_discrete(name = "Assay manufacturer") +
  scale_x_discrete(labels = c("Dried blood spot only" = "DBS",
                              "Plasma/serum and/or dried blood spot" = "Plasma/serum and/or DBS",
                              "Matched plasma/serum and dried blood spot" = "Matched plasma/serum and DBS",
                              "Plasma/serum only" = "Plasma/serum",
                              "Other: Not defined" = "Not defined")) +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1),
        plot.title = element_text(hjust = 0.5))

## Assay Manufacturer
manufac.table <- data.v2 %>%
                  group_by(eval_field) %>% 
                  count(assay_manufact)

ggplot(data = manufac.table, aes(x = reorder(assay_manufact, n), 
                                  y = n,
                                  fill = eval_field)) +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve = "single"), size = 0) +
  labs(title = "Number of studies using specific CDC-approved LAg manufacturing kits",
       x = "Assay manufacturer", y = "Count (n)") +
  scale_fill_discrete(name = "Assay manufacturer") +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1),
        plot.title = element_text(hjust = 0.5))

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
  gather(key = "sub_geo", value = "n", 2:19)

ggplot(data = gathered.region, aes(x = reorder(sub_geo, n), 
                                        y = n, 
                                        fill = eval_field)) +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve = "single"), size = 0) +  
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

### Reorganize df for subtype data, remove missings, and plot
gathered.subtype <- subtype %>% gather("column", "subtype", 2:6) %>% as.data.frame(table())

str(gathered.subtype)

gathered.subtype$subtype <- as.factor(gathered.subtype$subtype)

table(gathered.subtype$subtype)

gathered.subtype.v2 <- gathered.subtype %>% 
  filter(subtype != "NA")

gathered.subtype.v2 <- gathered.subtype.v2 %>% group_by(eval_field) %>%
  select(eval_field, subtype)

subtype.table <- as.data.frame(table(gathered.subtype.v2))

ggplot(data = subtype.table, aes(x = reorder(subtype, Freq), 
                                   y = Freq, 
                                   fill = eval_field)) +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve = "single"), size = 0) +  
  labs(title = "Number of studies based on HIV-1 subtypes",
       x = "HIV-1 Subtypes", y = "Count (n)") +
  scale_fill_discrete(name = "Type of study", labels = c("Evaluation", "Field Use")) +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1),
        plot.title = element_text(hjust = 0.5))

## MDRI

### Checking mdri, frr, assay manufacturer variables have expected responses
str(data.v2[,21:38])
str(data.v2[,13:16])
data.v2 %>% count(assay_manufact)
table(data.v2$assay_manufact, data.v2$eval_field)

### Keep only relevant variables and filter for only evaluation studies
mdri <- data.v2 %>% group_by(assay_manufact) %>%
  select(assay_manufact, eval_field, subtype_1, subtype_2, subtype_3, subtype_4, subtype_5, 
         mdri_1, mdri_1_5, mdri_2, mdri_other, mdri_1_vl_1000, mdri_1_5_vl_1000, mdri_2_vl_1000,
         mdri_vl_other, mdri_algorithm_other) %>%
  filter(eval_field == "Evaluation")

### Gather the mdri df and remove missings
gathered.mdri <- mdri %>% gather("column", "subtype", 3:7) %>% as.data.frame(table())
str(gathered.mdri)
gathered.mdri$subtype <- as.factor(gathered.mdri$subtype)

gathered.mdri <- gathered.mdri %>% 
  filter(subtype != "NA")

table(gathered.mdri$subtype)
table(gathered.mdri$assay_manufact, gathered.mdri$eval_field)

### Create new df with assay manufacturer, subtype, and MDRI thresholds/algorithms
mdri.table <- gathered.mdri %>% group_by(subtype) %>%
  select(assay_manufact, subtype, mdri_1, mdri_1_5, mdri_2, mdri_other, 
         mdri_1_vl_1000, mdri_1_5_vl_1000, mdri_2_vl_1000,
         mdri_vl_other, mdri_algorithm_other)

### Group by assay manufacturer and subtype, then sum by each mdri threshold/algorithm
mdri.sum <- mdri.table %>% group_by(assay_manufact, subtype) %>%
  summarize(mdri_1 = sum(mdri_1), mdri_1_5 = sum(mdri_1_5), mdri_2 = sum(mdri_2), 
            mdri_other = sum(mdri_other), mdri_1_vl_1000 = sum(mdri_1_vl_1000), 
            mdri_1_5_vl_1000 = sum(mdri_1_5_vl_1000), mdri_2_vl_1000 = sum(mdri_2_vl_1000), 
            mdri_vl_other = sum(mdri_vl_other), mdri_algorithm_other = sum(mdri_algorithm_other))

### Gather the df and filter out rows without any mdri threshold/algorithm counts
mdri.sum.gather <- mdri.sum %>% gather("mdri_threshold", "n", 3:11) %>% 
  as.data.frame(table()) %>% filter(n != 0)

### Plot the mdri df
ggplot(data = mdri.sum.gather, aes(x = mdri_threshold, 
                                   y = n,
                                   fill = assay_manufact)) +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve = "single"), size = 0) +
  facet_wrap(~ subtype, ncol = 2) +  
  labs(title = "Number of LAg algorithms and thresholds evaluated for estimating MDRI 
       based on assay manufacturer and HIV-1 subtype",
       x = "MDRI thresholds and algorithms", y = "Count (n)") +
  scale_x_discrete(labels = c("mdri_1" = "ODn < 1",
                              "mdri_1_5" = "ODn < 1.5",
                              "mdri_2" = "ODn < 2",
                              "mdri_other" = "Other ODn",
                              "mdri_1_vl_1000" = "ODn < 1 + VL < 1000",
                              "mdri_1_5_vl_1000" = "ODn < 1.5 + VL < 1000",
                              "mdri_2_vl_1000" = "ODn < 2 + VL < 1000",
                              "mdri_vl_other" = "Other ODn + VL",
                              "mdri_algorithm_other" = "Other algorithm")) +
  scale_fill_manual(name = "Assay manufacturer",
                    values = c("CDC" = "cornflowerblue",
                               "Not defined" = "grey",
                               "Sedia" = "seagreen3",
                               "Sedia and Maxim" = "blueviolet",
                               "Sedia vs. Maxim" = "tomato")) +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1),
        plot.title = element_text(hjust = 0.5))

## FRR

### Checking mdri, frr, assay manufacturer variables have expected responses
str(data.v2[,21:38])
str(data.v2[,13:16])
data.v2 %>% count(assay_manufact)
table(data.v2$assay_manufact, data.v2$eval_field)

### Keep only relevant variables and filter for only evaluation studies
frr <- data.v2 %>% group_by(assay_manufact) %>%
  select(assay_manufact, eval_field, subtype_1, subtype_2, subtype_3, subtype_4, subtype_5, 
         frr_1, frr_1_5, frr_2, frr_other, frr_1_vl_1000, frr_1_5_vl_1000, frr_2_vl_1000,
         frr_vl_other, frr_algorithm_other) %>%
  filter(eval_field == "Evaluation")

### Gather the frr df and remove missings
gathered.frr <- frr %>% gather("column", "subtype", 3:7) %>% as.data.frame(table())
str(gathered.frr)
gathered.frr$subtype <- as.factor(gathered.frr$subtype)

gathered.frr.v2 <- gathered.frr %>% 
  filter(subtype != "NA")

table(gathered.frr.v2$subtype)
table(gathered.frr.v2$assay_manufact, gathered.frr.v2$eval_field)

### Create new df with assay manufacturer, subtype, and frr thresholds/algorithms
frr.table <- gathered.frr.v2 %>% group_by(subtype) %>%
  select(assay_manufact, subtype, frr_1, frr_1_5, frr_2, frr_other, 
         frr_1_vl_1000, frr_1_5_vl_1000, frr_2_vl_1000,
         frr_vl_other, frr_algorithm_other)

### Group by assay manufacturer and subtype, then sum by each frr threshold/algorithm
frr.sum <- frr.table %>% group_by(assay_manufact, subtype) %>%
  summarize(frr_1 = sum(frr_1), frr_1_5 = sum(frr_1_5), frr_2 = sum(frr_2), 
            frr_other = sum(frr_other), frr_1_vl_1000 = sum(frr_1_vl_1000), 
            frr_1_5_vl_1000 = sum(frr_1_5_vl_1000), frr_2_vl_1000 = sum(frr_2_vl_1000), 
            frr_vl_other = sum(frr_vl_other), frr_algorithm_other = sum(frr_algorithm_other))

### Gather the df and filter out rows without any frr threshold/algorithm counts
frr.sum.gather <- frr.sum %>% gather("frr_threshold", "n", 3:11) %>% 
  as.data.frame(table()) %>% filter(n != 0)

### Plot the frr df
ggplot(data = frr.sum.gather, aes(x = frr_threshold, 
                                   y = n,
                                   fill = assay_manufact)) +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve = "single"), size = 0) +
  facet_wrap(~ subtype, ncol = 2) +  
  labs(title = "Number of LAg algorithms and thresholds evaluated for estimating frr 
       based on assay manufacturer and HIV-1 subtype",
       x = "frr thresholds and algorithms", y = "Count (n)") +
  scale_x_discrete(labels = c("frr_1" = "ODn < 1",
                              "frr_1_5" = "ODn < 1.5",
                              "frr_2" = "ODn < 2",
                              "frr_other" = "Other ODn",
                              "frr_1_vl_1000" = "ODn < 1 + VL < 1000",
                              "frr_1_5_vl_1000" = "ODn < 1.5 + VL < 1000",
                              "frr_2_vl_1000" = "ODn < 2 + VL < 1000",
                              "frr_vl_other" = "Other ODn + VL",
                              "frr_algorithm_other" = "Other algorithm")) +
  scale_fill_manual(name = "Assay manufacturer",
                    values = c("CDC" = "cornflowerblue",
                               "Not defined" = "grey",
                               "Sedia" = "seagreen3",
                               "Maxim" = "darkgoldenrod1",
                               "Sedia vs. Maxim" = "tomato")) +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1),
        plot.title = element_text(hjust = 0.5))

## LAg studies that compared against traditional/other HIV incidence measurements
table(data.v2$study_purpose)
incidence.comparison <- data.v2 %>% filter(grepl(pattern = "HIV incidence", x = study_purpose)) %>%
  filter(!grepl(pattern = "ICAP", x = journal)) #%>% 
#  filter(grepl(pattern = "Comparison", x = study_purpose))

table(incidence.comparison$study_purpose)
