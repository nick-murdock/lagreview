# THIS FILE IS FOR ALL DATA ANALYSIS PORTIONS OF THE COVIDENCE REVIEW

##############################################################################
# List of analysis and plots to complete from MPH capstone                  
# Tables and plots should be grouped by evaluation or field use when appropriate
# - Assay manufacturer plot grouped by eval or field use 
#   - also need to group manufacturers the same as the mdri plot
# - any other tables and plots you think may be interesting or important     
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

## Cleaning up subtype_1 fields where it was blank instead of not defined
## All rows in subtype_1 column should have a value
data.v2 %>% count(subtype_1)
data.v2 %>% count(subtype_2)
data.v2 %>% count(subtype_3)
data.v2 %>% count(subtype_4)
data.v2 %>% count(subtype_5)
data.v2$subtype_1[(data.v2$subtype_1 == "")]<- "Not defined"

data.v2 %>% count(subtype_1)

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
  mutate("assay_manufact" = ifelse(assay_manufact == "CDC", "CDC",
                            ifelse(assay_manufact == "Sedia", "Sedia",
                            ifelse(assay_manufact == "Maxim", "Maxim",
                            ifelse(assay_manufact == "Other: Not defined", "Not defined",
                            ifelse(assay_manufact == "Sedia vs. Maxim", "Sedia vs. Maxim",
                            ifelse(assay_manufact == "Other: Sedia (serum/plasma) and Maxim (DBS)" |
                                   assay_manufact == "Other: Sedia and Maxim", "Sedia and Maxim",
                            ifelse(assay_manufact == "Other: Sedia or Maxim", "Sedia or Maxim",
                            "NA")))))))) %>%
                  count(assay_manufact)

ggplot(data = manufac.table, aes(x = reorder(assay_manufact, n), 
                                  y = n,
                                  fill = eval_field)) +
  geom_bar(stat = "identity", position = "dodge", size = 0) +
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
  geom_bar(stat = "identity", position = "dodge", size = 0) +  
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

### Reorganize df, relabel same subtypes together that were entered differently, remove missings
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
                     "NA")))))))))))) %>%
  filter(subtype != "NA")

gathered.subtype.v2 <- gathered.subtype.v2 %>% group_by(eval_field) %>%
  select(eval_field, subtype)

subtype.table <- as.data.frame(table(gathered.subtype.v2))

ggplot(data = subtype.table, aes(x = reorder(subtype, Freq), 
                                   y = Freq, 
                                   fill = eval_field)) +
  geom_bar(stat = "identity", position = "dodge", size = 0) +  
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

### Gather the mdri df, Relabel same subtypes together, and remove missings
gathered.mdri <- mdri %>% gather("column", "subtype", 3:7) %>% as.data.frame(table())
str(gathered.mdri)
gathered.mdri$subtype <- as.factor(gathered.mdri$subtype)

gathered.mdri.v2 <- gathered.mdri %>% 
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
                     "NA")))))))))))) %>%
  filter(subtype != "NA")

table(gathered.mdri.v2$subtype)
table(gathered.mdri.v2$assay_manufact, gathered.mdri.v2$eval_field)

### Relabel same assay manufacturer variables
gathered.mdri.v2 <- gathered.mdri.v2 %>% 
  mutate("assay_manufact" = ifelse(assay_manufact == "CDC", "CDC", 
                            ifelse(assay_manufact == "Sedia", "Sedia",
                            ifelse(assay_manufact == "Maxim", "Maxim",
                            ifelse(assay_manufact == "Other: Not defined", "Not defined",
                            ifelse(assay_manufact == "Sedia vs. Maxim", "Sedia vs. Maxim",
                            ifelse(assay_manufact == "Other: Sedia (serum/plasma) and Maxim (DBS)" |
                                     assay_manufact == "Other: Sedia and Maxim", "Sedia and Maxim",
                            "NA")))))))

### Create new df with assay manufacturer, subtype, and MDRI thresholds/algorithms
mdri.table <- gathered.mdri.v2 %>% group_by(subtype) %>%
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
  geom_bar(stat = "identity", position = "dodge", size = 0) +
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

### Gather the frr df, Relabel same subtypes together, and remove missings
gathered.frr <- frr %>% gather("column", "subtype", 3:7) %>% as.data.frame(table())
str(gathered.frr)
gathered.frr$subtype <- as.factor(gathered.frr$subtype)

gathered.frr.v2 <- gathered.frr %>% 
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
                      "NA")))))))))))) %>%
  filter(subtype != "NA")

table(gathered.frr.v2$subtype)
table(gathered.frr.v2$assay_manufact, gathered.frr.v2$eval_field)

### Relabel same assay manufacturer variables
gathered.frr.v2 <- gathered.frr.v2 %>% 
  mutate("assay_manufact" = ifelse(assay_manufact == "CDC", "CDC", 
                            ifelse(assay_manufact == "Sedia", "Sedia",
                            ifelse(assay_manufact == "Maxim", "Maxim",
                            ifelse(assay_manufact == "Other: Not defined", "Not defined",
                            ifelse(assay_manufact == "Sedia vs. Maxim", "Sedia vs. Maxim",
                            ifelse(assay_manufact == "Other: Sedia (serum/plasma) and Maxim (DBS)" |
                                   assay_manufact == "Other: Sedia and Maxim", "Sedia and Maxim",
                            "NA")))))))

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
  geom_bar(stat = "identity", position = "dodge", size = 0) +
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
