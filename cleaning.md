Cleaning and preprocessing
================
Vitaly Lorman
3/25/2021

## Introduction

This file documents the process of downloading and processing the data
used in analyzing the effects of Larry Krasner’s tenure as Philadelphia
District Attorney on criminal charges. The analysis itself is written up
in the \`main.md’ file, and the introduction provides context for that
data processing carried out here.

## Reading in the data

The data we use is publicly availably and comes from the Philadelphia
District Attorney’s Office
(DAO).

``` r
arrests_URL<-"https://github.com/phillydao/phillydao-public-data/raw/master/docs/data/arrest_data_daily_citywide.csv"
download.file(arrests_URL, "arrests.csv")
arrests<-read.csv("arrests.csv")

charges_URL<-"https://github.com/phillydao/phillydao-public-data/raw/master/docs/data/charges_data_daily_citywide.csv"
download.file(charges_URL, "charges.csv")
charges<-read.csv("charges.csv")

arrests$date_value<-as.Date(arrests$date_value)
charges$date_value<-as.Date(charges$date_value)
```

## Preprocessing

We prepare the data for analysis via the following process:

### 1\. Group charge/arrest data by offense types

We group offenses for charges and arrests into 6 groups: violent,
property, drugs, firearms, other, and uncategorized. We then calculate
charge and arrest total in each of these categories.

``` r
violent_ca<-c("Homicide", "Non.Fatal.Shooting", "Rape", "Robbery.Gun",
           "Robbery.Other", "Aggravated.Assault.Gun", "Aggravated.Assault.Other",
           "Other.Assaults", "Sexual.Assault.and.Other.Sex.Offenses")
property_ca<-c("Burglary.Residential", "Burglary.Commercial", "Theft.of.Motor.Vehicle.Tag",
            "Theft.from.Person", "Theft.from.Auto", "Retail.Theft", "Theft",
            "Auto.Theft", "Fraud.Theft.of.Services", "Embezzlement")
drugs_ca<-c("Drug.Possession", "Drug.Sales", "DUI")
firearms_ca<-c("Illegal.Firearms.Possession")
other_ca<-c("Prostitution.Sex.Work", "Patronizing.Prostitutes", "Threats.of.Violence")
uncategorized_ca<-c("Uncategorized.Offenses")

arrests$arrests_violent<-rowSums(arrests[,violent_ca])
arrests$arrests_property<-rowSums(arrests[,property_ca])
arrests$arrests_drugs<-rowSums(arrests[,drugs_ca])
arrests$arrests_firearms<-arrests[,firearms_ca]
arrests$arrests_other<-rowSums(arrests[,other_ca])
arrests$arrests_uncategorized<-arrests[,uncategorized_ca]

charges$charges_violent<-rowSums(charges[,violent_ca])
charges$charges_property<-rowSums(charges[,property_ca])
charges$charges_drugs<-rowSums(charges[,drugs_ca])
charges$charges_firearms<-charges[,firearms_ca]
charges$charges_other<-rowSums(charges[,other_ca])
charges$charges_uncategorized<-charges[,uncategorized_ca]
```

### 2\. Merge charge and arrest data on common dates

We subset the charge and arrest data frames on common values of
date\_value and merge them by date\_value, keeping just the totals for
each of the 6 categories.

``` r
#Combining the data frames
arrest_dates<-unique(arrests$date_value)
charge_dates<-unique(charges$date_value)

common_dates<-as.Date(intersect(arrest_dates,charge_dates), origin='1970-01-01')

arrests<-subset(arrests, date_value %in% common_dates)
charges<-subset(charges, date_value %in% common_dates)

comb<-arrests[,c("date_value", "arrests_violent", "arrests_property","arrests_drugs",
                 "arrests_firearms", "arrests_other","arrests_uncategorized")] %>%
  merge(charges[,c("date_value", "charges_violent", "charges_property","charges_drugs",
                   "charges_firearms", "charges_other","charges_uncategorized")],
        by="date_value") 
```

### 3\. Filter to keep only dates of interest, assign units to treatment/control.

We filter to include only dates prior to 2017-06-29 and after
2018-01-01. We filter out dates after 2020-03-15 (to exclude judicial
actions after the COVID-19 pandemic began effecting the Philadelphia
courts). The reasoning behind this is discussed further in the
introduction of ‘main.md’. We create a binary treatment variable,
assigning it to be fALSE for dates prior to when Krasner took office and
TRUE for dates after.

``` r
comb_full<-comb
comb<-filter(comb, date_value <= "2017-06-28" | date_value>="2018-01-01")
comb<-filter(comb, date_value<"2020-03-15")
comb[comb$date_value>="2018-01-01","treatment"]<-TRUE
comb[comb$date_value<"2018-01-01", "treatment"]<-FALSE



write.csv(comb, "charges_all.csv")
write.csv(comb_full, "charges_all_full.csv")

charges_all<-comb
charges_all_full<-comb_full
```

## 4\. Reshape the data

We reshape into a long data frame with columns date\_value, type (arrest
or chage), group (violent, property, drugs, firearms, other, or
uncategorized) and the counts for each combination of the these.

``` r
#arrest_groups<-colnames(charges_all)[2:7]
#charges_groups<-colnames(charges_all)[8:13]
#incidents_groups<-colnames(charges_all)[14:18]


data_long<-charges_all%>%
  melt(id=c("date_value", "treatment"))
data_long_full<-charges_all_full%>%
  melt(id=c("date_value"))

#data_long$type<-NA
data_long[grep("^arrests", data_long$variable), "type"]<-"arrest"
data_long[grep("^charges", data_long$variable), "type"]<-"charge"

data_long_full[grep("^arrests", data_long_full$variable), "type"]<-"arrest"
data_long_full[grep("^charges", data_long_full$variable), "type"]<-"charge"
#data_long[grep("^incidents", data_long$variable), "type"]<-"incident"
#data_long$group<-NA
data_long[grep("violent$", data_long$variable), "group"]<-"violent"
data_long[grep("property$", data_long$variable), "group"]<-"property"
data_long[grep("drugs$", data_long$variable), "group"]<-"drugs"
data_long[grep("firearms$", data_long$variable), "group"]<-"firearms"
data_long[grep("other$", data_long$variable), "group"]<-"other"
data_long[grep("uncategorized$", data_long$variable), "group"]<-"uncategorized"

data_long_full[grep("violent$", data_long_full$variable), "group"]<-"violent"
data_long_full[grep("property$", data_long_full$variable), "group"]<-"property"
data_long_full[grep("drugs$", data_long_full$variable), "group"]<-"drugs"
data_long_full[grep("firearms$", data_long_full$variable), "group"]<-"firearms"
data_long_full[grep("other$", data_long_full$variable), "group"]<-"other"
data_long_full[grep("uncategorized$", data_long_full$variable), "group"]<-"uncategorized"


data_long<-select(data_long, "date_value", "type", "group", "value")
write.csv(data_long, "charges_all_long.csv")

data_long_full<-select(data_long_full, "date_value", "type", "group", "value")
write.csv(data_long_full, "charges_all_long_full.csv")
```
