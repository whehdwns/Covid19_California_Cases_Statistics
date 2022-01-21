library(dplyr)
library(ggplot2)
library(scales)

setwd("C:/Users/dongj/Desktop/Covid_Cal/Statewide_case_statistics_and_demographics")

data <- read.csv("./dataset/covid19casesdemographics_012122.csv")
data_v1 <- read.csv("./dataset/covid19casesdemographics_012122.csv")


unique(data_v1$demographic_category)


age_group_data <- data_v1[data_v1$demographic_category =='Age Group',]
head(age_group_data,3)

age_group_data <- filter(age_group_data,
                        !demographic_value %in% c('Missing','missing','Total'))

age_filter<- age_group_data %>% 
            filter(demographic_value %in% c("0-17", "18-49", "50-64", "65+"))

age_filter %>%
  ggplot( aes(x=report_date, y=total_cases, group=demographic_value, color=demographic_value)) +
  geom_line()+
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 3*29)])+
  labs(
    title="Total Cases by Age"
  )+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

age_filter %>%
  ggplot( aes(x=report_date, y=percent_cases, group=demographic_value, color=demographic_value)) +
  geom_line()+
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 3*29)])+
  labs(
    title="Percent Cases by Age"
  )+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

age_filter %>%
  ggplot( aes(x=report_date, y=percent_deaths, group=demographic_value, color=demographic_value)) +
  geom_line()+
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 3*29)])+
  labs(
    title="Percent Deaths by Age"
  )+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


gender_group_data <- data_v1[data_v1$demographic_category =='Gender',]
unique(gender_group_data$demographic_value )
gender_group_data <- filter(gender_group_data,
                         !demographic_value %in% c("Unknown",'Total'))

gender_filter<- gender_group_data %>% 
  filter(demographic_value %in% c("Female", "Male"))

gender_filter %>%
  ggplot( aes(x=report_date, y=total_cases, group=demographic_value, color=demographic_value)) +
  geom_line()+
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 3*29)])+
  labs(
    title="Total Cases by Gender"
  )+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

gender_filter %>%
  ggplot( aes(x=report_date, y=percent_cases, group=demographic_value, color=demographic_value)) +
  geom_line()+
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 3*29)])+
  labs(
    title="Percent Cases by Gender"
  )+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

gender_filter %>%
  ggplot( aes(x=report_date, y=percent_deaths, group=demographic_value, color=demographic_value)) +
  geom_line()+
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 3*29)])+
  labs(
    title="Percent Deaths by Gender"
  )+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

race_group_data <- data_v1[data_v1$demographic_category =='Race Ethnicity',]
unique(race_group_data$demographic_value )
race_group_data <- filter(race_group_data,
                            !demographic_value %in% c('Total'))

race_filter<- race_group_data %>% 
  filter(demographic_value %in% c("American Indian or Alaska Native", "Asian","Black",
                                  "Latino", "Multi-Race", 
                                  "Native Hawaiian and other Pacific Islander",
                                  "Other","White"))

race_filter %>%
  ggplot( aes(x=report_date, y=total_cases, group=demographic_value, color=demographic_value)) +
  geom_line()+
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 3*29)])+
  labs(
    title="Total Cases by Race"
  )+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

race_filter %>%
  ggplot( aes(x=report_date, y=percent_cases, group=demographic_value, color=demographic_value)) +
  geom_line()+
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 3*29)])+
  labs(
    title="Percent Cases by Race"
  )+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

race_filter %>%
  ggplot( aes(x=report_date, y=percent_deaths, group=demographic_value, color=demographic_value)) +
  geom_line()+
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 3*29)])+
  labs(
    title="Percent Deaths by Race"
  )+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
