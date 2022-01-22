library(dplyr)
library(ggplot2)
library(scales)
library(ggrepel)

setwd("C:/Users/dongj/Desktop/Covid_Cal/The_impact_on_hospitals_in_the_CA")

data <- read.csv("./dataset/covid19hospitalbycounty_012122.csv")
data_v1 <- read.csv("./dataset/covid19hospitalbycounty_012122.csv")

positive_patients_by_county<- aggregate(hospitalized_covid_confirmed_patients ~ county,
                                        data_v1,sum)

positive_patients_by_date<- aggregate(hospitalized_covid_confirmed_patients ~ todays_date,
                                        data_v1,sum)

suspected_patients_by_county<- aggregate(hospitalized_suspected_covid_patients ~ county,
                                        data_v1,sum)

suspected_patients_by_date<- aggregate(hospitalized_suspected_covid_patients ~ todays_date,
                                      data_v1,sum)


icu_positive_patients_by_county<- aggregate(icu_covid_confirmed_patients ~ county,
                                        data_v1,sum)

icu_positive_patients_by_date<- aggregate(icu_covid_confirmed_patients ~ todays_date,
                                      data_v1,sum)

icu_suspected_patients_by_county<- aggregate(icu_suspected_covid_patients ~ county,
                                         data_v1,sum)

icu_suspected_patients_by_date<- aggregate(icu_suspected_covid_patients ~ todays_date,
                                       data_v1,sum)

icu_available_beds_by_county<- aggregate(icu_available_beds ~ county,
                                             data_v1,sum)

icu_available_beds_by_date<- aggregate(icu_available_beds ~ todays_date,
                                           data_v1,sum)

#---Visualization ---------------

positive_patients_by_county_graph <- ggplot(data=positive_patients_by_county, 
                                        aes(x = hospitalized_covid_confirmed_patients, 
                                            y = reorder(county, 
                                                        hospitalized_covid_confirmed_patients, 
                                                        sum))
                                    )+
                                    geom_bar(stat = "identity",
                                             width=.6,
                                             position = position_dodge(width = 0.5),
                                             fill = "#69c8ff")+
                                    labs(
                                      title="Positive Patients by County",
                                      x = "Positive Patients",
                                      y = "County"
                                    )+
                                    theme(axis.text.y = element_text(size = 7)) +
                                    geom_text(aes(label= hospitalized_covid_confirmed_patients),
                                              hjust = -0.2, 
                                              size = 2.5,
                                              position = position_dodge(width = 1),
                                              inherit.aes = TRUE)
options(repr.plot.width = 14, repr.plot.height = 8)
positive_patients_by_county_graph

positive_patients_by_date_graph <-ggplot(data =positive_patients_by_date)+
                                  geom_line(aes(x=todays_date, 
                                                  y=hospitalized_covid_confirmed_patients,
                                                group= 1),
                                            col = "#0a0a0a",
                                            size =1)+
                                  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 3*29)])+
                                  labs(
                                    title="Positive Patients by Date",
                                    x = "date",
                                    y = "Positive Patietns"
                                  )+
                                  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
positive_patients_by_date_graph


suspected_patients_by_county_graph <- ggplot(data=suspected_patients_by_county, 
                                            aes(x = hospitalized_suspected_covid_patients, 
                                                y = reorder(county, 
                                                            hospitalized_suspected_covid_patients, 
                                                            sum))
                                      )+
                                      geom_bar(stat = "identity",
                                               width=.6,
                                               position = position_dodge(width = 0.5),
                                               fill = "#69c8ff")+
                                      labs(
                                        title="Suspected Patients by County",
                                        x = "Supsected Patients",
                                        y = "County"
                                      )+
                                      theme(axis.text.y = element_text(size = 7)) +
                                      geom_text(aes(label= hospitalized_suspected_covid_patients),
                                                hjust = -0.2, 
                                                size = 2.5,
                                                position = position_dodge(width = 1),
                                                inherit.aes = TRUE)
options(repr.plot.width = 14, repr.plot.height = 8)
suspected_patients_by_county_graph


suspected_patients_by_date_graph <-ggplot(data =suspected_patients_by_date)+
                                  geom_line(aes(x=todays_date, 
                                                y=hospitalized_suspected_covid_patients,
                                                group= 1),
                                            col = "#0a0a0a",
                                            size =1)+
                                  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 3*29)])+
                                  labs(
                                    title="Suspected Patients by Date",
                                    x = "date",
                                    y = "Suspected Patients"
                                  )+
                                  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
suspected_patients_by_date_graph

icu_positive_patients_by_county_graph <- ggplot(data=icu_positive_patients_by_county, 
                                             aes(x = icu_covid_confirmed_patients, 
                                                 y = reorder(county, 
                                                             icu_covid_confirmed_patients, 
                                                             sum))
                                        )+
                                        geom_bar(stat = "identity",
                                                 width=.6,
                                                 position = position_dodge(width = 0.5),
                                                 fill = "#69c8ff")+
                                        labs(
                                          title="ICU Positive Patients by County",
                                          x = "ICU Positive Patients",
                                          y = "County"
                                        )+
                                        theme(axis.text.y = element_text(size = 7)) +
                                        geom_text(aes(label= icu_covid_confirmed_patients),
                                                  hjust = -0.2, 
                                                  size = 2.5,
                                                  position = position_dodge(width = 1),
                                                  inherit.aes = TRUE)
options(repr.plot.width = 14, repr.plot.height = 8)
icu_positive_patients_by_county_graph


icu_positive_patients_by_date_graph <-ggplot(data =icu_positive_patients_by_date)+
                                      geom_line(aes(x=todays_date, 
                                                    y=icu_covid_confirmed_patients,
                                                    group= 1),
                                                col = "#0a0a0a",
                                                size =1)+
                                      scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 3*29)])+
                                      labs(
                                        title="ICU Positive Patients by Date",
                                        x = "date",
                                        y = "ICU Positive Patients"
                                      )+
                                      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
icu_positive_patients_by_date_graph


icu_suspected_patients_by_county_graph <- ggplot(data=icu_suspected_patients_by_county, 
                                                aes(x = icu_suspected_covid_patients, 
                                                    y = reorder(county, 
                                                                icu_suspected_covid_patients, 
                                                                sum))
                                          )+
                                          geom_bar(stat = "identity",
                                                   width=.6,
                                                   position = position_dodge(width = 0.5),
                                                   fill = "#69c8ff")+
                                          labs(
                                            title="ICU Suspected Patients by County",
                                            x = "ICU Suspected Patients",
                                            y = "County"
                                          )+
                                          theme(axis.text.y = element_text(size = 7)) +
                                          geom_text(aes(label= icu_suspected_covid_patients),
                                                    hjust = -0.2, 
                                                    size = 2.5,
                                                    position = position_dodge(width = 1),
                                                    inherit.aes = TRUE)
options(repr.plot.width = 14, repr.plot.height = 8)
icu_suspected_patients_by_county_graph


icu_suspected_patients_by_date_graph <-ggplot(data =icu_suspected_patients_by_date)+
                                      geom_line(aes(x=todays_date, 
                                                    y=icu_suspected_covid_patients,
                                                    group= 1),
                                                col = "#0a0a0a",
                                                size =1)+
                                      scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 3*29)])+
                                      labs(
                                        title="ICU Suspected Patients by Date",
                                        x = "date",
                                        y = "ICU Suspected Patients"
                                      )+
                                      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
icu_suspected_patients_by_date_graph


icu_available_beds_by_county_graph <- ggplot(data=icu_available_beds_by_county, 
                                                 aes(x = icu_available_beds, 
                                                     y = reorder(county, 
                                                                 icu_available_beds, 
                                                                 sum))
                                    )+
                                      geom_bar(stat = "identity",
                                               width=.6,
                                               position = position_dodge(width = 0.5),
                                               fill = "#69c8ff")+
                                      labs(
                                        title="ICU Available Beds by County",
                                        x = "ICU Available Beds",
                                        y = "County"
                                      )+
                                      theme(axis.text.y = element_text(size = 7)) +
                                      geom_text(aes(label= icu_available_beds),
                                                hjust = -0.2, 
                                                size = 2.5,
                                                position = position_dodge(width = 1),
                                                inherit.aes = TRUE)
options(repr.plot.width = 14, repr.plot.height = 8)
icu_available_beds_by_county_graph 

icu_available_beds_by_date_graph <-ggplot(data =icu_available_beds_by_date)+
                                   geom_line(aes(x=todays_date, 
                                                 y=icu_available_beds,
                                                 group= 1),
                                              col = "#0a0a0a",
                                              size =1)+
                                    scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 3*29)])+
                                    labs(
                                      title="ICU Available Beds by Date",
                                      x = "date",
                                      y = "ICU Available Beds"
                                    )+
                                    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
icu_available_beds_by_date_graph



