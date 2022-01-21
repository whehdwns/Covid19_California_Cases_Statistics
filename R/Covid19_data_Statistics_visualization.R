library(dplyr)
library(ggplot2)

setwd("C:/Users/dongj/Desktop/Covid_Cal/Statewide_case_statistics_and_demographics")

data <- read.csv("./dataset/covid19cases_test_012122.csv")
data_v1 <- read.csv("./dataset/covid19cases_test_012122.csv")

data_v1 <- data_v1[data_v1$area_type == 'County',]

data_v1 <- filter(data_v1,
                  !area %in% c('Out of state'))

unique(data_v1$area)

#Total Cases Group by Area-------------------------------------------------
total_cases_by_area <- aggregate(cases ~ area,
                                 data_v1,
                                 sum)
total_cases_by_area_graph <- ggplot(data=total_cases_by_area, 
                                    aes(x = cases, 
                                    y = reorder(area, 
                                                cases, 
                                                sum))
                                    )+
                            geom_bar(stat = "identity",
                                     width=.6,
                                     position = position_dodge(width = 0.5),
                                     fill = "#69c8ff")+
                            labs(
                              title="Total Cases by County",
                              y = "County"
                            )+
                            theme(axis.text.y = element_text(size = 7)) +
                            geom_text(aes(label=cases),
                                      hjust = -0.2, 
                                      size = 2.5,
                                      position = position_dodge(width = 1),
                                      inherit.aes = TRUE)
options(repr.plot.width = 14, repr.plot.height = 8)
total_cases_by_area_graph


#Total Death Group by Area-------------------------------------------------
total_deaths_by_area <- aggregate(deaths ~ area,
                                  data_v1,
                                  sum)
total_deaths_by_area_graph <- ggplot(data=total_deaths_by_area, 
                                    aes(x = deaths, 
                                        y = reorder(area, 
                                                    deaths, 
                                                    sum))
                                    )+
                              geom_bar(stat = "identity",
                                       width=.6,
                                       position = position_dodge(width = 0.5),
                                       fill = "#69c8ff")+
                              labs(
                                title="Total Deaths by County",
                                y = "County"
                              )+
                              theme(axis.text.y = element_text(size = 7)) +
                              geom_text(aes(label=deaths),
                                        hjust = -0.2, 
                                        size = 2.5,
                                        position = position_dodge(width = 1),
                                        inherit.aes = TRUE)
options(repr.plot.width = 14, repr.plot.height = 8)
total_deaths_by_area_graph



#Recent Date-------------------------------------------
RecentDate_end = data_v1$date[length(data_v1$date)-1]
RecentDate_start = data_v1$date[length(data_v1$date)-8]

recentdate_data_v1 <- data_v1[data_v1$date == RecentDate_end,]

#Newly Updated Cases-------------------------------------------------
newly_updated_cases_by_area <- aggregate(reported_cases ~ area,
                                         recentdate_data_v1,
                                         sum)
newly_updated_cases_by_area_graph <- ggplot(newly_updated_cases_by_area, 
                                            aes(x = reported_cases, 
                                                y = reorder(area, 
                                                            reported_cases, 
                                                            sum))
                                            )+
                                    geom_bar(stat = "identity",
                                             width=.6,
                                             position = position_dodge(width = 0.5),
                                             fill = "#69c8ff")+
                                    labs(
                                      title="Newly updated Cases by County",
                                      y = "County"
                                    )+
                                    theme(axis.text.y = element_text(size = 7)) +
                                    geom_text(aes(label=reported_cases),
                                              hjust = -0.2, 
                                              size = 2.5,
                                              position = position_dodge(width = 1),
                                              inherit.aes = TRUE)
options(repr.plot.width = 14, repr.plot.height = 8)
newly_updated_cases_by_area_graph


#Newly Updated Deaths-------------------------------------------------
newly_updated_deaths_by_area <- aggregate(reported_deaths ~ area,
                                          recentdate_data_v1,
                                          sum)
newly_updated_deaths_by_area_graph <- ggplot(data=newly_updated_deaths_by_area, 
                                            aes(x = reported_deaths, 
                                                y = reorder(area, 
                                                            reported_deaths, 
                                                            sum))
                                            )+
                                      geom_bar(stat = "identity",
                                               width=.6,
                                               position = position_dodge(width = 0.5),
                                               fill = "#69c8ff")+
                                      labs(
                                        title="Newly Updated Deaths by County",
                                        y = "County"
                                      )+
                                      theme(axis.text.y = element_text(size = 7)) +
                                      geom_text(aes(label=reported_deaths),
                                                hjust = 1, 
                                                size = 2.5,
                                                position = position_dodge(width = 1),
                                                inherit.aes = TRUE)
options(repr.plot.width = 14, repr.plot.height = 8)
newly_updated_deaths_by_area_graph

newly_population_by_area <- aggregate(population ~ area,recentdate_data_v1,max)
#c(total_cases_by_area$area)
#c(total_cases_by_area$cases)
#c(total_deaths_by_area$deaths)
#c(newly_population_by_area$population)
#c((total_cases_by_area$cases/newly_population_by_area$population)* 100000)

area <- c(total_cases_by_area$area)
cumulative_cases_per_100k <- c(round((total_cases_by_area$cases/newly_population_by_area$population)* 100000,2))
cumulative_deaths_per_100k <- c(round((total_deaths_by_area$deaths/newly_population_by_area$population)* 100000,2))

cumulative_per_100k <- data.frame(area, 
                                  cumulative_cases_per_100k,
                                  cumulative_deaths_per_100k)


#Cumulative Cases per 100k -----------------------------------------------------------
cumulative_cases_per_100k_graph <- ggplot(data=cumulative_per_100k, 
                                          aes(x = cumulative_cases_per_100k, 
                                              y = reorder(area, 
                                                          cumulative_cases_per_100k))
                                          )+
                                    geom_bar(stat = "identity",
                                             width=.6,
                                             position = position_dodge(width = 0.5),
                                             fill = "#69c8ff")+
                                    labs(
                                      title="Cumulative cases per 100k by County",
                                      y = "County"
                                    )+
                                    theme(axis.text.y = element_text(size = 7)) +
                                    geom_text(aes(label=cumulative_cases_per_100k),
                                              hjust = 0, 
                                              size = 2.5,
                                              position = position_dodge(width = 1),
                                              inherit.aes = TRUE)
options(repr.plot.width = 14, repr.plot.height = 8)
cumulative_cases_per_100k_graph


#Cumulative Deaths per 100k -----------------------------------------------------------
cumulative_deaths_per_100k_graph <- ggplot(data=cumulative_per_100k, 
                                          aes(x = cumulative_deaths_per_100k, 
                                              y = reorder(area, 
                                                          cumulative_deaths_per_100k))
                                          )+
                                    geom_bar(stat = "identity",
                                             width=.6,
                                             position = position_dodge(width = 0.5),
                                             fill = "#69c8ff")+
                                    labs(
                                      title = "Cumulative Deaths per 100k by County",
                                      y = "County"
                                    )+
                                    theme(axis.text.y = element_text(size = 7)) +
                                    geom_text(aes(label=cumulative_deaths_per_100k),
                                              hjust = 0, 
                                              size = 2.5,
                                              position = position_dodge(width = 1),
                                              inherit.aes = TRUE)
options(repr.plot.width = 14, repr.plot.height = 8)
cumulative_deaths_per_100k_graph

#Total by 7 days------------------------------------------------------

recentdate_data_v2 <- data_v1[data_v1$date >=RecentDate_start & data_v1$date<=RecentDate_end,]
head(recentdate_data_v2,5)

recentdate_data_v3 <- aggregate(cases ~ date, recentdate_data_v2,sum)
recentdate_data_v4 <- aggregate(deaths ~ date, recentdate_data_v2,sum)

date <- c(recentdate_data_v3$date)
Total_Cases_7days <- c(recentdate_data_v3$cases)
Total_Deaths_7days <- c(recentdate_data_v4$deaths)

covid_dataset_newly_dataset<- data.frame(date, 
                                         Total_Cases_7days,
                                         Total_Deaths_7days)
covid_dataset_newly_dataset

length(c(covid_dataset_newly_dataset$date))
cases_7day <- c()
deaths_7day <- c()
for (i in 1:length(c(covid_dataset_newly_dataset$date))){
  if (i == 1){
    cases_7day <- append(cases_7day, covid_dataset_newly_dataset$Total_Cases_7days[1])
    deaths_7day <- append(deaths_7day, covid_dataset_newly_dataset$Total_Deaths_7days[1])
  }else{
      cases_window = covid_dataset_newly_dataset$Total_Cases_7days[1:i]
      deaths_window = covid_dataset_newly_dataset$Total_Deaths_7days[1:i]
      cases_window_sum= sum(cases_window)
      deaths_window_sum= sum(deaths_window)
      cases_7day <- append(cases_7day, cases_window_sum)
      deaths_7day <- append(deaths_7day, deaths_window_sum)
  }
}


covid_dataset_newly_dataset$Total_Cases_7days_sum <- cases_7day
covid_dataset_newly_dataset$Total_Deaths_7days_sum <- deaths_7day
covid_dataset_newly_dataset

cases_7day_avg <- c()
deaths_7day_avg <- c()
for (i in 1:length(c(covid_dataset_newly_dataset$date))){
  cases_window_avg = covid_dataset_newly_dataset$Total_Cases_7days_sum[i] / (i)
  deaths_window_avg = covid_dataset_newly_dataset$Total_Deaths_7days_sum[i] / (i)
  cases_7day_avg <- append(cases_7day_avg, cases_window_avg)
  deaths_7day_avg <- append(deaths_7day_avg, deaths_window_avg)
}

Cases_per_100k_7_day_average = round((sum(cases_7day_avg)/7) / sum(newly_population_by_area$population)*100000,2)
Deaths_per_100k_7_day_average = round((sum(deaths_7day_avg)/7) / sum(newly_population_by_area$population)*100000,2)

Cases_per_100k_7_day_average


recentdate_data_v5 = subset(recentdate_data_v2, select = -c(area_type, cumulative_cases, cumulative_deaths,total_tests,
                                                            cumulative_total_tests, positive_tests, cumulative_positive_tests,
                                                            reported_cases, cumulative_reported_cases, reported_deaths,
                                                            cumulative_reported_deaths, reported_tests))
head(recentdate_data_v5,5)

extract_cases_7_days_average_per_100k <- function(area_name) {
  cases_7day <- c()
  cases_7day_avg <- c()
  county_toal_cases = recentdate_data_v5[recentdate_data_v5$area==area_name,]$cases
  population =  max(recentdate_data_v5[recentdate_data_v5$area==area_name,]$population)
  for (i in 1:length(c(covid_dataset_newly_dataset$date))){
    if (i == 1){
      cases_7day <- append(cases_7day, county_toal_cases[1])
    }else{
      cases_window = county_toal_cases[1:i]
      cases_window_sum= sum(cases_window)
      cases_7day <- append(cases_7day, cases_window_sum)
    }
  }
  for (i in 1:7){
    cases_window_avg = cases_7day[i] / (i)
    cases_7day_avg <- append(cases_7day_avg, cases_window_avg)
  }
  Cases_per_100k_7_day_average = round((sum(cases_7day_avg)/7) / population*100000,2)
  return(Cases_per_100k_7_day_average)
}

extract_deaths_7_days_average_per_100k <- function(area_name) {
  deaths_7day <- c()
  deaths_7day_avg <- c()
  county_toal_deaths = recentdate_data_v5[recentdate_data_v5$area==area_name,]$deaths
  population =  max(recentdate_data_v5[recentdate_data_v5$area==area_name,]$population)
  for (i in 1:length(c(covid_dataset_newly_dataset$date))){
    if (i == 1){
      deaths_7day <- append(deaths_7day, county_toal_deaths[1])
    }else{
      deaths_window = county_toal_deaths[1:i]
      deaths_window_sum= sum(deaths_window)
      deaths_7day <- append(deaths_7day, deaths_window_sum)
    }
  }
  for (i in 1:7){
    deaths_window_avg =  deaths_7day[i] / (i)
    deaths_7day_avg <- append(deaths_7day_avg, deaths_window_avg)
  }
  Deaths_per_100k_7_day_average = round((sum(deaths_7day_avg)/7) / population*100000,2)
  return(Deaths_per_100k_7_day_average )
}

extract_cases_7_days_average_per_100k("Alameda")
extract_deaths_7_days_average_per_100k("Alameda")

Area <- c()
Average_Cases_7_days <- c()
Average_Deaths_7_days <- c()
for (i in c(unique(recentdate_data_v5$area))){
  Area <- append(Area, i)
  Average_Cases_7_days <-append(Average_Cases_7_days, extract_cases_7_days_average_per_100k(i))
  Average_Deaths_7_days  <- append(Average_Deaths_7_days, extract_deaths_7_days_average_per_100k(i))
}

extract_7_days_average_per_100k<- data.frame(Area, 
                                             Average_Cases_7_days,
                                             Average_Deaths_7_days)
head(extract_7_days_average_per_100k,5)

extract_7_days_average_per_100k <- filter(extract_7_days_average_per_100k,
                                    !area %in% c('Unknown'))

#---------------extract_7_days_average_per_100k_graph (Cases)-----------------------------
extract_cases_7_days_average_per_100k_graph <- ggplot(data=extract_7_days_average_per_100k, 
                                                      aes(x = Average_Cases_7_days, 
                                                          y = reorder(Area, 
                                                                      Average_Cases_7_days))
                                                )+
                                              geom_bar(stat = "identity",
                                                       width=.6,
                                                       position = position_dodge(width = 0.5),
                                                       fill = "#69c8ff")+
                                              labs(
                                                title="Cases 7 day average (per 100k) by County",
                                                y = "County"
                                              )+
                                              theme(axis.text.y = element_text(size = 7)) +
                                              geom_text(aes(label=Average_Cases_7_days),
                                                        hjust = 0, 
                                                        size = 2.5,
                                                        position = position_dodge(width = 1),
                                                        inherit.aes = TRUE)
options(repr.plot.width = 14, repr.plot.height = 8)
extract_cases_7_days_average_per_100k_graph

#---------------extract_7_days_average_per_100k_graph (Deaths)-----------------------------
extract_deaths_7_days_average_per_100k_graph <- ggplot(data=extract_7_days_average_per_100k, 
                                                      aes(x = Average_Deaths_7_days, 
                                                          y = reorder(Area, 
                                                                      Average_Deaths_7_days))
                                                      )+
                                                geom_bar(stat = "identity",
                                                         width=.6,
                                                         position = position_dodge(width = 0.5),
                                                         fill = "#69c8ff")+
                                                labs(
                                                  title="Deaths 7 day average (per 100k) by County",
                                                  y = "County"
                                                )+
                                                theme(axis.text.y = element_text(size = 7)) +
                                                geom_text(aes(label=Average_Deaths_7_days),
                                                          hjust = 0, 
                                                          size = 2.5,
                                                          position = position_dodge(width = 1),
                                                          inherit.aes = TRUE)
options(repr.plot.width = 14, repr.plot.height = 8)
extract_deaths_7_days_average_per_100k_graph



#---------------Moving Average by Date-----------------------------
Total_Cases <- aggregate(cases ~ date, data_v1,sum)
Total_Cases <- Total_Cases[-1,]
row.names(Total_Cases) <- NULL
Total_Deaths <- aggregate(deaths ~ date, data_v1,sum)
Total_Deaths <- Total_Deaths[-1,]
row.names(Total_Deaths) <- NULL

date <- c(unique(data_v1$date))
date <- head(date, -1)
Total_Cases_7days <- c(Total_Cases$cases)
Total_Deaths_7days <- c(Total_Deaths$deaths)


covid_moving_average_dataset<- data.frame(date, 
                                         Total_Cases_7days,
                                         Total_Deaths_7days)
cases_7day  <- c()
deaths_7day <- c()
window_size =7
for (i in 1:length(c(covid_moving_average_dataset$date))){
  if (i ==1){
    cases_7day <- append(cases_7day, covid_moving_average_dataset$Total_Cases_7days[1])
    deaths_7day <- append(deaths_7day, covid_moving_average_dataset$Total_Deaths_7days[1])
  }else{
    cases_window = covid_moving_average_dataset$Total_Cases_7days[i:(window_size+i-1)]
    deaths_window = covid_moving_average_dataset$Total_Deaths_7days[i:(window_size+i-1)]
    cases_window_sum= sum(cases_window) / window_size
    deaths_window_sum= sum(deaths_window) / window_size
    cases_7day <- append(cases_7day, cases_window_sum) 
    deaths_7day <- append(deaths_7day, deaths_window_sum) 
  }
}

covid_moving_average_dataset$Cases_Moving_average_7days <- cases_7day
covid_moving_average_dataset$Deaths_Moving_average_7days <- deaths_7day
head(covid_moving_average_dataset, 5)


#---------------7_days_ moving_average cases_per_100k_graph-----------------------------
covid_moving_average_cases_per_100k_graph <- ggplot(covid_moving_average_dataset)+ 
                                             geom_bar(aes(x=date, 
                                                          y=Total_Cases_7days),
                                                      stat="identity", 
                                                      fill= "#69c8ff")+
                                             geom_line(aes(date, 
                                                           Cases_Moving_average_7days, 
                                                           group = 1),
                                                        col = "#0a0a0a", 
                                                       size=1)+
                                             labs(
                                                title="Cases per 100k (7-day average)",
                                                x = "Date"
                                              )+
                                              scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 4*25)])
options(repr.plot.width = 14, repr.plot.height = 8)
covid_moving_average_cases_per_100k_graph


#---------------7_days_ moving_average death_per_100k_graph-----------------------------
covid_moving_average_deaths_per_100k_graph <- ggplot(covid_moving_average_dataset)+ 
                                              geom_bar(aes(x=date, 
                                                           y=Total_Deaths_7days),
                                                       stat="identity", 
                                                       fill= "#69c8ff")+
                                              geom_line(aes(date, 
                                                            Deaths_Moving_average_7days, 
                                                            group = 1),
                                                        col = "#0a0a0a", 
                                                        size=1)+
                                              labs(
                                                title ="Deaths per 100k (7-day average)",
                                                x = "Date"
                                              )+
                                              scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 4*25)])
options(repr.plot.width = 14, repr.plot.height = 8)
covid_moving_average_deaths_per_100k_graph


Total_CA_Cases = sum(total_cases_by_area$cases)
Total_CA_Deaths = sum(total_deaths_by_area$deaths)
RecentCases = sum(data_v1[data_v1$date== RecentDate_end ,]$reported_cases)
RecentDeaths = sum(data_v1[data_v1$date==RecentDate_end, ]$reported_deaths)
Cases_percent = round((RecentCases/Total_CA_Cases)*100,4)
Deaths_percent = round((RecentDeaths/Total_CA_Deaths)*100,4)

cat("Cases (Statewide)", "\n",
    "Total US Cases : ", Total_CA_Cases, "\n", 
    "Covid19 cases in (", RecentDate_end ,"): ",RecentCases, "(+", Cases_percent, "%)", "\n",
    "Cases per 100k 7-day average (7 period ending", RecentDate_end ,"): ", Cases_per_100k_7_day_average, "\n",
    "-----------", "\n",
    "Deaths (Statewide)", "\n",
    "Total US Deaths : ", Total_CA_Deaths, "\n",
    "Covid19 Deaths in (", RecentDate_end ,"): ",RecentDeaths, "(+", Deaths_percent, "%)", "\n",
    "Deaths per 100k 7-day average (7 period ending", RecentDate_end ,"): ", Deaths_per_100k_7_day_average, "\n"
    )
