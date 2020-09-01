install.packages("readxl")

library(tidyverse)
library(readr)
library(readxl)
library(zoo)

library(readxl)
pop <- read_excel("data/PopulationEstimates.xls",
                                  skip = 2)

daily = read.csv("data/landdata-states.csv")

url = ('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv')
covid = read_csv(url)

covid_filter = covid %>%
    filter(state %in% state.name) %>%
    filter(date == max(date))

pop_filter = pop %>%
    select(fips = "FIPStxt", state = "State", "Area_Name", pop2019 = "POP_ESTIMATE_2019") %>%
    group_by(Area_Name) %>%
    slice_max(pop2019, n = 1) %>%
    ungroup()

info = covid

cumu = covid %>%
    filter(state == "California", date == max(date)) %>%
    group_by(county) %>%
    summarise(cases = sum(cases)) %>%
    ungroup() %>%
    mutate(newCases = cases - lag(cases)) %>%
    slice_max(newCases, n = 5) %>%
    select(county, cases)

knitr::kable(cumu, caption = "Most Cumulative Cases in CA Counties",
             col.names = c("County", "Cases"))

new_most_cases = info %>%
    filter(state == "California", date == max(date)) %>%
    group_by(county) %>%
    summarise(cases = sum(cases)) %>%
    ungroup() %>%
    mutate(newCases = cases - lag(cases)) %>%
    slice_max(newCases, n = 5) %>%
    select(county, newCases)

knitr::kable(new_most_cases, caption = "Most New COVID-19 Cases from CA Counties",
             col.names = c("County", "New Cases"))

covid_pop = covid_filter %>%
    right_join(pop_filter, by = c('county' = 'Area_Name')) %>%
    select(state.y, county, pop2019, cases) %>%
    mutate(cases_per_capita = cases/pop2019) %>%
    slice_max(cases, n = 5) %>%
    select (county, cases_per_capita, cases)

knitr::kable(covid_pop, caption = "Counties with the Most Cumulative Cases Per Capita",
             col.names = c("County", "Cases", "Cases Per Capita"))

most_cpc = covid_filter %>%
    right_join(pop_filter, by = c('county' = 'Area_Name')) %>%
    select(state.y, county, pop2019, cases) %>%
    mutate(newCases = cases - lag(cases)) %>%
    mutate(new_cpc = newCases/pop2019) %>%
    slice_max(newCases, n =5) %>%
    select(county, newCases, new_cpc)

knitr::kable(most_cpc, caption = "Counties with the Most New COVID-19 Cases Per Capita",
             col.names = c("County", "New Cases", "Cases Per Capita"))


two_weeks = covid %>%
    right_join(pop_filter, by = "fips") %>%
    select(state.y, county, pop2019, cases, date) %>%
    filter(date > max(date, na.rm = TRUE) - 14) %>%
    group_by(county) %>%
    mutate(newCases = cases - lag(cases)) %>%
    summarize(tot14 = sum(newCases, na.rm =TRUE) / (pop2019[1]/100000)) %>%
    slice_max(tot14, n = 5)

knitr::kable(two_weeks, caption = "Total Number of New Case per 100,000 people
             in the Last 14 Days",
             col.names = c("County", "Total Number of New Cases"))



# question 2

specific_sates = covid %>%
    filter(state %in% c('California', 'New York', 'Louisiana', 'Florida')) %>%
    group_by(date, state) %>%
    summarise(cases = sum(cases)) %>%
    group_by(state) %>%
    mutate(newCases = cases - lag(cases),
           roll7 = rollmean(newCases, 7, fill = NA, align="right"))

specific_sates %>%
    ggplot(aes(x = date, y = newCases, col = state)) +
    geom_col(aes(y = newCases), col = NA, fill = "green") +
    geom_line(aes(y = roll7), color = "darkred", size = 1) +
    facet_wrap(~state, scale = "free_y") +
    ggthemes::theme_gdocs() +
    theme(legend.position = "none") +
    labs(title = "Daily New COVID-19 Cases at the State Level",
         x = "Date",
         y = "New Cases",
         caption = "2020 Summer Session B for GEOG 176A",
         subtitle = "COVID-19 Data: NY-Times")

nc_tc = covid %>%
    right_join(pop_filter, by = "fips") %>%
    filter(state.x %in% c('California', 'New York', 'Louisiana', 'Florida')) %>%
    select(date, state.x, cases, pop2019) %>%
    group_by(date,state.x, pop2019) %>%
    summarise(cases = sum(cases)) %>%
    group_by(state.x) %>%
    mutate(newCases = cases - lag(cases)) %>%
    mutate(tot14 = sum(newCases, na.rm =TRUE) / pop2019) %>%
    mutate(roll7 = rollmean(tot14, 7, fill = NA, align="right"))


nc_tc %>%
    ggplot(aes(x = date , y = tot14)) +
    geom_col(aes(y = tot14), fill = "yellow", col = NA) +
    geom_line(aes(y = roll7), col = "blue", size = .5) +
    facet_wrap(~state.x, scale = "free_y") +
    ggthemes::theme_gdocs() +
    theme(legend.position = "none") +
    labs(title = "COVID-19 Cases Per Capita at the State Level",
         x = "Date",
         y = "Cases Per Capita",
         caption = "2020 Summer Session B for GEOG 176A",
         subtitle = "COVID-19 Data: NY-Times")








































