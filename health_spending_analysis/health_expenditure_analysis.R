library(tidyverse)

#I found this dataset through #TidyTuesday, a data science club that gives weekly datasets to play with!! They have been doing this CONSISTENTLY since 2018

#https://github.com/rfordatascience/tidytuesday/tree/main

#The latest data upload was a dataset on Global Health Spending from the WHO Global Health Expenditure Database (GHED) 

#The data tracks how much countries spend on health care, where the money comes from (government, private, or external sources), how it is channelled through financing schemes (e.g. government programmes, voluntary insurance, out-of-pocket payments), and what it is spent on (e.g. curative care, preventive care, medical goods). How health systems are funded and how resources are allocated directly shapes health outcomes, progress toward universal health coverage, and whether households face financial hardship when accessing care. These indicators are essential to inform health policy decisions.


#The data is organised in 3 main datasets:

financing_schemes <- read.csv("data/financing_schemes.csv")
health_spending <- read.csv("data/health_spending.csv")
spending_purpose <- read.csv("data/spending_purpose.csv")


str(financing_schemes)
str(health_spending)
str(spending_purpose)

#Details on each column can be found on the .md files in the data folder in our HackyHours gitgub https://github.com/BinghamtonBioHackyHours/Spring-2026-Hacky/tree/main/health_spending_analysis/data

#These links give some cool examples on the types of plots you can make using this (and maybe others) dataset
#https://data.one.org/analysis/health-spending-inequality
#https://data.one.org/analysis/hidden-fragility-of-prevention-spending
#https://data.one.org/analysis/african_gov_health-spending-off-track
#https://data.one.org/analysis/turning-point-in-health-financing

#Here are some questions that would be fun to answer by wrangling this dataset:
  
#Which countries rely most heavily on out-of-pocket payments for health care?

household_spending_che <- financing_schemes %>%
  filter(indicator_code == "hf3_che") %>%     # Filter by indicator code
  group_by(country_name, iso3_code) %>%       # Multiple years but we only want one
  slice_max(year, n = 1) %>%                  # Select only the latest year (2023)
  ungroup()

top_30 <- household_spending_che %>%
  slice_max(value, n = 30) %>%                # Get the top 30 countries in % of current health expenditure
  arrange(value) %>%                          # sorting(arranging) by % of current health expenditure
  mutate(country_name = factor(country_name, levels = country_name)) #Turn country name to factor in the order which they are arranged

ggplot(top_30, aes(x = value, y = country_name)) +
  geom_col(fill = "darkslateblue") +
  labs(
    title = "Countries With the Highest Out-of-Pocket Health Payments (OOP)",
    subtitle = "Household OOP as % of Current Health Expenditure (CHE) — 2023",
    x = "OOP as % of CHE",
    y = NULL,
    caption = "Source: WHO Global Health Expenditure Database"
  ) 

# What about the lowest 30?

min_30 <- household_spending_che %>%
  slice_min(value, n = 30) %>%                
  arrange(value) %>%                          
  mutate(country_name = factor(country_name, levels = country_name))

ggplot(min_30, aes(x = value, y = country_name)) +
  geom_col(fill = "darkslateblue") +
  labs(
    title = "Countries With the Lowest Out-of-Pocket Health Payments (OOP)",
    subtitle = "Household OOP as % of Current Health Expenditure (CHE) — 2023",
    x = "OOP as % of CHE",
    y = NULL,
    caption = "Source: WHO Global Health Expenditure Database"
  )


# That's nice but I kinda want to see the most populous countries in the world on that list


# The package wbstats is houses the World Bank API, a database with a LOT of data on all the countries in the world, AND it uses the same iso3_code for countries as this dataset

# https://cran.r-project.org/web/packages/wbstats/vignettes/wbstats.html
install.packages("wbstats")
library(wbstats)
# We can use this to filter for the top 30 most populous countries, get their iso3 code and filter our household spending df:

pop_2023_wbstats <- wb_data("SP.POP.TOTL", start_date = 2023, end_date = 2023) %>%
  slice_max(SP.POP.TOTL, n = 30) %>%
  pull(iso3c) 


populous_30 <- household_spending_che %>% 
  filter(iso3_code %in% pop_2023_wbstats) %>%   # filter by iso3_code
  arrange(value) %>%                            
  mutate(country_name = factor(country_name, levels = country_name))   


ggplot(populous_30, aes(x = value, y = country_name)) +
  geom_col(fill = "darkslateblue") +
  labs(
    title = "Top 30 Most Populous Countries Out-of-Pocket Health Payments (OOP)",
    subtitle = "Household OOP as % of Current Health Expenditure (CHE) — 2023",
    x = "OOP as % of CHE",
    y = NULL,
    caption = "Source: WHO Global Health Expenditure Database"
  )



#How has the balance between government, private, and external aid changed over time?

#What is the split between curative and preventive care spending across countries?

## Make a function that look at preventive care vs curative for a given country code

plot_cure_vs_prev <- function(country_iso){
  data_to_plot <- spending_purpose %>%
    filter(
      iso3_code == country_iso,
      indicator_code %in% c("hc1_che", "hc6_che"),
      unit == "% of current health expenditure"
    ) 
  
  country_label <- data_to_plot$country_name[1]
  
  ggplot(data_to_plot, aes(x = year, y = value, color = spending_purpose)) +
    geom_line(linewidth = 1) +
    scale_color_manual(values = c("Curative care" = "blue", "Preventive care" = "red")) +
    labs(
      title = paste("Curative vs. Preventive Care Spending —", country_label),
      subtitle = "As % of Current Health Expenditure",
      x = "Year",
      y = "% of CHE",
      color = NULL,
      caption = "Source: WHO Global Health Expenditure Database"
    )
  
}

plot_cure_vs_prev("USA")

#How did the COVID-19 pandemic impact health spending patterns?




