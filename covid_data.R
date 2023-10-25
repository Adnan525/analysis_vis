library(readr)
library(dplyr)

url <- "https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_daily_reports/11-03-2021.csv"
covid_data <- read_csv(url)

a <- covid_data %>% 
  arrange(desc(Confirmed)) %>% 
  select(Province_State, Country_Region, Confirmed, Lat, Long_) %>% 
  slice_head(n = 20)

print(a[,1:3])

# coordinates <- data.frame(Lat = c(a$Lat, -35.2809), Long_ = c(a$Long_, 149.1300), name = c(a$Province_State, "Canberra"))
# globe()
# add_arcs(coordinates = coordinates, origin = "Canberra", color = "blue")

c <- covid_data %>% 
  group_by(Country_Region) %>% 
  summarise(total_confirmed = sum(Confirmed)) %>% 
  arrange(desc(total_confirmed)) %>% 
  slice_head(n = 10)

print(c)
