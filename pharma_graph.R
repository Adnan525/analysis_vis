library(ggplot2)
library(tidyr)

data <- data.frame(
  ResearchCentre = c("ALL", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M"),
  "2015" = c("7.2%", "2.5%", "10.7%", "9.2%", "8.3%", "7.9%", "8.7%", "3.7%", "8.4%", "5.1%", "6.4%", "9.2%", "9.9%", "5.6%"),
  "2016" = c("7.6%", "3.0%", "11.4%", "9.7%", "8.6%", "8.2%", "9.1%", "4.0%", "8.8%", "5.6%", "6.8%", "9.7%", "10.2%", "6.0%"),
  "2017" = c("8.0%", "3.4%", "11.9%", "10.2%", "9.0%", "8.5%", "9.6%", "4.3%", "9.2%", "6.0%", "7.1%", "10.1%", "11.0%", "6.2%"),
  "2018" = c("8.1%", "3.7%", "12.0%", "10.4%", "9.1%", "10.1%", "9.6%", "4.5%", "9.5%", "6.2%", "7.2%", "10.2%", "11.1%", "6.3%"),
  "2019" = c("8.6%", "4.4%", "12.5%", "10.9%", "9.5%", "11.3%", "10.1%", "5.0%", "10.0%", "6.7%", "7.6%", "10.5%", "11.6%", "6.7%")
)
colnames(data) <- c("ResearchCentre", "2015", "2016", "2017", "2018", "2019")

# Remove '%' signs and convert values to numeric
for (col in 2:ncol(data)) {
  data[, col] <- as.numeric(sub("%", "", data[, col]))
}

data_long <- gather(data, key = "Year", value = "Percentage", -ResearchCentre)

ggplot(data_long, aes(x = as.numeric(Year), y = Percentage, color = ResearchCentre, group = ResearchCentre)) +
  geom_line() +
  geom_point(shape = 15, size = 2)+
  labs(title = "Percentage by Research Centre Over the Years", x = "Year", y = "Percentage") +
  theme_minimal() +
  scale_x_continuous(breaks = 2015:2019)
