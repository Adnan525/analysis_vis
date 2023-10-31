library(ggplot2)
library(tidyr)
library(directlabels)

data <- read.csv("dataset_cholesterol.csv")
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

# slope
df <- data_long %>% 
  filter(Year == 2015 | Year == 2019) %>% 
  group_by(ResearchCentre) %>% 
  mutate(increase = round(((max(Percentage) - min(Percentage))/ min(Percentage)) * 100, 2)) %>% 
  mutate(risk_category = ifelse(increase <= 20, "low(<20)", ifelse(increase > 20 & increase < 40, "moderate(<40)", "high(>40)"))) %>% 
  mutate(label_show = ifelse(risk_category == "high(>40)" | risk_category == "moderate(<40)", paste("Test Group : ", ResearchCentre, ", Increase :", as.character(increase), "%"), ""))

ggplot(df, aes(x = Year, y = Percentage, color = risk_category, group = ResearchCentre)) +
  geom_line() +
  labs(title = "Cholesterol Rate in year 2015 and 2019", x = "Year", y = "Percentage") +
  # geom_dl(aes(label = ResearchCentre), method = list(dl.combine("first.points"), cex = 1, hjust = 2)) +
  geom_dl(aes(label = label_show), method = list(dl.combine("last.points"), hjust = -0.2)) +
  scale_color_manual(values = c("high(>40)" = "red", "low(<20)" = "green", "moderate(<40)" = "blue")) +
  theme_minimal()+
  theme(legend.position = "bottom")


