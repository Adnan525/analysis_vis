library(lubridate)
library(dplyr)
library(readr)
library(ggplot2)
require(reshape2)
library(scales)

df <- data.frame(
  Date = c("2019-01", "2019-02", "2019-03", "2019-04", "2019-05", "2019-06", "2019-07", "2019-08", "2019-09", "2019-10", "2019-11", "2019-12"), 
  DirectSales = c(88.2, 76.3, 47.8, 76.1, 71.4, 58.6, 79.9, 69.4, 53.9, 80.8, 74.3, 46.4), 
  IndirectSales = c(82.2, 71.4, 88.7, 81.0, 88.4, 120.2, 83.5, 73.8, 98.0, 85.1, 94.4, 64.8))
df <- melt(df, "Date")
df <- df %>% mutate(status = ifelse(value <= 90, "pass", "fail"))

ggplot(df, aes(x = Date, y = value, fill = variable, color = status))+
  geom_bar(stat = "identity", position = "dodge", size = 1)+
  labs(title = "Number of Days(Average) Taken to Close A Deal by Month, Goal = 90 Days", x = "Month", y = "Number of Days to Close Deal", fill = "")+
  geom_text(aes(label = comma(value)), position = position_dodge(width = 0.9), vjust = -1, size=4, hjust = 0.5)+
  scale_fill_manual(values = c("DirectSales" = "yellow", "IndirectSales" = "cyan"))+
  scale_color_manual(values = c("fail" = "red", "pass" = "darkgreen"))+
  theme_classic()+
  theme(legend.position = "bottom")+
  guides(color = guide_legend(title = NULL))
