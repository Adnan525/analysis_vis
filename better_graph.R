library(lubridate)
library(dplyr)
library(readr)
library(ggplot2)
require(reshape2)
library(scales)

df <- data.frame(
  DATE = c(
    "2019-04", "2019-05", "2019-06", "2019-07", "2019-08", 
    "2019-09", "2019-10", "2019-11", "2019-12", "2020-01", "2020-02", "2020-03"
  ),
  CAPACITY = c(29263, 28037, 21596, 25895, 25813, 22427, 23605, 24263, 24243, 25533, 24467, 25194),
  DEMAND = c(46193, 49131, 50124, 48850, 47602, 43697, 41058, 37364, 34364, 34149, 25573, 25284)
)

df <- melt(df, "DATE")

ggplot(df, aes(x = DATE, y = value, fill = variable))+
  geom_bar(stat = "identity", position = "dodge")+
  labs(title = "Deman and Capacity of Project Hours by Month", x = "Month", y = "Project Hours", fill = "")+
  geom_text(aes(label = comma(value)), position = position_dodge(width = 0.9), vjust = 0.5, size=4, hjust = -0.1)+
  coord_flip()+
  theme_classic()+
  theme(legend.position = "bottom")
