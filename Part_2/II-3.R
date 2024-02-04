# Question 3

# set up
rm(list = ls())
library(ggplot2)

#(1)

# data load and pre-process
df_source <- read.csv('data/vixlarge.csv', header = FALSE, sep = ',')
colnames(df_source) <- c("date", "VIX")
df_source$date <- as.Date(df_source$date, format = "\'%Y-%m-%d\'")

# plot
VIX_date_plot <- ggplot(df_source, aes(date, VIX)) + geom_line() + 
  labs(title = "VIX and Date Variation", x = 'Date', y ='VIX')
ggsave(VIX_date_plot, filename = 'output/Q3_1_VIX_Date_plot.png', width = 8, height = 4)
