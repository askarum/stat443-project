library(ggplot2)
library(RColorBrewer)
library(tidyr)
library(dplyr)
library(lubridate)
data = read.csv("tiantan.csv") %>% 
  fill(PM2.5, PM10, SO2, NO2, CO, O3, TEMP, PRES, DEWP, RAIN, wd, WSPM, .direction = "up") %>%
  mutate(date = make_date(year, month, day))
# add season as a variable
data$season = as.factor(sapply(data$month,
                               FUN = function(m) {
                                 if (3 <= m & m < 6) {
                                   return("Spring")
                                 } else if (6 <= m & m < 9) {
                                   return('Summer')
                                 } else if (9<= m & m < 12) {
                                   return('Autumn')
                                 } else {
                                   return('Winter')
                                 }
                               }
))

## some plots about NO2
# NO2 change in 2013
data %>%
  filter(year == 2013) %>%
  select(date, NO2) %>%
  group_by(date) %>%
  summarize(daily_mean = mean(NO2)) %>%
  ggplot(aes(x=date, y=daily_mean)) +
    geom_area(fill="orange", alpha=0.5) +
    geom_line(color='orange') + ylim(0, 100)
    labs(title = 'Daily NO2 pollution in 2013', x = 'Month', y = 'NO2 concentration')

# NO2 average in months over four years
data %>%
  select(date, month, NO2) %>%
  group_by(month) %>%
  mutate(monthly_mean = mean(NO2)) %>%
  ggplot(aes(x = month, y = monthly_mean)) +
  geom_line(color='orange') + ylim(0, 100) +
  labs(title = 'NO2 monthly average pollution, 2013 to 2017', x = 'Month', y = 'NO2 concentration', caption = 'only first two month data in 2017')
  
# NO2 

# wind direction by season
data %>%
  select(wd, season) %>%
  count(wd, season) %>%
  ggplot(aes(x = wd, y = n, fill = factor(season, levels=c('Spring', 'Summer', 'Autumn', 'Winter')))) +
  geom_bar(stat='identity', position='fill') +
  scale_fill_brewer(palette = "Reds") +
  labs(title = 'Days of wind direction by season', x ='Wind direction', y='Days counts') +
  guides(fill=guide_legend(title="Season"))

