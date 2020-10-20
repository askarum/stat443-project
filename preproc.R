library(tidyverse)
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
# some plots
ggplot(data, aes(x = date, y = NO2)) + geom_line() + geom_smooth() + facet_wrap(~wd) # facets by year
ggplot(data, aes(x = date, y = NO2)) + geom_line() + geom_smooth() + facet_wrap(~month)
ggplot(data, aes(x = NO2, fill = season)) + geom_histogram(position='identity')
data %>%
  filter()

ggplot(data, aes(x = date, y = PM10)) + geom_line() + geom_smooth() # lines with smoothing
boxplot(PM2.5 ~ year, data) # boxplot, but not very good comparison
boxplot(PM10 ~ year, data)
