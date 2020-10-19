library(tidyverse)
library(lubridate)
data = read.csv("tiantan.csv") %>% 
  fill(PM2.5, PM10, SO2, NO2, CO, O3, TEMP, PRES, DEWP, RAIN, wd, WSPM, .direction = "up") %>%
  mutate(date = make_date(year, month, day))

# some plots
ggplot(data, aes(x = date, y = PM2.5)) + geom_line() + facet_wrap(~year) # facets by year
ggplot(data, aes(x = date, y = PM10)) + geom_line() + geom_smooth() # lines with smoothing
boxplot(PM2.5 ~ year, data) # boxplot, but not very good comparison
boxplot(PM10 ~ year, data)
