install.packages('tidyverse')
install.packages('lubridate')
install.packages('jsonlite')
install.packages('rvest')

library(tidyverse)
library(lubridate)
library(jsonlite)
library(rvest)

#fromJSON('https://api.covid19api.com/summary')
file = jsonlite::fromJSON('https://api.covid19api.com/country/indonesia/status/confirmed?from=2019-01-01T00:00:00Z&to=2020-12-31T00:00:00Z')
class(file)

write.csv(file, 'test.csv', row.names=FALSE)

tidyverse_packages()
dplyr::count(file, name='Country')