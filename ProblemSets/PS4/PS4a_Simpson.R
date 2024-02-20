system('wget -O dates.json "https://www.vizgr.org/historical-events/search.php?format=json&begin_date=00000101&end_date=20240209&lang=en"')
#had issues with 'wget' function so I ma using 'curl' instead 
system('curl -o dates.json "https://www.vizgr.org/historical-events/search.php?format=json&begin_date=00000101&end_date=20240209&lang=en"')

system('cat dates.json')

install.packages(c("jsonlite", "tidyverse"))
library(jsonlite)
library(tidyverse)

mylist <- fromJSON('dates.json')
mydf <- bind_rows(mylist$result[-1])

class(mydf)
#[1] "tbl_df"     "tbl"        "data.frame"
class(mydf$date)
#[1] "character"

head(mydf, n = 5)
