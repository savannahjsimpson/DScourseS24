#QUESTION 3: WEB SCRAPING
install.packages("rvest")
library(rvest)

url <- "https://en.wikipedia.org/wiki/Palm_Beach_County,_Florida"
webpage <- read_html(url)

data <- webpage %>%
  html_nodes("#mw-content-text > div.mw-content-ltr.mw-parser-output > table:nth-child(56)") %>%
  html_text()

data_frame <- data.frame(Column_Name = data)

write.csv(data_frame, file = "PBCWIKI.csv", row.names = FALSE)

PBCW_data <- read.csv("PBCWIKI.csv")
head(PBCW_data)



#QUESTION 4: API KEYS 
install.packages(c("httr", "jsonlite"))
library(httr)
library(jsonlite)

#fred_api_key <- "xxx" 
#I will not be including my API key in this script 

series_id <- "MEDLISPRIPERSQUFEE12099"

url <- paste0("https://api.stlouisfed.org/fred/series/observations?",
              "series_id=", series_id,
              "&fred_api_key=", fred_api_key,
              "&file_type=json")
response <- GET(url)

HIPBCdata <- fromJSON(content(response, "text"))

str(data)

df <- HIPBCdata$observations
df <- data.frame(Date = as.Date(df$date), Value = as.numeric(df$value))

library(ggplot2)

ggplot(df, aes(x = Date, y = Value)) +
  geom_line() +
  labs(title = "Median List Price of Houses Sold Per Square Foot",
       x = "Date",
       y = "Price (USD)")