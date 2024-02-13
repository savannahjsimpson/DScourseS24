#(a) Read the Florida insurance data CSV file into the database
path <- "/Users/savannahjsimpson/Downloads/FL_insurance_sample.csv"
insurance_data <- read.csv(path)
dbWriteTable(con, "insurance_data", insurance_data)

#(b) Print out the first 10 rows of the data set
query_b <- "SELECT * FROM insurance_data LIMIT 10"
result_b <- dbGetQuery(con, query_b)
print(result_b)

#(c) List which counties are in the sample
query_c <- "SELECT DISTINCT county FROM insurance_data"
result_c <- dbGetQuery(con, query_c)
print(result_c)

#(d) Compute the average property appreciation from 2011 to 2012
query_d <- "SELECT AVG(tiv_2012 - tiv_2011) AS avg_appreciation FROM insurance_data"
result_d <- dbGetQuery(con, query_d)
print(result_d)
#avg_appreciation = 398129.1

#(e) Create a frequency table of the construction variable
query_e <- "SELECT construction, COUNT(*) AS frequency FROM insurance_data GROUP BY construction"
result_e <- dbGetQuery(con, query_e)
print(result_e)