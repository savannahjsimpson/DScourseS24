R

install.packages("sparklyr")

library(sparklyr)
library(tidyverse)

spark_install(version = "3.0.0")
sc <- spark_connect(master = "local")

df1 <- as_tibble(iris)
df <- copy_to(sc, df1)

cat("Class of df1:", class(df1), "\n")
#Class of df1: tbl_df tbl data.frame 

cat("Class of df:", class(df), "\n")
#Class of df: tbl_spark tbl_sql tbl_lazy tbl 

cat("Column names of df1:", colnames(df1), "\n")
#Column names of df1: Sepal.Length Sepal.Width Petal.Length Petal.Width Species 
cat("Column names of df:", colnames(df), "\n")
#Column names of df: Sepal_Length Sepal_Width Petal_Length Petal_Width Species 

df %>% select(Sepal_Length, Species) %>% head() %>% print()
# Sepal_Length Species
#           <dbl> <chr>  
#1          5.1 setosa 
#2          4.9 setosa 
#3          4.7 setosa 
#4          4.6 setosa 
#5          5   setosa 
#6          5.4 setosa 

df %>% filter(Sepal_Length > 5.5) %>% head() %>% print()
#Sepal_Length Sepal_Width Petal_Length Petal_Width Species   
#.         <dbl>       <dbl>        <dbl>       <dbl> <chr>     
#1          5.8         4            1.2         0.2 setosa    
#2          5.7         4.4          1.5         0.4 setosa    
#3          5.7         3.8          1.7         0.3 setosa    
#4          7           3.2          4.7         1.4 versicolor
#5          6.4         3.2          4.5         1.5 versicolor
#6          6.9         3.1          4.9         1.5 versicolor

df %>% select(Sepal_Length, Species) %>% filter(Sepal_Length > 5.5) %>% head() %>% print()
#Sepal_Length Species   
#.         <dbl> <chr>     
#1          5.8 setosa    
#2          5.7 setosa    
#3          5.7 setosa    
#4          7   versicolor
#5          6.4 versicolor
#6          6.9 versicolor

df2 <- df %>% group_by(Species) %>% 
  summarize(mean = mean(Sepal_Length), count = n()) %>% 
  head() %>% print()
# Species     mean count
#. <chr>      <dbl> <dbl>
#1 versicolor  5.94    50
#2 virginica   6.59    50
#3 setosa      5.01    50

df2 <- df %>% group_by(Species) %>% 
  summarize(mean = mean(Sepal_Length), count = n()) %>% 
  head() %>% print()
#Species     mean count
#  <chr>      <dbl> <dbl>
#1 versicolor  5.94    50
#2 virginica   6.59    50
#3 setosa      5.01    50

df2 %>% arrange(Species) %>% head() %>% print()
#I received the same error here. 