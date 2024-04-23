#Load in each of my data sets 

#Jupiter Data
jupiter_pre_covid <- read.csv("jupitersales16.19.csv")
jupiter_post_covid <- read.csv("jupitersales21.24.csv")

#Boca Raton Data 
boca_pre_covid <- read.csv("bocasales16.19.csv")
boca_post_covid <- read.csv("bocasales21.24.csv")



#JUPITER PRE REGRESSION 
jupiter_pre_covid_model <- lm(Sale.Price ~ Lot.Size + Total.SQFT + Bed + Full.Bath, data = jupiter_pre_covid)
summary(jupiter_pre_covid_model)

jupiter_pre_covid_coefficients <- coef(summary(jupiter_pre_covid_model))
jupiter_pre_covid_coefficients
#.                Estimate   Std. Error    t value     Pr(>|t|)
#(Intercept)   -6727.36950  28782.78446 -0.2337289 8.152439e-01
#Lot.Size    1962399.52162 160884.01572 12.1976040 5.632560e-32
#Total.SQFT       98.57291     14.86363  6.6318192 5.452822e-11
#Bed          -20591.89104  13869.63036 -1.4846748 1.379493e-01
#Full.Bath     26918.99160  17239.67157  1.5614562 1.187367e-01

jupiter_pre_covid_p_values <- jupiter_pre_covid_coefficients[, "Pr(>|t|)"]
jupiter_pre_covid_p_values
# (Intercept)     Lot.Size   Total.SQFT          Bed    Full.Bath 
#8.152439e-01 5.632560e-32 5.452822e-11 1.379493e-01 1.187367e-01 

jupiter_pre_covid_adjusted_r_squared <- summary(jupiter_pre_covid_model)$adj.r.squared
jupiter_pre_covid_adjusted_r_squared
#[1] 0.5752474

jupiter_pre_covid_rmse <- sqrt(mean(jupiter_pre_covid_model$residuals^2))
jupiter_pre_covid_rmse
#[1] 211905.1



#JUPITER POST REGRESSION 
jupiter_post_covid_model <- lm(Sale.Price ~ Lot.Size + Total.SQFT + Bed + Full.Bath, data = jupiter_post_covid)
summary(jupiter_post_covid_model)

jupiter_post_covid_coefficients <- coef(summary(jupiter_post_covid_model))
jupiter_post_covid_coefficients
#.               Estimate   Std. Error  t value     Pr(>|t|)
#(Intercept) -303005.2366  41123.26233 -7.36822 4.646871e-13
#Lot.Size    2223346.0022 220698.89254 10.07411 1.886479e-22
#Total.SQFT      232.8562     21.92472 10.62071 1.292546e-24
#Bed           23528.9575  20393.28219  1.15376 2.489725e-01
#Full.Bath     51394.1190  26138.94122  1.96619 4.965164e-02

jupiter_post_covid_p_values <- jupiter_post_covid_coefficients[, "Pr(>|t|)"]
jupiter_post_covid_p_values
#(Intercept)     Lot.Size   Total.SQFT          Bed    Full.Bath 
#4.646871e-13 1.886479e-22 1.292546e-24 2.489725e-01 4.965164e-02 

jupiter_post_covid_adjusted_r_squared <- summary(jupiter_post_covid_model)$adj.r.squared
jupiter_post_covid_adjusted_r_squared
#[1] 0.7731982

jupiter_post_covid_rmse <- sqrt(mean(jupiter_post_covid_model$residuals^2))
jupiter_post_covid_rmse
#[1] 251971.4



#BOCA PRE REGRESSION 
boca_pre_covid_model <- lm(Sale.Price ~ Lot.Size + Total.SQFT + Bed + Full.Bath, data = boca_pre_covid)
summary(boca_pre_covid_model)

boca_pre_covid_coefficients <- coef(summary(boca_pre_covid_model))
boca_pre_covid_coefficients
#.               Estimate   Std. Error   t value     Pr(>|t|)
#(Intercept) -86187.67391 17414.024733 -4.949325 1.018304e-06
#Lot.Size    419114.48039 58515.643620  7.162435 2.838865e-12
#Total.SQFT      98.51079     6.741729 14.612097 1.511959e-40
#Bed           7435.89885  2851.065365  2.608112 9.375518e-03
#Full.Bath    65700.05672  8913.505999  7.370843 7.034033e-13

boca_pre_covid_p_values <- boca_pre_covid_coefficients[, "Pr(>|t|)"]
boca_pre_covid_p_values
#.(Intercept)     Lot.Size   Total.SQFT          Bed    Full.Bath 
#1.018304e-06 2.838865e-12 1.511959e-40 9.375518e-03 7.034033e-13 

boca_pre_covid_adjusted_r_squared <- summary(boca_pre_covid_model)$adj.r.squared
boca_pre_covid_adjusted_r_squared
#[1] 0.7554632

boca_pre_covid_rmse <- sqrt(mean(boca_pre_covid_model$residuals^2))
boca_pre_covid_rmse
#[1] 71588.28 



#BOCA POST REGRESSION  
boca_post_covid_model <- lm(Sale.Price ~ Lot.Size + Total.SQFT + Bed + Full.Bath, data = boca_post_covid)
summary(boca_post_covid_model)

boca_post_covid_coefficients <- coef(summary(boca_post_covid_model))
boca_post_covid_coefficients
#.              Estimate   Std. Error   t value     Pr(>|t|)
#(Intercept) -61776.9049  30395.62550 -2.032427 4.268809e-02
#Lot.Size    936695.4830 113884.27880  8.224976 2.046246e-15
#Total.SQFT     152.8241     12.78144 11.956721 7.406347e-29
#Bed          17600.8683   6158.90236  2.857793 4.460790e-03
#Full.Bath    48976.1568  17315.88645  2.828394 4.883878e-03

boca_post_covid_p_values <- boca_post_covid_coefficients[, "Pr(>|t|)"] 
boca_post_covid_p_values
# (Intercept)     Lot.Size   Total.SQFT          Bed    Full.Bath 
#4.268809e-02 2.046246e-15 7.406347e-29 4.460790e-03 4.883878e-03 

boca_post_covid_adjusted_r_squared <- summary(boca_post_covid_model)$adj.r.squared
boca_post_covid_adjusted_r_squared
#[1] 0.7170796

boca_post_covid_rmse <- sqrt(mean(boca_post_covid_model$residuals^2))
boca_post_covid_rmse
#[1] 132293.4  




library(knitr)

results <- data.frame(
  Model = c("Jupiter Pre-COVID", "Jupiter Post-COVID", "Boca Pre-COVID", "Boca Post-COVID"),
  Adjusted_R_squared = c(jupiter_pre_covid_adjusted_r_squared, jupiter_post_covid_adjusted_r_squared,
                         boca_pre_covid_adjusted_r_squared, boca_post_covid_adjusted_r_squared),
  RMSE = c(jupiter_pre_covid_rmse, jupiter_post_covid_rmse, boca_pre_covid_rmse, boca_post_covid_rmse),
  Lot_Size_Coefficient = c(jupiter_pre_covid_coefficients["Lot.Size", "Estimate"],
                           jupiter_post_covid_coefficients["Lot.Size", "Estimate"],
                           boca_pre_covid_coefficients["Lot.Size", "Estimate"],
                           boca_post_covid_coefficients["Lot.Size", "Estimate"]),
  Total_SQFT_Coefficient = c(jupiter_pre_covid_coefficients["Total.SQFT", "Estimate"],
                             jupiter_post_covid_coefficients["Total.SQFT", "Estimate"],
                             boca_pre_covid_coefficients["Total.SQFT", "Estimate"],
                             boca_post_covid_coefficients["Total.SQFT", "Estimate"])
)


kable(results, format = "latex", caption = "Regression Analysis Results",
      col.names = c("Model", "Adjusted R-squared", "RMSE", "Lot Size Coefficient", "Total SQFT Coefficient"),
      digits = 2, booktabs = TRUE, linesep = "") %>%
  kable_styling(latex_options = "hold_position")





