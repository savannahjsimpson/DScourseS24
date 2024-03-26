#Problem Set 7 

install.packages("mice")
install.packages("modelsummary")
library(mice)
library(modelsummary)


wages <- read.csv("wages.csv")
wages <- wages[complete.cases(wages[,c("hgc","tenure")]),]
modelsummary(wages)

summary_table <- datasummary_skim(wages, output = "latex")
print(summary_table)


model1 <- lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married, data=wages)

# mean imputation
wages$logwage_mean <- ifelse(is.na(wages$logwage), mean(wages$logwage, na.rm=TRUE), wages$logwage)
model2 <- lm(logwage_mean ~ hgc + college + tenure + I(tenure^2) + age + married, data=wages)

# impute with predictions 
model_pred <- lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married, data=wages)
wages$logwage_pred <- ifelse(is.na(wages$logwage), predict(model_pred, newdata=wages[is.na(wages$logwage),]), wages$logwage)
model3 <- lm(logwage_pred ~ hgc + college + tenure + I(tenure^2) + age + married, data=wages)

# multiple imputation
mi <- mice(wages, m=5, method='pmm', printFlag=FALSE)
model4 <- with(mi, lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married))

# regression table
reg_table <- modelsummary(list(model1, model2, model3, model4))

