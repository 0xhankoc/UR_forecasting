library(dplyr)
library(lubridate)
library(anytime)
setwd("/Users/orhankoc/Documents/uw/Q1/INDE-427/project")

# Step 1: Data pre-processing
macro <- read.csv(file = "us_macro.csv", header = TRUE)
ur_ <- read.csv(file = "other_countries_ur.csv", header = TRUE)
# reshape dataset such that "OECD_ur","G-7_ur","EA_ur","EU27_ur"
ur <- reshape(ur_, idvar = "Year", timevar = "Country", direction = "wide")
# format date
ur$Year <- anydate(ur$Year)
macro$Year <- dmy(paste("01", macro$Year))
macro$Year <- anydate(macro$Year)

# merge datasets with respect to year
output_dataset <- full_join(macro, ur, by = "Year")

# Create X matrix (predictors) and Y vector (outcome variable)
Y <- output_dataset$US_ur

output_dataset <- output_dataset[,-10]
output_dataset <- output_dataset[,-9]
output_dataset <- output_dataset[,-8]
output_dataset <- output_dataset[,-7]
output_dataset <- output_dataset[,-2]
output_dataset <- output_dataset[,-1]

X <- output_dataset
data <- data.frame(X,Y)
names(data)[7] <- c("Unemp_rate")

# Step 2: Create training data
train.ix <- sample(nrow(data),floor( nrow(data)/2) )
data.train <- data[train.ix,]
# Create a testing data (half the original data size)
data.test <- data[-train.ix,]

# Step 3: Use lm() function to build a full model with all predictors
lm.macro <- lm(Unemp_rate ~ ., data = data.train)
summary(lm.macro)
sqrt(mean(lm.macro$residuals^2))
# Step 4: Automatically delete all the insignificant variables, automatic model selection.
lm.macro.reduced <- step(lm.macro, direction="backward", test="F")
anova(lm.macro.reduced,lm.macro) 

# Step 5: Conduct diagnostics of the model
require("ggfortify") 
autoplot(lm.macro.reduced, which = 1:6, ncol = 3, label.size = 3)
pred.lm <- predict(lm.macro.reduced, data.test) 
cor(pred.lm, data.test$Unemp_rate) 













