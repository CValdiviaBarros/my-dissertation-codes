library(rugarch)
library(dplyr)

library(ggplot2)
library(gridExtra)
library(ggforce)

library(openxlsx)

Data<-read.csv("/home/cv42/Documents/Dissertation/sections R/section_5.2_GJR/GJR-GARCH.csv")

Data$Date <- as.Date(Data$Date)

Data<-select(Data, Date, Returns, vol)

returns <- Data$Returns

# Now let's define our model specification
spec = ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)), mean.model=list(armaOrder=c(0,0)))
#spec_2 = ugarchspec(variance.model=list(model="gjrGARCH", garchOrder=c(1,1)),
#                    mean.model=list(armaOrder=c(0,0)), 
#                    distribution.model="std")

spec_2 = ugarchspec(variance.model=list(model="gjrGARCH", garchOrder=c(1,1)),
                    mean.model=list(armaOrder=c(0,0)), 
                    distribution.model="std",
                    fixed.pars=list(shape=4))

# Create an empty object to store the forecast results
AIC_EGARCH = c()
vol_est_EGARCH = c()
vol_pred_EGARCH = c()
alpha<- c()
beta<- c()
theta<- c()
std_residuals_EGARCH<- c()

AIC_GJR = c()
vol_est_GJR = c()
vol_pred_GJR = c()
shape_GJR = c()

# We will use a rolling window of 252 days, so start the for loop at 252
for(i in 253:length(returns)) {
  # Subset the data for the rolling window
  returns.subset <- returns[(i-252):(i-1)]
  
  # Fit the model to the subset
  
  #EGARCH
  fit = ugarchfit(spec, data = returns.subset)
  AIC_EGARCH[i] <- infocriteria(fit)[1]
  vol_est_EGARCH[i] <- as.numeric(sigma(fit)[length(sigma(fit))][1])
  alpha[i]<-as.numeric(fit@fit$coef[3])
  beta[i]<-as.numeric(fit@fit$coef[4])
  theta[i]<- as.numeric(fit@fit$coef[5])
  std_residuals_EGARCH[i]<-as.numeric(fit@fit$residuals[length(fit@fit$residuals)])/as.numeric(sigma(fit)[length(sigma(fit))][1])
  # Forecast
  forecast<-ugarchforecast(fit, n.ahead = 1)
  vol_pred_EGARCH[i]<-as.numeric(forecast@forecast$sigmaFor[1])
  
  #GJR
  #fit_2 = ugarchfit(spec_2, data = returns.subset, start.pars=list(shape=4))
  fit_2 = ugarchfit(spec_2, data = returns.subset)
  AIC_GJR[i] <- infocriteria(fit_2)[1]
  vol_est_GJR[i] <- as.numeric(sigma(fit_2)[length(sigma(fit_2))][1])
  shape_GJR[i]<-as.numeric(fit_2@fit$coef[6])
  # Forecast
  forecast_2<-ugarchforecast(fit_2, n.ahead = 1)
  vol_pred_GJR[i]<-as.numeric(forecast_2@forecast$sigmaFor[1])
  
  # Print progress
  if(i %% 100 == 0) print(paste("Progress:", round(i/length(returns)*100, 2), "%"))
}

Data$AIC_EGARCH<-AIC_EGARCH
Data$vol_EGARCH<-vol_est_EGARCH
Data$avg_vol<-mean(Data$vol)
Data$pred_EGARCH<-vol_pred_EGARCH
Data$alpha<-alpha
Data$beta<-beta
Data$theta<-theta
Data$std_residuals_EGARCH<-std_residuals_EGARCH
Data$AIC_GJR<-AIC_GJR
Data$vol_GJR<-vol_est_GJR
Data$pred_GJR<-vol_pred_GJR
Data$shape_GJR<-shape_GJR

## save data in csv file
file_path_2 <- "/home/cv42/Documents/Dissertation/sections R/section_5.3_EGARCH/EGARCH_UGARCH.xlsx"

wb <- createWorkbook()

addWorksheet(wb, sheetName = "ugarch_output")
writeData(wb, sheet = "ugarch_output", Data, rowNames = TRUE)

saveWorkbook(wb, file = file_path_2, overwrite = TRUE)
