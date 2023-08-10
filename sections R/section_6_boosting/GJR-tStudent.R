library(rugarch)
library(dplyr)

library(ggplot2)
library(gridExtra)
library(ggforce)

library(openxlsx)
library(moments)

Data<-read.csv("/home/cv42/Documents/Dissertation/sections R/section_5.2_GJR/GJR-GARCH.csv")

Data$Date <- as.Date(Data$Date)

Data<-select(Data, Date, Returns, vol)

returns <- Data$Returns

# Now let's define our model specification
spec_2 = ugarchspec(variance.model=list(model="gjrGARCH", garchOrder=c(1,1)),
                    mean.model=list(armaOrder=c(0,0)), 
                    distribution.model="std")


# Create an empty object to store the forecast results

AIC_GJR <- c()
vol_est_GJR <- c()
vol_pred_GJR <- c()
shape_GJR <- c()
num_outliers <- c()
kur <- c()

# We will use a rolling window of 252 days, so start the for loop at 252
for(i in 253:length(returns)) {
  # Subset the data for the rolling window
  returns.subset <- returns[(i-252):(i-1)]
  
  # Fit the model to the subset
  
  #GJR
  fit_2 = ugarchfit(spec_2, data = returns.subset, start.pars=list(shape=4))
  AIC_GJR[i] <- infocriteria(fit_2)[1]
  vol_est_GJR[i] <- as.numeric(sigma(fit_2)[length(sigma(fit_2))][1])
  shape_GJR[i]<-as.numeric(fit_2@fit$coef[6])
  n_outliers<-sum(fit_2@fit$residuals < mean(fit_2@fit$residuals)-1*sd(fit_2@fit$residuals) | fit_2@fit$residuals > mean(fit_2@fit$residuals)+1*sd(fit_2@fit$residuals))
  num_outliers[i]<-n_outliers
  kur[i]<-kurtosis(fit_2@fit$residuals)
  
  # Forecast
  forecast_2<-ugarchforecast(fit_2, n.ahead = 1)
  vol_pred_GJR[i]<-as.numeric(forecast_2@forecast$sigmaFor[1])
  
  
  # Print progress
  if(i %% 100 == 0) print(paste("Progress:", round(i/length(returns)*100, 2), "%"))
}

Data$avg_vol<-mean(Data$vol)
Data$AIC_GJR<-AIC_GJR
Data$vol_GJR<-vol_est_GJR
Data$pred_GJR<-vol_pred_GJR
Data$shape_GJR<-shape_GJR
Data$num_outliers<-num_outliers
Data$kurtosis<-kur

## save data in csv file
file_path_2 <- "/home/cv42/Documents/Dissertation/sections R/section_6_boosting/GJR_UGARCH.xlsx"

wb <- createWorkbook()

addWorksheet(wb, sheetName = "ugarch_output")
writeData(wb, sheet = "ugarch_output", Data, rowNames = TRUE)

saveWorkbook(wb, file = file_path_2, overwrite = TRUE)