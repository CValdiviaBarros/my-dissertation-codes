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

# Create an empty object to store the forecast results
AIC_GJR = c()
vol_est_GJR = c()
vol_pred_GJR = c()
shape_GJR = c()
shape_GJR_real =c()

# We will use a rolling window of 252 days, so start the for loop at 252
for(i in 253:length(returns)) {
  # Subset the data for the rolling window
  returns.subset <- returns[(i-252):(i-1)]
  
  # Initialize variables to keep track of the best shape and its AIC
  best_shape <- NA
  best_shape_1<-NA
  best_aic <- Inf
  
  # Iterate over possible shapes
  for(shape in 3:50) {
    # Now let's define our model specification
    spec_2 = ugarchspec(variance.model=list(model="gjrGARCH", garchOrder=c(1,1)),
                        mean.model=list(armaOrder=c(0,0)), 
                        distribution.model="std",
                        fixed.pars=list(shape=shape))
      # Fit the model to the subset
      fit_2 <- ugarchfit(spec_2, data = returns.subset)
      
      # Check if this shape has a better (lower) AIC than the best one found so far
      aic <- infocriteria(fit_2)[1]
      if(aic < best_aic) {
        best_aic <- aic
        best_shape <- shape
        best_shape_1<-as.numeric(fit_2@fit$coef[6])
        best_vol <- as.numeric(sigma(fit_2)[length(sigma(fit_2))][1])
        best_forecast <- ugarchforecast(fit_2, n.ahead = 1)
        best_vol_pred <- as.numeric(best_forecast@forecast$sigmaFor[1])
      }
    }
  
  
  # Store the best results for this window
  AIC_GJR[i] <- best_aic
  vol_est_GJR[i] <- best_vol
  shape_GJR[i] <- best_shape
  shape_GJR_real[i] <- best_shape_1
  vol_pred_GJR[i] <- best_vol_pred
  
  # Print progress
  print(c((i-252)/(length(returns)-252)*100, best_shape, best_shape_1, best_aic))
  if(i %% 100 == 0) print(paste("Progress:", round(i/length(returns)*100, 2), "%"))
}

Data$avg_vol<-mean(Data$vol)
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
