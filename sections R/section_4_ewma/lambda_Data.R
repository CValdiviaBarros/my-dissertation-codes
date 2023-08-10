## DOWNLOAD DATA FROM QUANTMOD

## libraries used
library(quantmod)
library(dplyr)
library(openxlsx)

ticker_sp500 <- "^GSPC"
ticker_NASDAQ<-"^IXIC"
ticker_dj<- "^DJI"
ticker_Nikkei <- "^N225"


## data for S&P500 (5 years)
getSymbols(ticker_sp500, src = "yahoo", from = "2013-06-11", to = "2018-06-11")

## data for NASDAQ Composite (5 years)
getSymbols(ticker_NASDAQ, src = "yahoo", from = "2013-06-11", to = "2018-06-11")

## data for Dow Jones Industrial Average (5 years)
getSymbols(ticker_dj, src = "yahoo", from = "2013-06-11", to = "2018-06-11")

## merge data by date (index)
All_Data <- GSPC

## remove NAN values
clean_dataset <- na.omit(All_Data)

## save data as data.frame
df_all <- as.data.frame(clean_dataset)

## NASDAQ
df_NASDAQ<-na.omit(IXIC)
df_NASDAQ<-as.data.frame(df_NASDAQ)

## DJI
df_dji<-na.omit(DJI)
df_dji<-as.data.frame(df_dji)

## split data for both assets (sp500 and vix)
df_sp500 <- select(df_all, GSPC.Open, GSPC.High, GSPC.Low, GSPC.Close, GSPC.Volume)

## save data in csv file
file_path <- "/home/cv42/Documents/Dissertation/sections R/section_4_ewma/Data_lambda.xlsx"

wb <- createWorkbook()

addWorksheet(wb, sheetName = "df_all")
addWorksheet(wb, sheetName = "df_sp500")
addWorksheet(wb, sheetName = "df_NASDAQ")
addWorksheet(wb, sheetName = "df_dji")

writeData(wb, sheet = "df_all", df_all, rowNames = TRUE)
writeData(wb, sheet = "df_sp500", df_sp500, rowNames = TRUE)
writeData(wb, sheet = "df_NASDAQ", df_NASDAQ, rowNames = TRUE)
writeData(wb, sheet = "df_dji", df_dji, rowNames = TRUE)

saveWorkbook(wb, file = file_path, overwrite = TRUE)