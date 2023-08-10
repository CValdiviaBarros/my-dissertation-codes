
## libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(quantmod)

## read file SP500 sheet
file_path <- "/home/cv42/Documents/Dissertation/sections R/section_4_ewma/EWMA_MA.csv"
sheet_name <- "EWMA_MA"

## save in dataframe
df <- as.data.frame(read.csv(file_path))

##set Date as date time format
df$Date <- as.Date(df$Date)

## plot effective vol vs EWMA and MA models
effective_vol <- ggplot(data=df, aes(x=Date)) + 
  annotate("text",x = df$Date[640], y = 8.5, label = "Absolute Daily Return S&P500 Index (volatility proxy)", color = "black", size=5.5) +
  annotate("text",x = df$Date[610], y = 4, label = "EWMA model (rolling 252)", color = "blue", size=5.5) +
  annotate("text",x = df$Date[780], y = 2.8, label = "MA model (rolling 252)", color = "red", size=5.5) +
  geom_line(aes(y=vol),color = "black", linewidth = 0.3, alpha=0.5) +
  geom_line(aes(y=ewma),color = "blue", linewidth = 0.6) +
  geom_line(aes(y=MA),color = "red", linewidth = 0.6) +
  xlab("Date") +
  ylab("S&P500 daily volatility (in percentage)") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14))

print(effective_vol)

df_2<-select(df, Date, Returns, VAR_EMWA_2)
df_2$diff_VAR<-df_2$VAR_EMWA_2+df_2$Returns

# Create a vector of NaN values
new_values <- rep(NaN, nrow(df_2))
# Set values from row 252 until the end to 1
new_values[252:nrow(df)] <- 0

df_2$VAR_line<-new_values

## plot VAR EWMA and MA models
VAR <- ggplot(data=df, aes(x=Date)) + 
  annotate("text",x = df$Date[550], y = 4.5, label = "S&P500 daily returns", color = "black", size=5) +
  annotate("text",x = df$Date[10], y = 9.5, label = "(a)", color = "black", size=5) +
  annotate("text",x = df$Date[655], y = -7, label = "Value-At-Risk: EWMA model (rolling 252, lambda=0.82)", color = "red", size=5) +
  geom_line(aes(y=Returns),color = "black", linewidth = 0.3, alpha=0.7) +
  geom_line(aes(y=-VAR_EMWA_2),color = "red", linewidth = 0.6, alpha=0.5) +
  xlab("Date") +
  ylab("S&P500 daily returns (in percentage)") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.title.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 14))

diff_VAR<- ggplot(data=df_2, aes(x=Date)) + 
  annotate("text",x = df$Date[10], y = -0.02, label = "(b)", color = "black", size=5) +
  annotate("text",x = df$Date[500], y = -3.8, label = "-335 bps", color = "darkred", size=5) +
  geom_col(aes(y = ifelse(diff_VAR < 0, diff_VAR, 0)), fill = "darkred", alpha=0.5, width = 2) +
  annotate("text",x = df_2$Date[150], y = -3, label = "Total days with deficit: 35", color = "black", size=4) +
  annotate("text",x = df_2$Date[148], y = -2.5, label = "EWMA VaR statistics:", color = "black", size=4, fontface="bold") +
  annotate("text",x = df_2$Date[123], y = -3.5, label = "Ratio: 3.48%", color = "black", size=4) +
  annotate("text",x = df_2$Date[140], y = -4, label = "Max deficit: 335 bps", color = "black", size=4) +
  geom_point(aes(y = ifelse(diff_VAR < 0, diff_VAR, NaN)), fill = "darkred", size = 1.5, alpha=0.5) +
  annotate("text",x = aic_df$Date[1195], y = 0.25, label = "VaR deficit (in percentage)", color = "black", size=4) +
  annotate("text",x = df_2$Date[10], y = 19, label = "(b)", color = "black", size=5) +
  geom_line(aes(y = VAR_line), color = "black",linewidth = 0.3) +
  xlab("Date") +
  scale_y_continuous(limits = c(-6, 0.3))+
  ylab("VaR deficit (in percentage)") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14))

grid.arrange(VAR, diff_VAR, nrow=2)

file_path_2 <- "/home/cv42/Documents/Dissertation/sections R/section_4_ewma/lambda.csv"
## save lambda in dataframe
df_lambda <- as.data.frame(read.csv(file_path_2))


## plot VAR EWMA and MA models
lambda_plot <- ggplot(data=df_lambda, aes(x=lambda)) + 
  annotate("text",x = 0.81, y = 0.6, label = "S&P500 Min RMSE (lambda = 0.82)", color = "black", size=4, angle=90) +
  annotate("text",x = 0.85, y = 0.601, label = "DJI Min RMSE (lambda = 0.84)", color = "orange", size=4, angle=90) +
  annotate("text",x = 0.51, y = 0.606, label = "(a)", color = "black", size=5)+
  annotate("text",x =0.55, y = 0.5925, label = "S&P500 Index", color = "black", size=5, angle=-37) +
  annotate("text",x =0.6, y = 0.582, label = "Dow Jones Industrial", color = "orange", size=5, angle=-38.5) +
  geom_point(aes(y=rmse),color = "black", size = 0.6) +
  geom_line(aes(y=rmse),color = "black", linewidth = 0.5) +
  geom_point(aes(y=rmse_DJI),color = "orange", size = 0.6) +
  geom_line(aes(y=rmse_DJI),color = "orange", linewidth = 0.3) +
  geom_vline(xintercept = 0.82, linetype="dashed", color="black", linewidth=0.3)+
  geom_vline(xintercept = 0.84, linetype="dashed", color="orange", linewidth=0.3)+
  xlab(expression(lambda)) +
  ylab("RMSE") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14))

effective_vol_2 <- ggplot(data=df, aes(x=Date)) + 
  annotate("text",x = df$Date[80], y = 12, label = "(b)", color = "black", size=5) +
  annotate("text",x = df$Date[720], y = 8.5, label = "Absolute Daily Return (volatility proxy)", color = "black", size=5) +
  annotate("text",x = df$Date[780], y = 5, label = "EWMA model (rolling 252, lambda 0.94)", color = "red", size=5) +
  annotate("text",x = df$Date[780], y = 6, label = "EWMA model (rolling 252, lambda 0.82)", color = "blue", size=5) +
  geom_line(aes(y=vol),color = "black", linewidth = 0.3, alpha=0.5) +
  geom_line(aes(y=ewma),color = "red", linewidth = 0.3) +
  geom_line(aes(y=ewma_2),color = "blue", linewidth = 0.3) +
  xlab("Date") +
  ylab("S&P500 daily volatility (in percentage)") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14))

grid.arrange(lambda_plot, effective_vol_2, ncol=2)
