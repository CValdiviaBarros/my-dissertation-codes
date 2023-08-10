## libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(Metrics)
library(ggforce)

## read file EGARCH
file_path <- "/home/cv42/Documents/Dissertation/sections R/section_5.3_EGARCH/EGARCH_UGARCH.xlsx"

## save in dataframe
df <- as.data.frame(read_excel(file_path))
df <- df[, -1]

##set Date as date time format
df$Date <- as.Date(df$Date)

#select columns
df_2<-select(df, Date,Returns, vol, vol_EGARCH, pred_EGARCH)

## read file GJR vol
file_path_5 <- "/home/cv42/Documents/Dissertation/sections R/section_5.2_GJR/GJR-GARCH_VAR.csv"
df_5 <- as.data.frame(read.csv(file_path_5))

##set Date as date time format and delete nan values
df_5$Date <- as.Date(df_5$Date)

df_2$VAR_GJR.GARCH<-df_5$VAR_GJR.GARCH

df_2$EGARCH_VAR<-df_2$pred_EGARCH*(-qnorm(0.05))

df_2$diff_VAR<-df_2$EGARCH_VAR+df_2$Returns

# Create a vector of NaN values
new_values <- rep(NaN, nrow(df_2))
# Set values from row 252 until the end to 1
new_values[253:nrow(df)] <- 0

df_2$VAR_line<-new_values
df_2$avg_vol<-mean(df_2$vol)

## plot VAR GARCH and EWMA_2 models
VAR <- ggplot(data=df_2, aes(x=Date)) + 
  annotate("text",x = df_2$Date[550], y = 4.5, label = "S&P500 daily returns", color = "black", size=5) +
  annotate("text",x = df_2$Date[10], y = 9.5, label = "(a)", color = "black", size=5) +
  annotate("text",x = df_2$Date[750], y = -12.5, label = "Model 4: Value-At-Risk using EGARCH(1,1) (rolling 252)", color = "red", size=5) +
  annotate("text",x = df_2$Date[760], y = -9, label = "Model 3: Value-At-Risk using GJR-GARCH(1,1)(rolling 252)", color = "blue", size=5) +
  geom_line(aes(y=Returns),color = "black", linewidth = 0.3, alpha=0.7) +
  geom_line(aes(y=-EGARCH_VAR),color = "red", linewidth = 0.6, alpha=0.5) +
  geom_line(aes(y=-VAR_GJR.GARCH),color = "blue", linewidth = 0.6, alpha=0.5) +
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
  annotate("text",x = df_2$Date[10], y = -0.02, label = "(b)", color = "black", size=5) +
  annotate("text",x = df_2$Date[450], y = -5.6, label = "-503 bps", color = "darkred", size=5) +
  annotate("text",x = df_2$Date[150], y = -3, label = "Total days with deficit: 68", color = "black", size=4) +
  annotate("text",x = df_2$Date[153.5], y = -2.5, label = "EGARCH VaR statistics:", color = "black", size=4, fontface="bold") +
  annotate("text",x = df_2$Date[123], y = -3.5, label = "Ratio: 6.76%", color = "black", size=4) +
  annotate("text",x = df_2$Date[140], y = -4, label = "Max deficit: 503 bps", color = "black", size=4) +
  geom_col(aes(y = ifelse(diff_VAR < 0, diff_VAR, 0)), fill = "darkred", alpha=0.5, width = 2) +
  annotate("text",x = aic_df$Date[1195], y = 0.25, label = "VaR deficit (in percentage)", color = "black", size=4) +
  geom_point(aes(y = ifelse(diff_VAR < 0, diff_VAR, NaN)), fill = "darkred", size = 1.5, alpha=0.5) +
  annotate("text",x = df_2$Date[10], y = 19, label = "(b)", color = "black", size=5) +
  geom_line(aes(y = VAR_line), color = "black",linewidth = 0.3) +
  xlab("Date") +
  scale_y_continuous(limits = c(-6, 0.3))+
  ylab("EGARCH VaR deficit (in percentage)") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14))

grid.arrange(VAR, diff_VAR, nrow=2)