## libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(slider)
library(forcats)
library(tidyr)
library(zoo)

## read file SP500 sheet
file_path <- "/home/cv42/Documents/Dissertation/sections R/section_4_ewma/EWMA_MA.csv"
sheet_name <- "EWMA_MA"

## save in dataframe
df <- as.data.frame(read.csv(file_path))

##set Date as date time format
df$Date <- as.Date(df$Date)

df_2<-select(df, Date, Returns, vol)

df_3<-subset(df, vol <= 4)


# Assuming `df` is your dataframe and `returns` is your column of returns

# Define the breaks for the quantiles
breaks <- seq(trunc(min(df_2$Returns))-1, trunc(max(df_2$Returns))+1, by = 1)

# Use cut to create quantile groups
df_2$groups <- cut(df_2$Returns, breaks = breaks, include.lowest = TRUE, labels = paste0("[", breaks[-length(breaks)], ",", breaks[-1], "]"))

#lag vol
df_2 <- df_2 %>%
  mutate(vol_lag = lead(vol, n = 1, default = NA))

#omit nan
df_2 <- na.omit(df_2)

#sum 5 lag vols
df_2 <- df_2 %>%
  mutate(sum_vol_next5 = slide_dbl(.x = vol_lag, .f = ~sum(.x, na.rm = TRUE), .before = 0, .after = 4))

#omite nan
df_2 <- na.omit(df_2)

#average 5 days vol (lag)
df_2$vol_lag_5avg<-df_2$sum_vol_next5/5

#summary lag vol
df_summary_vol <- df_2 %>%
  group_by(groups) %>%
  summarise(average_value = mean(vol_lag, na.rm = TRUE))

#summary 5 days vol (lag)
df_summary_vol5 <- df_2 %>%
  group_by(groups) %>%
  summarise(aveage_5d_vol = mean(vol_lag_5avg, na.rm = TRUE))

#Final data (first plot)
df_summary_vol$average_5d_vol<-df_summary_vol5$aveage_5d_vol
Final_data<-as.data.frame(df_summary_vol)
colnames(Final_data)<-c("Returns", "lag_vol", "avg_5d_lag_vol")
Negative_returns <- Final_data[6:9,]
Positive_returns <- Final_data[10:13,]
Positive_returns <- Positive_returns[rev(1:nrow(Positive_returns)),]
Negative_returns$Returns<-Positive_returns$Returns
Lag_vol_data <- Positive_returns
Lag_vol_data$lag_vol_neg <- Negative_returns$lag_vol
Lag_vol_data$avg_5d_lag_vol_neg <- Negative_returns$avg_5d_lag_vol

leverage_effect <- ggplot(data=Lag_vol_data, aes(x=Returns)) + 
  coord_cartesian(xlim = c(0.5, 4.5))+
  geom_segment(aes(x = Lag_vol_data$Returns[4], xend = Lag_vol_data$Returns[4], y = Lag_vol_data$avg_5d_lag_vol[4]+0.05, yend = Lag_vol_data$avg_5d_lag_vol_neg[4]-0.05),
               color = "gray", arrow = arrow(type = 'closed', length=unit(3.2, "mm")), size=0.3) +
  annotate("text",x = 4.2, y = 1.575, label = "+37 bps", color = "black", size=4) +
  geom_segment(aes(x = Lag_vol_data$Returns[3], xend = Lag_vol_data$Returns[3], y = Lag_vol_data$avg_5d_lag_vol[3]+0.05, yend = Lag_vol_data$avg_5d_lag_vol_neg[3]-0.05),
               color = "gray", arrow = arrow(type = 'closed', length=unit(3.2, "mm")), size=0.3) +
  annotate("text",x = 3.2, y = 1.31, label = "+20 bps", color = "black", size=4) +
  geom_segment(aes(x = Lag_vol_data$Returns[2], xend = Lag_vol_data$Returns[2], y = Lag_vol_data$avg_5d_lag_vol[2]+0.05, yend = Lag_vol_data$avg_5d_lag_vol_neg[2]-0.05),
               color = "gray", arrow = arrow(type = 'closed', length=unit(3.2, "mm")), size=0.3) +
  annotate("text",x = 2.2, y = 1.02, label = "+29 bps", color = "black", size=4) +
  geom_segment(aes(x = Lag_vol_data$Returns[1], xend = Lag_vol_data$Returns[1], y = Lag_vol_data$avg_5d_lag_vol[1]+0.05, yend = Lag_vol_data$avg_5d_lag_vol_neg[1]-0.05),
               color = "gray", arrow = arrow(type = 'closed', length=unit(3.2, "mm")), size=0.3) +
  annotate("text",x = 1.2, y = 0.736, label = "+13 bps", color = "black", size=4) +
  annotate("text",x = 2.5, y = 1.75, label = "S&P500 Leverage Effect: 5 days forward", color = "black", size=4, fontface="bold") +
  geom_point(aes(y = avg_5d_lag_vol), color = "black", size = 12, alpha = 0.4, shape = 21, fill = "darkgreen", stroke = 2) +
  annotate("point", x=0.98,y=1.58, color = "black", size = 8, alpha = 0.4, shape = 21, fill = "darkgreen", stroke = 2) +
  annotate("text",x = 1.8, y = 1.58, label = ": Daily positives returns inside bucket", color = "black", size=4) +
  annotate("point", x=0.98,y=1.65, color = "black", size = 8, alpha = 0.4, shape = 21, fill = "darkred", stroke = 2) +
  annotate("text",x = 1.8, y = 1.65, label = ": Daily negatives returns inside bucket", color = "black", size=4) +
  annotate("text",x = Lag_vol_data$Returns[4], y = Lag_vol_data$avg_5d_lag_vol[4], label = round(Lag_vol_data$avg_5d_lag_vol[4],2), color = "black", size=4, fontface="bold") +
  annotate("text",x = Lag_vol_data$Returns[3], y = Lag_vol_data$avg_5d_lag_vol[3], label = round(Lag_vol_data$avg_5d_lag_vol[3],2), color = "black", size=4, fontface="bold") +
  annotate("text",x = Lag_vol_data$Returns[2], y = Lag_vol_data$avg_5d_lag_vol[2], label = round(Lag_vol_data$avg_5d_lag_vol[2],2), color = "black", size=4, fontface="bold") +
  annotate("text",x = Lag_vol_data$Returns[1], y = Lag_vol_data$avg_5d_lag_vol[1], label = round(Lag_vol_data$avg_5d_lag_vol[1],2), color = "black", size=4, fontface="bold") +
  geom_point(aes(y = avg_5d_lag_vol_neg), color = "black", size = 12, alpha = 0.4, shape = 21, fill = "darkred", stroke = 2)+ 
  annotate("text",x = Lag_vol_data$Returns[4], y = Lag_vol_data$avg_5d_lag_vol_neg[4], label = round(Lag_vol_data$avg_5d_lag_vol_neg[4],2), color = "black", size=4, fontface="bold") +
  annotate("text",x = Lag_vol_data$Returns[3], y = Lag_vol_data$avg_5d_lag_vol_neg[3], label = round(Lag_vol_data$avg_5d_lag_vol_neg[3],2), color = "black", size=4, fontface="bold") +
  annotate("text",x = Lag_vol_data$Returns[2], y = Lag_vol_data$avg_5d_lag_vol_neg[2], label = round(Lag_vol_data$avg_5d_lag_vol_neg[2],2), color = "black", size=4, fontface="bold") +
  annotate("text",x = Lag_vol_data$Returns[1], y = Lag_vol_data$avg_5d_lag_vol_neg[1], label = round(Lag_vol_data$avg_5d_lag_vol_neg[1],2), color = "black", size=4, fontface="bold") +
  annotate("text",x = 0.25, y = 1.76, label = "(b)", color = "black", size=5) +
  xlab("Buckets Absolute Daily Returns (in percentage)") +
  ylab("Volatility: Avg. Abosulute Return (next 5 days)") +
  scale_y_continuous(limits = c(0.6, 1.8))+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14))

leverage_effect_2 <- ggplot(data=Lag_vol_data, aes(x=Returns)) + 
  coord_cartesian(xlim = c(0.5, 4.5))+
  geom_segment(aes(x = Lag_vol_data$Returns[4], xend = Lag_vol_data$Returns[4], y = Lag_vol_data$lag_vol[4]+0.05, yend = Lag_vol_data$lag_vol_neg[4]-0.05),
               color = "gray", arrow = arrow(type = 'closed', length=unit(3.2, "mm")), size=0.3) +
  annotate("text",x = 4.2, y = 1.07, label = "+27 bps", color = "black", size=4) +
  geom_segment(aes(x = Lag_vol_data$Returns[3], xend = Lag_vol_data$Returns[3], y = Lag_vol_data$lag_vol[3]+0.05, yend = Lag_vol_data$lag_vol_neg[3]-0.05),
               color = "gray", arrow = arrow(type = 'closed', length=unit(3.2, "mm")), size=0.3) +
  annotate("text",x = 3.2, y = 1.41, label = "+37 bps", color = "black", size=4) +
  geom_segment(aes(x = Lag_vol_data$Returns[2], xend = Lag_vol_data$Returns[2], y = Lag_vol_data$lag_vol[2]+0.05, yend = Lag_vol_data$lag_vol_neg[2]-0.05),
               color = "gray", arrow = arrow(type = 'closed', length=unit(3.2, "mm")), size=0.3) +
  annotate("text",x = 2.2, y = 1.02, label = "+29 bps", color = "black", size=4) +
  geom_segment(aes(x = Lag_vol_data$Returns[1], xend = Lag_vol_data$Returns[1], y = Lag_vol_data$lag_vol[1]+0.05, yend = Lag_vol_data$lag_vol_neg[1]-0.05),
               color = "gray", arrow = arrow(type = 'closed', length=unit(3.2, "mm")), size=0.3) +
  annotate("text",x = 1.2, y = 0.745, label = "+19 bps", color = "black", size=4) +
  annotate("text",x = 2.5, y = 1.75, label = "S&P500 Leverage Effect: 1 day forward", color = "black", size=4, fontface="bold") +
  geom_point(aes(y = lag_vol), color = "black", size = 12, alpha = 0.4, shape = 21, fill = "darkgreen", stroke = 2) +
  annotate("point", x=0.98,y=1.58, color = "black", size = 8, alpha = 0.4, shape = 21, fill = "darkgreen", stroke = 2) +
  annotate("text",x = 1.8, y = 1.58, label = ": Daily positives returns inside bucket", color = "black", size=4) +
  annotate("point", x=0.98,y=1.65, color = "black", size = 8, alpha = 0.4, shape = 21, fill = "darkred", stroke = 2) +
  annotate("text",x = 1.8, y = 1.65, label = ": Daily negatives returns inside bucket", color = "black", size=4) +
  annotate("text",x = Lag_vol_data$Returns[4], y = Lag_vol_data$lag_vol[4], label = round(Lag_vol_data$lag_vol[4],2), color = "black", size=4, fontface="bold") +
  annotate("text",x = Lag_vol_data$Returns[3], y = Lag_vol_data$lag_vol[3], label = round(Lag_vol_data$lag_vol[3],2), color = "black", size=4, fontface="bold") +
  annotate("text",x = Lag_vol_data$Returns[2], y = Lag_vol_data$lag_vol[2], label = round(Lag_vol_data$lag_vol[2],2), color = "black", size=4, fontface="bold") +
  annotate("text",x = Lag_vol_data$Returns[1], y = Lag_vol_data$lag_vol[1], label = round(Lag_vol_data$lag_vol[1],2), color = "black", size=4, fontface="bold") +
  geom_point(aes(y = lag_vol_neg), color = "black", size = 12, alpha = 0.4, shape = 21, fill = "darkred", stroke = 2)+ 
  annotate("text",x = Lag_vol_data$Returns[4], y = Lag_vol_data$lag_vol_neg[4], label = round(Lag_vol_data$lag_vol_neg[4],2), color = "black", size=4, fontface="bold") +
  annotate("text",x = Lag_vol_data$Returns[3], y = Lag_vol_data$lag_vol_neg[3], label = round(Lag_vol_data$lag_vol_neg[3],2), color = "black", size=4, fontface="bold") +
  annotate("text",x = Lag_vol_data$Returns[2], y = Lag_vol_data$lag_vol_neg[2], label = round(Lag_vol_data$lag_vol_neg[2],2), color = "black", size=4, fontface="bold") +
  annotate("text",x = Lag_vol_data$Returns[1], y = Lag_vol_data$lag_vol_neg[1], label = round(Lag_vol_data$lag_vol_neg[1],2), color = "black", size=4, fontface="bold") +
  annotate("text",x = 0.25, y = 1.76, label = "(a)", color = "black", size=5) +
  xlab("Buckets Absolute Daily Returns (in percentage)") +
  ylab("Volatility: Avg. Abosulute Return (next day)") +
  scale_y_continuous(limits = c(0.6, 1.8))+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14))

grid.arrange(leverage_effect_2, leverage_effect, ncol=2)




