## libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(Metrics)
library(ggforce)
library(tidyverse)
library(zoo)

## read file GJR-optimum
file_path <- "/home/cv42/Documents/Dissertation/sections R/section_6_boosting/GJR_UGARCH_opt.xlsx"

## save in dataframe
df <- as.data.frame(read_excel(file_path))
df <- df[, -1]

##set Date as date time format
df$Date <- as.Date(df$Date)

df$qt<-qt(0.95,df$shape_GJR)

df$VAR_GJR_opt<-df$pred_GJR*df$qt

## read file EGARCH

file_path_5 <- "/home/cv42/Documents/Dissertation/sections R/section_5.3_EGARCH/EGARCH_UGARCH.xlsx"

## save in dataframe
df_5 <- as.data.frame(read_excel(file_path_5))
df_5 <- df_5[, -1]

##columns from EGARCH to GJR dataframe
df$AIC_EGARCH<-df_5$AIC_EGARCH
df$vol_EGARCH<-df_5$vol_EGARCH
df$pred_EGARCH<-df_5$pred_EGARCH
df$qn<--qnorm(0.05)
df$VAR_EGARCH<-df$pred_EGARCH*df$qn
df$diff_aic <- df$AIC_EGARCH-df$AIC_GJR

## select the min AIC
threshold<-0.03
df$threshold_AIC<-pmin(df$AIC_GJR, df$AIC_EGARCH)*threshold

## plot AIC GJR vs AIC EGARCH (gap)

AIC_diff <- ggplot(data = df, aes(x = Date, y = diff_aic)) +
  geom_area(aes(y = ifelse(diff_aic < 0, diff_aic, 0)), fill = "darkgreen") +
  geom_area(aes(y = ifelse(diff_aic > 0, diff_aic, 0)), fill = "orange") +
  geom_line(aes(y=diff_aic),color = "black", linewidth = 0.6) +
  geom_line(aes(y=threshold_AIC),color = "red", linewidth = 0.3) +
  geom_line(aes(y=-threshold_AIC),color = "red", linewidth = 0.3) +
  geom_rect(data = df, aes(xmin =Date[10] , xmax = Date[20], ymin = 0.2, ymax = 0.213), color = "orange", fill = "orange")+
  annotate("text",x = df$Date[130], y = 0.2065, label = "GJR-GARCH (lower AIC)", color = "black", size=5) +
  geom_rect(data = df, aes(xmin =Date[10] , xmax = Date[20], ymin = 0.17, ymax = 0.183), color = "darkgreen", fill = "darkgreen")+
  annotate("text",x = df$Date[120], y = 0.1765, label = "EGARCH (lower AIC)", color = "black", size=5) +
  geom_segment(x=df$Date[7] ,xend =df$Date[23], y = 0.1465, yend = 0.1465, color = "red",
               linewidth=0.3) +
  annotate("text",x = df$Date[131], y = 0.1465, label = "threshold (3% lower AIC)", color = "black", size=5) +
  xlab("Date") +
  ylab("Delta AIC (R)") +
  scale_y_continuous(limits = c(-.25, .25))+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14))

print(AIC_diff)


##VaR mix model
min_AIC <- ifelse(na.omit(df$AIC_GJR) < na.omit(df$AIC_EGARCH), "GJR", "EGARCH")
# Create a vector of NaN values
new_values <- rep(NaN, nrow(df))
# Set values from row 252 until the end to 1
new_values[253:nrow(df)] <- min_AIC
df$min_AIC<-new_values

##Conditional VaR
df$dummy_threshold<-ifelse(abs(df$diff_aic)>df$threshold_AIC,1,0)
df$VAR_mean<-0.5*df$VAR_GJR_opt+0.5*df$VAR_EGARCH

df$VaR_mix<-ifelse(df$dummy_threshold == 1 & df$min_AIC =="GJR", df$VAR_GJR_opt, 
                   ifelse(df$dummy_threshold == 1 & df$min_AIC == "EGARCH", df$VAR_EGARCH, 
                          ifelse(df$dummy_threshold == 0 & df$min_AIC !="NaN",df$VAR_mean, "NaN")))
df$VaR_mix<-as.numeric(as.character(df$VaR_mix))

VAR <- ggplot(data=df, aes(x=Date)) + 
  annotate("text",x = df$Date[550], y = 4.5, label = "S&P500 daily returns", color = "black", size=5) +
  annotate("text",x = df$Date[10], y = 17.4, label = "(a)", color = "black", size=5) +
  annotate("text",x = df$Date[741], y = -10, label = "Model 5: Mix GJR-GARCH and EGARCH models (rolling 252)", color = "red", size=5) +
  geom_line(aes(y=Returns),color = "black", linewidth = 0.3, alpha=0.7) +
  geom_line(aes(y=-VaR_mix),color = "red", linewidth = 0.6, alpha=0.5) +
  annotate("text",x = df$Date[135], y = -14, label = "GJR-GARCH (6.4%)", color = "black", size=4) +
  annotate("text",x = df$Date[150], y = -11.5, label = "Model Selected", color = "black", size=5) +
  geom_segment(x=df$Date[70] ,xend =df$Date[230], y = -12.3, yend = -12.3, color = "black",
               linewidth=0.1) +
  geom_segment(x=df$Date[70] ,xend =df$Date[230], y = -12.5, yend = -12.5, color = "black",
               linewidth=0.1) +
  annotate("text",x = df$Date[125], y = -16, label = "EGARCH (1.6%)", color = "black", size=4) +
  annotate("text",x = df$Date[167], y = -18, label = "Average GJR & EGARCH (92.0%)", color = "black", size=4) +
  annotate("point", x=df$Date[70],y=-14, color = "orange", size = 1, alpha = 1, fill = NA) +
  annotate("point", x=df$Date[70],y=-16, color = "darkgreen", size = 1, alpha = 1, fill = NA) +
  annotate("point", x=df$Date[70],y=-18, color = "gray", size = 1, alpha = 1, fill = NA) +
  xlab("Date") +
  ylab("S&P500 daily returns (in percentage)") +
  scale_y_continuous(limits = c(-18, 18))+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.title.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 14))

df$VAR_color<-ifelse(df$dummy_threshold == 1 & df$min_AIC =="GJR", "orange", 
                     ifelse(df$dummy_threshold == 1 & df$min_AIC == "EGARCH", "darkgreen", 
                            ifelse(df$dummy_threshold == 0 & df$min_AIC !="NaN","blue", "NaN")))

AIC_best <- ggplot(data=df, aes(x=Date)) + 
  geom_point(aes(y = 2, color = VAR_color), fill=NA, size = 0.5, show.legend = FALSE) +
  scale_color_manual(values = c("gray","darkgreen","white","orange")) +
  xlab("Date") +
  ylab("AIC") +
  scale_y_continuous(limits = c(1.9, 2.1))+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.title.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_text(size = 14, color = "white"),
        axis.text.y = element_text(size = 14, colour = "white"))

df$diff_VAR<-df$VaR_mix+df$Returns
new_values_2 <- rep(NaN, nrow(df))
# Set values from row 252 until the end to 1
new_values_2[253:nrow(df)] <- 0

df$VAR_line<-new_values_2

deficit<- ggplot(data=df, aes(x=Date)) + 
  annotate("text",x = df$Date[10], y = -0.02, label = "(b)", color = "black", size=5) +
  annotate("text",x = df$Date[505], y = -4.7, label = "-422 bps", color = "darkred", size=5) +
  annotate("text",x = df$Date[135], y = -3, label = "Total days with deficit: 52", color = "black", size=4) +
  annotate("text",x = df$Date[135], y = -2.5, label = "Model 5 VaR statistics:", color = "black", size=4, fontface="bold") +
  annotate("text",x = df$Date[107.5], y = -3.5, label = "Ratio: 5.17%", color = "black", size=4) +
  annotate("text",x = df$Date[125], y = -4, label = "Max deficit: 422 bps", color = "black", size=4) +
  geom_col(aes(y = ifelse(diff_VAR < 0, diff_VAR, 0)), fill = "darkred", alpha=0.5, width = 2) +
  annotate("text",x = df$Date[1195], y = 0.25, label = "VaR deficit (in percentage)", color = "black", size=4) +
  geom_point(aes(y = ifelse(diff_VAR < 0, diff_VAR, NaN)), fill = "darkred", size = 1.5, alpha=0.5) +
  geom_line(aes(y = VAR_line), color = "black",linewidth = 0.3) +
  xlab("Date") +
  scale_y_continuous(limits = c(-6, 0.3))+
  ylab("VaR deficit (in percentage)") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14))


heights <- c(1, 0.1,0.7)
grid.arrange(VAR, AIC_best,deficit, nrow=3,heights = heights)


##rolling count 252 (deficit in percent)
df_2<-select(df, Date, Returns,vol,VaR_mix, VAR_GJR_opt, VAR_EGARCH)
df_2$deff_GJR<-df_2$VAR_GJR_opt+df_2$Returns
df_2$deff_EGARCH<-df_2$VAR_EGARCH+df_2$Returns
df_2$deff_mix<-df_2$VaR_mix+df_2$Returns

##EGARCH rolling 252 deficit ratio
df_2$deff_EGARCH[df_2$deff_EGARCH >= 0] <- 0
df_2$deff_EGARCH[df_2$deff_EGARCH < 0] <- 1
roll_sum_EGARCH <- rollapply(df_2$deff_EGARCH, width=252, FUN=sum, align="right", fill=NA, partial=TRUE)
df_2$sum_EGARCH <- roll_sum_EGARCH/252*100

##GJR rolling 252 deficit ratio
df_2$deff_GJR[df_2$deff_GJR >= 0] <- 0
df_2$deff_GJR[df_2$deff_GJR < 0] <- 1
roll_sum_GJR <- rollapply(df_2$deff_GJR, width=252, FUN=sum, align="right", fill=NA, partial=TRUE)
df_2$sum_GJR <- roll_sum_GJR/252*100

##MIX  rolling 252 deficit ratio
df_2$deff_mix[df_2$deff_mix >= 0] <- 0
df_2$deff_mix[df_2$deff_mix < 0] <- 1
roll_sum_mix <- rollapply(df_2$deff_mix, width=252, FUN=sum, align="right", fill=NA, partial=TRUE)
df_2$sum_mix <- roll_sum_mix/252*100

new_values_3 <- rep(NaN, nrow(df_2))
# Set values from row 252 until the end to 1
new_values_3[504:nrow(df_2)] <- 5
df_2$ideal<-new_values_3

new_values_4 <- rep(NaN, nrow(df_2))
# Set values from row 252 until the end to 1
new_values_4[504:nrow(df_2)] <- mean(na.omit(df_2$sum_mix))
df_2$mix_avg<-new_values_4

ratio <- ggplot(data=df_2, aes(x=Date)) + 
  geom_line(aes(y=df_2$sum_mix),color = "red", linewidth = 1) +
  geom_line(aes(y=df_2$ideal),color = "black", linewidth = 0.6, alpha=0.5, linetype="dashed") +
  geom_line(aes(y=df_2$mix_avg),color = "red", linewidth = 1, alpha=0.5, linetype="dashed") +
  geom_ribbon(aes(ymin = 5, ymax = ifelse(sum_mix > 5,sum_mix,5)), 
              fill = "darkred", alpha = 0.1)+
  geom_ribbon(aes(ymin = ifelse(sum_mix < 5,sum_mix,5), ymax =5), 
              fill = "darkred", alpha = 0.1)+
  annotate("text",x = df$Date[850], y = 5.2, label = "Ideal Model: 5.00%", color = "gray", size=4) +
  annotate("text",x = df$Date[250], y = 9, label = "Model 5 deficit ratio (rolling 252 days)", color = "black", size=5) +
  annotate("text",x = df$Date[850], y = 5.7, label = "Model 5 avg Rolling 252 days deficit ratio (5.47%)", color = "red", size=4, alpha=0.5) +
  geom_segment(x=df$Date[100] ,xend =df$Date[120], y = 9, yend = 9, color = "red",
               linewidth=1) +
  xlab("Date") +
  ylab("VaR deficit ratio (in percentage)") +
  scale_y_continuous(limits = c(0,10))+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14))

print(ratio)