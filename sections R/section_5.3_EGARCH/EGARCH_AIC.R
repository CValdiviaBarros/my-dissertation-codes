## libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(Metrics)
library(ggforce)
library(zoo)

## read file EGARCH
file_path <- "/home/cv42/Documents/Dissertation/sections R/section_5.3_EGARCH/EGARCH_UGARCH.xlsx"

## save in dataframe
df <- as.data.frame(read_excel(file_path))
df <- df[, -1]

##set Date as date time format
df$Date <- as.Date(df$Date)

df<-select(df, Date, vol, AIC_EGARCH, AIC_GJR)
df$min_AIC<-pmin(df$AIC_EGARCH,df$AIC_GJR)
df$color_AIC<-ifelse(df$AIC_EGARCH <= df$AIC_GJR, "darkgreen", "orange")
df$min_AIC<-ifelse(df$AIC_EGARCH <= df$AIC_GJR, 2, 1)
df$vol_rolling_252 <- rollmean(df$vol, k=252, fill = NA, align = "right")
df$avg_vol<-mean(df$vol)
df$below <- df$min_AIC/df$min_AIC*-3
df$above <- df$min_AIC/df$min_AIC*0
df$diff_aic <- df$AIC_EGARCH-df$AIC_GJR

## plot fit GJR model vs theta and lag returns
effective_vol_2 <- ggplot(data=df, aes(x=Date)) + 
  annotate("text",x = df$Date[10], y = 11.9, label = "(a)", color = "black", size=5) +
  annotate("segment", x = df$Date[50], xend = df$Date[80], y = 10.5, yend = 10.5, color = "black", linewidth = 0.3, alpha=0.5)+
  annotate("text",x = df$Date[171], y = 9.5, label = "Average Abosolute Daily Return", color = "black", size=4) +
  annotate("segment", x = df$Date[50], xend = df$Date[80], y = 9.5, yend = 9.5, color = "black", linewidth = 1.2, alpha=0.5, linetype="dashed")+
  annotate("text",x = df$Date[185], y = 10.5, label = "Absolute Daily Return (proxy volatility)", color = "black", size=4) +
  annotate("segment", x = df$Date[50], xend = df$Date[80], y = 8.5, yend = 8.5, color = "red", linewidth = 1.2, alpha=0.5)+
  annotate("text",x = df$Date[175], y = 8.5, label = "Abosolute Daily Return (MA=252)", color = "black", size=4) +
  geom_line(aes(y=vol),color = "black", linewidth = 0.3, alpha=0.5) +
  geom_line(aes(y=vol_rolling_252),color = "red", linewidth = 1.2, alpha=0.5) +
  geom_line(aes(y=avg_vol),color = "black", linewidth = 1.2, alpha=0.5, linetype="dashed") +
  xlab("Date") +
  ylab("S&P500 daily volatility (in percentage)") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.title.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 14))

## plot AIC GJR models vs AIC conventional GARCH(1,1)

AIC_models <- ggplot(data=df, aes(x=Date)) + 
  annotate("text",x = df$Date[10], y = 3.68, label = "(b)", color = "black", size=5) +
  annotate("segment", x = df$Date[50], xend = df$Date[80], y = 2.8, yend = 2.8, color = "darkgreen", linewidth = 0.6, alpha=1)+
  annotate("segment", x = df$Date[50], xend = df$Date[80], y=3 , yend = 3, color = "orange", linewidth = 0.6, alpha=1)+
  annotate("text",x = df$Date[200], y = 3, label = "AIC GJR-GARCH(1,1) - tStudent (avg: 2.917)", color = "black", size=4) +
  annotate("text",x = df$Date[188], y = 2.8, label = "AIC EGARCH(1,1) - Normal (avg: 2.918)", color = "black", size=4)+ 
  annotate("text", x = df$Date[1100], y = 2.5,
           label = "bold('AIC')[italic('(R)')] == frac(2*k - 2*ln(hat(L)), N)", 
           parse = TRUE, size=5)+
  geom_line(aes(y=AIC_GJR),color = "orange", linewidth = 0.6, alpha=1) +
  geom_line(aes(y=AIC_EGARCH),color = "darkgreen", linewidth = 0.6, alpha=1) +
  xlab("Date") +
  ylab("AIC (R)") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.title.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 14))

AIC_best <- ggplot(data=df, aes(x=Date)) + 
  geom_point(aes(y = -min_AIC, color = color_AIC), fill=NA, size = 0.5, show.legend = FALSE) +
  annotate("text",x = df$Date[629], y = -0.4, label = "Dot represents the best model (Lower AIC)", color = "black", size=4) +
  annotate("text",x = df$Date[155], y = -1, label = "GJR-GARCH(1,1) - tStudent", color = "black", size=4) +
  annotate("text",x = df$Date[145], y = -2, label = "EGARCH(1,1) - Normal", color = "black", size=4) +
  annotate("point", x=df$Date[70],y=-1, color = "orange", size = 0.5, alpha = 1, fill = NA) +
  annotate("point", x=df$Date[70],y=-2, color = "darkgreen", size = 0.5, alpha = 1, fill = NA) +
  geom_line(aes(y=above),color = "black", linewidth = 0.3) +
  geom_line(aes(y=above+.2),color = "black", linewidth = 0.3) +
  geom_line(aes(y=below),color = "black", linewidth = 0.3) +
  geom_line(aes(y=below-.2),color = "black", linewidth = 0.3) +
  scale_color_manual(values = c("darkgreen","orange","white")) +
  xlab("Date") +
  ylab("AIC") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.title.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_text(size = 14, color = "white"),
        axis.text.y = element_text(size = 14, colour = "white"))

AIC_diff <- ggplot(data = df, aes(x = df$Date, y = df$diff_aic)) +
  annotate("text",x = df$Date[10], y = 0.2, label = "(c)", color = "black", size=5) +
  geom_area(aes(y = ifelse(diff_aic < 0, df$diff_aic, 0)), fill = "darkgreen") +
  geom_area(aes(y = ifelse(diff_aic > 0, df$diff_aic, 0)), fill = "orange") +
  geom_line(aes(y=diff_aic),color = "black", linewidth = 0.6) +
  geom_curve(aes(x = Date[1100], y = -0.04, xend = Date[1000], yend = -0.2),arrow = arrow(length = unit(0.03, "npc")),curvature = -0.2,linewidth=0.5)+
  annotate("text",x = df$Date[970], y = 0.2, label = "GJR-GARCH(1,1) - tStudent (lower AIC)", color = "orange", size=4) +
  annotate("text",x = df$Date[895], y = -0.2, label = "EGARCH(1,1) - Normal (lower AIC)", color = "darkgreen", size=4) +
  geom_curve(aes(x = Date[750], y = 0.06, xend = Date[850], yend = 0.2),arrow = arrow(length = unit(0.03, "npc")),curvature = -0.2,size=0.5)+
  xlab("Date") +
  ylab("Delta AIC (R)") +
  scale_y_continuous(limits = c(-.25, .25))+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14))

heights <- c(1, 0.8, 0.4,1)
grid.arrange(effective_vol_2, AIC_models,AIC_best,AIC_diff, nrow=4,heights = heights)