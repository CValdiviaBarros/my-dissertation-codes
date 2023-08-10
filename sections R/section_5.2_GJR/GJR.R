
## libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(Metrics)
library(ggforce)


## read file SP500 sheet
file_path <- "/home/cv42/Documents/Dissertation/sections R/section_5.2_GJR/GJR-GARCH.csv"
sheet_name <- "GJR-GARCH"

## save in dataframe
df <- as.data.frame(read.csv(file_path))

##set Date as date time format
df$Date <- as.Date(df$Date)

df$avg_vol <- mean(df$vol)

df$lag_Returns <- lag(df$Returns)

df_3<-select(df, Date, vol, GJR_vol_est, GARCH_vol_est)
df_3<- na.omit(df_3)


## plot fit GJR model vs theta and lag returns
effective_vol <- ggplot(data=df, aes(x=Date)) + 
  annotate("text",x = df$Date[10], y = 11.9, label = "(a)", color = "black", size=5) +
  annotate("text",x = df$Date[630], y = 10, label = "Absolute Daily Return S&P500 Index (volatility proxy)", color = "black", size=5) +
  annotate("text",x = df$Date[800], y = 5, label = "GJR-GARCH(1,1) model (rolling 252)", color = "blue", size=5) +
  geom_line(aes(y=vol),color = "black", linewidth = 0.3, alpha=0.5) +
  geom_line(aes(y=avg_vol),color = "black", linewidth = 0.6, linetype="dashed") +
  geom_line(aes(y=GJR_vol_est),color = "blue", linewidth = 0.6, alpha=0.5) +
  xlab("Date") +
  ylab("S&P500 daily volatility (in percentage)") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.title.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 14))

theta <- ggplot(df, aes(x = Date)) +
  annotate("text",x = df$Date[10], y = 0.9, label = "(b)", color = "black", size=5) +
  geom_col(aes(y = dummy_theta, fill = dummy_returns), alpha=1, width = 0.5) +
  annotate("text", x = df$Date[850], y = 0.6, 
           label = "italic('Weights')*': '*theta", 
           parse = TRUE,
           color="darkred",
           alpha=1, fontface = "bold")+
  geom_line(aes(y=theta),color = "darkred", linewidth = 1.2, alpha=0.5) +
  annotate("segment", x = df$Date[170], xend = df$Date[170], y = 0, yend = 1, color = "red", linewidth = 0.5)+
  annotate("text",x = df$Date[160], y = 0.5, label = expression(paste(I[(n-1)], "=1 ")), color = "black", size=4, angle=90) +
  annotate("text",x = df$Date[180], y = 0.5, label = expression(paste("color: ", "Lag Returns")), color = "black", size=4, angle=90) +
  scale_fill_gradient(low = "red", high = "white", 
                       limits = c(-10, 0), oob = scales::squish,
                       guide = guide_colorbar(title = expression("Lag Returns (" * u[n-1] * ")"),title.position = "top", title.hjust = 0.5)) +
  ylab(expression("Weight (" * theta * ")"))+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.title.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.position = c(0.1, 0.5))

alpha <- ggplot(df, aes(x = Date)) +
  annotate("text",x = df$Date[10], y = 0.9, label = "(c)", color = "black", size=5) +
  geom_line(aes(y=alpha),color = "orange", linewidth = 1.2, alpha=0.5) +
  annotate("text", x = df$Date[850], y = 0.13, 
           label = "italic('Weights')*': '*alpha", 
           parse = TRUE,
           color="orange",
           alpha=1, fontface = "bold")+
  geom_line(aes(y=beta),color = "darkgreen", linewidth = 1.2, alpha=0.5) +
  annotate("text", x = df$Date[850], y = 0.75, 
           label = "italic('Weights')*': '*beta", 
           parse = TRUE,
           color="darkgreen",
           alpha=1, fontface = "bold")+
  xlab("Date") +
  ylab(expression("Weight (" * alpha *", "* beta * ")"))+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14))

grid.arrange(effective_vol, theta,alpha, nrow=3)

df_2<-select(df, Date, Returns, VAR_GARCH)
df_2$diff_VAR<-df_2$VAR_GARCH+df_2$Returns

# Create a vector of NaN values
new_values <- rep(NaN, nrow(df_2))
# Set values from row 252 until the end to 1
new_values[252:nrow(df)] <- 0

## read file standarized residuals
file_path_2 <- "/home/cv42/Documents/Dissertation/sections R/section_5.2_GJR/GJR-GARCH_standarized.csv"

## save in dataframe
sr <- as.data.frame(read.csv(file_path_2))

##set Date as date time format and delete nan values
sr$Date <- as.Date(sr$Date)
sr<-na.omit(sr)
sr$SR_avg <- mean(sr$std_residuals)
  
#plot qq-plot

QQplot <- ggplot(data=sr, aes(sample=sr$std_residuals))+
  geom_circle(aes(x0 = -3, y0 = -4, r = 2.5), color = "darkred", fill = "white", size = 0.8) +
  annotate("text",x = 0, y = 5.5, label = "QQ-Plot GJR-GARCH(1,1) model", color = "black", size=4, fontface="bold") +
  annotate("text",x = -6, y = 6, label = "(a)", color = "black", size=5) +
  geom_qq(size=2, color="black", alpha=0.5)+
  annotate("segment", x = -7, xend = 7, y = -7, yend = 7, color = "black", linewidth = 0.5, linetype="dashed")+
  xlab("Standardize Residuals: Theoretical Quantiles (Normal Distribution)")+
  ylab("Standardize Residuals: Empirical Quantiles")+
  scale_y_continuous(limits = c(-7, 7))+
  scale_x_continuous(limits = c(-7, 7))+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14))

## read file standarized residuals
file_path_3 <- "/home/cv42/Documents/Dissertation/sections R/section_4_garch/GARCH_standarized.csv"

## save in dataframe
sr_2 <- as.data.frame(read.csv(file_path_3))

##set Date as date time format and delete nan values
sr_2$Date <- as.Date(sr_2$Date)
sr_2<-na.omit(sr_2)
sr_2$SR_avg <- mean(sr_2$std_residuals)


QQplot_2 <- ggplot(data = sr_2, aes(sample = std_residuals)) +
  geom_circle(aes(x0 = -3, y0 = -4, r = 2.5), color = "darkred", fill = "white", size = 0.8) +
  annotate("text", x = 0, y = 5.5, label = "QQ-Plot GARCH(1,1) model", color = "black", size = 4, fontface = "bold") +
  annotate("text", x = -6, y = 6, label = "(b)", color = "black", size = 5) +
  geom_qq(size = 2, color = "black", alpha = 0.5) +
  annotate("segment", x = -7, xend = 7, y = -7, yend = 7, color = "black", linewidth = 0.5, linetype="dashed")+
  xlab("Standardized Residuals: Theoretical Quantiles (Normal Distribution)") +
  ylab("Standardized Residuals: Empirical Quantiles") +
  scale_y_continuous(limits = c(-7, 7)) +
  scale_x_continuous(limits = c(-7, 7)) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14))

grid.arrange(QQplot, QQplot_2, ncol=2)


## read file AIC
file_path_4 <- "/home/cv42/Documents/Dissertation/sections R/section_5.2_GJR/AIC_GJR.csv"

## save in dataframe
df_4 <- as.data.frame(read.csv(file_path_4))

##set Date as date time format and delete nan values
df_4$Date <- as.Date(df_4$Date)
df_4$avg_vol<-mean(df_4$vol)
df_4$below <- df_4$min_AIC/df_4$min_AIC*-4
df_4$above <- df_4$min_AIC/df_4$min_AIC*0

df_4$diff_aic <- df_4$aic_gjr_tStudent-df_4$aic_gjr_normal

## plot fit GJR model vs theta and lag returns
effective_vol_2 <- ggplot(data=df_4, aes(x=Date)) + 
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

AIC_models <- ggplot(data=df_4, aes(x=Date)) + 
  annotate("text",x = df$Date[10], y = 890, label = "(b)", color = "black", size=5) +
  annotate("segment", x = df$Date[50], xend = df$Date[80], y = 740, yend = 740, color = "darkgreen", linewidth = 0.6, alpha=1)+
  annotate("segment", x = df$Date[50], xend = df$Date[80], y = 780, yend = 780, color = "orange", linewidth = 0.6, alpha=1)+
  annotate("segment", x = df$Date[50], xend = df$Date[80], y = 820, yend = 820, color = "darkred", linewidth = 0.6, alpha=1)+
  annotate("text",x = df$Date[187], y = 820, label = "AIC GARCH(1,1) - Normal (avg: 746.4)", color = "black", size=4) +
  annotate("text",x = df$Date[200], y = 780, label = "AIC GJR-GARCH(1,1) - Normal (avg: 738.7)", color = "black", size=4) +
  annotate("text",x = df$Date[203], y = 740, label = "AIC GJR-GARCH(1,1) - tStudent (avg: 729.7)", color = "black", size=4)+ 
  annotate("text", x = df_4$Date[1100], y = 700,
           label = "bold('AIC')[italic('(python)')] == 2*k - 2*ln(hat(L))", 
           parse = TRUE, size=5)+
  geom_line(aes(y=aic_garch),color = "darkred", linewidth = 0.6, alpha=1) +
  geom_line(aes(y=aic_gjr_normal),color = "orange", linewidth = 0.6, alpha=1) +
  geom_line(aes(y=aic_gjr_tStudent),color = "darkgreen", linewidth = 0.6, alpha=1) +
  xlab("Date") +
  ylab("AIC (python)") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.title.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 14))

AIC_best <- ggplot(data=df_4, aes(x=Date)) + 
  geom_point(aes(y = -min_AIC, color = color_AIC), fill=NA, size = 0.5, show.legend = FALSE) +
  annotate("text",x = df$Date[629], y = -0.4, label = "Dot represents the best model (Lower AIC)", color = "black", size=4) +
  annotate("text",x = df$Date[145], y = -0.4, label = "GARCH(1,1) - Normal", color = "black", size=4) +
  annotate("text",x = df$Date[155], y = -1.9, label = "GJR-GARCH(1,1) - Normal", color = "black", size=4) +
  annotate("text",x = df$Date[160], y = -3.4, label = "GJR-GARCH(1,1) - tStudent", color = "black", size=4) +
  annotate("point", x=df_4$Date[70],y=-0.4, color = "darkred", size = 0.5, alpha = 1, fill = NA) +
  annotate("point", x=df_4$Date[70],y=-1.9, color = "orange", size = 0.5, alpha = 1, fill = NA) +
  annotate("point", x=df_4$Date[70],y=-3.4, color = "darkgreen", size = 0.5, alpha = 1, fill = NA) +
  geom_line(aes(y=above),color = "black", linewidth = 0.3) +
  geom_line(aes(y=above+.2),color = "black", linewidth = 0.3) +
  geom_line(aes(y=below),color = "black", linewidth = 0.3) +
  geom_line(aes(y=below-.2),color = "black", linewidth = 0.3) +
  scale_color_manual(values = c("white","darkgreen", "darkred","orange")) +
  xlab("Date") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.title.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_text(size = 14, color = "white"),
        axis.text.y = element_text(size = 14, colour = "white"))

AIC_diff <- ggplot(data = df_4, aes(x = df_4$Date, y = df_4$diff_aic)) +
  annotate("text",x = df$Date[10], y = 18, label = "(c)", color = "black", size=5) +
  geom_area(aes(y = ifelse(diff_aic < 0, df_4$diff_aic, 0)), fill = "darkgreen") +
  geom_area(aes(y = ifelse(diff_aic > 0, df_4$diff_aic, 0)), fill = "orange") +
  geom_line(aes(y=diff_aic),color = "black", linewidth = 0.6) +
  geom_curve(aes(x = Date[730], y = -19, xend = Date[830], yend = -23),arrow = arrow(length = unit(0.03, "npc")),curvature = 0.2,size=0.5)+
  annotate("text",x = df$Date[680], y = 6, label = "GJR-GARCH(1,1) - Normal (lower AIC)", color = "orange", size=4) +
  annotate("text",x = df$Date[945], y = -23, label = "GJR-GARCH(1,1) - tStudent (lower AIC)", color = "darkgreen", size=4) +
  annotate("text",x = df$Date[629], y = 20, label = "GJR-GARCH(1,1): Normal vs tStudent", color = "black", size=4) +
  geom_curve(aes(x = Date[470], y = 1, xend = Date[570], yend = 6),arrow = arrow(length = unit(0.03, "npc")),curvature = -0.2,size=0.5)+
  xlab("Date") +
  ylab("Delta AIC (python)") +
  scale_y_continuous(limits = c(-26, 26))+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14))

heights <- c(1, 0.8, 0.4,1)
grid.arrange(effective_vol_2, AIC_models,AIC_best,AIC_diff, nrow=4,heights = heights)

#plot VAR best GJR-GARCH(1,1) volatility model

## read file AIC
file_path_5 <- "/home/cv42/Documents/Dissertation/sections R/section_5.2_GJR/GJR-GARCH_VAR.csv"

## save in dataframe
df_5 <- as.data.frame(read.csv(file_path_5))

##set Date as date time format and delete nan values
df_5$Date <- as.Date(df_5$Date)

# Create a vector of NaN values
new_values <- rep(NaN, nrow(df_5))
# Set values from row 252 until the end to 1
new_values[252:nrow(df)] <- 0

df_5$VAR_line<-new_values

df_5$diff_VAR<-df_5$VAR_GJR.GARCH+df_5$Returns

## plot VAR GARCH and EWMA_2 models
VAR <- ggplot(data=df_5, aes(x=Date)) + 
  annotate("text",x = df_5$Date[550], y = 4.5, label = "S&P500 daily returns", color = "black", size=5) +
  annotate("text",x = df_5$Date[10], y = 9.5, label = "(a)", color = "black", size=5) +
  annotate("text",x = df_5$Date[730], y = -9.5, label = "Model 3: Value-At-Risk using GJR-GARCH(1,1) (rolling 252)", color = "red", size=5) +
  geom_line(aes(y=Returns),color = "black", linewidth = 0.3, alpha=0.7) +
  geom_line(aes(y=-VAR_GJR.GARCH),color = "red", linewidth = 0.6, alpha=0.5) +
  xlab("Date") +
  ylab("S&P500 daily returns (in percentage)") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.title.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 14))

diff_VAR<- ggplot(data=df_5, aes(x=Date)) + 
  annotate("text",x = df_5$Date[10], y = -0.02, label = "(b)", color = "black", size=5) +
  annotate("text",x = df_5$Date[500], y = -4.5, label = "-393 bps", color = "darkred", size=5) +
  geom_col(aes(y = ifelse(diff_VAR < 0, diff_VAR, 0)), fill = "darkred", alpha=0.5, width = 2) +
  geom_point(aes(y = ifelse(diff_VAR < 0, diff_VAR, NaN)), fill = "darkred", size = 1.5, alpha=0.5) +
  annotate("text",x = df_2$Date[150], y = -3, label = "Total days with deficit: 24", color = "black", size=4) +
  annotate("text",x = df_2$Date[162], y = -2.5, label = "GJR-GARCH VaR statistics:", color = "black", size=4, fontface="bold") +
  annotate("text",x = df_2$Date[123], y = -3.5, label = "Ratio: 2.39%", color = "black", size=4) +
  annotate("text",x = df_2$Date[140], y = -4, label = "Max deficit: 393 bps", color = "black", size=4) +
  annotate("text",x = df_5$Date[10], y = 19, label = "(b)", color = "black", size=5) +
  annotate("text",x = aic_df$Date[1195], y = 0.25, label = "VaR deficit (in percentage)", color = "black", size=4) +
  geom_line(aes(y = VAR_line), color = "black",linewidth = 0.3) +
  xlab("Date") +
  scale_y_continuous(limits = c(-6, 0.3))+
  ylab("VaR deficit (in percentage)") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14))

grid.arrange(VAR, diff_VAR, nrow=2)

