
## libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(Metrics)


## read file SP500 sheet
file_path <- "/home/cv42/Documents/Dissertation/sections R/section_4_garch/GARCH.csv"
sheet_name <- "GARCH"

## save in dataframe
df <- as.data.frame(read.csv(file_path))

##set Date as date time format
df$Date <- as.Date(df$Date)

df <- df %>%
  arrange(Date) %>%
  mutate(cumbeta = beta,
         cumalpha = alpha + beta,
         cumgamma = alpha + beta + gamma)
df$base<-df$cumalpha*0
df$avg_vol <- mean(df$vol)

df_3<-select(df, Date, vol, GARCH_vol_est, ewma_2)
df_3<- na.omit(df_3)

# Calculate RMSE for Model GARCH
rmse_model_1 <- sqrt(mse(df_3$vol, df_3$GARCH_vol_est))
print(paste("GARCH RMSE: ", rmse_model_1))

# Calculate RMSE for Model EWMA
rmse_model_2 <- sqrt(mse(df_3$vol, df_3$ewma_2))
print(paste("EWMA RMSE: ", rmse_model_2))


## plot VAR EWMA and MA models
effective_vol <- ggplot(data=df, aes(x=Date)) + 
  annotate("text",x = df$Date[10], y = 11.9, label = "(a)", color = "black", size=5) +
  annotate("text",x = df$Date[630], y = 10, label = "Absolute Daily Return S&P500 Index (volatility proxy)", color = "black", size=5) +
  annotate("text",x = df$Date[800], y = 5, label = "GARCH(1,1) model (rolling 252)", color = "blue", size=5) +
  annotate("text",x = df$Date[800], y = 3.8, label = "EWMA model (rolling 252, lambda 0.82)", color = "red", size=5) +
  annotate("text",x = df$Date[1100], y = 11.9, label = "RMSE", color = "black", size=5, fontface = "bold") +
  annotate("text",x = df$Date[1100], y = 10.9, label = "GARCH model: 0.947", color = "black", size=5) +
  annotate("text",x = df$Date[1100], y = 9.9, label = "EWMA model: 0.959", color = "black", size=5) +
  geom_line(aes(y=vol),color = "black", linewidth = 0.3, alpha=0.5) +
  geom_line(aes(y=avg_vol),color = "black", linewidth = 0.6, linetype="dashed") +
  geom_line(aes(y=GARCH_vol_est),color = "blue", linewidth = 0.6, alpha=0.5) +
  geom_line(aes(y=ewma_2),color = "red", linewidth = 0.6, alpha=0.5) +
  geom_rect(aes(xmin = df$Date[1000], xmax = df$Date[1200], ymin = 9, ymax = 12.8),
              fill = "transparent", color = "black", size=0.2, alpha=0)+
  xlab("Date") +
  ylab("S&P500 daily volatility (in percentage)") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.title.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 14))

weights <- ggplot(df, aes(x = Date)) +
  annotate("text",x = df$Date[10], y = 0.95, label = "(b)", color = "black", size=5) +
  geom_area(aes(y = cumgamma), fill = "red", alpha=0.3) +
  annotate("text", x = df$Date[840], y = 0.92, 
           label = "italic('Weights')*': '*gamma", 
           parse = TRUE,
           color="white",
           alpha=1, fontface = "bold")+
  geom_area(aes(y = cumalpha), fill = "orange", alpha=0.3) +
  annotate("text", x = df$Date[530], y = 0.85, 
           label = "italic('Weights')*': '*alpha", 
           parse = TRUE,
           color="white", alpha=1,
           fontface = "bold")+
  geom_area(aes(y = cumbeta), fill = "darkgreen", alpha=0.3) +
  annotate("text", x = df$Date[1100], y = 0.5, 
           label = "italic('Weights')*': '*beta", 
           parse = TRUE,
           color="white",
           alpha=1,
           fontface = "bold")+
  geom_line(aes(y=cumbeta))+
  geom_line(aes(y=cumalpha))+
  geom_line(aes(y=cumgamma))+
  geom_line(aes(y=base))+
  scale_y_continuous(limits = c(-0.05, 1.02)) +
  xlab("Date") +
  ylab("GARCH(1,1) Weights") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14))

grid.arrange(effective_vol, weights, nrow=2)

df_2<-select(df, Date, Returns, VAR_GARCH)
df_2$diff_VAR<-df_2$VAR_GARCH+df_2$Returns

# Create a vector of NaN values
new_values <- rep(NaN, nrow(df_2))
# Set values from row 252 until the end to 1
new_values[252:nrow(df)] <- 0

df_2$VAR_line<-new_values

## plot VAR GARCH and EWMA_2 models
VAR <- ggplot(data=df, aes(x=Date)) + 
  annotate("text",x = df$Date[550], y = 4.5, label = "S&P500 daily returns", color = "black", size=5) +
  annotate("text",x = df$Date[10], y = 9.5, label = "(a)", color = "black", size=5) +
  annotate("text",x = df$Date[730], y = -9.5, label = "Model 2: Value-At-Risk using GARCH(1,1) (rolling 252)", color = "red", size=5) +
  annotate("text",x = df$Date[758], y = -7, label = "Model 1: Value-At-Risk using EWMA(lambda=0.82) (rolling 252)", color = "blue", size=5) +
  geom_line(aes(y=Returns),color = "black", linewidth = 0.3, alpha=0.7) +
  geom_line(aes(y=-VAR_GARCH),color = "red", linewidth = 0.6, alpha=0.5) +
  geom_line(aes(y=-VAR_EMWA_2),color = "blue", linewidth = 0.6, alpha=0.5) +
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
  annotate("text",x = df$Date[500], y = -4, label = "-341 bps", color = "darkred", size=5) +
  geom_col(aes(y = ifelse(diff_VAR < 0, diff_VAR, 0)), fill = "darkred", alpha=0.5, width = 2) +
  geom_point(aes(y = ifelse(diff_VAR < 0, diff_VAR, NaN)), fill = "darkred", size = 1.5, alpha=0.5) +
  annotate("text",x = df_2$Date[10], y = 19, label = "(b)", color = "black", size=5) +
  annotate("text",x = df_2$Date[150], y = -3, label = "Total days with deficit: 27", color = "black", size=4) +
  annotate("text",x = df_2$Date[150], y = -2.5, label = "GARCH VaR statistics:", color = "black", size=4, fontface="bold") +
  annotate("text",x = df_2$Date[123], y = -3.5, label = "Ratio: 2.68%", color = "black", size=4) +
  annotate("text",x = df_2$Date[140], y = -4, label = "Max deficit: 341 bps", color = "black", size=4) +
  annotate("text",x = aic_df$Date[1195], y = 0.25, label = "VaR deficit (in percentage)", color = "black", size=4) +
  geom_line(aes(y = VAR_line), color = "black",linewidth = 0.3) +
  xlab("Date") +
  scale_y_continuous(limits = c(-6, 0.3))+
  ylab("GARCH VaR deficit (in percentage)") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14))

grid.arrange(VAR, diff_VAR, nrow=2)


## read file standarized residuals
file_path_2 <- "/home/cv42/Documents/Dissertation/sections R/section_4_garch/GARCH_standarized.csv"

## save in dataframe
sr <- as.data.frame(read.csv(file_path_2))

##set Date as date time format and delete nan values
sr$Date <- as.Date(sr$Date)
sr<-na.omit(sr)
sr$SR_avg <- mean(sr$std_residuals)

#plot standarized residuals

standarized_residuals <- ggplot(data=sr, aes(x=Date))+
  annotate("text",x = sr$Date[10], y = 4.5, label = "(a)", color = "black", size=5) +
  geom_point(aes(y=std_residuals, fill = Returns), size=2.5, shape=21, alpha=0.5) +
  scale_fill_gradient2(low = "red", mid = "gray", high = "green", midpoint = 0, 
                        limits = c(-8, 8), oob = scales::squish,
                        guide = guide_colorbar(title.position = "top", title.hjust = 0.5)) +
  annotate("text", x = sr$Date[500], y = 3.5, 
           label = expression("Standardized Residuals = " * frac((Returns - hat(mu)), hat(sigma))), 
           parse = TRUE)+
  geom_hline(yintercept = sr$SR_avg, linetype="dashed", color="red", linewidth=0.6)+
  xlab("Date")+
  ylab("Standardized Residuals (in percentage)")+
  scale_y_continuous(limits = c(-5.2, 5.2))+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14))

#plot empirical vs normal (Standardized residuals)

distributions <- ggplot(data=sr, aes(x=sr$std_residuals))+
  annotate("text",x = -6.3, y = .42, label = "(b)", color = "black", size=5) +
  annotate("text",x = 3.5, y = .42, label = "Histogram Standardized Residuals", color = "black", size=4) +
  annotate("text",x = 3.2, y = .35, label = "Standard Normal Distribution", color = "blue", size=4) +
  geom_histogram(aes(y = after_stat(density)), bins = 50, color = "black", fill="lightgray") +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), color = "blue", linewidth = 0.5) +
  xlab("Standardized Residuals (in percentage)")+
  ylab("Density")+
  scale_x_continuous(limits = c(-7, 7))+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14))
  
#plot qq-plot

QQplot <- ggplot(data=sr, aes(sample=sr$std_residuals))+
  annotate("text",x = -6, y = 6, label = "(c)", color = "black", size=5) +
  geom_qq(size=2, color="black", alpha=0.5)+
  geom_abline(intercept = 0, slope=1, linetype="dashed", color="black", linewidth=0.5)+
  xlab("Standardize Residuals: Theoretical Quantiles (Normal Distribution)")+
  ylab("Standardize Residuals: Empirical Quantiles")+
  scale_y_continuous(limits = c(-7, 7))+
  scale_x_continuous(limits = c(-7, 7))+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14))

grid.arrange(standarized_residuals, distributions,QQplot, ncol=3)

##test for normality
shapiro.test(sr$std_residuals)
