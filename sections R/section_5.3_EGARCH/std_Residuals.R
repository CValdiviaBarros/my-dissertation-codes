
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

#used for standardize residuals
sr<- na.omit(df)


##set Date as date time format and delete nan values
sr$Date <- as.Date(sr$Date)
sr$SR_avg <- mean(sr$std_residuals_EGARCH)

#plot standarized residuals

standarized_residuals <- ggplot(data=sr, aes(x=Date))+
  annotate("text",x = sr$Date[10], y = 4.7, label = "(a)", color = "black", size=5) +
  annotate("text",x = sr$Date[629], y = 4.4, label = "Standardize Residuals EGARCH(1,1) - Normal", color = "black", size=4) +
  geom_point(aes(y=std_residuals_EGARCH), size=2.5, shape=21, alpha=0.5, fill="gray") +
  #scale_fill_gradient2(low = "red", mid = "gray", high = "green", midpoint = 0, 
  #                     limits = c(-8, 8), oob = scales::squish,
  #                     guide = guide_colorbar(title.position = "top", title.hjust = 0.5)) +
  geom_hline(yintercept = sr$SR_avg, linetype="dashed", color="red", linewidth=0.6)+
  xlab("Date")+
  ylab("Standardized Residuals (in percentage)")+
  scale_y_continuous(limits = c(-5, 5))+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14))

#plot empirical vs normal (Standardized residuals)

distributions <- ggplot(data=sr, aes(x=sr$std_residuals_EGARCH))+
  annotate("text",x = -6.3, y = .405, label = "(b)", color = "black", size=5) +
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

QQplot <- ggplot(data=sr, aes(sample=sr$std_residuals_EGARCH))+
  geom_circle(aes(x0 = -3, y0 = -4, r = 2.5), color = "darkgreen", fill = "white", size = 0.8) +
  annotate("text",x = -6, y = 6.5, label = "(c)", color = "black", size=5) +
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
shapiro.test(sr$std_residuals_EGARCH)
