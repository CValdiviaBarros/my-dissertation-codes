## libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(Metrics)
library(ggforce)
library(tidyverse)

## read file GJR-optimum
file_path <- "/home/cv42/Documents/Dissertation/sections R/section_6_boosting/GJR_UGARCH_opt.xlsx"

## save in dataframe
df <- as.data.frame(read_excel(file_path))
df <- df[, -1]

##set Date as date time format
df$Date <- as.Date(df$Date)

df$qt<-qt(0.95,df$shape_GJR)

df$VAR_GJR_opt<-df$pred_GJR*df$qt

df$diff_VAR<-df$VAR_GJR_opt+df$Returns

# Create a vector of NaN values
new_values <- rep(NaN, nrow(df))
# Set values from row 252 until the end to 1
new_values[253:nrow(df)] <- 0

df$VAR_line<-new_values
df$avg_vol<-mean(df$vol)

file_path_5 <- "/home/cv42/Documents/Dissertation/sections R/section_5.3_EGARCH/EGARCH_UGARCH.xlsx"

## save in dataframe
df_5 <- as.data.frame(read_excel(file_path_5))

df$VAR_GJR_fix<--qt(0.05,df_5$shape_GJR)*df_5$pred_GJR
df$delta_VAR_models<--df$VAR_GJR_fix+df$VAR_GJR_opt
df$point<-df$shape_GJR/df$shape_GJR*2.5

## plot VAR GARCH and EWMA_2 models
VAR <- ggplot(data=df, aes(x=Date)) + 
  annotate("text",x = df$Date[550], y = 4.5, label = "S&P500 daily returns", color = "black", size=5) +
  annotate("text",x = df$Date[10], y = 9.5, label = "(a)", color = "black", size=5) +
  annotate("text",x = df$Date[741], y = -10, label = "Model 3: Value-At-Risk using GJR-GARCH(1,1) (rolling 252)", color = "blue", size=5) +
  annotate("text",x = df$Date[750], y = -13.5, label = "Model 3 v2: Value-At-Risk using GJR-GARCH(1,1) (rolling 252)", color = "red", size=5) +
  geom_line(aes(y=Returns),color = "black", linewidth = 0.3, alpha=0.7) +
  geom_line(aes(y=-VAR_GJR_opt),color = "red", linewidth = 0.6, alpha=0.5) +
  geom_line(aes(y=-VAR_GJR_fix),color = "blue", linewidth = 0.3, alpha=0.5) +
  xlab("Date") +
  ylab("S&P500 daily returns (in percentage)") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.title.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 14))

diff_VAR <- ggplot(data = df, aes(x = Date, y = delta_VAR_models)) +
  geom_area(aes(y = ifelse(delta_VAR_models < 0, delta_VAR_models, 0)), fill = "gray") +
  annotate("text",x = df$Date[10], y = 4.9, label = "(b)", color = "black", size=5) +
  geom_area(aes(y = ifelse(delta_VAR_models > 0, delta_VAR_models, 0)), fill = "blue") +
  geom_line(aes(y=delta_VAR_models),color = "black", linewidth = 0.6) +
  annotate("text",x = df$Date[125], y = 0.4, label = "df = 4", color = "black", size=4) +
  annotate("text",x = df$Date[125], y = 3.8, label = "df > 30", color = "black", size=4) +
  annotate("text",x = df$Date[48], y = 2.03, label = "Optimum df", color = "black", size=4.5, angle=90) +
  annotate("point",x = df$Date[97], y = 3.8, color = "green", size=0.5, fill="transparent") +
  annotate("point",x = df$Date[97], y = 0.4, color = "red", size=0.5, fill="transparent") +
  annotate("text",x = df$Date[749], y = -3.2, label = "Model 3 v2 VaR estimates are less conservative than Model 3", color = "black", size=5) +
  annotate("text",x = df$Date[1160], y = 3.2, label = "Shape tStudent distribution (optimum df)", color = "black", size=4) +
  annotate("text",x = df$Date[1177], y = 0.4, label = "Delta VaR (Model 3 v2 - Model 3)", color = "black", size=4) +
  geom_curve(aes(x = Date[450], y = -2, xend = Date[550], yend = -3.2),arrow = arrow(type = 'closed', length=unit(2.2, "mm")),curvature = 0.4,linewidth=0.5)+
  geom_point(aes(y = point, color = shape_GJR, fill = ifelse(is_na, "white", NA)), fill=NA, size = 0.5, show.legend = TRUE) +
  scale_color_gradient2(low = "red", high = "green", mid = "orange", midpoint = 13,limits = c(4, 30),oob = scales::squish,
                        guide = guide_colorbar(title.position = "top", title.hjust = 0.5, title = "tStudent df")) +
  geom_segment(x=df$Date[252] ,xend =df$Date[1258], y = 2.8, yend = 2.8, color = "black",
               linewidth=0.02) +
  geom_segment(x=df$Date[252] ,xend =df$Date[1258], y = 2.2, yend = 2.2, color = "black",
               linewidth=0.02) +
  xlab("Date") +
  ylab("Delta VaR (Model 3 v2 - Model 3)") +
  scale_y_continuous(limits = c(-5, 5))+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        legend.position = c(0.1, 0.7),
        legend.title = element_blank(),
        legend.text = element_blank(),
        axis.title.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 14))

deficit<- ggplot(data=df, aes(x=Date)) + 
  annotate("text",x = df$Date[10], y = -0.02, label = "(c)", color = "black", size=5) +
  annotate("text",x = df$Date[505], y = -4.9, label = "-424 bps", color = "darkred", size=5) +
  annotate("text",x = df$Date[150], y = -3, label = "Total days with deficit: 38", color = "black", size=4) +
  annotate("text",x = df$Date[170], y = -2.5, label = "GJR-GARCH v2 VaR statistics:", color = "black", size=4, fontface="bold") +
  annotate("text",x = df$Date[123], y = -3.5, label = "Ratio: 3.77%", color = "black", size=4) +
  annotate("text",x = df$Date[140], y = -4, label = "Max deficit: 424 bps", color = "black", size=4) +
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

grid.arrange(VAR,diff_VAR, deficit,nrow=3)