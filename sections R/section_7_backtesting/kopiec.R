## libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(Metrics)
library(ggforce)

## read file SP500 sheet
file_path <- "/home/cv42/Documents/Dissertation/sections R/section_7_backtesting/data_kopiec.csv"

## save in dataframe
df <- as.data.frame(read.csv(file_path))
df<- df[24:75,]

#critical values

critical <- ggplot(data=df, aes(x=m)) + 
  geom_ribbon(data = subset(df, m >= 37 & m <= 64),
              aes(ymin = test, ymax = 3.84), alpha =1  ,fill="gray")+
  geom_line(aes(y=test),color = "black", linewidth = 0.8) +
  geom_hline(yintercept = 3.84, linetype="dashed", color="blue", linewidth=0.8)+
  geom_segment(x=37.4 ,xend =37.4, y = 0, yend = 10, color = "darkgreen",
               linewidth=0.4, linetype="dashed") +
  annotate("point", x=37.4,y=3.84, color = "darkred", size = 10, alpha = 1) +
  annotate("text", x=37.4,y=3.84, color = "white", size = 4, fontface="bold", label="38")+
  annotate("point", x=64.4,y=3.84, color = "darkred", size = 10, alpha = 1) +
  annotate("text", x=64.4,y=3.84, color = "white", size = 4, fontface="bold", label="64")+
  geom_segment(x=30 ,xend =32, y = 19, yend = 19, color = "black",
               linewidth=0.65) +
  annotate("text", x=37,y=19, color = "black", size = 5, label="Kupiec Test Value")+
  geom_segment(x=27 ,xend =27, y = 0, yend = 10, color = "red",
               linewidth=0.4, linetype="dashed") +
  annotate("text", x=27,y=12.4, color = "black", size = 5, label="Model 2: GARCH(1,1)", angle=90)+
  annotate("text", x=27.5,y=9.35, color = "red", size = 4, label="(m = 27)", angle=90)+
  geom_segment(x=35 ,xend =35, y = 0, yend = 10, color = "red",
               linewidth=0.4, linetype="dashed") +
  annotate("text", x=35,y=12.6, color = "black", size = 5, label=expression(paste("Model 1: EWMA(", lambda, "=0.82)")), angle=90)+
  annotate("text", x=35.5,y=9.35, color = "red", size = 4, label="(m = 35)", angle=90)+
  annotate("text", x=37.4,y=13.1, color = "black", size = 5, label="Model 3 v2: GJR-GARCH(1,1)", angle=90)+
  annotate("text", x=37.9,y=9.35, color = "darkgreen", size = 4, label="(m = 38)", angle=90)+
  geom_segment(x=68 ,xend =68, y = 0, yend = 10, color = "red",
               linewidth=0.4, linetype="dashed") +
  annotate("text", x=68,y=12.5, color = "black", size = 5, label="Model 4: EGARCH(1,1)", angle=90)+
  annotate("text", x=68.5,y=9.35, color = "red", size = 4, label="(m = 68)", angle=90)+
  geom_segment(x=52 ,xend =52, y = 0, yend = 10, color = "darkgreen",
               linewidth=0.37, linetype="dashed") +
  annotate("text", x=50,y=1.92, color = "white", size = 5, fontface="bold", label="Ho: Accept Model")+
  annotate("text", x=52,y=12.1, color = "black", size = 5, label="Model 5: Mix Model", angle=90)+
  annotate("text", x=52.5,y=9.35, color = "darkgreen", size = 4, label="(m = 52)", angle=90)+
  annotate("text", x=72.3,y=3.5, color = "blue", size = 4, label="(95% Critical Value = 3.84)")+
  xlab("# exceptions (m)") +
  ylab("Kupiec Test Value") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14))
print(critical)