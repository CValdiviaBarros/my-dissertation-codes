library(dplyr)
library(ggplot2)
library(gridExtra)
library(ggforce)
library(zoo)
library(openxlsx)
library(readxl)


# load data optimum GJR degrees of freedom

## read file EGARCH
file_path <- "/home/cv42/Documents/Dissertation/sections R/section_6_boosting/GJR_UGARCH_opt.xlsx"

## save in dataframe
opt <- as.data.frame(read_excel(file_path))
opt <- opt[, -1]

##set Date as date time format
opt$Date <- as.Date(opt$Date)
opt<-select(opt, Date, Returns, vol, avg_vol,vol_GJR, AIC_GJR, shape_GJR)

#load  data optimum GJR directly MLE
dir<-read.xlsx("/home/cv42/Documents/Dissertation/sections R/section_6_boosting/GJR_UGARCH.xlsx")
dir$Date <- as.Date(dir$Date)

opt$num_outliers<-dir$num_outliers
opt$kur<-dir$kurtosis

distributions <- ggplot(data = opt, aes(x = shape_GJR)) +
  geom_histogram(aes(fill = ifelse(seq_along(..count..) == 1, "black", "lightgray")), bins = 46, color = "black", alpha = 1, position = "identity", show.legend = FALSE) +
  scale_fill_manual(values = c("black", "lightgray")) +
  geom_segment(x=mean(na.omit(opt$shape_GJR)) ,xend =mean(na.omit(opt$shape_GJR)), y = 0, yend = 200, color = "blue",
               linewidth=1.5, linetype="dashed") +
  geom_segment(x=30,xend =30, y = 9, yend = 70, color = "black",
               linewidth=0.6) +
  geom_segment(x = 30, xend = 50, y = 70, yend = 70,
               color = "black", arrow = arrow(type = 'closed', length=unit(2.2, "mm")), linewidth=0.4) +
  annotate("text",x = 15, y = 198, label = "Avg. Optimum df:", color = "blue", size=5) +
  annotate("text",x = 15, y = 190, label = "12.52", color = "blue", size=5) +
  annotate("text",x = 40, y = 75, label = "df > 30: tStudent dist. ~ Standard Normal dist.", color = "black", size=5) +
  annotate("text",x = 40, y = 65, label = "10.04% total windows", color = "black", size=5) +
  annotate("text", x = 4, y = 90, label = "4.47% total windows", color = "black", size = 5, angle=90) +
  annotate("text", x = 4, y = 24, label = "df = 4 ", color = "white", size = 5, angle=90) +
  annotate("text", x = 50, y = 17, label = "df = 50", color = "white", size = 5, angle=90) +
  xlab("Optimum df (tStudent)") +
  ylab("Frequency") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14))

print(distributions)


aic_df<- select(opt, Date, AIC_GJR, shape_GJR)
aic_df$Date <- as.Date(aic_df$Date)


names(aic_df)[names(aic_df) == "AIC_GJR"] <- "AIC_GJR_opt"
names(aic_df)[names(aic_df) == "shape_GJR"] <- "shape_GJR_opt"


GJR_old<-read.xlsx("/home/cv42/Documents/Dissertation/sections R/section_5.3_EGARCH/EGARCH_UGARCH.xlsx")


aic_df$AIC_GJR_fix<-GJR_old$AIC_GJR
aic_df$shape_GJR_fix<-GJR_old$shape_GJR
aic_df$diff_aic<-aic_df$AIC_GJR_opt-aic_df$AIC_GJR_fix
#aic_df$point<-ifelse(aic_df$shape_GJR_opt == 4 | aic_df$shape_GJR_opt > 30, 0.03, NA)
#aic_df$color<-ifelse(aic_df$shape_GJR_opt == 4, "red", ifelse(aic_df$shape_GJR_opt > 30, "green", NA))
aic_df$point<-aic_df$AIC_GJR_fix/aic_df$AIC_GJR_fix*0.03

#plot difference AIC GJR classic (df=4) and GJR optimum df

AIC_diff <- ggplot(data = aic_df, aes(x = Date, y = diff_aic)) +
  geom_area(aes(y = ifelse(diff_aic < 0, diff_aic, 0)), fill = "darkgreen") +
  geom_area(aes(y = ifelse(diff_aic > 0, diff_aic, 0)), fill = "orange") +
  geom_line(aes(y=diff_aic),color = "black", linewidth = 0.6) +
  annotate("text",x = aic_df$Date[760], y = -0.2, label = "GJR-GARCH(1,1) - tStudent optimum df (lower AIC)", color = "darkgreen", size=5) +
  annotate("text",x = aic_df$Date[125], y = 0.06, label = "df = 4", color = "black", size=4) +
  annotate("text",x = aic_df$Date[125], y = 0.155, label = "df > 30", color = "black", size=4) +
  annotate("text",x = aic_df$Date[48], y = 0.105, label = "Optimum df", color = "black", size=4.5, angle=90) +
  annotate("point",x = aic_df$Date[97], y = 0.155, color = "green", size=0.5, fill="transparent") +
  annotate("point",x = aic_df$Date[97], y = 0.06, color = "red", size=0.5, fill="transparent") +
  annotate("text",x = aic_df$Date[1257/2], y = 0.23, label = "GJR-GARCH(1,1) - tStudent optimum df vs GJR-GARCH(1,1) - tStudent (df = 4)", color = "black", size=5) +
  annotate("text",x = aic_df$Date[1270/2], y = -0.223, label = "(Model 3 v2)", color = "darkgreen", size=5) +
  annotate("text",x = aic_df$Date[1160], y = 0.045, label = "Shape tStudent distribution (optimum df)", color = "black", size=4) +
  annotate("text",x = aic_df$Date[1225], y = 0.01, label = "Delta AIC (R)", color = "black", size=4) +
  geom_curve(aes(x = Date[480], y = -0.05, xend = Date[580], yend = -0.2),arrow = arrow(type = 'closed', length=unit(2.2, "mm")),curvature = 0.4,linewidth=0.5)+
  geom_point(aes(y = point, color = shape_GJR_opt), fill=NA, size = 0.5, show.legend = TRUE) +
  scale_color_gradient2(low = "red", high = "green", mid = "orange", midpoint = 13,limits = c(4, 30),oob = scales::squish,
                        guide = guide_colorbar(title.position = "top", title.hjust = 0.5, title = "tStudent df")) +
  geom_segment(x=aic_df$Date[252] ,xend =aic_df$Date[1257], y = 0.035, yend = 0.035, color = "black",
               linewidth=0.02) +
  geom_segment(x=aic_df$Date[252] ,xend =aic_df$Date[1257], y = 0.025, yend = 0.025, color = "black",
               linewidth=0.02) +
  xlab("Date") +
  ylab("Delta AIC (R)") +
  scale_y_continuous(limits = c(-.25, .25))+
  theme(panel.background = element_rect(fill = "white"),
        legend.position = c(0.1, 0.7),
        legend.title = element_blank(),
        legend.text = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14))

print(AIC_diff)


