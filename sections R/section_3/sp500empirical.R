
## libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(gridExtra)

## read file SP500 sheet
file_path <- "/home/cv42/Documents/Dissertation/Data/Data.xlsx"
sheet_name <- "df_sp500"

## save in dataframe
df <- as.data.frame(read_excel(file_path, sheet=sheet_name))

##change columns names and Date as date time format
colnames(df) <- c("Date","Open", "High", "Low", "Close", "Volume")
df$Date <- as.Date(df$Date)


## filter data from the firsdt 5 years

start_date <- as.Date("2018-06-11")
end_date <- as.Date("2023-06-11")
df_calibration <- df %>% filter(Date >= start_date & Date <= end_date)

## compute daily returns

returns <- c(NA,(diff(df_calibration$Close) / lag(df_calibration$Close)[2:length(df_calibration$Close)])*100)
df_calibration$Returns <- returns

## delete firs row (NA values)
df_clean <- na.omit(df_calibration)


## plot price
Price <- ggplot(data=df_clean, aes(x=Date,y=Close)) + 
  annotate("text",x = df_clean$Date[48], y = 4700, label = "(a)", color = "black", size=5) +
  geom_line(color = "black", linewidth = 0.3, alpha=0.5) +
  xlab("Date") +
  ylab("S&P500 Index (in dollars)") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14))
print(Price)

## plot return
Return <- ggplot(data=df_clean, aes(x=Date,y=Returns)) + 
  annotate("text",x = df_clean$Date[48], y = 8.5, label = "(b)", color = "black", size=5) +
  geom_line(color = "black", linewidth = 0.3, alpha=0.5) +
  xlab("Date") +
  ylab("S&P500 Index Daily Returns (in percentage)") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14))

grid.arrange(Price, Return, ncol=2)

#plot QQplot

QQplot <- ggplot(data=df_clean, aes(sample=Returns))+
    annotate("text",x = -4.5, y = 4.8, label = "(a)", color = "black", size=5) +
    geom_qq(size=2, color="black", alpha=0.5)+
    geom_abline(intercept = 0, slope=1, linetype="dashed", color="black", linewidth=0.5)+
    xlab("Returns: Theoretical Quantiles (Normal Distribution)")+
    ylab("Returns: Empirical Quantiles")+
  scale_x_continuous(limits = c(-5, 5))+
  scale_y_continuous(limits = c(-5, 5))+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14))

#plot empirical vs normal and t-student

distributions <- ggplot(data=df_clean, aes(x=Returns))+
  annotate("text",x = -4.5, y = 0.51, label = "(b)", color = "black", size=5) +
  annotate("text",x = 2.5, y = 0.4, label = "Histogram S&P500 daily returns", color = "black", size=5) +
  annotate("text",x = 2.1, y = 0.2, label = "Normal", color = "red", size=5) +
  annotate("text",x = 3.8, y = 0.08, label = "t-Student (df=4)", color = "blue", size=5) +
  geom_histogram(aes(y = after_stat(density)), bins = 50, color = "black", fill="lightgray") +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 0.0137*100), color = "red", size = 0.5) +
  stat_function(fun = dt, args = list(df = 4), color = "blue", linetype = "dashed", size = 0.5) +
  xlab("Daily Returns")+
  ylab("Density")+
  scale_x_continuous(limits = c(-5, 5))+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14))

grid.arrange(QQplot, distributions, ncol=2)

##test for normality
shapiro.test(df_clean$Close)

##VaR using t-Student 3df
alpha=0.95
t_value <- qt(alpha, df = 4)
volatility <- sd(df_clean$Returns)
VaR_t <- t_value*volatility

##sim returns using a t-Student
set.seed(69)
n<- length(df_clean$Close)
sigma<- sd(df_clean$Returns)
df<-4
random_values <- rt(n,df)*sigma
print(head(random_values, 10))

df_clean$sim <- random_values

random_values_2 <- rnorm(n)*sigma

df_clean$sim_2 <- random_values_2


## plot empirical return vs t-Student
Empirical <- ggplot(data=df_clean, aes(x=Date)) + 
  annotate("text",x = df_clean$Date[700], y = 7, label = "Empirical Daily Returns", color = "black", size=5) +
  annotate("text",x = df_clean$Date[48], y = 12, label = "(a)", color = "black", size=5) +
  geom_line(aes(y=Returns),color = "black", linewidth = 0.3, alpha=0.5) +
  xlab("Date") +
  ylab("S&P500 Index Daily Returns (in percentage)") +
  scale_y_continuous(limits = c(-15, 15))+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14))

Simulated <- ggplot(data=df_clean, aes(x=Date)) + 
  annotate("text",x = df_clean$Date[700], y = 8, label = "t-Student iid Daily Returns (df=4)", color = "blue", size=5) +
  annotate("text",x = df_clean$Date[700], y = 6, label = "Normal iid Daily Returns", color = "red", size=5) +
  annotate("text",x = df_clean$Date[48], y = 12, label = "(b)", color = "black", size=5) +
  geom_line(aes(y=sim),color = "blue", linewidth = 0.3, alpha=0.5) +
  geom_line(aes(y=sim_2),color = "red", linewidth = 0.3, alpha=0.5) +
  xlab("Date") +
  labs(y = "") +
  scale_y_continuous(limits = c(-15, 15))+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14))

grid.arrange(Empirical, Simulated, ncol=2)



