
## libraries
library(readxl)
library(dplyr)
library(ggplot2)


## read file SP500 sheet
file_path <- "/home/cv42/Documents/Dissertation/Data/Data.xlsx"
sheet_name <- "df_sp500"

## save in dataframe
df <- as.data.frame(read_excel(file_path, sheet=sheet_name))

##change columns names and Date as date time format
colnames(df) <- c("Date","Open", "High", "Low", "Close", "Volume")
df$Date <- as.Date(df$Date)


## filter data from the first 5 years

start_date <- as.Date("2018-06-11")
end_date <- as.Date("2023-06-11")
df_calibration <- df %>% filter(Date >= start_date & Date <= end_date)

## compute daily returns

returns <- c(NA,(diff(df_calibration$Close) / lag(df_calibration$Close)[2:length(df_calibration$Close)])*100)
df_calibration$Returns <- returns

## delete firs row (NA values)
df_clean <- na.omit(df_calibration)


## daily volatility from data (constant)
sd(df_clean$Returns)

## confidence level
confidence_level <- 0.95

## inverse cumulative normal distribution
qnorm(confidence_level)

##VaR
sd(df_clean$Returns)*qnorm(confidence_level)

# Plot VaR first model
mean <- 0     
volatility <- sd(df_clean$Returns)/100

# Calculate VaR
z_score <- qnorm(1 - confidence_level)
VaR <- -z_score * volatility

# Create plot
x <- seq((mean - 3 * volatility)*100, (mean + 3 * volatility)*100, length.out = 100)
y <- dnorm(x, mean*100, volatility*100)
z<- dt(x, df = 4)
df <- data.frame(x, y)

p <- ggplot(df, aes(x)) + 
  geom_line(aes(y = y), color = "black", linewidth = 0.3) +
  geom_vline(xintercept = (mean - VaR)*100, linetype = "dashed", color = "red", linewidth = 0.7) +
  annotate("text",x = -2.5, y = 0.15, label = "VaR (-2.25%)", color = "red", size=5, angle=90) +
  annotate("text",x = 2.1, y = 0.25, label = "Normal Distribution", color = "black", size=5) +
  xlab("Return (in percentage)") +
  ylab("Density (Normal Distribution)") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14))
print(p)


both <- ggplot(df, aes(x)) + 
  geom_line(aes(y = y), color = "red", linewidth = 0.3) +
  geom_line(aes(y = z), color = "blue", linewidth = 0.3) +
  geom_vline(xintercept = (mean - VaR)*100, linetype = "dashed", color = "red", linewidth = 0.7) +
  geom_vline(xintercept = -2.92, linetype = "dashed", color = "blue", linewidth = 0.7) +
  annotate("text",x = -2, y = 0.3, label = "VaR (-2.25%)", color = "red", size=5, angle=90) +
  annotate("text",x = -2.67, y = 0.3, label = "VaR (-2.92%)", color = "blue", size=5, angle=90) +
  annotate("text",x = 3, y = 0.15, label = "Normal Distribution", color = "red", size=5) +
  annotate("segment", x = -2.25, xend = -2.92, y = 0.2, yend = 0.2,
           colour = "black", size = 0.5, arrow = arrow()) +
  annotate("segment", x = -2.25, xend = -2.92, y = 0.15, yend = 0.15,
           colour = "black", size = 0.5, arrow = arrow()) +
  annotate("text",x = 1.3, y = 0.3, label = "t-Student", color = "blue", size=5) +
  xlab("Return (in percentage)") +
  ylab("Density") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14))
print(both)

