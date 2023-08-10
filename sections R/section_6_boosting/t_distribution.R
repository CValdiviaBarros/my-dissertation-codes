
## libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(Metrics)
library(ggforce)

#plot normal vs t-Distribution

# Create a sequence of x-values
x <- seq(-3, 3, length.out = 1000)

# Create a function to generate t-distribution curves
t_dist <- function(df_val) {
  return(dt(x, df_val))
}

# Create a data frame to store the results
df <- data.frame(x = rep(x, times = 29), df = rep(2:30, each = length(x)))

# Add t-distribution values to the data frame
df$density <- t_dist(df$df)
df$density_df4 <- t_dist(4)

# Define the degrees of freedom range
df_range <- data.frame(df_val = 2:30)
df_range$quantile <- qt(0.05, df_range$df_val)

# Plot the t-distribution curves with color scale
distributions <- ggplot(df, aes(x = x, y = density, color = df)) +
  geom_segment(data = df_range, aes(x = quantile, xend = quantile, y = -0, yend = 0.2, color = df_val),
               linewidth=0.6, alpha = 1) +
  geom_segment(x=min(df_range$quantile) ,xend = qnorm(0.05), y = 0.203, yend = 0.203, color = "black",
               linewidth=0.2 ) +
  geom_segment(x=min(df_range$quantile) ,xend = qnorm(0.05), y = 0.206, yend = 0.206, color = "black",
               linewidth=0.2 ) +
  geom_segment(x=1.5 ,xend = 1.7, y = 0.35, yend = 0.35, color = "blue",
               linewidth=0.6 ) +
  geom_segment(x=qnorm(0.05) ,xend = qnorm(0.05), y = 0, yend = 0.2, color = "blue",
               linewidth=0.35 )+
  geom_segment(aes(x = min(df_range$quantile), xend = max(df_range$quantile), y = 0.15, yend = 0.15),
               color = "black", arrow = arrow(type = 'closed', length=unit(2.2, "mm")), size=0.5) +
  annotate("text", x = -2.29, y = 0.22, 
           label = expression(paste("5"^"th", " percentile (lower tail)")), 
           color = "black", size=5)+
  annotate("text",x = 2.5, y = 0.35, label = "Standard Normal Distribution", color = "black", size=5) +
  geom_line(aes(group = interaction(df, color=factor(df))), linewidth = 0.6) +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), color = "blue", linewidth = 0.6) +
  xlab(expression(mu)) +
  ylab("Density") +
  scale_x_continuous(limits = c(-3, 3)) +
  scale_color_gradient2(low = "red", high = "green", mid = "orange", midpoint = 15,breaks = c(4, 17, 30),
                       guide = guide_colorbar(title.position = "top", title.hjust = 0.5, title = "tStudent df")) +
  geom_segment(aes(x = 0, xend = 0, y = t_dist(2)[500], yend = t_dist(50)[500]),
               color = "black", arrow = arrow(type = 'closed', length=unit(2.2, "mm")), size=0.5) +
  geom_segment(aes(x = 2.6997, xend = 2.6997, y = t_dist(2)[950], yend = t_dist(50)[950]),
               color = "black", arrow = arrow(type = 'closed', length=unit(2.2, "mm")), size=0.5) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        legend.position = c(0.82, 0.68),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 11),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14))

print(distributions)

