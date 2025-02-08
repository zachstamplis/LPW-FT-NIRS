library(plotly)
temp <- ggplot(scan_avg_filter) + 
  geom_point(aes(x = length, y = read_age)) + 
  geom_smooth(method = "lm",aes(x = length, y = read_age))

ggplotly(temp)

temp <- lm(data = scan_avg_filter, read_age~length)
model <- temp
plot(temp)

# Calculate Cook's distance
cooksd <- cooks.distance(model)

# Plot Cook's distance
plot(cooksd, pch = 19, main = "Cook's Distance", ylab = "Cook's Distance")
abline(h = 4 / nrow(scan_avg_filter), col = "red")  # Threshold line


# Calculate standardized residuals
std_resid <- rstandard(model)

# Identify outliers (e.g., |standardized residual| > 2)
outliers <- which(abs(std_resid) > 2)
print(outliers)

temp2 <- scan_avg_filter[outliers, ]
66 73 84 95 
45 50 57 61 


Model           Mean_R2        Mean_RMSE
1  Linear 1 0.416283298958027 18.1541240324697
2  Linear 2 0.428992955262995  17.997554335278
3  Linear 3 0.386496773015425 18.2113258536145
4  Linear 4 0.395228673237983 18.1318629329185
5  Linear 5 0.412503225224622 18.5064003565688
6     GAM 1 0.432840954130841 17.8628098483628
7     GAM 2 0.423328397765526 18.2496128244122
8     GAM 3 0.432222552961419 18.1179662882239
9     GAM 4 0.397882634510109  18.648138819628
10    GAM 5 0.414177246329477 18.0918576172457
11      PLS  0.45799004411534 17.9375188640713
12      VIP 0.473107595875795 17.7108822204832