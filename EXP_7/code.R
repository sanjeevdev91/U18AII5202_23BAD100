data$Likes <- as.numeric(data$Likes)
data$Shares <- as.numeric(data$Shares)

data <- na.omit(data)

plot(data$Likes, data$Shares,
     main = "Over-Plotting in Social Media Data",
     xlab = "Likes",
     ylab = "Shares",
     col = "blue",
     pch = 16)

plot(data$Likes, data$Shares,
     col = rgb(0, 0, 1, alpha = 0.2),
     pch = 16,
     main = "Alpha Blending to Reduce Over-Plotting",
     xlab = "Likes",
     ylab = "Shares")

plot(jitter(data$Likes),
     jitter(data$Shares),
     col = "darkgreen",
     pch = 16,
     main = "Jittering Technique",
     xlab = "Likes",
     ylab = "Shares")

likes_bins <- cut(data$Likes, breaks = 10)
shares_bins <- cut(data$Shares, breaks = 10)

agg_data <- table(likes_bins, shares_bins)

heatmap(agg_data,
        col = heat.colors(256),
        main = "Aggregation and Binning Heatmap")
