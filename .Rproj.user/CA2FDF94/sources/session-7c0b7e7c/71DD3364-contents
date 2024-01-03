library(ggplot2)

data <- read.csv("Finance.csv")
View(data)
head(data)
print(nrow(data))
#class(data$Date)
summary(lm(data$Close ~ data$High))
summary(lm(data$Open ~ data$Low))


dabien <- lm(data$Close ~ data$Open + data$High + data$Low + data$Volume)

predicted_data <- data.frame(Predicted = predict(dabien), Observed = data$Close)
summary(dabien)
# Vẽ đồ thị so sánh giữa dự đoán và giá thực tế
ggplot(predicted_data, aes(x = Observed, y = Predicted)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +  # Vẽ đường hồi quy
  labs(title = "Linear Regression Model Visualization",
       x = "Observed Close Price",
       y = "Predicted Close Price")
fitted(dabien)

resid(dabien)

draw_resid <- par(mfrow = c(2, 2))

plot(dabien)

summary(lm(data$Open ~ data$Volume))
summary(lm(Open ~ High + Low, data = data))

plot(data$Open)
plot(data$Volume)


