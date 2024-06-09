library(ggplot2)

data <- read.csv("Finance.csv")
View(data)
head(data)
print(nrow(data))

par(mfrow = c(1,3))
#plot(data$Open, data$Close, pch = 16, col = 'blue', main = "Open vs Close")
plot(data$Volume, data$Close, pch = 16, col = 'blue', main = "Volume vs Close")
plot(data$High, data$Close, pch = 16, col = 'blue', main = "High vs Close")
plot(data$Low, data$Close, pch = 16, col = 'blue', main = "Low vs Close")


#class(data$Date)
summary(lm(data$Close ~ data$High))
summary(lm(data$Close ~ data$Low))
summary(lm(data$Close ~ data$Volume))

dabien <- lm(data$Close ~ data$High + data$Low + data$Volume)

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
confint(dabien)
resid(dabien)

draw_resid <- par(mfrow = c(2, 2))


plot(dabien)


*dabien1 <- lm(data$Close ~ data$High + data$Low)
summary(dabien1)
confint(dabien1)
