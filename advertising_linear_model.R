library(rgl)

data <- read.csv("data\\Advertising.csv")
data <- data[,-1]
linear_model <- lm(sales ~ TV + radio + newspaper, data=data)

linear_model_sales_TV_radio <- lm(sales ~ TV + radio, data=data)
linear_model_sales_TV_newspaper <- lm(sales ~ TV + newspaper, data=data)
linear_model_sales_newspaper_radio <- lm(sales ~ radio + newspaper, data=data)

sum(cor(data)[4,1:3] ** 2)

par(mfrow=c(1,3))

s3d <- scatterplot3d(data[c("TV", "radio", "sales")], type = "h", color = adjustcolor("blue", alpha=.3),
                     angle = 55, scale.y = 0.7, pch = 16, main = "Sales = b0 + b1 * TV + b2 * radio", )
s3d$plane3d(linear_model_sales_TV_radio)


s3d <- scatterplot3d(data[c("TV", "newspaper", "sales")], type = "h", color = adjustcolor("red", alpha=.3),
                     angle = 55, scale.y = 0.7, pch = 16, main = "Sales = b0 + b1 * TV + b2 * newspaper", )

s3d$plane3d(linear_model_sales_TV_newspaper)


s3d <- scatterplot3d(data[c("newspaper", "radio", "sales")], type = "h", color = adjustcolor("green", alpha=.3),
                     angle = 55, scale.y = 0.7, pch = 16, main = "Sales = b0 + b1 * radio + b2 * newspaper", )

s3d$plane3d(linear_model_sales_newspaper_radio)

dev.off()
