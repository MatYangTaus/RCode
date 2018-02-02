library(dplyr)

set.seed(2018-02-01)

n <- 10

data <- data_frame(x = runif(n),
                   y = runif(n))

test <- cor.test(data$x, data$y)

while(!(test$p.value < 0.05 && abs(test$estimate) > 0.9)) {
     data$x <- runif(n)
     data$y <- runif(n)
     test <- cor.test(data$x, data$y)
}

plot(data, xlim = c(0, 1), ylim = c(0, 1), asp = 1)
