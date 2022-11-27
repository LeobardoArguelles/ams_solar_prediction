library(MASS)

data <- read.csv("../datos_procesados.csv")
data <- subset(data, select = -c(X))

full.model <- lm(ACME ~ ., data = data)

step.model <- stepAIC(full.model, direction = "both", trace = FALSE)

summary(step.model)
