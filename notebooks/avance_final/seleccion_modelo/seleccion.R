
## library(caret)

data <- read.csv("datos_procesados.csv")

## Especificar el método de validación cruzada
## ctrl <- trainControl(method = "cv", number = 10)
## Split data
spec = c(train = .7, test = .2, validate = .1)

g = sample(cut(
  seq(nrow(data)),
  nrow(data)*cumsum(c(0,spec)),
  labels = names(spec)
))

split_data = split(data, g)

## Ajustal el modelo

## linear <- train(ACME ~ apcp + dlwrf + dswrf + pres_msl + pwat + spfh + tcolc +
##                 tmax + tmin + tmp2m + tmpsfc + ulwrfsfc + ulwrfatm,
##                 data = data, method = "lm", trControl = ctrl)
## poly <- train(ACME ~ apcp + poly(dlwrf, 2) + poly(dswrf, 3) + pres_msl + pwat + spfh + tcolc +
##                 tmax + tmin + tmp2m + tmpsfc + ulwrfsfc + ulwrfatm,
##                 data = data, method = "lm", trControl = ctrl)
## bag <- train(ACME ~ apcp + dlwrf + dswrf + pres_msl + pwat + spfh + tcolc +
##                 tmax + tmin + tmp2m + tmpsfc + ulwrfsfc + ulwrfatm,
##                 data = data, method = "treebag", trControl = ctrl)

## View summary
## print(linear)
## print(poly)
linear <- lm(ACME ~ apcp + dlwrf + dswrf + pres_msl + pwat + spfh + tcolc +
                tmax + tmin + tmp2m + tmpsfc + ulwrfsfc + ulwrfatm,
                data = split_data$train)
poly <- lm(ACME ~ apcp + poly(dlwrf, 2) + poly(dswrf, 3) + pres_msl + pwat + spfh + tcolc +
                tmax + tmin + tmp2m + tmpsfc + ulwrfsfc + ulwrfatm,
                data = split_data$train)
predictions_poly <- predict(poly, split_data$test)
predictions_linear <- predict(linear, split_data$test)
x11()
par(mfrow=c(2,1))
plot(predictions_poly, split_data$test[,"ACME"], xlab = "Predictions", ylab="Actual")
abline(a=0, b=1, col="red", lwd=2)
plot(predictions_linear, split_data$test[,"ACME"], xlab = "Predictions", ylab="Actual")
abline(a=0, b=1, col="red", lwd=2)
locator(1)
