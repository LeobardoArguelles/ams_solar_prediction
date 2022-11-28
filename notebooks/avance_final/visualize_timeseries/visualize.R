
library(forecast)
data <- subset(read.csv("../datos_procesados.csv"), select=-c(X))

data[1:32, c("Date", "apcp")]

## tsdata <- ts(data$apcp, frequency=1)
## apcp_data <- decompose(tsdata, "multiplicative")

## x11()
## ## png(file="timeseries.png")
## plot(apcp_data)
## locator(1)
## ## dev.off()
