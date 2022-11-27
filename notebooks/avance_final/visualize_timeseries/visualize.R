
data <- read.csv("../datos_procesados.csv")

x11()
png(file="timeseries.png")
plot(1:length(data$ACME), data$ACME, type="l", col=4)
locator(1)
dev.off()
