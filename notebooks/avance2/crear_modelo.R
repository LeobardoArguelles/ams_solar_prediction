# Cargar datos
data <- read.csv("datos_procesados.csv")

# Crear el modelo
lmm.fit <- lm(ACME ~ poly(dlwrf, 2) + poly(dswrf, 3) + ulwrfatm + tmax +
                tmpsfc + pwat, data=energy.acme)
plot(lmm.fit)
summary(lmm.fit)