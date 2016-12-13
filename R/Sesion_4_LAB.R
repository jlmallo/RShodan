
download.file(url = "https://raw.githubusercontent.com/humbertcostas/courses/master/04_ExploratoryAnalysis/exploratoryGraphs/data/avgpm25.csv", destfile = "./avgpm25.csv")

pollution <- read.csv("~/Downloads/avgpm25.csv", colClasses = c("numeric", "character", "factor", "numeric", "numeric"))

head(pollution, n=2)

summary(pollution$pm25)

#Histograma con observaciones y 50 divisiones
hist(pollution$pm25, col = "green", breaks = 50)
rug(pollution$pm25)

#Añadimos un par de linias verticales
abline(v = 12, lwd = 2)
abline(v = median(pollution$pm25), col = "magenta", lwd = 4)

#Gráfica de barras
barplot(table(pollution$region), col = "wheat", main = "Observaciones por Region")

# Gráfica tipo pie ("quesito")
library(MASS); pie(table(pollution$region))

## Exploración rápida de DOS dimensión
# Multiple Boxplots
boxplot(pm25 ~ region, data = pollution, col = "red")


# Multiple Histograms
par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))
hist(subset(pollution, region == "east")$pm25, col = "green")
hist(subset(pollution, region == "west")$pm25, col = "green")


# Gráficos de dispersion con color
with(pollution, plot(latitude, pm25, col = region))
abline(h = 12, lwd = 2, lty = 2)


# Dispersión multiple
par(mfrow = c(1, 2), mar = c(5, 4, 2, 1))
with(subset(pollution, region == "west"), plot(latitude, pm25, main = "West"))
with(subset(pollution, region == "east"), plot(latitude, pm25, main = "East"))


# Sistema Base
library(datasets)
data(cars)
with(cars, plot(speed, dist))

# lattice
library(lattice)
state <- data.frame(state.x77, region = state.region)
xyplot(Life.Exp ~ Income | region, data = state, layout = c(4, 1))

# ggplot2
library(ggplot2)
data(mpg)
qplot(displ, hwy, data = mpg)

# barplot
barplot(height = c(10,5,3,6,12))


# Histograma
library(datasets)
hist(airquality$Ozone)

# Dispersión
with(airquality, plot(Wind, Ozone))


# Boxplot
airquality <- transform(airquality, Month = factor(Month))
boxplot(Ozone ~ Month, airquality, xlab="Month", ylab="Ozone")

# Gráfica básica con título
with(airquality, plot(Wind, Ozone))
title(main = "Ozone and Wind in New York City")

#Gráfica básica con título y puntos de colores
with(airquality, plot(Wind, Ozone, main = "Ozone and Wind in New York City"))
with(subset(airquality, Month == 5), points(Wind, Ozone, col = "blue"))

#Gráfica básica con ... muchos extras!
with(airquality, plot(Wind, Ozone, main = "Ozone and Wind in New York City", type = "n"))
with(subset(airquality, Month == 5), points(Wind, Ozone, col = "blue"))
with(subset(airquality, Month != 5), points(Wind, Ozone, col = "red"))
legend("topright", pch = 1, col = c("blue", "red"), legend = c("May", "Other Months"))


#Múltiples gráficos
par(mfrow = c(1, 2))
with(airquality, {
  plot(Wind, Ozone, main = "Ozone and Wind")
  plot(Solar.R, Ozone, main = "Ozone and Solar Radiation") })


par(mfrow = c(1, 3), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0)) with(airquality, {
  plot(Wind, Ozone, main = "Ozone and Wind")
  plot(Solar.R, Ozone, main = "Ozone and Solar Radiation") plot(Temp, Ozone, main = "Ozone and Temperature") mtext("Ozone and Weather in New York City", outer = TRUE)
})

#Clústering jerárquico
set.seed(1234)
x <- rnorm(12, mean = rep(1:3, each = 4), sd = 0.2)
y <- rnorm(12, mean = rep(c(1,2,1), each = 4), sd = 0.2)
plot(x,y,col="blue",pch=19,cex=2)
text(x+0.05, y+0.05, labels = as.character(1:12))

df <- data.frame(x=x,y=y)
distxy <- dist(df)
cluster <- hclust(distxy)
plot(cluster)

#Clústering k-means
set.seed(1234)
x <- rnorm(12, mean = rep(1:3, each = 4), sd = 0.2)
y <- rnorm(12, mean = rep(c(1,2,1), each = 4), sd = 0.2)
plot(x,y,col="blue",pch=19,cex=2)
text(x+0.05, y+0.05, labels = as.character(1:12))



set.seed(1234)
x <- rnorm(12, mean = rep(1:3, each = 4), sd = 0.2)
y <- rnorm(12, mean = rep(c(1,2,1), each = 4), sd = 0.2)
df <- data.frame(x,y)
kmeansObj <- kmeans(df, centers = 3)

## Pertenencia a cada cluster
kmeansObj$cluster

## Posición de los centroides
kmeansObj$centers


## Representación gráfica del clustering
par(mar = rep(0.2, 4))
plot(x,y,col=kmeansObj$cluster, pch=19, cex=2)
points(kmeansObj$centers, col = 1:3, pch=3, cex=3, lwd=3)
