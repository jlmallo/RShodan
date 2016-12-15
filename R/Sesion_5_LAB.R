library("ggplot2")
str(mpg)
qplot(x = displ, y = hwy, data = mpg)

#Añadimos Colores
qplot(x = displ, y = hwy, data = mpg, color = drv)

#Añadimos objetos de tipo geometric
qplot(x = displ, y = hwy, data = mpg, geom = c("point","smooth"))


qplot(x = displ, y = hwy, data = mpg, color = drv) + geom_smooth(method="lm")


#Histograma
qplot(x = hwy, data = mpg, fill = drv)


#Facets
qplot(x = displ, y = hwy, data = mpg, facets = .~drv)

#Facets (múltiples gráficas)
qplot(x = hwy,data = mpg,facets = drv~.,binwidth = 2)

# Densidad
qplot(x = hwy, data = mpg, geom = "density")

qplot(x = hwy, data = mpg, geom = "density", color = drv)


#Gráficos complejos: Composición - Geom
g <- ggplot(mpg, aes(displ, hwy, color = class))
g + geom_point()


g + geom_count()

g + geom_density2d()

g + geom_tile()

g + geom_bin2d()

g + geom_violin()

g + geom_boxplot()


#Gráficos con mapas
library("raster")
adm <- getData('GADM', country='ESP', level=4)
cat<-(adm[adm$NAME_1=="Cataluña",])
plot(cat, bg="dodgerblue", axes=T)
plot(cat, lwd=10, border="skyblue", add=T)
plot(cat, col="green4", add=T)
grid()
cat$valors <- rnorm(1:length(cat$NAME_4), 10, 3) > plot(cat, col=cat$valors, add=T)



# Solo latitud y longitud
install.packages("ggmap")
library(ggmap)
ej <- get_map(location="Mexico", source="google", maptype="terrain", zoom=5)
ggmap(ej)
ej <- get_map(location=c(right=-85, left=-121, bottom=13, top=33), source="osm", color="bw")
ggmap(ej)
