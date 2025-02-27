#Codigo para problema 2
mis_dades <- iris
dim(mis_dades)
names(mis_dades)

mean(mis_dades$Petal.Length)#media, sumatorio de Xi dividido el numero de experimentos
sd(mis_dades$Petal.Length)#sirve para ver la diferencia entre la media
hist(mis_dades$Petal.Length)

#com fer la recta de regresion
x <- mis_dades$Petal.Length
y <- mis_dades$Sepal.Length
plot(x,y)
#m= (sumatori(Xi-Xmedia)*(yi-ymedia))/sum (Xi-Xmedia)^2
#b= ymedia-m*Xmedia

m <- sum((x-mean(x))*(y-mean(y)))/sum((x-mean(x))^2)
m
b <- mean(y)-m*mean(x)
b

m*1.5+b

#dibuixar recta de regresio
#AltGR+4+espace = ~
mod<- lm(y~x) #dibuixar leix de y
summary(mod)
#date frame=
ypredict <- predict(mod, data.frame(x=x))

plot(x,y)
lines(x,ypredict)

#coeficiente de determinacion
Rsq=sum((ypredict-mean(y))^2)/sum((y-mean(y))^2)
Rsq
#coeficiente de correlación
sqrt(Rsq)

summary(mod)
