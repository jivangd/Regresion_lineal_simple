rm(list = ls(all.names = TRUE))
#@Ivan Gutierrez Diaz

getwd()

perform <- read.csv("performance.csv")


#I)
head(perform)
str(perform)
par(mar=c(4,4,1,1))
par(mfrow=c(1,1))
plot(api00~ell, data=perform, cex=.9, cex.axis=.7, cex.lab=.8)

#Ajuste
fit=lm(api00~ell, data=perform)
summary(fit)

#Veificación de supuestos

#Se puede decir que hay una relación lineal entre el rendimiento académico 
#de la escuela y estudiantes que requieren cursos de recuperación de inglés.
#Pues se rechaza H0 con una significancia de α = .05, pues p − value = 2.2e-16 < α = .05.


#1 Homocedasticidad
library(lmtest)
lmtest::bptest(fit)
#Como  p-value = 0.0953 > α = .05 por lo no se rechaza H0 es decir no se 
#encuentra evidencia en contra de que la varianza depende linealmente
library(car)
car::ncvTest(fit,~ell)
#Como  p = 0.12879 > α = .05 por lo no se rechaza H0 es decir no se encuentra
#evidencia en contra de que la varianza depende linealmente

par(mfrow=c(1,2)) 
par(mar=c(4, 5, 1, 1))
plot(fit, 3)
#Se observa una nube de datos alrededor del 0 por lo que no se encuentra evidencia en contra
#de la homocedasticidad.
#En conclusion que se cumple con el supuesto de homocedasticidad

#2 Linealidad

par(mfrow=c(1,1)) 
par(mar=c(4, 5, 1, 1))
plot(fit, 1)
#No se logra observar una línea recta sobre el cero por lo que se encuentra evidencia
#en contra

perform$ell1=perform$ell+.1
#pues el Dataframe incluye dos observaciones iguales a 0, sumamos algo muy pequeño 
#para poder hacer la prueba de boxTidwell y no sea un valor que nos cambie la interpretacion
#del modelo

boxTidwell(api00~ell1, data=perform)
#La salida es el estimador de la potencia 0.48637 aprox 0.5
#Test Ho: lambda=1 vs Ha:lambda != 1.
#Aquí se rechaza H0,.pues p-value = 1.504e-11
#Conluimos que no se cumple el supuesto de linealidad

#3 NORMALIDAD
#Como n es grande (n=400), haremos las pruebas correspondientes
par(mar=c(4, 5, 1, 1))
par(mfrow=c(1,1))
plot(fit, 2)
#Se observa buena que un gran porcentaje de los puntos caen sobre la diagonal
# aunque al final de la diagonal se salen varios valores
#(se arreglará al arreglar linealidad)

##Función para crear errores de forma automatizada
library(broom)

performfit=augment(fit)
head(performfit)

#Pruebas de normalidad
shapiro.test(performfit$.std.resid)
#No cumple el supuesto ya que se rechaza H0 pues p-value = 0.007596 < 0.05,
#se encontro evidencia en contra de la normalidad

library(nortest)
nortest::lillie.test(performfit$.std.resid)
#No cumple el supuesto ya que se rechaza H0 pues p-value = 0.01955 < 0.05
#se encontro evidencia en contra de la normalidad

library(tseries)
tseries::jarque.bera.test(performfit$.std.resid)
#No cumple el supuesto ya que se rechaza H0 pues p-value = 0.02696 < 0.05
#se encontro evidencia en contra de la normalidad

#Como en las tres pruebas se encontro evidencia en contra de la normalidad y
#la gráfica no es totalmente convincente concluimos que no se cumple el supuesto 
#de normalidad

## 4 INDEPENDENCIA O COVARIANZA 0
#Se cumple pues las observaciones vienen de una muestra aleatoria


#II)
## Corregiremos la linealidad mediante Box-Tidwell y apartir de este ajuste
#comprobaremos los demás supuestos en espera de que se logren cumplir

#1 Linealidad
boxTidwell(api00~ell1, data=perform)
# Nos sugiere el método usar una lambda cercana a 0.486, usaremos lambda=.5

#Se hace la transformación con lambda=0.5
perform$Xprima=perform$ell1^0.5

#Hacemos el nuevo ajuste
fit2=lm(api00~Xprima, data=perform)
summary(fit2)
par(mfrow=c(1,1)) 
par(mar=c(4, 5, 1, 1))
plot(fit2, 1)
#Ahora la línea es más apegada a una línea recta sobre el 0 o la línea punteada
boxTidwell(api00~Xprima, data=perform)
#Ya no se rechaza H0  y lambda= 0.9727 muy cercana a 1



#2 Homocedasticidad

lmtest::bptest(fit2)
#Como  p-value = 0.1167 > α = .05 por lo no se rechaza H0 es decir no se encuentra
#evidencia encontra de que la varianza depende linealmente

car::ncvTest(fit2,~Xprima)
#Como  p-value = 0.119 > α = .05 por lo no se rechaza H0 es decir no se encuentra
#evidencia encontra de que la varianza depende linealmente

par(mfrow=c(1,2)) 
par(mar=c(4, 5, 1, 1))
plot(fit2, 3)
#Se observa una nube de datos alrededor del 0 por lo que no se encuentra evidencia en contra
#de la homocedasticidad.
#Por lo tanto concluimos que se cumple con el supuesto de homocedasticidad

#3 Normalidad

par(mar=c(4, 5, 1, 1))
par(mfrow=c(1,1))
plot(fit2, 2)
#Se observa buena que un gran porcentaje de los puntos caen sobre la diagonal
#Por lo que no se encuentra evidencia en contra de linealidad

##Función para crear errores de forma automatizada

performfit2=augment(fit2)
head(performfit2)

#Pruebas de normalidad
shapiro.test(performfit2$.std.resid)
#No se rechaza pues p-value = 0.1388 > 0.05

nortest::lillie.test(performfit2$.std.resid)
#No se rechaza pues p-value = 0.353 > 0.05

tseries::jarque.bera.test(performfit2$.std.resid)
#No se rechaza pues p-value = 0.2737 > 0.05
#Como en las tres pruebas no se encontro evidencia en contra de la normalidad
#concluimos que se cumple el supuesto de normalidad

#4 COVARIANZA 0 O INDEPENDENCIA
#Se cumple pues las observaciones vienen de una muestra aleatoria

summary(fit2)
#Tenemos finalmente que en el modelo ajustado B0=887.429 B1=-41.148
#Dada la transformación (ell+.1)^0.5 por Box-Tidwell

#III)
par(mfrow=c(1,1)) 
par(mar=c(4, 5, 1, 1))
plot(perform$ell, perform$api00, xlab = TeX("$x$"), ylab=TeX("$y$") )
abline(fit, col= "red")
lines(sort(perform$ell),fitted(fit2)[order(perform$ell)], type = "l")

#IV)
summary(fit2)
anova(fit2)
#F=665.7 y p-value = 2.2e-16 F es la prueba asociada a la tabla anova y
#el p-value < 0.05 nos dice que se rechaza H0 es decir que existe una relación
#lineal entre el modelo por otro lado el coeficiente de determinación es
#Multiple R-squared:  0.6258 Se tiene que el 62.58 % de la variabilidad 
#el desempeño academico de las escuelas se explica por el modelo que 
#incluye la variable estudiantes que requieren cursos de recuperación de inglés.

#V)
#Al hacer el ajuste correspondiente (fit2),se cumplieron los supuesto de homocedasticidad,
#linealidad y normalidad, además de un p-value asociado a la prueba anova, que
#nos indican que se esta a favor de que “A mayor porcentaje de estudiantes que
#requieren cursos de recuperación de inglés es menor el desempeño de la escuela”
# se puede observar en el gráfico una relación lineal negativa que con el ánalisis
#anterior es contundente esta relación lineal
rm(list=ls())

