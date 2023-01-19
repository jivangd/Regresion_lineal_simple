#EJERCICO 6_A. Uso del modelo de regresión lineal simple
#@Leilani García Córtes 
#Limpiamos el enviroment
rm(list=ls(all.names=TRUE))

#Instalamos las librerías que usaremos
library(car)
library(broom)
library(lmtest)
library(nortest)
library(dplyr)
library(kableExtra)
library(MASS)
library(knitr)

#Creamos la tabla del peso de los huevos en gramos
x=c(79,93,100,105,101,96,96,109,70,71,87)
y=c(133,148,164,171,165,159,162,170,127,133,148)

Datos6=data.frame(cbind(x,y))
kable(t(Datos6))%>%
  kable_styling(bootstrap_options="striped", full_width = F)

#________________________________________________________________________________________________________________________

#I) Ajuste la recta de regresion del peso del huevo mayor (y) dado el peso del huevo menor (X). Comente sobre el ajuste del modelo, es decir, si parece 
#correcto y si se cumplen los supuestos.

#Lo primero que haremos será ajustar el modelo
fit=lm(y~x, data=Datos6)
plot(x,y, main = "Peso en gramos de los huevos de 11 nidadas")
abline(fit, col="green")

#Observemos los primero resultadps
summary(fit)

#Visualizamos los coeficientes
coef(fit)

#Rescatamos los coeficientes
b_0 = fit$coefficients[1]
b_1 = fit$coefficients[2]

#Visualizamos las variables
b_0
b_1

#De lo obtenido anteriormente podemos decir que sí ue existe una relación lineal entre 'x' y 'y', pues observemos que 
# p-value = 3.35e - 07 < alpha, así que se rechaza H0 con significancia alpha=0.05. De la misma manera podemos confirmar 
#esto observando nuestro gráfico (Peso en gramos de los huevos de 11 nidadas)
#Por otro lado, de lo obtenido en b_0 y b_1, el modelo de regresión lineal está dado por y = 45.67417 + 1.169398 x
#Gracias a los datos que tenemos de beta 0 y beta 1, sabemos que nuestro modelo 
#con prueba de hipotesis H0: b1=0 vs Ha: b1=! 0

#------------------------REVISEMOS LOS SUPUESTOS-----------------------------------------------

#REVISANDO LINEALIDAD_______________________________________________________________________

#Linealidad
plot(fit,1)

#Dado que existen valores que salen de la banda formada por [-4,4] (eje y) podríamos tener razones para creer que lo dicho anteriormente,
#es decir, la relación líneal no se cumple. Pero para esto, haremos la prueba de BoxTidwell para obtener un resultado más concreto

library(car)

#Prueba de BoxTidwell
boxTidwell(y~x, data=Datos6)

#De lo anterior, hemos obtenido que p-value=0.6983,  con una significancia del 0.05. Así que, diremos que 
#no se rechaza la hipotesis nula (lambda = 1), por lo que no hace falta transformar la variable x, cumpliendo nuestro supuesto de linealidad


#REVISANDO ALEATORIDAD_______________________________________________________________________

#Autocorrelograma
par(mar=c(5,4,4,3))
par(mfrow=c(1,2))
#Errores libreria broom
library(broom)
Datossfit = augment(fit)
acf(Datossfit$.std.resid)

#A simple vista no se observan patrones, cumpliendo la aleatoridad, sin embargo realizaremos pruebas

library(randtests)

randtests::runs.test(Datossfit$.std.resid)
#Obtenemos que el p-value=0.5023

#Pueba para la autocorrelacion de orden 1
library(lmtest)

lmtest::dwtest(fit)
#p-value=0.1073

#Prueba de rachas
library(lawstat)

lawstat::runs.test(Datossfit$.std.resid, plot.it=TRUE)
#p-value= 0.3507

#Dado que no se rechaza H0 con una significancia de 0.05, de las tres pruebas concluimos que no existen razones suficientes para rechazar el supuesto de aleatoriedad
#Así que concluimos que el supuesto de aleatoriedad se cumple

#REVISANDO NORMALIDAD_____________________________________________________________________________________

#Errores studentizados
ErrorSt=studres(fit)
qqPlot(ErrorSt, dist="t", df=length(ErrorSt)-3)

#Por la desviación de los datos observada, podríamos dudar del cumplimiento del supuesto de Normalidad, así que 
#realizaremos la prueba Kolmogorov-Smirnov
ks.test(ErrorSt, "pt",length(ErrorSt)-3)

#Se obtiene p-value= 0.716 > 0.05
#Por lo que podemos asumir que los errores tienen una distribucion t student, probando el supuesto de normalidad,
#Pues no rehazamos la hipotesis nula H0 con significancia de 0.05



#Probaremos si se cumple la normalidad cuando n es grande

normtest::jb.norm.test(Datossfit$.std.resid)
#p-value=0.4175 > 0.05

nortest::lillie.test(Datossfit$.std.resid)
#p-value=0.2684 > 0.05

shapiro.test(Datossfit$.std.resid)
#p-value = 0.4132 > 0.05

#Dado que no se rechaza H0 con una significancia de 0.05, de las tres pruebas concluimos que no existen razones suficientes para rechazar el supuesto de normalidad
#Así que concluimos que el supuesto de normalidad se cumple

#REVISANDO HOMOCEDASTICIDAD____________________________________________________________________________________

plot(fit, 3)

#Realizamos una prueba con errores estandarizados:
car::ncvTest(fit)

#Realizamos una rueba con errores studentizados:
lmtest::bptest(fit)

#De la prueba con errores estandarizados se obtiene que p-value=0.67268 > 0.05
#De la prueba con errores studentizados se obtiene quep-value= 0.5941 > 0.05
#Por lo que podemos asumir que no rehazamos la hipotesis nula H0 con significancia 0.05
#Hemos encontrado que se cumple satisfactoriamente el supuesto de homocedasticidad


#CONCLUSIOM DEL MODELO-----------------
#Hemos concluido que los supuestos de normalidad, lienalidad, homocedasticidad y aleatoriedad para el modelo se cumplen


#_____________________________________________________________________________________________________________________________________

#II) Los investigadores tienen la sospecha de que en promedio se puede decir que la diferencia entre el peso
#mayor y el peso menor es constante (es decir, no depende del peso del huevo menor observado). Usando
#el modelo en I) realice una prueba de hipótesis para responder la pregunta de los investigadores.


#Dado que el peso del huevo menor está dado por la variable 'x', buscamos esta variable no afecte el modelo. 
#Todo esto en términos de la esperanza se ve como;
# E(y-x) = c    sii    E((ylx)-x) = c    sii    E(ylx) - E(x) = c    sii   E(ylx) - x = c    sii    B0 + b1x - x = c 


#Encontremos ese valor de x que no afecte la esperanza cuando b1=1.
#Prueba de hipotesis: H0: B1 = 1 vs Ha: B1 = ! 1

library(multcomp)
Matzoz1=matrix(c(0,1), ncol=2, nrow=1)
c=1

pr = glht(fit, linfct=Matzoz1, rhs=c, alternative ="two.sided")
summary(pr)
#p-value=0.0877 > 0.05

#De lo anterior, como el p-value es mayor a 0.05, no se rechaza la hipotesis nula H0, con una significancia de 0.05, 
#Entonces, no podemos rechazar el supuesto de que en promedio la diferencia del peso del huevo mayor y menor es constante 



#III) Posteriormente se observa el peso de los huevos de una nueva nidada, observándose un peso de 75 y 130 gramos. 
#Usando un intervalo adecuado, comente sobre la sospecha de que la nidada de huevos sí proviene de pinguinos Macaroni.


#Creamos un intervalo de prediccion tomando el nuevo valor de x

egg = data.frame(x = c(75))
new_int = predict(fit, egg, interval = "prediction", level = 0.95)
head(new_int)

#Observamos que el intervalo de prediccion es (124.0271, 142.731)
#Y como y=130, pertenece al intervalo de prediccón, entonces podemos decir que con una confianza de 95%,
#Por lo tanto, concluimos que la nueva nidada pertenece a los pinguinos Macaroni

#Eliminamos las variables
rm(list=ls())

