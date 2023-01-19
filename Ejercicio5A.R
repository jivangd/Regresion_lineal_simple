rm(list = ls(all.names = TRUE))
#@Ivan Gutierrez Diaz

getwd()

medic <- read.csv("Ejercicio5A.csv")

##I) 
head(medic)
str(medic)
medic$Med <- as.factor(medic$Med)
summary(medic)
#100 observaciones la mitad recibió medicamento y la otra no
#Carga viral menor es de 7.181 y Maxima de 12382
#Mediana 9.958 y Media 9.994
levels(medic$Med)

boxplot(Y ~ Med, data = medic, col = "white", outline=FALSE)
stripchart(Y ~ Med, data = medic,
           method = "jitter",
           pch = 19,
           col = 2:4,
           vertical = TRUE,
           add = TRUE)
#Se observan ambas poblaciones con una varianza similar; si se observa una carga
#viral menor en la población que recibio la vacuna.La carga viral oscila alrededor 
#de 10 independientemente de a aplicación de la vacuna.
#Algo que resalta en el dataframe es que a las personas menores de 60 años, coinciden
#con la no aplicación de la vacuna por lo que poodría tener impacto en las medias
#ya que una persona de menor edad suele ser más sana comparada con una mayor

##II)

fit <- lm(Y ~ Med, data=medic)
 
library(multcomp)
#H_0: mu1>=mu2 vs H_a: mu1<mu2
#H_0: b0>=b0+b1 vs H_a: b0<b0+b1
#H_0: 0>=b1 vs H_a: 0<b1     
MatZ0Z1=matrix(c(0,1), ncol=2, nrow=1)
c=0
prueba1=glht(fit, linfct=MatZ0Z1, rhs=c, alternative ="greater")
summary(prueba1)
#Como p-value = 0.999 > 0.05 no se rechaza H0. i.e., mu1>mu2
#Por lo tanto se tieneen promedio menos carga viral cuando se aplica el medicamento
#por lo que lo que se esta a favor de lo que menciono la compañía
#Son válidos pues
summary(fit)
#Se utilizará este modelo
#E(Y|Med = NO) = b0 = mu1 = 10.3034
#E(Y|Med = SI) = b0+b1 = mu2 = 10.3034-0.6191 =  9.6843 < m1
#Suponiendo que se cumplen la homocedasticidad y la normalidad

##III)
#Verificacipon de cumplimiento de los supuestos
#Homocedasticidad
plot(fit, 3)
library(lmtest)
lmtest::bptest(fit)
library(car)
car::ncvTest(fit)
#Ambos p-values son mayores a 0.05 y la gráfica parece ser muy simétrica en ambas 
#observaciones por lo que no se encuentra evidencia en contra dela homocedasticidad
#Se cumle el supuesto

#Normalidad
plot(fit, 2)
library(broom)
Datosfit=augment(fit)
head(Datosfit)
shapiro.test(Datosfit$.std.resid)
library(nortest)
nortest::lillie.test(Datosfit$.std.resid)
library(tseries)
tseries::jarque.bera.test(Datosfit$.std.resid)
#A excepción de la observación 11 y 18 los puntos se quedan sobre la diagonal, y 
#las tres pruebas de hipótesisi arrojan un p-value mayor a 0.05 por lo que es 
#contundente elcumplimiento del supuesto de normalidad

#ALEATOREIDAD
library(latex2exp)
par(mar=c(4, 5, 3, 1))
par(mfrow=c(1,3))
plot(1:length(Datosfit$.std.resid), Datosfit$.std.resid, xlab = TeX("$i$"), ylab=TeX("$e_s$")   )

#autocorrelograma de los errores
acf(Datosfit$.std.resid)

#Prueba de rachas
library(lawstat)
lawstat::runs.test(Datosfit$.std.resid, plot.it = TRUE)
library(randtests)
randtests::runs.test(Datosfit$.std.resid)
#No se encontro evidencia en contra de que las observaciones son aleatorias pues 
#p-value = 0.3149 > 0.05


#Prueba para autocorrelación de orden 1
library(lmtest)
lmtest::dwtest(fit, alternative = c("two.sided"))
library(car)  #Prueba con 5 ordenes
durbinWatsonTest(fit, max.lag=5)
#Todos los p-values son mayores a 0.05 por lo que si hay aleatoridad en la muestra

##IV
factor(medic$Edad)
boxplot(Y ~ Med, data = medic, col = "white", outline=FALSE)
boxplot(Y ~ Edad, data = medic, col = "white", outline=FALSE)
stripchart(Y ~ Edad, data = medic,
           method = "jitter",
           pch = 19,
           col = 2:4,
           vertical = TRUE,
           add = TRUE)
fit2 <- lm(Y ~ Edad, data=medic)
summary(fit2)
#la diferencia en las medias ahora es mayor y no cumplen con homocedasticidad 

# Se observa que la carga viral es en promedio  mayor en los pacientes menores de 60 años, 
#esto debido a que al grupo completo no se le aplico el medicamento y como era 
#de esperarse tuvieron mayor carga viral, es decir,
#no fue homogenea esta selección puesto que de las 20 personas
#menores de 60 todas pertenecieron al grupo placebo y esto genero la falta de contundencia
# las observaciones porque al ser menores de 60 y haber aplicado el medicamento 
#su carga viral hubiese esperado que fuese menor que a las personas que le aplicaron
# el medicamento mayores de 60.

#Así que los resultados en III también se le atribuyen no solo
#al medicamento también se le atribuyen al no tomar datos parejos ya sea todos mayores
#de 60 o dividir equitativamente el placebo y medicamento en los grupos de edad
#aparte de que la interpretación es más complicada cuando hay dos diferentes grupos
#de edad

##V) 
#Para ello homogeniciaremos los datos nos quedaremos solo con los mayore a 60
medic_60 <- medic[1:80, ]
boxplot(Y ~ Med, data = medic_60, col = "white", outline=FALSE)
stripchart(Y ~ Med, data = medic_60,
           method = "jitter",
           pch = 19,
           col = 2:4,
           vertical = TRUE,
           add = TRUE)
fit3 <- lm(Y ~ Med, data=medic_60)
summary(fit3)
#Ahora que los datos son homegeneos la las medias difieren menos y parece
#que agregar a estas personas menores de 60 se hizo apróposito para generar
#una mayor diferencia en las medias para que el gobierno se convencierá
#de que el medicamento tiene un fuerte impacto.Sin embargo, con este ánalisis
#observamos que no hay contundencia en haber sido aplicado o no el medicamento
#por lo que el gobierno debería no comprareste medicamento

rm(list=ls())

