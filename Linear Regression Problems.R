library(ISLR)
library(corrplot)
#Generamos la matriz de ScatterPlot de la Base de Datos:
pairs(~Auto$mpg+Auto$cylinders+Auto$displacement+Auto$horsepower+Auto$weight+Auto$acceleration+Auto$year+Auto$origin+Auto$name,data = Auto)

#Generamos la matriz de correlacion entre las variables:
M<-cor(Auto[1:8])
M

#Creamos un modelo lineal donde mpg es la variable de respuesta:
# y el resto son explicativas:
m1<-lm(Auto$mpg~Auto$cylinders+Auto$displacement+Auto$horsepower+Auto$weight+Auto$acceleration+Auto$year+Auto$origin, data = Auto)

#Hacemos un resumen del modelo:
summary(m1)

#Graficamos el modelo:
plot(m1)

#Veamos la coreelacion entre variables para asi proponer interacciones:
corrplot(M, type = "upper", order = "hclust", 
         tl.col = "black")

#Escribimos nuestro modelo con interacciones:
m2<-lm(Auto$mpg~Auto$cylinders+Auto$displacement+Auto$horsepower+Auto$weight+Auto$acceleration+Auto$year+Auto$origin+Auto$displacement:Auto$cylinders+Auto$horsepower:Auto$weight+Auto$acceleration:Auto$year, data = Auto)

#Comprobamos su anova:
anova(m2)

#Hacemos el resumen:
summary(m2)

#Escribimos nuestro modelo, pero tomando los logaritmos de las variables explicativas:
m3<-lm(Auto$mpg~log(Auto$cylinders)+log(Auto$displacement)+log(Auto$horsepower)+log(Auto$weight)+log(Auto$acceleration)+log(Auto$year)+log(Auto$origin), data = Auto)

#Hacemos el resumen:
summary(m3)

#Graficamos el modelo:
plot(m3)

#Escribimos nuestro modelo, pero tomando los cuadrados de las variables explicativas:
m4<-lm(Auto$mpg~(Auto$cylinders)^2+(Auto$displacement)^2+(Auto$horsepower)^2+(Auto$weight)^2+(Auto$acceleration)^2+(Auto$year)^2+(Auto$origin)^2, data = Auto)

#Hacemos el resumen:
summary(m4)

#Graficamos el modelo:
plot(m4)

#Escribimos nuestro modelo, pero tomando las raices cuadradas de las variables explicativas:
m5<-lm(Auto$mpg~sqrt(Auto$cylinders)+sqrt(Auto$displacement)+sqrt(Auto$horsepower)+sqrt(Auto$weight)+sqrt(Auto$acceleration)+sqrt(Auto$year)+sqrt(Auto$origin), data = Auto)

#Hacemos el resumen:
summary(m5)

#Graficamos el modelo:
plot(m5)

#Escribimos nuestro modelo, pero agregando los logaritmos de las variables explicativas:
m6<-lm(Auto$mpg~Auto$cylinders+Auto$displacement+Auto$horsepower+Auto$weight+Auto$acceleration+Auto$year+Auto$origin+log(Auto$cylinders)+log(Auto$displacement)+log(Auto$horsepower)+log(Auto$weight)+log(Auto$acceleration)+log(Auto$year)+log(Auto$origin), data = Auto)

#Hacemos el resumen:
summary(m6)

#Graficamos el modelo:
plot(m6)

#Escribimos nuestro modelo, pero agregando los cuadrados de las variables explicativas:
m7<-lm(Auto$mpg~Auto$cylinders+Auto$displacement+Auto$horsepower+Auto$weight+Auto$acceleration+Auto$year+Auto$origin+(Auto$cylinders)^2+(Auto$displacement)^2+(Auto$horsepower)^2+(Auto$weight)^2+(Auto$acceleration)^2+(Auto$year)^2+(Auto$origin)^2, data = Auto)

#Hacemos el resumen:
summary(m7)

#Graficamos el modelo:
plot(m7)

#Escribimos nuestro modelo, pero agregando las raices cuadradas de las variables explicativas:
m8<-lm(Auto$mpg~Auto$cylinders+Auto$displacement+Auto$horsepower+Auto$weight+Auto$acceleration+Auto$year+Auto$origin+sqrt(Auto$cylinders)+sqrt(Auto$displacement)+sqrt(Auto$horsepower)+sqrt(Auto$weight)+sqrt(Auto$acceleration)+sqrt(Auto$year)+sqrt(Auto$origin), data = Auto)

#Hacemos el resumen:
summary(m8)

#Graficamos el modelo:
plot(m8)

#Ejercicio 2 de la tarea:

library(mgcv)
library(MASS)
library(rcompanion)
library(MASS)

fp<-file.path("C:/Users/Jose Fuentes/Desktop/Posgrado/Tercer Semestre/Aprendizaje estadístico automatizado/Tareas/Tarea 1/BD/June_13_data.csv")
crashdata<-read.csv(fp)
View(BD)

#Hacemos la grafica de barra de las tablas de contingencia para cada variable
#predictora vs la variable de respuesta:
barplot(prop.table(table(BD$Rd_Feature)),legend=F,beside=F)
barplot(prop.table(table(BD$year)),legend=F,beside=F) 
barplot(prop.table(table(BD$Weather)),legend=F,beside=F) 
barplot(prop.table(table(BD$Month)),legend=F,beside=F) 
barplot(prop.table(table(BD$Time_of_Day)),legend=F,beside=F) 
barplot(prop.table(table(BD$Rd_Character)),legend=F,beside=F) 
barplot(prop.table(table(BD$Rd_Class)),legend=F,beside=F) 
barplot(prop.table(table(BD$Rd_Configuration)),legend=F,beside=F) 
barplot(prop.table(table(BD$Rd_Surface)),legend=F,beside=F) 
barplot(prop.table(table(BD$Rd_Conditions)),legend=F,beside=F) 
barplot(prop.table(table(BD$Light)),legend=F,beside=F) 
barplot(prop.table(table(BD$Traffic_Control)),legend=F,beside=F) 
barplot(prop.table(table(BD$Work_Area)),legend=F,beside=F) 

#Hacemos las variables de tipo Factor:
char<- as.factor(BD$Rd_Character)#Rd_Character
fea<- as.factor(BD$Rd_Feature) #Rd_Feature
class<- as.factor(BD$Rd_Class) #Rd_Class
conf<- as.factor(BD$Rd_Configuration) #Rd_Configuration
sur<- as.factor(BD$Rd_Surface) #Rd_Surface
cond<- as.factor(BD$Rd_Conditions) #Rd_Conditions
lig<- as.factor(BD$Light) #Light
wea<- as.factor(BD$Weather) #Weather
tcon<- as.factor(BD$Traffic_Control) #Traffic_Control
warea<- as.factor(BD$Work_Area) #Work_Area
tday<- as.factor(BD$Time_of_Day) #Time_of_Day
year<- as.factor(BD$year) #Year
month<- as.factor(BD$Month) #Month

#Realizamos el modelo aditivo:
model1<- lm(BD$Crash_Score~wea+year+month+tday+fea+char+class+conf+sur+cond+lig+tcon+warea,data = BD)

#Hacemos el resumen:
summary(model1)

#Graficamos para ver como se comporta el modelo:
plot(model1)

#Realizamos box cox con el objetivo de ver que transformacion se recomienda para
#realizar a la variable de respuesta:
bc<-boxcox(BD$Crash_Score~wea+year+month+tday+fea+char+class+conf+sur+cond+lig+tcon+warea,data = BD,lambda = seq(-1, 1, length = 10))

#Revisamos el valor exacto de lambda:
best.lam<-bc$x[which(bc$y==max(bc$y))]
best.lam

#Elevamos a la lambda de la variable de respuesta:
salida<- lm(BD$Crash_Score^best.lam~wea+year+month+tday+fea+char+class+conf+sur+cond+lig+tcon+warea)

#Hacemos el resumen:
summary(salida)

#Graficamos:
par(mfrow=c(1,2))
plot(model1, which=2, pch=19, cex=.25)
plot(salida, which=2, pch=19, cex=.25)
drop1(salida,test="F")

#Analizamos la significancia de las variables discretas:
drop1(lm(BD$Crash_Score^best.lam~year,data = BD), test = "F")
drop1(lm(BD$Crash_Score^best.lam~month,data = BD), test = "F")
drop1(lm(BD$Crash_Score^best.lam~tday,data = BD), test = "F")
drop1(lm(BD$Crash_Score^best.lam~fea,data = BD), test = "F")
drop1(lm(BD$Crash_Score^best.lam~char,data = BD), test = "F")
drop1(lm(BD$Crash_Score^best.lam~class,data = BD), test = "F")
drop1(lm(BD$Crash_Score^best.lam~conf,data = BD), test = "F")
drop1(lm(BD$Crash_Score^best.lam~sur,data = BD), test = "F")
drop1(lm(BD$Crash_Score^best.lam~cond,data = BD), test = "F")
drop1(lm(BD$Crash_Score^best.lam~lig,data = BD), test = "F")
drop1(lm(BD$Crash_Score^best.lam~wea,data = BD), test = "F")
drop1(lm(BD$Crash_Score^best.lam~tcon,data = BD), test = "F")
drop1(lm(BD$Crash_Score^best.lam~warea,data = BD), test = "F")

#Comprobamos para el modelo global:
summary(salida)

#Regresion sin variables explicativas:
RLM.Vacio<-lm(formula= BD$Crash_Score^best.lam ~1,BD)

#Hacemos el resumen:
summary(RLM.Vacio)

#Regresion con todas lasvariables explicativas:
RLM.Completo<-lm(formula= BD$Crash_Score^best.lam ~wea+year+month+tday+fea+char+class+conf+sur+cond+lig+tcon+warea,data=BD)

#Hacemos el resumen:
summary(RLM.Completo)

#Regresion forward:
RLM.Forward<-step(RLM.Vacio,
                  scope=list(lower=RLM.Vacio,upper=RLM.Completo),
                  direction="forward")
#Hacemos el resumen:
summary(RLM.Forward)

#Regresion backward:
RLM.backward<-step(RLM.Completo,
                   scope=list(lower=RLM.Vacio,upper=RLM.Completo),
                   direction="backward")
#Hacemos el resumen:
summary(RLM.backward)

#Regresion stepwise:
RLM.stepwise<-step(RLM.Vacio,
                   scope=list(lower=RLM.Vacio,upper=RLM.Completo),
                   direction="both")
#Hacemos el resumen:
summary(RLM.stepwise)

#Hacemos el anova del modelo del stepwise:
anova(RLM.stepwise)

#Ahora hacemos el agrupamiento de niveles de cada factor:

#Primer factor:
levels(sur)
contrasts(sur)
levels(sur)[3:5]="GroovedConcreteOtherSmoothAsphalt"
table(sur)

model2= lm(BD$Crash_Score^best.lam ~ class + fea + lig + tcon +tday + char +sur , data = BD)

anova(RLM.stepwise,model2)
summary(model2)

#Segundo factor:
levels(tcon)
contrasts(tcon)
levels(tcon)[c(2:3)]="OtherSignal" 
table(tcon)

model3= lm(BD$Crash_Score^best.lam ~class + fea + lig + tcon +tday  + char +sur)
anova(model2,model3)
summary(model3)

levels(tcon)
contrasts(tcon)
levels(tcon)[c(3:4)]="Stop-SignYield"
table(tcon)

model4= lm(BD$Crash_Score^best.lam ~class + fea + lig + tcon +tday + 
              char +sur)

anova(model3,model4)
summary(model4)

#Tercer factor:
levels(fea)
contrasts(fea)
levels(fea)[c(1:2)]="DrivewayIntersection"
table(fea)

model5= lm(BD$Crash_Score^best.lam ~class + fea + lig + tcon +tday  + 
             char +sur)

anova(model4,model5)
summary(model5)

#Cuarto factor:
levels(char)
contrasts(char)
levels(char)[c(1:4)]="Curve-Grade-Level-OtherOther"
table(char)

model6= lm(BD$Crash_Score^best.lam ~class + fea + lig + tcon +tday  + 
             char +sur)

anova(model5,model6)
summary(model6)

levels(char)
contrasts(char)
levels(char)[c(1,4)]="Curve-Grade-Level-OtherOtherStraigth-Other"
table(char)

model7= lm(BD$Crash_Score^best.lam ~class + fea + lig + tcon +tday  + 
             char +sur)

anova(model6,model7)
summary(model7)

#Quinto factor:
levels(lig)
contrasts(lig)
levels(lig)[c(1:3)]="Dark-lit-not-liteDawn"
table(lig)

model8= lm(BD$Crash_Score^best.lam ~class + fea + lig + tcon +tday + 
             char +sur)

anova(model7,model8)
summary(model8)

#Sexto factor:
levels(tday)
contrasts(tday)
levels(tday)[c(1:2)]="12"
table(tday)

model9= lm(BD$Crash_Score^best.lam ~class + fea + lig + tcon +tday + 
             char +sur)

anova(model8,model9)
summary(model9)

levels(tday)
contrasts(tday)
levels(tday)[c(2:3)]="23"
table(tday)

model10= lm(BD$Crash_Score^best.lam ~class + fea + lig + tcon +tday + 
             char +sur)

anova(model9,model10)
summary(model10)

#Hacemos el anova del ultimo modelo:
anova(model10)

#Hacemos plot:
plot(model10)

#Le sumamos 0 al modelo 9:
modelf<- lm(BD$Crash_Score^best.lam ~class + fea + lig + tcon +tday + 
              char +sur+0)

#Hacemos el resumen:
summary(modelf)

#Se comprueba ahora la bondad del ajuste:
par(mfrow=c(1,2))
plot(RLM.stepwise, which=2, pch=19, cex=.25)
plot(modelf, which=2, pch=19, cex=.25)
plot(RLM.stepwise$fitted.values, modelf$fitted.values)
plot(modelf)



