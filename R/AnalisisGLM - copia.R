#library(olsrr)
library(dplyr)
#library(Hmisc)
library(sjPlot)
library(leaps)
library(bestglm)
library(ggplot2)
library(visreg)

#importar datos
df1 <- readxl::read_excel(here("Data", "spicu2.xlsx"), sheet = "in")
View(df1)

#corregir los nombres que pusiste feos
vremove <- c(" ")
vreplace <- c("_")

names(df1) <- gsub(vremove, vreplace, names(df1))
names(df1)

#seleccionar variables que interesan al tronco 
dfdatos <- df1 %>% 
  select(Sitio, Sexo, Distancia24hrs, Distancia_total_reccorida, Temporada,
         NDVI_SMO:Altura_de_arbol)
View(dfdatos)

#ver que el tipo de dato sea correcto para cada variable
str(dfdatos)

#dejar los datos listos para analisis: corregir al tipo de dato correcto de cada variable
#y preparar variables categoricas
dfdatosori <- dfdatos
dfdatos <- dfdatos %>% 
  mutate(Distancia24hrs = as.numeric(Distancia24hrs),
         Sitio_cons = ifelse(Sitio == "Conservado", 1, 0),
         Sexo_macho = ifelse(Sexo == "Macho", 1, 0),
         Sex_hembra = ifelse(Sexo == "Hembra", 1, 0),
         Temporada_seca = ifelse(Temporada == "Secas", 1, 0)) %>% 
  select(Distancia24hrs, Distancia_total_reccorida, NDVI_SMO:Altura_de_arbol,
         Sitio_cons:Temporada_seca)

#verificar que todo este correcto
str(dfdatos)
View(dfdatos)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#Dividiremos este analisis en dos, para distancia por dia y para distancia total

### Analisis Distancia por dia ###################################


#creamos el dataframe con las variables que nos interesan
dfdatos1 <- dfdatos[,c(1, 3:10, 12, 13)]
str(dfdatos1)
View(dfdatos1)


#echamos un primer vistazo a la distribucion de los datos y guardamos la grafica
hist(x = dfdatos1$Distancia24hrs, breaks = 7, xlab = "Distancia recorrida", main = "Distribucion datos 24hrs")

plot(density(na.omit(dfdatos1$Distancia24hrs)))
#Estandarizamos los datos para un mejor analisis excluyendo obviamente las varibles 
#categoricas

Estandarizar <- function(x){
  (x-mean(x, na.rm = T))/sd(x, na.rm = T)
}

dfdatos1Est <- dfdatos1
dfdatos1Est[c(2:8)] <- apply(X = dfdatos1Est[c(2:8)], MARGIN = 2, FUN = Estandarizar)


#comprobamos que este correcto
View(dfdatos1Est)
apply(X = dfdatos1Est, MARGIN = 2, FUN = mean)
apply(X = dfdatos1Est, MARGIN = 2, FUN = sd)


#veamos que tan relacionadas estan las variables cuantitativas

#esta matriz corresponde a los datos ya estandarizados de variables cuantitativas
correlaciones <- cor(dfdatos1Est[c(1:8)], use ="complete.obs", method = "spearman")
correlaciones



#y esta a los datos originales, vemos que las correlaciones no se ven afectadas
correlaciones1 <- cor(dfdatos1[c(1:8)], use ="complete.obs", method = "spearman")
correlaciones1



#usando pearson
corrpearson <- cor(dfdatos1Est[c(1:8)], use ="complete.obs", method = "pearson")
corrpearson


#pearson sobre datos originales
corrpearson1 <- cor(dfdatos1[c(1:8)], use ="complete.obs", method = "pearson")
corrpearson1



#ver que son identicas
identical(correlaciones, correlaciones1)
identical(correlaciones, corrpearson)


#que tan relacionadas estan es decir su nivel de significancia de unas con otras
probcor <- Hmisc::rcorr(as.matrix(dfdatos1Est[c(1:8)]), type = "spearman")
probcor$P
str(probcor$P)
class(probcor$P)



#ordenar de mayor a menor los resultados

#obtener las variables que mas estan relacionadas con distancia por día
relaciones <- as.data.frame(correlaciones)
relaciones[order(relaciones$Distancia24hrs),]
rel <- relaciones[order(abs(relaciones$Distancia24hrs), decreasing = T), 1, drop = F]
rel




#hacer lo mismo pero para la significancia
probcor1 <- as.data.frame(probcor$P)
probcor1[order(probcor1$Distancia24hrs), 1 , drop = F]
probcor1 <- probcor1[order(probcor1$Distancia24hrs), 1 , drop = F]
probcor1
str(probcor1)



#prueba para comprobar que los datos de la matriz son iguales a si se 
#sacaran 1 a 1
cor(dfdatos1Est$Distancia24hrs, dfdatos1Est$EVI_SMO, use ="complete.obs", method = "spearman")

#########################
#Con esto nos damos una idea de que varibles cuantitativas influyen mas en el desplazamiento de los
#sapos por dia

#graficado para que sea facilmente observable
PerformanceAnalytics::chart.Correlation(dfdatos1Est[c(1:8)], histogram = T,
                                        method = "spearman")


pairs(dfdatos1Est[c(1:8)])


psych::pairs.panels(dfdatos1Est[c(1:8)], method = "spearman", stars = TRUE,  
             hist.col = 4, smooth = TRUE, scale = F, density = TRUE,
             pch = 21, lm = F, jiggle = T, ci = TRUE)


#####################



#ahora vamos con las relaciones entre las variables cualitativas
#nos interesa saber cual de estas tres esta mas correlacionada con la distancia

#realizaremos primero unos boxplot para verlo visualmente
dfdatosori

boxplot(formula = as.numeric(Distancia24hrs) ~ Sitio, data = na.omit(dfdatosori),
        na.action=na.pass)
boxplot(formula = as.numeric(Distancia24hrs) ~ Sexo, data = na.omit(dfdatosori),
        na.action=na.pass)
boxplot(formula = as.numeric(Distancia24hrs) ~ Temporada, data = na.omit(dfdatosori),
        na.action=na.pass)
#visualmente vemos como se comportan las variables cualitativas en relacion a los datos


#ahora veremos cual de esas tres variables tiene mayor influencia en la distancia
#recorrida analizandolas por separado, es decir sin influencia de las otras
#utilizare el metodo llamado point biserial correlation
dfdatos1Est

relacionSexo <- cor.test(dfdatos1Est$Sex_hembra, dfdatos1Est$Distancia24hrs) 
relacionSitio <- cor.test(dfdatos1Est$Sitio_cons, dfdatos1Est$Distancia24hrs)
relacionTemporada <- cor.test(dfdatos1Est$Temporada_seca, dfdatos1Est$Distancia24hrs)

print(relacionSexo)
print(relacionSitio)
print(relacionTemporada)
#con esto tenemos la correlacion entre las varibles respecto a la distancia por dia y 
#su significancia

#ordenaremos las varibles respecto a sui orden de importancia
v <- c(relacionSexo$p.value, relacionSitio$p.value, relacionTemporada$p.value)
names(v) <- c("Sexo", "Sitio", "Temporada")
v <- as.data.frame(v[order(v)])  #y listo aqui tenemos el orden de importancia de las variables en relacion
#a la distancia por dia
names(v) <- "p_value"
v



#graficaremos tambien la correacion de las variables categoricas respecto a la distancia
#quitando los NA
dfdatos1Est1 <- na.omit(dfdatos1Est)
dfdatos1Est1
cor(dfdatos1Est1[9:11])
corrplot::corrplot(cor(dfdatos1Est1[c(1, 9:11)]))



############################
#esto a continuacion no es estrictamente correcto ya que se utilizaron metodos diferentes para 
#calcular el p_value, pero de todas formas es una buena guia para identificar las 
#variables mas importantes
probcor1
names(v) <- names(probcor1)
str(rbind(v,probcor1))
importancia_variables <- rbind(probcor1, v)
importancia_variables <-  importancia_variables[order(importancia_variables), ,
                                                drop = F]

importancia_variables
str(importancia_variables)
importancia_variables[-11, ,drop = F]
########################

#con esto tenemos ya identificadas las principales variables que afectan al modelo
#########3





#############################################################################################
#empezaremos a intentar ajustar los datos a un modelo, iniciaremos con uno 
#lineal ya que asi podremos observar las distribuciones de los residuales
#tomaremos todas las variables aunque como hemos visto no todas son significantes

#dfdatos1Est1 <- na.omit(dfdatos1Est[,-10])
dfdatos1Est1

m1 <- lm(formula = Distancia24hrs ~ ., data = dfdatos1Est1)
summary(m1)
m1res <- resid(m1)
#plot(m1)
qqnorm(m1res)
hist(m1res)
plot(density(m1res))
#con las graficas anteriores visualmente podriamos decir que los datos se ajustan 
#mas o menos bien a una recta y son mas o menos normales

#veamoslo formalmente
olsrr::ols_test_normality(m1)
AIC(m1)
shapiro.test(m1res)

#parece que este modelo no es el mas adecuado para estos datos, veamos si reduciendo
#las variables aunicamente las cuantitativas mejora el desempeño
importancia_variables
m2 <- lm(formula = Distancia24hrs ~ EVI_SMO + LST_SMO + NDWI_SMO, data = dfdatos1Est1)
summary(m2)
m2res <- resid(m2)
plot(density(m2res))

olsrr::ols_test_normality(m2)
AIC(m2)
shapiro.test(m2res)
#vemos que no mejora, incluso empeora, seleccionaremos las variables en el orden  que previamente 
#identificamos con mayor correlacion a la distancia

m3 <- lm(formula = Distancia24hrs ~ EVI_SMO + LST_SMO + NDWI_SMO + Sex_hembra +
           Temporada_seca, data = dfdatos1Est1)
summary(m3)
m3res <- resid(m3)
#plot(m3)
qqnorm(m3res)
hist(m3res)
plot(density(m3res))

#tests formales de normalidad
olsrr::ols_test_normality(m3)
AIC(m3)
shapiro.test(m3res)

#hay una mejora significativa respecto a los otros dos, pero aun asi no es suficientemente adecuado

#probaremos unicamente seleccionando las variables con mayor significancia respecto 
#a la distancia

############################## Definitivamente el mejor modelo lineal
importancia_variables
m4 <- lm(formula = Distancia24hrs ~ EVI_SMO + LST_SMO + Sex_hembra +
           Temporada_seca, data = dfdatos1Est1)
summary(m4)
m4res <- resid(m4)
#plot(m4)
qqnorm(m4res)
hist(m4res)
plot(density(m4res))

#tests formales de normalidad
olsrr::ols_test_normality(m4)

AIC(m1)
AIC(m2)
AIC(m3)
AIC(m4)
#############################3


#Hemos visto que el modelo mejora bastante al reducir las variables a aquellas que 
#identificamos en el analisis previo de correlacion, sim embargo vemos en las pruebas
#formales de normalidad que los datos no responden a una distribucion normal,
#
#por lo visto en los graficos parece ser mas bien una distribucion gamma asi que probaremos entonces 
#con unos modelos lineales generalizados  para estos datos
plot_model(m3 ,type = "pred", show.data=T, ci.lvl = NA)

#empezaremos con un modelo glm que incluya todas las variables para ver su resultado
#LA FUNCIO LINK QUE UTILIZAREMOS ES LA FUNCION INVERSA

gm1 <- glm(formula = Distancia24hrs ~., family = Gamma, data = dfdatos1Est1)
summary(gm1)
gm1res <- resid(gm1)
qqnorm(gm1res)
hist(gm1res)
plot(density(gm1res))

#pruebas mas formales
AIC(gm1)
1-gm1$deviance/gm1$null.deviance   #este es el pseudo r2


#vemos que no es exactamente el mejor modelo, asi que vamos a identificar cual es
#la mejor combinacion posible de variables para obtener un mejor desempeño,
#probaremos con la distribucion gama con funcion de enlace inversa, log y identidad
#y veremos de todas las combinaciones posibles cual se ajusta mejor.

#################################################################################
############3antes de eso mi sospecha es que este seria un buen modelo
##veamos si asi es
#m4 <- lm(formula = Distancia24hrs ~ EVI_SMO + LST_SMO + Sex_hembra +
 #          Temporada_seca, data = dfdatos1Est1)

gm2 <- glm(formula = Distancia24hrs ~ EVI_SMO + LST_SMO + Sex_hembra + NDWI_SMO +
             Temporada_seca, family = Gamma(link = "log"), data = dfdatos1Est1)

summary(gm2)
AIC(gm2)
1-gm2$deviance/gm2$null.deviance
#plot(gm2)
plot_model(gm2, type= "pred")

###############################################################################

#para probar todos los modelos se hace mediante la construccion de una matriz,
#para esto es necesario reacomodar los datos para que puedan realizarse las combinaciones posibles

dfmatrix <- dfdatos1Est1[,c(2:length(dfdatos1Est1),1)]
names(dfmatrix)[11] <- "y"
dfmatrix
dfmatrix <- as.data.frame(dfmatrix)

modelos1 <- bestglm(Xy = dfmatrix, 
                   family = Gamma(link = "inverse"), 
                   IC = "AIC",
                   method = "exhaustive")
modelos1
modelos1$BestModels

modelos2 <- bestglm(Xy = dfmatrix, 
                    family = Gamma(link = "log"), 
                    IC = "AIC",
                    method = "exhaustive")
modelos2
modelos2$BestModels

#con esto ya hemos determinado de forma exaustiva cual seria el mejor glm gamma para estos datos
#procederemos a probar la distribucion normal o gaussiana

modelos3 <- bestglm(Xy = dfmatrix, 
                    family = gaussian(link = "identity"), 
                    IC = "AIC",
                    method = "exhaustive")
modelos3
modelos3$BestModels


modelos4 <- bestglm(Xy = dfmatrix, 
                    family = gaussian(link = "log"), 
                    IC = "AIC",
                    method = "exhaustive")
modelos4
modelos4$BestModels


#con esto hemos notado que son constantemente las mismas variables las que influyen
#en los modelos, y de acuerdo a las pruebas realizadas seleccionamos el siguiente modelo 
#con las variabbles NDVI NDWI LST SEXO TEMPORADA.

gm3 <- glm(formula = Distancia24hrs ~ NDVI_SMO + NDWI_SMO + LST_SMO + Sex_hembra +
             Temporada_seca, family = Gamma(link = "log"), data = dfdatos1Est1)

summary(gm3)
AIC(gm3)
1-gm3$deviance/gm3$null.deviance
#plot(gm3)
plot_model(gm3, type="pred")



df1 <- na.omit(dfdatos1)
df1
gm9 <- glm(formula = Distancia24hrs ~ NDVI_SMO + NDWI_SMO + LST_SMO + Sex_hembra +
             Temporada_seca, family = Gamma(link = "log"), data = df1)

summary(gm9)
AIC(gm9)
1-gm9$deviance/gm9$null.deviance
#plot(gm3)
plot_model(gm9, type="pred")



visreg(gm3, 
       gg = TRUE, 
       scale="response")

visreg(gm9, "LST_SMO", )

#con esto tenemos un modelo que se ajusta bastante bien a los datos y en el cual podemos hacer predicciones 
#usaremos los mismos datos de entrada originales (a falta de nuevos datos) para
#ver que tal predice nuestro modelo
dfdatos1Est

predicciones <- exp(predict(object = gm3, newdata = dfdatos1Est1))
predicciones

#vemos que produce un vector con las predicciones y son valores bastante cercanos a los datos
#lo cual ya se ha probado mediante los analisis anteriores.


























#guardar los datos en csv
i <- 0
for (datas in ldf) {
  i <- i+1
  write.csv(x = datas, file = vnombres[i])
}











predicciones <- predicciones * sd(dfdatos1Est1$Distancia24hrs) + mean(dfdatos1Est1$Distancia24hrs)

usp2 <- sp * sd(cars$speed) + mean(cars$speed)


