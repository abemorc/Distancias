
# Analisis correlacion ----------------------------------------------------



#importar datos
dfdatos1 <- read_csv(file = here("Data/Processed_Data/Distancias_sapos.csv"))

#creamos el dataframe con las variables que nos interesan
dfdatos1 <- dfdatos[,c(1, 3:10, 12, 13)]

#observar que todo este correcto
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


