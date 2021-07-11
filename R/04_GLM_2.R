
# Influencia ambiente en distancia recorrida incilus ----------------------


#para este analisis  se busca determinar como el ambiente y la perturbacion del 
#sitio afecta a la distancia recorrida por los sapos 

#importar datos
#como el ranking de influencia de variables lo obtendremos con la suma de los
#pesos de akaike no necesitamos las variables estandarizadas

df2 <- read_csv(file = here("Data/Processed_Data/Distancias_sapos.csv"))

#seleccion variables que necesitamos
df2 <- df2[c(2:10)]





# correlacion (practicamente igual que el primero) ------------------------

### Variables cuantitativas ###

cor2 <- cor(df2[c(1:8)], use ="complete.obs", method = "spearman")
cor2
#cor(df2, use ="complete.obs", method = "spearman")

pcor2 <-  Hmisc::rcorr(as.matrix(df2[c(1:8)]), type = "spearman")[[3]]
pcor2

#ordenarlas

cor2 <- as.data.frame(cor2)
cor2 <- cor2[order(abs(cor2$Distancia_total_reccorida), decreasing = T), 1, drop = F]
cor2

pcor2 <- as.data.frame(pcor2)
pcor2 <- pcor2[order(pcor2$Distancia_total_reccorida), 1 , drop = F]
pcor2

#graficas
PerformanceAnalytics::chart.Correlation(df2[c(1:8)], histogram = T,
                                        method = "spearman")
pairs(df2[c(1:8)])

psych::pairs.panels(df2[c(1:8)], method = "spearman", stars = TRUE,  
                    hist.col = 4, smooth = TRUE, scale = F, density = TRUE,
                    pch = 21, lm = F, jiggle = T, ci = TRUE)



### categoricas ###

#inspeccion visual
dfdatosori %>%
  ggplot( aes(x=as.factor(Sitio), y=as.numeric(Distancia_total_reccorida), fill=as.factor(Sitio))) +
  geom_boxplot(notch = F) +
  geom_jitter(color="black", size=3, alpha=0.9) +
  labs(fill = "Sitio",
       x = "Estado del Sitio",
       y = "Distancia total",
       title = "Relacion sitio con distancia")

# point biserial correlation
pbc <- cor.test(df2$Sitio_cons, df2$Distancia_total_reccorida)
pbc
#no parece muy relacionada

#la distancia por dia y sitio tampoco estan tan fuertemente relacionadas
relacionSitio

#veamos para las demas variables
cor.test(df2$Sitio_cons, df2$CME_SMO_15) ###solo esta es importante
cor.test(df2$Sitio_cons, df2$NDVI_SMO)
cor.test(df2$Sitio_cons, df2$EVI_SMO)
cor.test(df2$Sitio_cons, df2$NDWI_SMO)
cor.test(df2$Sitio_cons, df2$LST_SMO)
cor.test(df2$Sitio_cons, df2$Altura_de_arbol)
cor.test(df2$Sitio_cons, df2$Sitio_cons)
### De aqui podemos concluir que el estado del sitio esta muy fuertemente
### relacionado con la variale CME_SMO

#grafica
corrplot::corrplot(cor(df2[c(1, 9)]))
str(df2)
#definitivamente no es una grafica que nos interese mostrar


#veamos si podemos obtener algo mejor

#conteo de sitios
table(df2$Sitio_cons)

###mediante el ajuste de un modelo lineal y unicamente la variable categorica
###podemos obtener mediante el r.adjusted del modelo un grado de relacion
###se obtiene sacando la raiz cuadrada del r.adjusted
pm1 <- lm(df2$Distancia_total_reccorida ~ df2$Sitio_cons)
pm1su <- summary(pm1)
pm1su
pm1su$r.squared
sqrt(pm1su$r.squared)
#conclusion:
##mmm obtenemos el mismo valor, no mejora

##probando con la categoria sin variable dummy
pm1 <- lm(df2$Distancia_total_reccorida ~ dfdatosori$Sitio)
pm1su <- summary(pm1)
pm1su
pm1su$r.squared
sqrt(pm1su$r.squared)
#conclusion:
#tampoco obtenemos nada
#mmaldita sea

#no se me ocurre nadaaaaaaa
#tus datos estan troncos

#intentemos otras formas
####



# Explorar relacion entre sitio y distancia ---------------------------------


#en el boxplot vemos que la distancia tiende a ser mayor en sitios perturbados
#veamos entonces los datos en una tabla

df2 %>% 
  group_by(Sitio_cons) %>% 
  summarize(observaciones = n(),
            promedio_dis = mean(Distancia_total_reccorida),
            minimo_dis = min(Distancia_total_reccorida),
            maximo_dis = max(Distancia_total_reccorida),
            rango_dis = (maximo_dis - minimo_dis),
            desviacion_dis = sd(Distancia_total_reccorida),
            )
#si hay una clara tendencia a aumentar la distancia en base al estado del sitio


###Replicare esto para distancia por dia
###
#creo la funcion para conocer el error estandar
se <- function(x, ...) sqrt(var(x, ...)/length(x))
####

#Por sexo  ~  Distancia24hrs
na.omit(dfdatos1) %>% 
  group_by(Sex_hembra) %>% 
  summarize(observaciones = n(),
            promedio_dis = mean(Distancia24hrs),
            minimo_dis = min(Distancia24hrs),
            maximo_dis = max(Distancia24hrs),
            rango_dis = (maximo_dis - minimo_dis),
            desviacion_dis = sd(Distancia24hrs),
            se(Distancia24hrs))

#Por sitio  ~  Ditancia24hrs
na.omit(dfdatos1) %>% 
  group_by(Sitio_cons) %>% 
  summarize(observaciones = n(),
            promedio_dis = mean(Distancia24hrs),
            minimo_dis = min(Distancia24hrs),
            maximo_dis = max(Distancia24hrs),
            rango_dis = (maximo_dis - minimo_dis),
            desviacion_dis = sd(Distancia24hrs),
            se(Distancia24hrs))

#ok, busquemos mas formas de comprobarlo



# Regresion logistica -----------------------------------------------------


## una forma de encontrar una relacion entre una variable categorica de dos niveles
#y otra continua seria mediante la contruccion de una regresion logistica
#aqui la logica es que si ambas variables estan relacionadas entonces podemos 
#construir un modelo de la continua como preduictora y de la categorica como respuesta,
#en nuestro caso seria lo siguiente: preedecir en base a la distancia24hrs si el sitio
#presenta un estado perturbado o conservado.
#en caso de que nuestra suposicion sea correcta entonces el modelo presentara un
#buen ajuste

#regresion logistica
str(df2)
as.factor(df2$Sitio_cons)
rgl <- glm(formula = Sitio_cons ~ Distancia_total_reccorida,
           family = binomial(link = "logit"),data = df2)

#veamops como salio
summary(rgl)
#pseudo r2 McFadden         
1-rgl$deviance/rgl$null.deviance
#otra medida de fit
pscl::pR2(rgl)
###### parece que no es buen modelo

#hagamos otro solo con el intercepto para comparar
rgli <- glm(formula = Sitio_cons ~ 1,
            family = binomial(link = "logit"),data = df2)

summary(rgli)
1-rgli$deviance/rgli$null.deviance
pR2(rgli)
##### pues en realidad si indica mas con la variable de sitio pero es 
#practicamente nada

#intentemos con otra funcion de enlace 
rgl0 <- glm(formula = Sitio_cons ~ Distancia_total_reccorida,
            family = binomial(link = "log"),data = df2)
summary(rgl0)
1-rgl0$deviance/rgl0$null.deviance
pR2(rgl0)


exp(predict(object = rgl0, newdata = df2))
predicciones


####
df3 <- df2
df3$Sitio_cons <- as.factor(df3$Sitio_cons)
str(df3)
rgl3 <- glm(formula = Sitio_cons ~ Distancia_total_reccorida,
            family = binomial(link = "logit"),data = df3)

summary(rgl3)
1-rgl3$deviance/rgl3$null.deviance
pR2(rgl3)
predict(object = rgl0, newdata = df2)
# no obtenemos nada de aqui tampoco, la regresion logistica
# practicamente indica una nula relacion




# Algoritmo de clasificacion ML -------------------------------------------


###usando un algoritmo de clasificacion llamado naive bayes clasification
#clasificador Bayesiano ingenuo
#es un algoritmo de aprendizaje automático basado en el teorema de Bayes
library(e1071)
fitNB <- naiveBayes(formula = Sitio_cons ~ Distancia_total_reccorida, data = df2)
fitNB
fitNB$apriori
fitNB$tables
fitNB$levels
fitNB$isnumeric
fitNB$call

summary(fitNB)

pred1 <- predict(fitNB, df2)
pred1
tabp <- table(df2$Sitio_cons, pred1, dnn = c("Actual", "predicciones"))
tabp
confusionMatrix(tabp)
# el modelo nos muestra una precicion del 66%
# mmm no esta mal


#para comparar
table(predict(fitNB, df2))
df2$Sitio_cons
table(df2$Sitio_cons)
#
set.
indx <- sample(2, 30, replace = T, prob = c(0.7, 0.3))
table(indx)

#hagamos otro modelo de clasificacion para ver si lo podemos mejorar
library(naivebayes) 

df3

mn <- naive_bayes(Sitio_cons ~ Distancia_total_reccorida, data = df3)
pnaive <- predict(mn, df3)
pnaive
confusionMatrix(pnaive, df3$Sitio_cons)


#enfoque machine learning 
#con datos de entrenamiento y set de prueba

ind <- sample(2,nrow(df3), replace = TRUE, prob = c(0.7,0.3) ) #70% entrenamiento y 30% test
trainData<- df3[ind==1,]
testData<- df3[ind==2,]

mn2 <- naive_bayes(Sitio_cons ~ Distancia_total_reccorida, data = trainData)
mn2
pnaive2 <- predict(mn2, testData)
pnaive2
confusionMatrix(pnaive2, testData$Sitio_cons)
### casi lo mismo por lo tanto podemos concluir que efectivamente hay una relacion
#del 66% en la variable sitio y distancias



# Idea varianza -----------------------------------------------------------


#Una forma que encontre que se usa para estimar la correlacion entre dos variables
#una continua y una categorica es midiando la variance de la variable continua
#y comparandola contra la varianza de la misma variable continua pero por grupos 
#en base a los niveles de la variable categorica

#si la varianza baja despues de estar agrupada, significa entonces que la variable 
#categorica puede explicar la mayoria de la varianza en la continua, es decir 
#existe una fuerte relacion entre ambas
#si la varianza no cambia mucho significa que no estan relacionadas

#veamos entonces la varianza de la distancia total
vdis <- var(df2$Distancia_total_reccorida)

vgroup <- df2 %>% 
  group_by(Sitio_cons) %>% 
  summarize(observaciones = n(),
            varianza = var(Distancia_total_reccorida),
            desviacion_dis = sd(Distancia_total_reccorida))

## Eureka :)
#jajaja pues si logramos encontrar una relacion tambien por medio de la varianza
#

dfvar <- data.frame(Varianza = c(vdis, vgroup$varianza),
                    row.names =  c("Total", "Perturbado", "Conservado"))
dfvar

### Conclusiones:
# con el algoritmo de clasificacion y por medio de la estimacion de la varianza
# para ambas categorias del sitio podemos comprobar la existencia de la relacion.




# Replica de "Statical analisis" que esta en el pdf que me mandaste -------
# ahi usan un conjunto de modelos glm para determinar que variables afectan mas
# al movimiento, despues usan la suma de los pesos de akaike para cada variable 
# para determinar una especie de ranking de las variables en base a en cauntos
# modelos aparecia y su peso relativo en ellos










