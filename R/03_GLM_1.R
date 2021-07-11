
# Modelos GLM -------------------------------------------------------------




#como para esto nos basaremos en algunos objetos obtenidos del analisis de 
#correlacion, entonces cargaremos la .Data generada ahi

load("GLM.RData")


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

importancia_variables
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
BIC(gm1)
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

visreg(gm3, 
       gg = TRUE, 
       scale="response")


#####################################################
dfdatos1
df1 <- na.omit(dfdatos1)
df1
gm9 <- glm(formula = Distancia24hrs ~ NDVI_SMO + NDWI_SMO + LST_SMO + Sex_hembra +
             Temporada_seca, family = Gamma(link = "log"), data = df1)

summary(gm9)
AIC(gm9)
1-gm9$deviance/gm9$null.deviance
#plot(gm3)
plot_model(gm9, type="pred")



visreg(gm9, "LST_SMO", main = "DFSDF")

visreg(gm9,
       gg = T, scale = "response")
######################################################

#con esto tenemos un modelo que se ajusta bastante bien a los datos y en el cual podemos hacer predicciones 
#usaremos los mismos datos de entrada originales (a falta de nuevos datos) para
#ver que tal predice nuestro modelo
dfdatos1Est

predicciones <- exp(predict(object = gm3, newdata = dfdatos1Est1))
predicciones



dfdatos1Est$Predicciones <- predicciones
largo <- length(dfdatos1Est)
mostrar <- c(largo, 1:(largo-1))
mostrar

dfdatos1Est[mostrar]


1:length(dfdatos1Est)-1



#vemos que produce un vector con las predicciones y son valores bastante cercanos a los datos
#lo cual ya se ha probado mediante los analisis anteriores.

