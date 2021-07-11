rmarkdown::render(here("Rmd", "Reporte.Rmd"), output_file = here("Reportes", "Reporte.html"))

kbl(dfdatos1Est, digits = 3, booktabs = T)

here()

kbl(head(iris), digits = 3, booktabs = T)

dt <- head(iris)
kbl(dt, booktabs = T) %>%
  kable_styling(latex_options = "striped")
kbl(dt, caption = "Demo table", booktabs = T) %>%
  kable_styling(latex_options = c("striped", "hold_position"))


correlaciones %>%
  kbl(caption = "CORRELACIONES", digits = 3, align = "c") %>%
  kable_classic_2(full_width = F) %>% 
  footnote(general = "Calculado por el método de Spearman")


library(devtools)
install_github("holtzy/epuRate")
library(epuRate)


detach("package:vegan", unload=TRUE)



d <- runif(50)
d
curve(ecdf(data), ylab = "FD", type = "s", lwd = 2)
x
library(MASS)

Tweets <- data.frame(
retweetCount = c(1388, 762, 748, 436, 342, 320, 
                 312, 295, 264, 251, 196, 190, 175, 167, 165, 163, 149, 148, 148, 
                 146, 133, 132, 126, 124, 122, 122, 121, 120, 118, 118, 114, 113, 
                 112, 110, 108, 107, 104, 101, 100, 96, 95, 94, 93, 92, 90, 90, 
                 89, 89, 87, 86, 84, 83, 83, 83, 82, 82, 82, 82, 78, 78, 78, 76, 
                 76, 76, 76, 74, 74, 73, 73, 72, 72, 71, 70, 70, 70, 70, 69, 69, 
                 69, 68, 68, 67, 65, 65, 65, 65, 63, 62, 62, 61, 61, 61, 61, 60, 
                 60, 59, 59, 59, 59, 58), ME_words = c(2, 2, 2, 0, 0, 1, 1, 0, 
                                                       1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 1, 
                                                       0, 3, 0, 1, 0, 1, 1, 4, 1, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 
                                                       0, 0, 2, 2, 0, 0, 1, 0, 1, 0, 0, 2, 0, 0, 0, 1, 0, 1, 1, 0, 0, 
                                                       0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                       0, 1, 0, 0, 0, 1, 0, 0), Moral_words = c(0, 0, 1, 1, 1, 2, 0, 
                                                                                                0, 0, 1, 0, 1, 2, 0, 1, 1, 1, 2, 0, 1, 0, 1, 1, 0, 2, 0, 1, 1, 
                                                                                                1, 0, 1, 1, 1, 1, 0, 2, 0, 1, 1, 1, 2, 0, 1, 1, 1, 1, 0, 1, 0, 
                                                                                                0, 5, 1, 1, 1, 1, 2, 0, 1, 1, 0, 1, 0, 1, 1, 1, 0, 2, 0, 0, 0, 
                                                                                                1, 1, 2, 0, 0, 0, 0, 0, 1, 3, 1, 0, 0, 1, 0, 0, 0, 1, 1, 1, 1, 
                                                                                                1, 1, 0, 0, 2, 2, 1, 0, 0), Emo_words = c(0, 0, 1, 1, 0, 0, 2, 
                                                                                                                                          0, 1, 0, 2, 0, 1, 0, 1, 2, 0, 3, 1, 1, 2, 0, 0, 0, 0, 0, 1, 1, 
                                                                                                                                          1, 2, 0, 1, 0, 0, 0, 1, 0, 1, 0, 2, 0, 0, 1, 0, 1, 1, 2, 0, 0, 
                                                                                                                                          1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 3, 0, 0, 2, 0, 0, 1, 0, 
                                                                                                                                          1, 1, 0, 1, 0, 0, 1, 0, 0, 1, 2, 2, 1, 0, 0, 0, 0, 2, 1, 0, 0, 
                                                                                                                                          1, 0, 0, 1, 2, 2, 0, 0, 0))
M1 <- glm.nb(retweetCount ~ ME_words + Moral_words + 
               Emo_words, data = Tweets)
summary(M1)

nboot <- 1000
bres <- matrix(NA,nrow=nboot,
               ncol=length(coef(M1)),
               dimnames=list(rep=seq(nboot),
                             coef=names(coef(M1))))
bres

bootsize <- 200
for (i in seq(nboot)) {
  bdat <- Tweets[sample(nrow(Tweets),size=bootsize,replace=TRUE),]
  bfit <- update(M1, data=bdat)  ## refit with new data
  bres[i,] <- coef(bfit)
}

data.frame(mean_est=colMeans(bres),
           t(apply(bres,2,quantile,c(0.025,0.975))))
apply(bres,2,quantile,c(0.025,0.975))
?quantile
?apply

quantile(bres)

a <- as.character(1:4)
expand.grid(a)
combn(1:5,m = 3)
b <- names(dfdatos1)[2:11]
b
c <- combn(b, 1)
c[1,1]


pm1 <- glm(formula = Distancia24hrs ~ c[1,1], family = Gamma(link = "log"), data = dfdatos1)
summary(pm1)

#####################
vars <- c("price","model","size","year","color")
vars

N <- list(1,2,3,4)
N

COMB <- sapply(N, function(m) combn(x=vars[2:5], m))
COMB

combn(x=vars[2:5], 4)

COMB2 <- list()
seq(COMB)


k=0

for(i in seq(COMB)){
  tmp <- COMB[[i]]
  for(j in seq(ncol(tmp))){
    k <- k + 1
    COMB2[[k]] <- formula(paste("price", "~", paste(tmp[,j], collapse=" + ")))
  }
}

res <- vector(mode="list", length(COMB2))
for(i in seq(COMB2)){
  res[[i]] <- lm(COMB2[[i]], data=data)
}



#Then, you can call these formulas and store the model objects using a list or possibly give unique names with the assign function:
  
  res <- vector(mode="list", length(COMB2))
for(i in seq(COMB2)){
  res[[i]] <- lm(COMB2[[i]], data=data)
}


##################################3
library(MuMIn)
backup_options <- options()

df <- mtcars[,c("mpg","cyl","disp","hp","wt")]  # subset of mtcars
full.model <- lm(formula =  mpg ~ cyl + disp + hp + wt,data = df)       # model for predicting mpg
options(na.action = "na.fail")
aa <- dredge(full.model, beta = "none")
getCall(aa,i = 1)
aa[1:2,]

qqq <- get.models(aa,subset = delta==0)
qqq$`10`


q <- get.models(aa, subset = 1)[[1]]

q1 <- q$terms
q2 <- q$call
str(q2)
class(q2)

str(q)

library(broom)

tidy(gm3)
glance(gm3)





?menu
menu(c("sdfsdfsdf", "sdgdfgdg"),graphics = T,title = "prueba")
select.list()
readline()


  
fullm <- glm(formula = Distancia24hrs ~., family = Gamma(link = "log"), data = dfdatos1Est1)
mprueba <- dredge(fullm)
dfdatos1Est1

View(mprueba)
str(mprueba)
mprueba$weight
pp <- Weights(mprueba)
str(pp)
sw(mprueba)        #suma de akaike weights

oo <- subset(mprueba, delta == 0)
oo
ii <- mprueba[1]


getCall(mprueba, i = 1)
mprueba[1,]


View(pp)

?get.models
modelosalida <- get.models(mprueba, 1)[[1]]
modelosalida
get.models(mprueba, 1)
summary(mprueba)

z <- glm(modelosalida, data = dfdatos1Est1)
summary(z)

modelosalida2 <- get.models(mprueba, 6)[[1]]
modelosalida2$formula

zz <- glm(modelosalida2, data = dfdatos1Est1)
summary(zz)
summary(gm3)

AIC(z)
AIC(gm3)
AIC(zz)
AIC(gm9)

glm(Distancia24hrs ~ LST_SMO + NDVI_SMO + NDWI_SMO + Sex_hembra + 
      Temporada_seca, family = Gamma(link = "log"), data = dfdatos1Est1 )

options(backup_options)

View(combn(b, 2))



# create a dataset
data <- data.frame(
  name=c( rep("A",500), rep("B",500), rep("B",500), rep("C",20), rep('D', 100)  ),
  value=c( rnorm(500, 10, 5), rnorm(500, 13, 1), rnorm(500, 18, 1), rnorm(20, 25, 4), rnorm(100, 12, 1) )
)

# Plot
df2 %>%
  ggplot( aes(x=Sitio_cons, y=Distancia_total_reccorida, fill=as.factor(Sitio_cons))) +
  geom_boxplot() +
  
  geom_jitter(color="black", size=3, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("asdasd") +
  xlab("Sitio")

dfdatosori





boxplot(formula = as.numeric(Distancia_total_reccorida) ~ Sitio, data = na.omit(dfdatosori),
        na.action=na.pass)


data12 <- data.frame(
  name=c( rep("A",500), rep("B",500), rep("B",500), rep("C",20), rep('D', 100)  ),
  value=c( rnorm(500, 10, 5), rnorm(500, 13, 1), rnorm(500, 18, 1), rnorm(20, 25, 4), rnorm(100, 12, 1) )
)
str(data12)




str(df2)
ggplot(df2, aes(x = as.factor(Sitio_cons), y = Distancia_total_reccorida)) + geom_boxplot()

library(viridis)
library(hrbrthemes)

df2 %>% 
  ggplot(aes(y=Distancia_total_reccorida, x=Sitio_cons, group=Sitio_cons, fill=as.factor(Sitio_cons))) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9)



# mas pruebas -------------------------------------------------------------

set.seed(100)
X <- runif(500, 1, 100)
X

set.seed(200)
noise <- rnorm(500, 0, 25)
noise

Y <- X + noise
Y

## Correlation of X and Y
print(cor(X, Y))


modelXY <- lm(Y ~ X)


## model summary
sumryXY <- summary(modelXY)
sumryXY

## r-sq of model
rsqXY <- sumryXY$r.squared

print(rsqXY)

## square root of r-sq
print(sqrt(rsqXY))


#####
#make a contionues vriable into categorical
X_cat <- cut(X, breaks = 5, labels = c("Sam", "Pete",
                                       "Jon", "Tom", "Chris"))
cut(X, breaks = 5, labels = c("Sam", "Pete",
                              "Jon", "Tom", "Chris"))
summary(X_cat)
?cut
X

#make regresiion model
modelX_catY <- lm(Y ~ X_cat)

## summary
sumryX_catY <- summary(modelX_catY)
print(sumryX_catY)


##
# r-sq
rsqX_catY <- sumryX_catY$r.squared

print(rsqX_catY)
## [1] 0.526957
## square root of r-sq
print(sqrt(rsqX_catY))
## [1] 0.725918

###33##prueba de glm2
pm1 <- lm(df2$Distancia_total_reccorida ~ df2$Sitio_cons)
pm1su <- summary(pm1)
pm1su$r.squared
sqrt(pm1su$r.squared)

##
pm1 <- lm(df2$Distancia_total_reccorida ~ dfdatosori$Sitio)
pm1su <- summary(pm1)
pm1su$r.squared
sqrt(pm1su$r.squared)


######
#para describir una variable agrupada todas las variables
library(psych)
describeBy(dfdatos1, group="Sex_hembra")



#regresion logistica
set.seed(1)

## sample independent variables
d = data.frame(hours_studied = rpois(n=1000, lambda=12),
               selftest = rbinom(n=1000, size=1, prob=0.25),
               alcohol = rpois(n=1000, lambda=10))
head(d)
str(d)

## linear prediction 
mu = -1.5 + 0.4*d$hours_studied + 1.2*d$selftest + -0.2*d$alcohol
head(mu)
str(mu)

## transform with inverse of "logit" link
prob = 1 / (1 + exp(-mu))
head(prob)
## generate x from prediction with binomial error distribution
d$passed_test = y = rbinom(1000, 1, prob = prob)

head(d)
str(d)

#hacer modelo
m = glm(passed_test ~ hours_studied + selftest + alcohol, 
        data=d, family=binomial)
summary(m)
1-m$deviance/m$null.deviance
pR2(m)
predict(object = m, newdata = d)
plot_model(m, type='pred', grid = T)
