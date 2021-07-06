gm5 <- glm(formula = Distancia24hrs ~ EVI_SMO + NDVI_SMO + NDWI_SMO + LST_SMO + Sex_hembra +
             Temporada_seca, family = Gamma(link = "log"), data = dfdatos1Est1)

summary(gm5)
AIC(gm3)
1-gm3$deviance/gm3$null.deviance
#plot(gm3)
plot_model(gm3, type="pred")

visreg(gm3, 
       gg = TRUE, 
       scale="response")
