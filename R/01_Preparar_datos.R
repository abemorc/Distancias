
# Importar datos ----------------------------------------------------------


#no es necesario asignar a diferentes variables cada dataset, es hecho aqui 
#para que veas claramente el proceso


dfdatos <- readxl::read_excel(here("Data", "Raw_Data", "spicu2.xlsx"), sheet = "in")
View(dfdatos)

#corregir nombres
vremove <- c(" ")
vreplace <- c("_")

names(dfdatos) <- gsub(vremove, vreplace, names(dfdatos))
names(dfdatos)

#seleccionar variables que nos interesan
dfdatos <- dfdatos %>% 
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
         Sexo_macho = ifelse(Sexo == "Macho", 1, 0), #esta variable dummy es porque sexo tiene 3 categorias
         Sex_hembra = ifelse(Sexo == "Hembra", 1, 0),
         Temporada_seca = ifelse(Temporada == "Secas", 1, 0)) %>% 
  select(Distancia24hrs, Distancia_total_reccorida, NDVI_SMO:Altura_de_arbol,
         Sitio_cons:Temporada_seca)

#verificar que todo este correcto
str(dfdatos)
View(dfdatos)

#guardar los datos
write_csv(x = dfdatos, file = here("Data/Processed_Data/Distancias_sapos.csv"),
          append = F, col_names = T)
