


edu <-read_excel('Datos/edu.xlsx')
edu_1<- edu %>%
  dplyr::select(-TAMANIO_PROMEDIO_DE_GRUPO,-COBERTURA_BRUTA,-COBERTURA_BRUTA_TRANSICION,
         -COBERTURA_BRUTA_PRIMARIA,-COBERTURA_BRUTA_SECUNDARIA,
         -COBERTURA_BRUTA_MEDIA )

PIB <-read_excel('Datos/PIB.xlsx')
mapa<-read_excel('Datos/Mapa colombia.xlsx')



general<-merge(PIB,edu_1, by="CODIGO_DEPARTAMENTO")

data <-merge(general,mapa, by="CODIGO_DEPARTAMENTO")

