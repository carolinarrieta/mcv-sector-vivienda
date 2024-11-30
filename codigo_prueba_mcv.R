rm()

pacman::p_load(tidyverse,pder,sf,tmap,tmaptools,viridis,summarytools,terra,glue,
               lmtest, zoo, Hmisc, AER, skimr,modelsummary,plm,gt,table1,haven,
               ggthemes,wesanderson,estimatr,dplyr,sf,rgeoda,tidylog,spatialreg,
               spdep,readxl,DT,data.table,plotly,lubridate, shinyWidgets, shiny, 
               openxlsx2,openxlsx, tidyr, margins, knitr, pscl, pROC)

data_2021 <- read_sav("/Users/carolina/Library/CloudStorage/OneDrive-Personal/MCV/2021/MICRODATOS ECV ANTIOQUIA 2021 ANONIMIZADOS.sav")
data_2023 <- read_sav("/Users/carolina/Library/CloudStorage/OneDrive-Personal/MCV/2023/MICRODATOS ECV ANTIOQUIA 2023 ANONIMIZADOS.sav")

data_2021 <- data_2021 %>% distinct()
data_2023 <- data_2023 %>% distinct()

data1_2021 <- data_2021 %>% select(c(NoForm, CodPersona,sHogar, codUnico, Ciudad, Zona, ComunaCorr, 
                                     NomComuna, BarrioVereda, NomBarrio, Estrato, NomMpio, Region_c, P_014,P_012,P_049,
                                     SubRegion, Region, Region_a, SubRegion_a, Region_b, P_172, P_173,P_174,
                                     P_175, P_176,P_177, P_178, P_179, P_180, P_181, P_182, P_183, P_184, P_185, P_186, 
                                     P_187, P_188, P_189, P_190, P_191, P_192, P_193, P_194, P_195, P_196, P_197, P_198, 
                                     P_199, P_200,P_204, P_205, P_206, P_208, P_347,P_252, P_269, P_318,P_319,P_394, FE, FEH, FEV))

data1_2023 <- data_2023 %>% select(c(NoForm,Hogar, persona, Ciudad, ComunaCorr, NomComuna,BarrioVereda, NomBarrio,Estrato,
                                     NomMpio, NomMunicipio, Cod_Zona, NombreZona, Cod_SubRegion, NomSubRegion, Ubicacion, NomProvincia,
                                     P_014,P_012,P_049, P_172, P_173,P_174,
                                     P_175, P_176,P_177, P_178, P_179, P_180, P_181, P_182, P_183, P_184, P_185, P_186, 
                                     P_187, P_188, P_189, P_190, P_191, P_192, P_193, P_194, P_195, P_196, P_197, P_198, 
                                     P_199, P_200,P_204, P_205, P_206, P_208, P_347,P_252, P_269, P_318,Fexp_Viv, Fexp_Hog, FExp_Per))

# Contar cuántas observaciones tienen NA en la columna Fexp_Hog
sum(is.na(data1_2023$Fexp_Hog))
# Resumen de NA en cada columna del dataframe
colSums(is.na(data1_2023))

# Filtrar las filas donde Fexp_Hog tiene NA
data1_2023 <- data1_2023 %>%
  filter(!is.na(Fexp_Hog))

data1_2023 %>%
  group_by(P_252) %>%
  summarise(conteo_ponderado = sum(Fexp_Hog)) %>%
  mutate(porcentaje = conteo_ponderado / sum(conteo_ponderado, na.rm = TRUE)) %>%
  arrange(desc(porcentaje)) %>%  # Ordenar de mayor a menor por porcentaje
  print(n = 30)

data1_2021 %>%
  summarise(promedio_hogares_por_vivienda = mean(P_252, na.rm = TRUE))



#Proporciones básicas

#Comuna de la muestra

data1_2021 %>%
  group_by(ComunaCorr) %>%
  summarise(conteo_ponderado = sum(FEH)) %>%
  mutate(porcentaje = conteo_ponderado / sum(conteo_ponderado, na.rm = TRUE)) %>%
  arrange(desc(porcentaje)) %>%  # Ordenar de mayor a menor por porcentaje
  print(n = 30)

data1_2023 %>%
  group_by(NomComuna) %>%
  summarise(conteo_ponderado = sum(Fexp_Hog)) %>%
  mutate(porcentaje = conteo_ponderado / sum(conteo_ponderado, na.rm = TRUE)) %>% 
  print(n = 30)

#Cantidad de personas en la vivienda

data1_2021 %>%
  group_by(P_014) %>%
  summarise(conteo_ponderado = sum(FEH)) %>%
  mutate(porcentaje = conteo_ponderado / sum(conteo_ponderado, na.rm = TRUE)) %>% 
  arrange(desc(porcentaje)) %>%  # Ordenar de mayor a menor por porcentaje
  print(n = 30)

data1_2023 %>%
  group_by(P_014) %>%
  summarise(conteo_ponderado = sum(Fexp_Hog)) %>%
  mutate(porcentaje = conteo_ponderado / sum(conteo_ponderado, na.rm = TRUE)) %>% 
  arrange(desc(porcentaje)) %>%  # Ordenar de mayor a menor por porcentaje
  print(n = 30)

#Cantidad de hogares en la vivienda
data1_2021 %>%
  group_by(P_012) %>%
  summarise(conteo_ponderado = sum(FEH)) %>%
  mutate(porcentaje = conteo_ponderado / sum(conteo_ponderado, na.rm = TRUE)) %>% 
  print(n = 30)

data1_2023 %>%
  group_by(P_012) %>%
  summarise(conteo_ponderado = sum(Fexp_Hog)) %>%
  mutate(porcentaje = conteo_ponderado / sum(conteo_ponderado, na.rm = TRUE)) %>% 
  print(n = 30)

#Ultimo nivel de estudios
data1_2021 %>%
  group_by(P_049) %>%
  summarise(conteo_ponderado = sum(FEH)) %>%
  mutate(porcentaje = conteo_ponderado / sum(conteo_ponderado, na.rm = TRUE)) %>% 
  arrange(desc(porcentaje)) %>% 
  print(n = 30)

data1_2023 %>%
  group_by(P_049) %>%
  summarise(conteo_ponderado = sum(Fexp_Hog)) %>%
  mutate(porcentaje = conteo_ponderado / sum(conteo_ponderado, na.rm = TRUE)) %>% 
  arrange(desc(porcentaje))%>% 
  print(n = 30)

#Vivienda propia
data1_2021 %>%
  group_by(P_252) %>%
  summarise(conteo_ponderado = sum(FEH)) %>%
  mutate(porcentaje = conteo_ponderado / sum(conteo_ponderado, na.rm = TRUE)) %>% 
  arrange(desc(porcentaje)) %>% 
  print(n = 30)

data1_2023 %>%
  group_by(P_252) %>%
  summarise(conteo_ponderado = sum(Fexp_Hog)) %>%
  mutate(porcentaje = conteo_ponderado / sum(conteo_ponderado, na.rm = TRUE)) %>% 
  arrange(desc(porcentaje))%>% 
  print(n = 30)
#Personas que viven en una vivienda hechas con los siguientes materiales:
#1.Materiales de desechos y otros  (zinc, tela, lona, cartón, latas,  desechos, plásticos, etc.)    
#2.Madera burda    
#3.Bahareque sin revocar, guadua, caña, esterilla, Otro vegetal    

data1_2021 %>%
  group_by(P_173) %>%
  summarise(conteo_ponderado = sum(FEH)) %>%
  mutate(porcentaje = conteo_ponderado / sum(conteo_ponderado, na.rm = TRUE)) %>% 
  print(n = 30)

data1_2023 %>%
  group_by(P_173) %>%
  summarise(conteo_ponderado = sum(Fexp_Hog)) %>%
  mutate(porcentaje = conteo_ponderado / sum(conteo_ponderado, na.rm = TRUE)) %>% 
  print(n = 30)

# Definimos qué materiales corresponden a "malas condiciones"
mat_mal <- c("1", "2","3")

# Crear una nueva columna para identificar si el material es de malas condiciones (TRUE/FALSE)
data1_2021$malas_condiciones <- ifelse(data1_2021$P_173 %in% mat_mal, 1, 0)
data1_2023$malas_condiciones <- ifelse(data1_2023$P_173 %in% mat_mal, 1, 0)

#proporcion_mal <- sum(data1_2021$P_173 %in% mat_mal * data1_2021$FEH) / sum(data1_2021$FEH)

# Crear una nueva columna con el conteo ponderado
data1_2021 %>%
  group_by(malas_condiciones) %>%
  summarise(conteo_ponderado = sum(FEH)) %>%
  mutate(porcentaje = conteo_ponderado / sum(conteo_ponderado, na.rm = TRUE)) %>% 
  print(n = 30)

data1_2023 %>%
  group_by(malas_condiciones) %>%
  summarise(conteo_ponderado = sum(Fexp_Hog)) %>%
  mutate(porcentaje = conteo_ponderado / sum(conteo_ponderado, na.rm = TRUE)) %>% 
  print(n = 30)

#Fuente del agua

data1_2021 %>%
  group_by(P_175) %>%
  summarise(conteo_ponderado = sum(FEH)) %>%
  mutate(porcentaje = conteo_ponderado / sum(conteo_ponderado, na.rm = TRUE))

data1_2023 %>%
  group_by(P_175) %>%
  summarise(conteo_ponderado = sum(Fexp_Hog)) %>%
  mutate(porcentaje = conteo_ponderado / sum(conteo_ponderado, na.rm = TRUE))

#Potable o no potable

data1_2021 %>%
  group_by(P_176) %>%
  summarise(conteo_ponderado = sum(FEH)) %>%
  mutate(porcentaje = conteo_ponderado / sum(conteo_ponderado, na.rm = TRUE))

data1_2023 %>%
  group_by(P_176) %>%
  summarise(conteo_ponderado = sum(Fexp_Hog)) %>%
  mutate(porcentaje = conteo_ponderado / sum(conteo_ponderado, na.rm = TRUE))

# Energía

data1_2021 %>%
  group_by(P_184) %>%
  summarise(conteo_ponderado = sum(FEH)) %>%
  mutate(porcentaje = conteo_ponderado / sum(conteo_ponderado, na.rm = TRUE))

data1_2023 %>%
  group_by(P_184) %>%
  summarise(conteo_ponderado = sum(Fexp_Hog)) %>%
  mutate(porcentaje = conteo_ponderado / sum(conteo_ponderado, na.rm = TRUE))

#Horas que cuenta con el servicio de energía

data1_2021 %>%
  group_by(P_185) %>%
  summarise(conteo_ponderado = sum(FEH)) %>%
  mutate(porcentaje = conteo_ponderado / sum(conteo_ponderado, na.rm = TRUE))%>% 
  print(n = 30)

data1_2023 %>%
  group_by(P_185) %>%
  summarise(conteo_ponderado = sum(Fexp_Hog)) %>%
  mutate(porcentaje = conteo_ponderado / sum(conteo_ponderado, na.rm = TRUE))%>% 
  print(n = 30)

#Calidad del servicio

data1_2021 %>%
  group_by(P_186) %>%
  summarise(conteo_ponderado = sum(FEH)) %>%
  mutate(porcentaje = conteo_ponderado / sum(conteo_ponderado, na.rm = TRUE))

data1_2023 %>%
  group_by(P_186) %>%
  summarise(conteo_ponderado = sum(Fexp_Hog)) %>%
  mutate(porcentaje = conteo_ponderado / sum(conteo_ponderado, na.rm = TRUE))

#Acueducto 

data1_2021 %>%
  group_by(P_187) %>%
  summarise(conteo_ponderado = sum(FEH)) %>%
  mutate(porcentaje = conteo_ponderado / sum(conteo_ponderado, na.rm = TRUE))

data1_2023 %>%
  group_by(P_187) %>%
  summarise(conteo_ponderado = sum(Fexp_Hog)) %>%
  mutate(porcentaje = conteo_ponderado / sum(conteo_ponderado, na.rm = TRUE))

#Calidad del servicio

data1_2021 %>%
  group_by(P_189) %>%
  summarise(conteo_ponderado = sum(FEH)) %>%
  mutate(porcentaje = conteo_ponderado / sum(conteo_ponderado, na.rm = TRUE))

data1_2023 %>%
  group_by(P_189) %>%
  summarise(conteo_ponderado = sum(Fexp_Hog)) %>%
  mutate(porcentaje = conteo_ponderado / sum(conteo_ponderado, na.rm = TRUE))

#Alcantarillado

data1_2021 %>%
  group_by(P_190) %>%
  summarise(conteo_ponderado = sum(FEH)) %>%
  mutate(porcentaje = conteo_ponderado / sum(conteo_ponderado, na.rm = TRUE))

data1_2023 %>%
  group_by(P_190) %>%
  summarise(conteo_ponderado = sum(Fexp_Hog)) %>%
  mutate(porcentaje = conteo_ponderado / sum(conteo_ponderado, na.rm = TRUE))

#Calidad del servicio

data1_2021 %>%
  group_by(P_191) %>%
  summarise(conteo_ponderado = sum(FEH)) %>%
  mutate(porcentaje = conteo_ponderado / sum(conteo_ponderado, na.rm = TRUE))

data1_2023 %>%
  group_by(P_191) %>%
  summarise(conteo_ponderado = sum(Fexp_Hog)) %>%
  mutate(porcentaje = conteo_ponderado / sum(conteo_ponderado, na.rm = TRUE))

#Gas Natural

data1_2021 %>%
  group_by(P_194) %>%
  summarise(conteo_ponderado = sum(FEH)) %>%
  mutate(porcentaje = conteo_ponderado / sum(conteo_ponderado, na.rm = TRUE))

data1_2023 %>%
  group_by(P_194) %>%
  summarise(conteo_ponderado = sum(Fexp_Hog)) %>%
  mutate(porcentaje = conteo_ponderado / sum(conteo_ponderado, na.rm = TRUE))

#Calidad del servicio

data1_2021 %>%
  group_by(P_195) %>%
  summarise(conteo_ponderado = sum(FEH)) %>%
  mutate(porcentaje = conteo_ponderado / sum(conteo_ponderado, na.rm = TRUE))

data1_2023 %>%
  group_by(P_195) %>%
  summarise(conteo_ponderado = sum(Fexp_Hog)) %>%
  mutate(porcentaje = conteo_ponderado / sum(conteo_ponderado, na.rm = TRUE))

#vive en zona de riesgos 

data1_2021 %>%
  group_by(P_204) %>%
  summarise(conteo_ponderado = sum(FEH)) %>%
  mutate(porcentaje = conteo_ponderado / sum(conteo_ponderado, na.rm = TRUE))

data1_2023 %>%
  group_by(P_204) %>%
  summarise(conteo_ponderado = sum(Fexp_Hog)) %>%
  mutate(porcentaje = conteo_ponderado / sum(conteo_ponderado, na.rm = TRUE))

#En año se ha visto afectada por alguna catástrofe
data1_2021 %>%
  group_by(P_205) %>%
  summarise(conteo_ponderado = sum(FEH)) %>%
  mutate(porcentaje = conteo_ponderado / sum(conteo_ponderado, na.rm = TRUE))

data1_2023 %>%
  group_by(P_205) %>%
  summarise(conteo_ponderado = sum(Fexp_Hog)) %>%
  mutate(porcentaje = conteo_ponderado / sum(conteo_ponderado, na.rm = TRUE))

#Percepción de calidad de vida
data1_2021 %>%
  group_by(P_347) %>%
  summarise(conteo_ponderado = sum(FEH)) %>%
  mutate(porcentaje = conteo_ponderado / sum(conteo_ponderado, na.rm = TRUE))

data1_2023 %>%
  group_by(P_347) %>%
  summarise(conteo_ponderado = sum(Fexp_Hog)) %>%
  mutate(porcentaje = conteo_ponderado / sum(conteo_ponderado, na.rm = TRUE))

mal <- c("1", "2")

data1_2021$mala_cv <- ifelse(data1_2021$P_347 %in% mal, 1, 0)
data1_2023$mala_cv <- ifelse(data1_2023$P_347 %in% mal, 1, 0)

data1_2021 %>%
  group_by(mala_cv) %>%
  summarise(
    conteo_ponderado = sum(FEH, na.rm = TRUE), 
    hogares_unicos = n_distinct(data1_2021$sHogar)) %>%
  mutate(porcentaje = conteo_ponderado / sum(conteo_ponderado, na.rm = TRUE), 
         conteo_ponderado1 = conteo_ponderado / hogares_unicos)

data1_2023 %>%
  group_by(mala_cv) %>%
  summarise(conteo_ponderado = sum(Fexp_Hog, na.rm = TRUE))%>%
  mutate(porcentaje = conteo_ponderado / sum(conteo_ponderado, na.rm = TRUE))

kable(summary(data1_2021[,c("ComunaCorr", "P_014", "P_012", "P_049", "P_173", "P_175", 
                            "P_176", "P_184", "P_185", "P_186", "P_187")]))

kable(summary(data1_2021[,c ("P_187", "P_189", "P_190", "P_191", "P_194", "P_194", "P_195", "P_204", "P_205", "P_347")]))

mpl <- lm(mala_cv  ~ factor(ComunaCorr) + P_014 + P_014 + factor(P_049) + 
  factor(P_173) + factor(P_175) + factor(P_176) + factor(P_184) + 
  factor(P_185) + factor(P_186) + factor(P_187) + factor(P_189) + 
  factor(P_190) + factor(P_191) + factor(P_194) + factor(P_195) + 
  factor(P_204) + factor(P_205), data = data1_2021)
summary(mpl)

# Modelo probit

data1_2021$P_173<- factor(data1_2021$P_173, levels = c("11", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
data1_2021$P_049<- factor(data1_2021$P_049, levels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "-98", "-99"))
data1_2021$P_172<- factor(data1_2021$P_172, levels = c("5", "1", "2", "3", "4"))
data1_2021$P_175<- factor(data1_2021$P_175, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
data1_2021$P_176<- factor(data1_2021$P_176, levels = c("1", "2"))
data1_2021$P_184<- factor(data1_2021$P_184, levels = c("4","1", "2", "3"))
data1_2021$P_187<- factor(data1_2021$P_187, levels = c("3","1", "2"))
data1_2021$P_190<- factor(data1_2021$P_190, levels = c("1", "2"))
data1_2021$P_204<- factor(data1_2021$P_204, levels = c("2", "1", "-98", "-99"))
data1_2021$P_205<- factor(data1_2021$P_205, levels = c("6", "2", "3", "4", "5", "1","-98", "-99"))


probit <- glm(mala_cv ~ P_014 + I(P_014^2) +factor(P_049) + factor(P_172) + factor(P_173) + factor(P_175) + factor(P_176) + factor(P_184) +
              factor(P_187) + factor(P_190) + factor(P_194) + factor(P_204) + factor(P_205) + factor (P_252) + P_269, data=data1_2021,
              family=binomial(link="probit")) 

probit_23 <- glm(mala_cv ~ P_014 + I(P_014^2) +factor(P_049) + factor(P_172) + factor(P_173) + factor(P_175) + factor(P_176) + factor(P_184) +
                factor(P_187) + factor(P_190) + factor(P_194) + factor(P_204) + factor(P_205) + factor (P_252) + P_269, data=data1_2023,
              family=binomial(link="probit")) 

logit <- glm(mala_cv ~ P_014 + I(P_014^2) +factor(P_049) + factor(P_172) + factor(P_173) + factor(P_175) + factor(P_176) + factor(P_184) +
                factor(P_187) + factor(P_190) + factor(P_194) + factor(P_204) + factor(P_205) + factor (P_252) + P_269, data=data1_2021,
              family=binomial(link="logi")) 

summary(probit)
summary(probit_23)

margins(probit)
summary(margins(probit))
plot(margins(probit))


# Predicciones del modelo probit
probabilidad <- predict(probit, type = "response")
probabilidad1 <- predict(probit_23, type = "response")

# Calcular la curva ROC y AUC
# Calcular la curva ROC
roc_curve <- roc(data1_2021$mala_cv, probabilidad)
roc_curve_23 <- roc(data1_2023$mala_cv, probabilidad1)

# Ver el AUC
auc(roc_curve)

plot(roc_curve, main = "Curva ROC probit 2023", col = "blue", lwd = 2)
text(0.6, 0.4, paste("AUC =", round(auc(roc_curve), 2)), col = "red", cex = 1.2)

# Ver el AUC
auc(roc_curve_23)
plot(roc_curve_23, main = "Curva ROC probit 2023", col = "blue", lwd = 2)
text(0.6, 0.4, paste("AUC =", round(auc(roc_curve_23), 2)), col = "red", cex = 1.2)



