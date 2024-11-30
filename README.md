Aquí tienes el resumen completo en formato **Markdown**:

```markdown
### Resumen Detallado del Código

Este código en R realiza un análisis detallado de los datos de la encuesta de calidad de vida en Medellín para los años 2021 y 2023, centrándose en la percepción de la calidad de vida a nivel de hogares. A continuación, se detalla la estructura del código, las variables utilizadas y el modelo **Probit** implementado.

#### 1. **Cargar Paquetes**  
El código comienza cargando una serie de paquetes necesarios para el análisis, la manipulación de datos, la visualización y la modelización. Algunos de los más relevantes son:
- `tidyverse`: para la manipulación de datos y visualización.
- `plm`, `lmtest`, `AER`: para la modelización de datos y pruebas de regresión.
- `margins`, `pROC`: para el análisis de márgenes y curvas ROC.
- `ggthemes`, `tmap`: para la visualización de datos geoespaciales y gráficos temáticos.

La función `pacman::p_load()` se usa para cargar estos paquetes en una sola línea.

```r
pacman::p_load(tidyverse, pder, sf, tmap, tmaptools, viridis, summarytools, terra, glue, lmtest, zoo, Hmisc, AER, skimr, modelsummary, plm, gt, table1, haven, ggthemes, wesanderson, estimatr, dplyr, sf, rgeoda, tidylog, spatialreg, spdep, readxl, DT, data.table, plotly, lubridate, shinyWidgets, shiny, openxlsx2, openxlsx, tidyr, margins, knitr, pscl, pROC)
```

#### 2. **Carga de Datos**
Se cargan los datos de las encuestas de calidad de vida para los años 2021 y 2023 utilizando la función `read_sav()` del paquete `haven`, que lee archivos `.sav` (SPSS).

```r
data_2021 <- read_sav("/path/to/ECV_ANTIOQUIA_2021.sav")
data_2023 <- read_sav("/path/to/ECV_ANTIOQUIA_2023.sav")
```

Los datos se limpian y se eliminan las observaciones duplicadas con `distinct()`.

```r
data_2021 <- data_2021 %>% distinct()
data_2023 <- data_2023 %>% distinct()
```

#### 3. **Selección y Preparación de Datos**
Después de cargar los datos, se seleccionan las columnas relevantes para cada año utilizando `select()`, con el fin de centrarse en las variables que serán necesarias para el análisis posterior.

```r
data1_2021 <- data_2021 %>% select(c(...))
data1_2023 <- data_2023 %>% select(c(...))
```

#### 4. **Limpieza de Datos**
El código filtra las filas que contienen valores `NA` en la columna `Fexp_Hog` (gastos del hogar) utilizando `filter()` y `is.na()` para eliminar esos registros.

```r
data1_2023 <- data1_2023 %>% filter(!is.na(Fexp_Hog))
```

#### 5. **Proporciones y Agrupaciones**
Se agrupan los datos por diferentes variables, como `P_252`, `P_014`, `P_173`, entre otras, para calcular las **proporciones ponderadas**. Esto se hace utilizando `group_by()` para agrupar los datos por cada categoría y `summarise()` para obtener el total ponderado con `sum(Fexp_Hog)`, seguido de `mutate()` para calcular el porcentaje.

```r
data1_2023 %>%
  group_by(P_252) %>%
  summarise(conteo_ponderado = sum(Fexp_Hog)) %>%
  mutate(porcentaje = conteo_ponderado / sum(conteo_ponderado, na.rm = TRUE)) %>% 
  arrange(desc(porcentaje)) %>% 
  print(n = 30)
```

#### 6. **Modelo Probit**
El **modelo Probit** es utilizado para predecir la probabilidad de que una variable categórica (`mala_cv`) tome los valores **0** o **1**. En este caso, **1** representa una percepción negativa de la calidad de vida. El modelo se ajusta utilizando la función `glm()` con la familia `binomial(link="probit")`.

```r
probit <- glm(mala_cv ~ P_014 + I(P_014^2) + factor(P_049) + factor(P_172) + factor(P_173) + factor(P_175) + factor(P_176) + factor(P_184) + factor(P_187) + factor(P_190) + factor(P_194) + factor(P_204) + factor(P_205) + factor(P_252) + P_269, data=data1_2021, family=binomial(link="probit"))
```

- **Variables dependientes y explicativas**: 
  - `mala_cv` es la variable dependiente que tiene los valores **0** o **1**, representando la percepción de calidad de vida negativa (1) o positiva (0).
  - Las variables explicativas incluyen el número de personas en la vivienda (`P_014`), el nivel educativo (`P_049`), y otros factores como el material de las viviendas (`P_173`), entre otras.

#### 7. **Modelo Logit**
El código también ajusta un modelo **Logit** para hacer una comparación con el modelo Probit y evaluar cuál es más adecuado para los datos.

```r
logit <- glm(mala_cv ~ P_014 + I(P_014^2) + factor(P_049) + factor(P_172) + factor(P_173) + factor(P_175) + factor(P_176) + factor(P_184) + factor(P_187) + factor(P_190) + factor(P_194) + factor(P_204) + factor(P_205) + factor(P_252) + P_269, data=data1_2021, family=binomial(link="logit"))
```

#### 8. **Análisis de Márgenes**
Se calculan los **efectos marginales** utilizando la función `margins()` para observar cómo un cambio unitario en cada variable explicativa afecta la probabilidad de tener una mala calidad de vida.

```r
margins(probit)
summary(margins(probit))
```

#### 9. **Curva ROC y AUC**
El rendimiento del modelo se evalúa mediante la **curva ROC** y el **AUC** (Área Bajo la Curva), lo que proporciona una medida de la capacidad del modelo para discriminar entre las clases. Se utiliza la función `roc()` del paquete `pROC` para calcular la curva y el AUC.

```r
roc_curve <- roc(data1_2021$mala_cv, probabilidad)
auc(roc_curve)
plot(roc_curve, main = "Curva ROC probit 2023", col = "blue", lwd = 2)
```

#### Conclusión
Este código proporciona un análisis exhaustivo de los factores que afectan la percepción de la calidad de vida en Medellín entre los años 2021 y 2023. Se utilizan modelos **Probit** y **Logit** para predecir la probabilidad de una mala calidad de vida, y se evalúa el rendimiento de los modelos utilizando la curva **ROC** y el **AUC**. El análisis de márgenes permite interpretar cómo las variables explicativas influyen en la probabilidad de una mala percepción de calidad de vida, lo cual es crucial para las políticas públicas orientadas a mejorar el bienestar social en la ciudad.

---
```
