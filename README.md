# Mercados de carbono e índices verdes: modelos EGARCH-X y DCC-GARCH

Repositorio asociado a la tesis de **Maestría en Finanzas de la Universidad Externado de Colombia**, orientada al análisis de la relación entre los mercados de carbono y el comportamiento financiero de distintos segmentos de inversión verde.

## Objetivo

El repositorio contiene los códigos desarrollados en **R** para analizar la asociación entre los retornos de los mercados de carbono y:

1. la volatilidad condicional de índices verdes, mediante modelos **EGARCH-X(1,1)**; y  
2. la evolución temporal de las correlaciones entre ambos mercados, mediante un modelo **DCC-GARCH**.

La investigación utiliza información diaria correspondiente al periodo comprendido entre el **3 de enero de 2018 y el 31 de mayo de 2024**.

## Mercados de carbono analizados

Se consideran cuatro series de mercados de carbono:

- **EU ETS** — European Union Emissions Trading System (`EEXXT3PA Index`)
- **Guangdong** (`CNNCGDEP Index`)
- **Hubei** (`CNNCHBEP Index`)
- **Shenzhen** (`CNNCSZEP Index`)

## Índices verdes analizados

Se utilizan siete índices bursátiles asociados con distintos segmentos de energías limpias y tecnologías vinculadas con la transición energética:

- **ECO**
- **GRNBIO**
- **GRNREG**
- **GRNGEO**
- **GRNFUEL**
- **GRNSOLAR**
- **GRNWIND**

Estos índices representan el **desempeño financiero** de las empresas que los componen y no constituyen medidas directas de desempeño ambiental, reducción de emisiones o impacto climático.

## Estructura del repositorio

```text
tesis-mercados-carbono-indices-verdes/
│
├── README.md
│
├── R/
│   ├── 01_preparacion_datos.R
│   ├── 02_modelo_EGARCH_X.R
│   ├── 03_modelo_DCC_GARCH.R
│   └── 04_diagnosticos_y_graficos.R
│
├── data/
│   └── README.md
│
└── output/
    └── README.md
```

### `01_preparacion_datos.R`

Realiza la preparación de la información utilizada en los modelos:

- carga de las series;
- organización de fechas;
- selección de los cuatro mercados de carbono y los siete índices verdes;
- cálculo de retornos logarítmicos diarios;
- generación de los objetos utilizados posteriormente en EGARCH-X y DCC-GARCH;
- prueba de Dickey-Fuller aumentada (ADF).

### `02_modelo_EGARCH_X.R`

Estima un modelo **EGARCH-X(1,1)** para cada uno de los siete índices verdes.

Los retornos diarios de EU ETS, Guangdong, Hubei y Shenzhen se incorporan como variables exógenas contemporáneas en la ecuación de varianza.

El script incluye:

- estimación de los siete modelos EGARCH-X;
- extracción de coeficientes y p-valores;
- diagnósticos sobre residuos estandarizados;
- ajuste de los 28 contrastes mediante el procedimiento de **Benjamini-Hochberg (FDR)**;
- exportación de resultados.

La utilización de \(x_{k,t}\) responde al carácter contemporáneo del análisis: los coeficientes deben interpretarse como asociaciones del mismo periodo y no como relaciones predictivas o causales.

### `03_modelo_DCC_GARCH.R`

Estima la estructura multivariada utilizada para analizar las correlaciones condicionales dinámicas entre los mercados de carbono y los índices verdes.

El script incluye:

- estimación de modelos GARCH(1,1) univariados;
- comparación entre las especificaciones **DCC-MVN** y **DCC-MVT**;
- selección de la distribución multivariada;
- extracción de los parámetros DCC;
- evaluación de la persistencia;
- construcción de las **28 combinaciones carbono-verde**;
- cálculo de correlaciones promedio;
- análisis por subperiodos.

Los subperiodos considerados son:

- 2018-2019
- COVID-2020
- 2021-2022
- 2023-2024

### `04_diagnosticos_y_graficos.R`

Contiene los diagnósticos complementarios y la generación de figuras utilizadas para apoyar la interpretación de los resultados.

Se incluyen:

- Ljung-Box sobre residuos estandarizados \(z_t\);
- Ljung-Box sobre \(z_t^2\);
- prueba ARCH-LM;
- gráficos de correlaciones dinámicas;
- mapas de calor de las correlaciones promedio por subperiodo.

## Datos

Las series de precios utilizadas en la investigación fueron obtenidas de **Bloomberg** y expresadas en dólares estadounidenses.

Debido a las condiciones de licencia y redistribución de la fuente, **los datos originales de Bloomberg no se publican en este repositorio**.

Para reproducir las estimaciones, el usuario debe disponer de un archivo con la misma estructura de variables indicada en `data/README.md`.

## Orden de ejecución

Los scripts deben ejecutarse en el siguiente orden:

```text
1. R/01_preparacion_datos.R
2. R/02_modelo_EGARCH_X.R
3. R/03_modelo_DCC_GARCH.R
4. R/04_diagnosticos_y_graficos.R
```

Los resultados generados se almacenan en la carpeta:

```text
output/
```

## Principales paquetes de R

Entre los paquetes utilizados se encuentran:

```r
readxl
dplyr
tidyr
xts
rugarch
rmgarch
tseries
FinTS
ggplot2
openxlsx
writexl
reshape2
```

## Consideraciones metodológicas

Los modelos estimados permiten analizar asociaciones financieras y dependencia dinámica entre los mercados estudiados.

Los resultados:

- no permiten establecer causalidad;
- no demuestran una dirección de transmisión entre mercados;
- no constituyen una medida integral de riesgo climático;
- no implican que una correlación positiva represente integración elevada;
- deben interpretarse teniendo en cuenta las diferencias institucionales, regionales y sectoriales de los mercados analizados.

La comparación entre el EU ETS y los mercados piloto chinos tiene un propósito comparativo y no supone que sus derechos de emisión sean intercambiables ni que exista integración regulatoria directa entre ambas jurisdicciones.

## Reproducibilidad

Los códigos publicados corresponden a las especificaciones utilizadas en la versión final de la investigación y permiten reproducir las estimaciones, diagnósticos y principales resultados econométricos a partir de la base de datos original.

## Autor

**Hernán Darío Rodríguez Martínez**  
Maestría en Finanzas  
Universidad Externado de Colombia

## Uso académico

Este repositorio se publica con fines de transparencia, trazabilidad y reproducibilidad académica de los procedimientos econométricos utilizados en la investigación.
