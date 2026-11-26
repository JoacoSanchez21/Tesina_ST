# Imputación de Datos Faltantes en Series Temporales Univariadas: Evaluación Basada en Desempeño de Pronóstico

Este proyecto tiene como objetivo evaluar distintos métodos de imputación de datos faltantes en series temporales univariadas, considerando tanto la calidad de los valores imputados como su impacto en el desempeño de los modelos de pronóstico.

El estudio se desarrolla utilizando como caso de aplicación la serie mensual de demanda de energía eléctrica de Argentina. A partir de la serie original, se generan distintos escenarios de datos faltantes, considerando diferentes mecanismos de ausencia (MCAR, MAR y MNAR) y proporciones de valores faltantes.

Se comparan diversos métodos de imputación y se analiza su efecto sobre los pronósticos obtenidos mediante modelos ARIMA y ETS. Asimismo, se incorpora Prophet como modelo de referencia, ajustado directamente sobre la serie con observaciones faltantes. La evaluación contempla tanto el error de imputación como el error de pronóstico, utilizando principalmente la raíz del error cuadrático medio (RMSE).

Los archivos incluidos en este repositorio permiten reproducir el flujo de trabajo completo de la investigación, desde el análisis exploratorio de la serie y el diseño de las simulaciones hasta la evaluación y comparación de los métodos considerados.

## Metodología

El desarrollo del trabajo comprende las siguientes etapas:

1. **Análisis exploratorio:** estudio de las características de la serie temporal, incluyendo su evolución, tendencia y comportamiento estacional.
2. **Diseño de escenarios:** generación de datos faltantes bajo distintos mecanismos de ausencia y proporciones.
3. **Imputación:** aplicación y comparación de diferentes métodos para reconstruir las observaciones faltantes.
4. **Evaluación de la imputación:** medición del error entre los valores originales y los imputados.
5. **Evaluación del pronóstico:** ajuste de modelos de series temporales sobre las series imputadas y análisis de su capacidad predictiva.
6. **Comparación de resultados:** evaluación conjunta de los métodos a partir de los errores obtenidos y análisis de la relación entre la calidad de imputación y el desempeño de pronóstico.

## Estructura

```text
.
├── .gitignore
├── .Rprofile
├── README.md
├── Tesis.Rproj
├── renv.lock
│
├── Tesina.qmd
├── Tesina.pdf
│
├── Anteproyecto/
│   ├── Anteproyecto.qmd
│   ├── Anteproyecto.pdf
│   └── referencias.bib
│
├── Análisis/
│   ├── 01-Funciones.R
│   ├── 02-Ejecuciones.R
│   └── 03-Generación de gráficos.R
│
├── Datos/
│   └── demanda.csv
│
├── Resultados/
│   ├── MAR/
│   │   ├── resultados_MAR_10.rds
│   │   ├── resultados_MAR_25.rds
│   │   └── resultados_MAR_40.rds
│   │
│   ├── MCAR/
│   │   ├── resultados_MCAR_10.rds
│   │   ├── resultados_MCAR_25.rds
│   │   └── resultados_MCAR_40.rds
│   │
│   ├── MCAR_bloques/
│   │   ├── resultados_MCAR_bloques_10.rds
│   │   ├── resultados_MCAR_bloques_25.rds
│   │   └── resultados_MCAR_bloques_40.rds
│   │
│   ├── MNAR/
│   │   ├── resultados_MNAR_10.rds
│   │   ├── resultados_MNAR_25.rds
│   │   └── resultados_MNAR_40.rds
│   │
│   └── Resumenes/
│       ├── evaluacion_pronosticos.rds
│       └── ranking_spearman.rds
│
├── Figuras/
│   ├── fig_imputacion_10.png
│   ├── fig_imputacion_25.png
│   ├── fig_imputacion_40.png
│   ├── fig_spearman10.png
│   ├── fig_spearman25.png
│   ├── fig_spearman40.png
│   ├── figA_ARIMA.png
│   ├── figB_ETS.png
│   └── figC_prophet_vs_mejor.png
│
├── Referencias/
│   └── referencias.bib
│
├── Recursos/
│   └── logounr.png
│
└── renv/
```

## Instrucciones

1. **Clonar el repositorio**

   ```bash
   git clone [URL_DEL_REPOSITORIO]
   cd [Tesina_ST]
   ```

2. **Preparar el entorno de trabajo**

    Este proyecto utiliza renv para administrar las dependencias y garantizar la reproducibilidad del entorno de trabajo.

    Luego de clonar el repositorio y abrir el proyecto en RStudio, ejecutar:

    - Si no se tiene el paquete `renv`, primero debe realizarse la instalación, con `install.packages("renv")`

    - `renv::restore()`

    Este comando instala automáticamente las versiones de los paquetes registradas en el archivo renv.lock.

3. **Ejecutar el análisis**

   Ejecutar los archivos en el orden establecido en el repositorio. Es importante respetar el orden de ejecución, ya que algunas etapas utilizan objetos y resultados generados previamente.

## Reproducibilidad

Las simulaciones se realizan mediante múltiples iteraciones de generación de datos faltantes e imputación, con el propósito de obtener resultados comparables entre los distintos escenarios analizados.

Se recomienda ejecutar el proyecto utilizando las versiones de R y de los paquetes registradas en los archivos de configuración del repositorio, a fin de facilitar la reproducción de los resultados.