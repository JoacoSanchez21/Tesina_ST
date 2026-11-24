source("01-Funciones.R")

#-------------------------------------------------------------------------------
# MCAR (Disperso)
ejecutar_simulacion(
  serie_train, serie_test,
  mecanismo_na  = introducir_NA,
  prop          = 0.10,
  n_iteraciones = 100,
  resultado     = "Resultados/MCAR/resultados_MCAR_10.rds"
)

ejecutar_simulacion(
  serie_train, serie_test,
  mecanismo_na  = introducir_NA,
  prop          = 0.25,
  n_iteraciones = 100,
  resultado     = "Resultados/MCAR/resultados_MCAR_25.rds"
)

ejecutar_simulacion(
  serie_train, serie_test,
  mecanismo_na  = introducir_NA,
  prop          = 0.40,
  n_iteraciones = 100,
  resultado     = "Resultados/MCAR/resultados_MCAR_40.rds"
)

#-------------------------------------------------------------------------------
# MCAR (En bloques)

ejecutar_simulacion(
  serie_train, serie_test,
  mecanismo_na  = introducir_bloques_pct,
  prop          = 0.10,
  n_iteraciones = 100,
  resultado     = "Resultados/MCAR/resultados_MCAR_bloques_10.rds"
)

ejecutar_simulacion(
  serie_train, serie_test,
  mecanismo_na  = introducir_bloques_pct,
  prop          = 0.25,
  n_iteraciones = 100,
  resultado     = "Resultados/MCAR/resultados_MCAR_bloques_25.rds"
)

ejecutar_simulacion(
  serie_train, serie_test,
  mecanismo_na  = introducir_bloques_pct,
  prop          = 0.40,
  n_iteraciones = 100,
  resultado     = "Resultados/MCAR/resultados_MCAR_bloques_40.rds"
)

#-------------------------------------------------------------------------------
# MNAR

ejecutar_simulacion(
  serie_train, serie_test,
  mecanismo_na  = introducir_NA_MNAR,
  prop          = 0.10,
  n_iteraciones = 100,
  resultado     = "Resultados/MNAR/resultados_MNAR_10.rds"
)

ejecutar_simulacion(
  serie_train, serie_test,
  mecanismo_na  = introducir_NA_MNAR,
  prop          = 0.25,
  n_iteraciones = 100,
  resultado     = "Resultados/MNAR/resultados_MNAR_25.rds"
)

ejecutar_simulacion(
  serie_train, serie_test,
  mecanismo_na  = introducir_NA_MNAR,
  prop          = 0.40,
  n_iteraciones = 100,
  resultado     = "Resultados/MNAR/resultados_MNAR_40.rds"
)

#-------------------------------------------------------------------------------
# MAR

ejecutar_simulacion(
  serie_train, serie_test,
  mecanismo_na  = introducir_NA_MAR,
  prop          = 0.10,
  meses_altos = c(1, 7, 12),
  n_iteraciones = 100,
  resultado     = "Resultados/MAR/resultados_MAR_10.rds"
)

ejecutar_simulacion(
  serie_train, serie_test,
  mecanismo_na  = introducir_NA_MAR,
  prop          = 0.25,
  meses_altos = c(1, 7, 12),
  n_iteraciones = 100,
  resultado     = "Resultados/MAR/resultados_MAR_25.rds"
)

ejecutar_simulacion(
  serie_train, serie_test,
  mecanismo_na  = introducir_NA_MAR,
  prop          = 0.40,
  meses_altos = c(1, 7, 12),
  n_iteraciones = 100,
  resultado     = "Resultados/MAR/resultados_MAR_40.rds"
)

#-------------------------------------------------------------------------------
# Pronóstico usando la serie completa
ejecutar_pronosticos(
  serie_train = serie_train,
  serie_test = serie_test,
  modelos = c("ARIMA", "ETS", "Prophet"),
  horizontes = c(3, 6, 12),
  archivo = "Resultados/evaluacion_pronosticos.rds"
)
