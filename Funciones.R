Sys.setenv(TZ = "GMT") 


library(dplyr)
library(fable)
library(fpp3)
library(forecast)
library(ggplot2)
library(ggfortify)
library(gridExtra)
library(gt)
library(imputeTS)
library(knitr)
library(lubridate)
library(prophet)
library(readr)
library(tidyr)
library(tidyverse)
library(tseries)
library(tsibble)
library(tsibbledata)
library(zoo)

set.seed(123)

datos <- read_csv("demanda.csv", show_col_types = FALSE) %>%
  mutate(fecha = ymd(fecha),
         anio  = year(fecha),
         mes  = month(fecha),
         demanda = demanda/1000)
vals <- datos$demanda

serie_completa <- ts(vals, 
         start = c(2001, 1), 
         frequency = 12)

serie <- window(serie_completa, end = c(2024, 12))
serie_train <- window(serie,start=c(2010,1), end = c(2024, 12))
serie_test <- window(serie_completa, start = c(2025, 1)) 


#-------------------------------------------------------------------------------
#MCAR
introducir_NA <- function(y, prop = 0.1) {
  y_na <- y
  n <- length(y)
  idx <- sample(2:(n - 1), size = round(prop * n))
  y_na[idx] <- NA
  return(y_na)
}

#-------------------------------------------------------------------------------
# En bloques

introducir_bloques_pct <- function(y, prop = 0.1, tam_min = 2, tam_max = 10) {
  y_na <- y
  n <- length(y)
  total_na <- round(prop * n)
  na_actual <- 0
  while (na_actual < total_na) {
    tam_bloque <- sample(tam_min:tam_max, 1)
    if (na_actual + tam_bloque > total_na) {
      tam_bloque <- total_na - na_actual
    }
    inicio <- sample(2:(n - tam_bloque), 1)
    indices <- inicio:(inicio + tam_bloque - 1)
    nuevos_na <- sum(!is.na(y_na[indices]))
    y_na[indices] <- NA
    na_actual <- na_actual + nuevos_na
  }
  return(y_na)
}

#-------------------------------------------------------------------------------
# MAR temporal
introducir_NA_MAR <- function(y, prop = 0.1, razon = 2, meses_altos) {
  y_na <- y
  n    <- length(y)
  int  <- 2:(n - 1)
  mes  <- as.integer(cycle(y))
  w    <- ifelse(mes[int] %in% meses_altos, razon, 1)
  idx  <- sample(int, size = floor(prop * n), prob = w)
  y_na[idx] <- NA
  return(y_na)
}

#-------------------------------------------------------------------------------
# MNAR
# beta controla la intensidad (0 = MCAR).
introducir_NA_MNAR <- function(y, prop = 0.1, beta = 1) {
  y_na <- y
  n    <- length(y)
  int  <- 2:(n - 1)
  z    <- as.numeric(scale(as.numeric(y)))[int]
  idx  <- sample(int, size = floor(prop * n), prob = exp(beta * z))
  y_na[idx] <- NA
  return(y_na)
}

#-------------------------------------------------------------------------------
# MEDIDAS DEL ERROR
# 1) Métricas de imputación
metricas_imputacion <- function(serie_real, serie_imp, indices_na) {
  error <- serie_imp[indices_na] - serie_real[indices_na]
  data.frame(
    RMSE = sqrt(mean(error^2, na.rm = TRUE)),
    MAE  = mean(abs(error), na.rm = TRUE),
    MAPE = mean(abs(error) / pmax(abs(serie_real[indices_na]), 1e-8) * 100, na.rm = TRUE)
  )
}

# 2) Ajuste y pronóstico
ajustar_pronostico <- function(serie, modelo = c("ARIMA", "ETS"), h = 1) {
  modelo <- match.arg(modelo)
  if (modelo == "ARIMA") {
    forecast(auto.arima(serie), h = h)
  } else {
    forecast(ets(serie), h = h)
  }
}

# 3) Métricas de pronóstico
metricas_pronostico <- function(pronostico, serie_test) {
  acc <- accuracy(pronostico, serie_test)
  data.frame(
    RMSE = acc["Test set", "RMSE"],
    MAE  = acc["Test set", "MAE"],
    MAPE = acc["Test set", "MAPE"]
  )
}

# 4) Imputadores
imputadores <- list(
  `Valor aleatorio`                              = function(x) na_random(x),
  `Valor definido`                               = function(x) na_replace(x, fill = 10),
  `Valor medio`                                  = function(x) na_mean(x, option = "mean"),
  `Valor mediano`                                = function(x) na_mean(x, option = "median"),
  `Valor modal`                                  = function(x) na_mean(x, option = "mode"),
  `Interpolación lineal`                         = function(x) na_interpolation(x, option = "linear"),
  `Interpolación spline`                         = function(x) na_interpolation(x, option = "spline"),
  `Interpolación de Stineman`                    = function(x) na_interpolation(x, option = "stine"),
  `Media móvil simple`                           = function(x) na_ma(x, weighting = "simple"),
  `Media móvil lineal ponderada`                 = function(x) na_ma(x, weighting = "linear"),
  `Media móvil ponderada exponencial`            = function(x) na_ma(x, weighting = "exponential"),
  `Última observación trasladada hacia adelante` = function(x) na_locf(x, option = "locf"),
  `Siguiente observación trasladada hacia atrás` = function(x) na_locf(x, option = "nocb"),
  `Descomposición estacional`                    = function(x) na_seadec(x, algorithm = "interpolation"),
  `División estacional`                          = function(x) na_seasplit(x),
  `Suavizado de Kalman en un modelo estructural` = function(x) na_kalman(x, model = "StructTS"),
  `Representación del espacio en estados de ARIMA y suavizado de Kalman` = function(x) na_kalman(x, model = "auto.arima")
)

#-------------------------------------------------------------------------------
# Función principal
ejecutar_simulacion <- function(
    serie_train, serie_test, mecanismo_na, prop,
    horizontes = c(3, 6, 12),
    n_iteraciones = 100, resultado,
    semilla = 123,
    ...
) {
  
  horizontes <- sort(unique(as.integer(horizontes)))
  h_max <- max(horizontes)
  stopifnot(length(serie_test) >= h_max)
  
  reales <- as.numeric(serie_test)[1:h_max]
  
  resultados_imp <- list()
  resultados_pro <- list()
  
  inicio_train <- start(serie_train)
  inicio_test  <- start(serie_test)
  
  fecha_inicio_train <- as.Date(sprintf("%04d-%02d-01", inicio_train[1], inicio_train[2]))
  fecha_inicio_test  <- as.Date(sprintf("%04d-%02d-01", inicio_test[1],  inicio_test[2]))
  
  fechas_train <- seq.Date(fecha_inicio_train, by = "month", length.out = length(serie_train))
  fechas_test  <- seq.Date(fecha_inicio_test,  by = "month", length.out = h_max)
  
  calcular_metricas <- function(pronosticos) {
    bind_rows(lapply(horizontes, function(h) {
      error <- reales[1:h] - pronosticos[1:h]
      data.frame(
        Horizonte = h,
        RMSE_Pro  = sqrt(mean(error^2)),
        MAE_Pro   = mean(abs(error)),
        MAPE_Pro  = mean(abs(error) / pmax(abs(reales[1:h]), 1e-8) * 100)
      )
    }))
  }
  
  for (i in seq_len(n_iteraciones)) {
    
    cat("Iteración", i, "de", n_iteraciones, "...\n")
    
    set.seed(semilla + i)
    
    serie_NA   <- mecanismo_na(serie_train, prop = prop, ...)
    indices_na <- which(is.na(serie_NA))
    
    resultado_prophet <- tryCatch({
      
      datos_prophet <- data.frame(ds = fechas_train, y = as.numeric(serie_NA))
      
      modelo_prophet <- prophet::prophet(
        df = datos_prophet,
        seasonality.mode   = "multiplicative",
        yearly.seasonality = TRUE,
        weekly.seasonality = FALSE,
        daily.seasonality  = FALSE,
        uncertainty.samples = 0             # CAMBIO
      )
      
      pred <- predict(modelo_prophet, data.frame(ds = fechas_test))
      calcular_metricas(as.numeric(pred$yhat))
      
    }, error = function(e) {
      message("Error en Prophet, iteración ", i, ": ", e$message)
      NULL
    })
    
    if (!is.null(resultado_prophet)) {
      resultados_pro[[length(resultados_pro) + 1]] <-
        cbind(data.frame(Iteracion = i, Metodo = "Sin imputación", Modelo = "Prophet"),
              resultado_prophet)
    }
    
    # --- Imputación y modelos ---
    for (nombre in names(imputadores)) {
      
      serie_imp <- tryCatch({
        
        x <- imputadores[[nombre]](serie_NA)
        
        if (length(x) != length(serie_train) || anyNA(x)) {
          stop("Imputación incompleta o longitud incorrecta.")
        }
        
        ts(as.numeric(x), start = start(serie_train), frequency = frequency(serie_train))
        
      }, error = function(e) {
        message("Error en imputación ", nombre, ", iteración ", i, ": ", e$message)
        NULL
      })
      
      if (is.null(serie_imp)) next
      
      met_imp <- metricas_imputacion(serie_train, serie_imp, indices_na)
      
      resultados_imp[[length(resultados_imp) + 1]] <-
        data.frame(
          Iteracion = i,
          Metodo    = nombre,
          RMSE_Imp  = met_imp$RMSE,
          MAE_Imp   = met_imp$MAE,
          MAPE_Imp  = met_imp$MAPE
        )
      
      for (modelo in c("ARIMA", "ETS")) {
        
        resultado_modelo <- tryCatch({
          fc <- ajustar_pronostico(serie_imp, modelo = modelo, h = h_max)   # CAMBIO: h_max
          calcular_metricas(as.numeric(fc$mean))
        }, error = function(e) {
          message("Error en ", modelo, ", método ", nombre, ", iteración ", i, ": ", e$message)
          NULL
        })
        
        if (!is.null(resultado_modelo)) {
          resultados_pro[[length(resultados_pro) + 1]] <-
            cbind(data.frame(Iteracion = i, Metodo = nombre, Modelo = modelo),
                  resultado_modelo)
        }
      }
    }
  }
  
  metricas_imp <- bind_rows(resultados_imp)
  metricas_pro <- bind_rows(resultados_pro)
  
  resumen_imp <- metricas_imp %>%
    group_by(Metodo) %>%
    summarise(
      N_Iteraciones = n(),
      across(c(RMSE_Imp, MAE_Imp, MAPE_Imp), mean),
      .groups = "drop"
    )
  
  resumen_pro <- metricas_pro %>%
    group_by(Metodo, Modelo, Horizonte) %>%
    summarise(
      N_Iteraciones = n(),
      across(c(RMSE_Pro, MAE_Pro, MAPE_Pro), mean),
      .groups = "drop"
    )
  
  resultados_finales <- list(
    Metricas_Imputacion = metricas_imp,
    Metricas_Pronostico = metricas_pro,
    Resumen_Imputacion  = resumen_imp,
    Resumen_Pronostico  = resumen_pro,
    Meta = list(prop = prop, horizontes = horizontes, n_iteraciones = n_iteraciones)
  )
  
  dir.create(dirname(resultado), recursive = TRUE, showWarnings = FALSE)
  saveRDS(resultados_finales, file = resultado)
}

#-------------------------------------------------------------------------------
# Función para calcular pronósticos usando la serie sin faltantes
ejecutar_pronosticos <- function(
    serie_train,
    serie_test,
    modelos = c("ARIMA", "ETS", "Prophet"),
    horizontes = c(3, 6, 12),
    archivo = "Resultados/evaluacion_pronosticos.rds"
) {
  
  if (!is.ts(serie_train) || !is.ts(serie_test)) {
    stop("Las series deben ser objetos ts.")
  }
  
  if (frequency(serie_train) != 12 ||
      frequency(serie_test) != 12) {
    stop("Las series deben tener frecuencia mensual.")
  }
  
  if (NCOL(serie_train) != 1 || NCOL(serie_test) != 1) {
    stop("Las series deben ser univariadas.")
  }
  
  if (anyNA(serie_train) || anyNA(serie_test)) {
    stop("Las series no deben contener valores faltantes.")
  }
  
  indice_fin_train <- start(serie_train)[1] * 12 +
    start(serie_train)[2] - 1 + length(serie_train) - 1
  
  indice_inicio_test <- start(serie_test)[1] * 12 +
    start(serie_test)[2] - 1
  
  if (indice_inicio_test != indice_fin_train + 1) {
    stop("serie_test debe comenzar inmediatamente después de serie_train.")
  }
  
  horizontes <- sort(unique(as.integer(horizontes)))
  
  if (length(horizontes) == 0 ||
      anyNA(horizontes) ||
      any(horizontes < 1)) {
    stop("Los horizontes deben ser enteros positivos.")
  }
  
  h_max <- max(horizontes)
  
  if (length(serie_test) < h_max) {
    stop("serie_test no tiene suficientes observaciones para el horizonte máximo.")
  }
  
  modelos <- match.arg(
    modelos,
    choices = c("ARIMA", "ETS", "Prophet"),
    several.ok = TRUE
  )
  
  inicio_train <- start(serie_train)
  
  fecha_inicio_train <- as.Date(
    sprintf("%04d-%02d-01",
            inicio_train[1], inicio_train[2])
  )
  
  fechas_train <- seq.Date(
    from = fecha_inicio_train,
    by = "month",
    length.out = length(serie_train)
  )
  
  inicio_test <- start(serie_test)
  
  fecha_inicio_test <- as.Date(
    sprintf("%04d-%02d-01",
            inicio_test[1], inicio_test[2])
  )
  
  fechas_test <- seq.Date(
    from = fecha_inicio_test,
    by = "month",
    length.out = h_max
  )
  
  lista_metricas <- list()
  
  for (modelo in modelos) {
    
    if (modelo == "ARIMA") {
      
      ajuste <- forecast::auto.arima(serie_train)
      
      fc <- forecast::forecast(
        ajuste,
        h = h_max
      )
      
      pronosticos <- as.numeric(fc$mean)
    }
    
    if (modelo == "ETS") {
      
      ajuste <- forecast::ets(serie_train)
      
      fc <- forecast::forecast(
        ajuste,
        h = h_max
      )
      
      pronosticos <- as.numeric(fc$mean)
    }
    
    if (modelo == "Prophet") {
      
      datos_prophet <- data.frame(
        ds = fechas_train,
        y = as.numeric(serie_train)
      )
      
      ajuste <- prophet::prophet(
        df = datos_prophet,
        seasonality.mode = "multiplicative",
        yearly.seasonality = TRUE,
        weekly.seasonality = FALSE,
        daily.seasonality = FALSE
      )
      
      futuro <- data.frame(ds = fechas_test)
      
      pred <- predict(ajuste, futuro)
      
      pronosticos <- as.numeric(pred$yhat)
    }
    
    reales <- as.numeric(serie_test)[1:h_max]
    
    metricas <- do.call(
      rbind,
      lapply(horizontes, function(h) {
        
        error <- reales[1:h] - pronosticos[1:h]
        
        data.frame(
          Modelo = modelo,
          Horizonte = h,
          RMSE = sqrt(mean(error^2)),
          MAE = mean(abs(error)),
          MAPE = mean(
            abs(error) /
              pmax(abs(reales[1:h]), 1e-8) * 100
          )
        )
      })
    )
    
    lista_metricas[[modelo]] <- metricas
  }
  
  resultados_finales <- do.call(rbind, lista_metricas)
  
  rownames(resultados_finales) <- NULL
  
  saveRDS(resultados_finales, file = archivo)
  
  return(resultados_finales)
}

