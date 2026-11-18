
library(tidyverse)
library(scales)

dir_res <- "Resultados"
dir_fig <- "Figuras"

etiquetas_mec <- c(
  MCAR         = "MCAR\n(disperso)",
  MCAR_bloques = "MCAR\n(en bloques)",
  bloques      = "MCAR\n(en bloques)",
  MAR          = "MAR",
  MNAR         = "MNAR"
)
orden_mec <- c("MCAR\n(disperso)", "MCAR\n(en bloques)", "MAR", "MNAR")

archivos <- list.files(dir_res, pattern = "^resultados_.*\\.rds$",
                       recursive = TRUE, full.names = TRUE)
#-------------------------------------------------------------------------------
#Gráficos de calor para evaluar calidad de imputación
lim_color    <- c(0.4, 2.0)
umbral_texto <- 1.7

etiq_metodo <- c(
  "Suavizado de Kalman en un modelo estructural"                         = "Kalman estructural",
  "Representación del espacio en estados de ARIMA y suavizado de Kalman" = "Kalman-ARIMA",
  "División estacional"                                                  = "División estacional",
  "Descomposición estacional"                                            = "Descomp. estacional",
  "Interpolación lineal"                                                 = "Interp. lineal",
  "Interpolación de Stineman"                                            = "Interp. Stineman",
  "Interpolación spline"                                                 = "Interp. spline",
  "Media móvil ponderada exponencial"                                    = "Media móvil exponencial",
  "Media móvil lineal ponderada"                                         = "Media móvil lineal",
  "Media móvil simple"                                                   = "Media móvil simple",
  "Última observación trasladada hacia adelante"                         = "LOCF",
  "Siguiente observación trasladada hacia atrás"                         = "NOCB"
)   

orden_fam <- c("Estructurales", "Interpolación", "Medias móviles",
               "Traslado de observaciones", "Valores globales")
familia_de <- c(
  "Kalman estructural" = "Estructurales", "Kalman-ARIMA" = "Estructurales",
  "División estacional" = "Estructurales", "Descomp. estacional" = "Estructurales",
  "Interp. lineal" = "Interpolación", "Interp. Stineman" = "Interpolación",
  "Interp. spline" = "Interpolación",
  "Media móvil exponencial" = "Medias móviles", "Media móvil lineal" = "Medias móviles",
  "Media móvil simple" = "Medias móviles",
  "LOCF" = "Traslado de observaciones", "NOCB" = "Traslado de observaciones",
  "Valor medio" = "Valores globales", "Valor mediano" = "Valores globales",
  "Valor modal" = "Valores globales", "Valor aleatorio" = "Valores globales",
  "Valor definido" = "Valores globales"
)

leer_escenario <- function(f) {
  m <- str_match(basename(f), "^resultados_(.+)_(\\d+)\\.rds$")
  if (is.na(m[1, 1])) return(NULL)
  r   <- readRDS(f)
  pct <- as.integer(m[1, 3])
  
  if (!is.null(r$Meta$prop) && round(r$Meta$prop * 100) != pct) {
    warning("El porcentaje del nombre no coincide con Meta$prop en ", basename(f))
  }
  
  as_tibble(r$Resumen_Imputacion) %>%
    transmute(mec_cod = m[1, 2], pct = pct, Metodo = as.character(Metodo),
              N_Iteraciones, RMSE_Imp)
}

imp <- map(archivos, leer_escenario) %>% bind_rows() %>%
  mutate(
    mecanismo = coalesce(unname(etiquetas_mec[mec_cod]), mec_cod),
    mecanismo = factor(mecanismo, levels = unique(c(orden_mec, mecanismo)))
  )

imp <- imp %>%
  mutate(
    metodo_corto = coalesce(unname(etiq_metodo[Metodo]), Metodo),
    familia      = factor(coalesce(unname(familia_de[metodo_corto]), "Otros"),
                          levels = c(orden_fam, "Otros"))
  )

orden_met <- imp %>%
  group_by(familia, metodo_corto) %>%
  summarise(m = mean(RMSE_Imp), .groups = "drop") %>%
  arrange(familia, m)
imp <- imp %>% mutate(metodo_corto = factor(metodo_corto, levels = rev(orden_met$metodo_corto)))

fmt <- function(x) formatC(x, format = "f", digits = 3, decimal.mark = ",")

mapa_imputacion <- function(p) {
  d <- filter(imp, pct == p)
  mejores <- d %>%
    group_by(mecanismo) %>%
    filter(round(RMSE_Imp, 3) == min(round(RMSE_Imp, 3))) %>%
    ungroup()
  
  ggplot(d, aes(mecanismo, metodo_corto, fill = RMSE_Imp)) +
    geom_tile(colour = "white", linewidth = 0.6) +
    geom_tile(data = mejores, fill = NA, colour = "black", linewidth = 1) +
    geom_text(aes(label = fmt(RMSE_Imp), colour = RMSE_Imp > umbral_texto), size = 3) +
    scale_colour_manual(values = c("FALSE" = "black", "TRUE" = "white"), guide = "none") +
    scale_fill_distiller(
      palette = "YlOrRd", direction = 1, limits = lim_color, oob = squish,
      breaks = c(0.5, 1, 1.5, 2), labels = c("0,5", "1,0", "1,5", "\u2265 2,0"),
      name = "RMSE de\nimputación"
    ) +
    facet_grid(familia ~ ., scales = "free_y", space = "free_y") +
    scale_x_discrete(position = "top") +
    labs(x = NULL, y = NULL) +
    theme_minimal(base_size = 10) +
    theme(
      panel.grid      = element_blank(),
      strip.text.y    = element_text(angle = 0, hjust = 0, face = "bold"),
      axis.text.x     = element_text(face = "bold"),
      plot.caption    = element_text(hjust = 0, size = 8),
      legend.position = "right"
    )
}

dir.create(dir_fig, showWarnings = FALSE)
for (p in c(10, 25, 40)) {
  ggsave(file.path(dir_fig, sprintf("fig_imputacion_%02d.png", p)),
         mapa_imputacion(p), width = 7.5, height = 6.5, dpi = 300)
}

#-------------------------------------------------------------------------------
#Gráficos para evaluar desempeño del pronóstico
archivo_base <- file.path(dir_res, "evaluacion_pronosticos.rds")

etiq_metodo <- c(
  "Descomposición estacional"                                            = "Descomp. estacional",
  "División estacional"                                                  = "División estacional",
  "Suavizado de Kalman en un modelo estructural"                         = "Kalman estructural",
  "Representación del espacio en estados de ARIMA y suavizado de Kalman" = "Kalman-ARIMA",
  "Última observación trasladada hacia adelante"                         = "LOCF"
)
pal <- c(
  "Descomp. estacional"   = "#1b9e77",
  "División estacional"   = "#d95f02",
  "Kalman estructural"    = "#7570b3",
  "Kalman-ARIMA"          = "#e7298a",
  "LOCF"                  = "#66a61e",
  "Prophet (sin imputar)" = "black"
)

y_graf <- c(0.5, 1.7)    

leer_escenario <- function(f) {
  m <- str_match(basename(f), "^resultados_(.+)_(\\d+)\\.rds$")
  if (is.na(m[1, 1])) return(NULL)
  r <- readRDS(f)
  as_tibble(r$Resumen_Pronostico) %>%
    transmute(mec_cod = m[1, 2], pct = as.integer(m[1, 3]),
              Metodo = as.character(Metodo), modelo = as.character(Modelo),
              h = as.integer(Horizonte), N_Iteraciones, RMSE_Pro)
}

res <- map(archivos, leer_escenario) %>% bind_rows()

b <- readRDS(archivo_base)
if (!is.data.frame(b)) b <- b$Metricas
base <- as_tibble(b) %>%
  transmute(modelo = recode(toupper(Modelo), ARIMA = "ARIMA", ETS = "ETS", PROPHET = "Prophet"),
            h = as.integer(Horizonte), RMSE_base = RMSE)

res <- res %>%
  mutate(
    mecanismo = coalesce(unname(etiquetas_mec[mec_cod]), mec_cod),
    mecanismo = factor(mecanismo, levels = unique(c(orden_mec, mecanismo))),
    
    metodo_corto = if_else(
      modelo == "Prophet", "Prophet (sin imputar)",
      coalesce(unname(etiq_metodo[Metodo]), Metodo)
    ),
    
    destacado = metodo_corto %in% names(pal),
    
    h_lab = factor(
      paste0("h = ", h),
      levels = paste0("h = ", sort(unique(h)))
    ),
    
    pct_lab = factor(
      paste0(pct, " %"),
      levels = paste0(sort(unique(pct)), " %")
    )
  ) %>%
  left_join(base, by = c("modelo", "h")) %>%
  mutate(
    razon = RMSE_Pro / RMSE_base
  )

tema <- theme_bw(base_size = 10) +
  theme(legend.position  = "bottom",
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "grey92"),
        strip.text.y     = element_text(angle = 0))

escala_x <- scale_x_continuous(breaks = c(10, 25, 40), labels = function(x) paste0(x, " %"))
coma     <- label_number(decimal.mark = ",", accuracy = 0.01)

fig <- function(mod) {
  
  d <- res %>%
    filter(
      modelo %in% c(mod, "Prophet"),
      is.finite(RMSE_Pro)
    )
  
  etiqueta_base <- paste("Pronóstico con la serie completa")
  
  base_mod <- base %>%
    filter(modelo == mod) %>%
    mutate(serie_leyenda = etiqueta_base)
  
  color_mod <- if (mod == "ARIMA") "#1f78b4" else "#e31a1c"
  
  pal_fig <- c(
    pal,
    setNames(color_mod, etiqueta_base)
  )
  
  ggplot(d, aes(h, group = metodo_corto)) +
    
    geom_line(
      data = base_mod,
      aes(
        x = h, y = RMSE_base,
        colour = serie_leyenda,
        group = 1
      ),
      inherit.aes = FALSE,
      linetype = "dashed",
      linewidth = 0.5
    ) +
    
    geom_point(
      data = base_mod,
      aes(x = h, y = RMSE_base, colour = serie_leyenda),
      inherit.aes = FALSE,
      shape = 1,
      size = 1.6,
      show.legend = FALSE
    ) +
    
    geom_line(
      data = filter(d, !destacado),
      aes(y = RMSE_Pro),
      colour = "grey75",
      linewidth = 0.4
    ) +

    geom_line(
      data = filter(d, destacado),
      aes(y = RMSE_Pro, colour = metodo_corto),
      linewidth = 0.9
    ) +
    
    geom_point(
      data = filter(d, destacado),
      aes(y = RMSE_Pro, colour = metodo_corto),
      size = 1.6
    ) +
    
    facet_grid(mecanismo ~ pct_lab) +
    
    scale_x_continuous(
      breaks = sort(unique(d$h)
      )) +
    
    scale_y_continuous(labels = coma) +
    
    scale_colour_manual(
      values = pal_fig,
      breaks = c(names(pal), etiqueta_base)
    ) +
    
    labs(
      x = "Horizonte de pronóstico (meses)",
      y = "RMSE de pronóstico",
      colour = NULL
    ) +
    
    tema +
    
    theme(
      plot.caption = element_text(
        size = 10,
        hjust = 0.5,
        lineheight = 1.2,
        margin = margin(t = 8)
      ),
      plot.caption.position = "plot"
    )
}

dir.create(dir_fig, showWarnings = FALSE)
ggsave(file.path(dir_fig, "figA_ARIMA.png"), fig("ARIMA"), width = 8.5, height = 8.5, dpi = 300)
ggsave(file.path(dir_fig, "figB_ETS.png"),   fig("ETS"),   width = 8.5, height = 8.5, dpi = 300)

mejor <- res %>%
  filter(
    modelo %in% c("ARIMA", "ETS"),
    is.finite(RMSE_Pro)
  ) %>%
  group_by(mecanismo, pct, pct_lab, modelo, h) %>%
  summarise(
    RMSE = min(RMSE_Pro),
    .groups = "drop"
  )

proph <- res %>%
  filter(
    modelo == "Prophet",
    is.finite(RMSE_Pro)
  ) %>%
  transmute(
    mecanismo, pct, pct_lab, modelo, h,
    RMSE = RMSE_Pro
  )

d_c <- bind_rows(mejor, proph)

base_line <- base %>%
  transmute(
    modelo,
    h,
    RMSE = RMSE_base
  )

gC <- ggplot(
  d_c,
  aes(h, RMSE, colour = modelo, group = modelo)
) +
  geom_line(
    aes(linetype = "Con faltantes"),
    linewidth = 0.9
  ) +
  geom_line(
    data = base_line,
    aes(
      h, RMSE,
      colour = modelo,
      linetype = "Serie completa",
      group = modelo
    ),
    inherit.aes = FALSE,
    linewidth = 0.5
  ) +
  
  geom_point(
    data = base_line,
    aes(h, RMSE, colour = modelo),
    inherit.aes = FALSE,
    shape = 1,
    size = 1.6
  ) +
  
  geom_point(size = 1.6) +
  
  facet_grid(
    mecanismo ~ pct_lab,
    scales = "free_y"
  ) +
  
  scale_x_continuous(
    breaks = sort(unique(d_c$h))
  ) +
  
  scale_y_continuous(labels = coma) +
  
  scale_colour_manual(
    values = c(
      ARIMA = "#1f78b4",
      ETS = "#e31a1c",
      Prophet = "#33a02c"
    )
  ) +
  
  labs(
    x = "Horizonte de pronóstico (meses)",
    y = "RMSE de pronóstico",
    colour = NULL,
    linetype = NULL
  ) +
  
  tema +
  
  theme(
    plot.caption = element_text(
      size = 10,
      hjust = 0.5,
      lineheight = 1.2
    ),
    plot.caption.position = "plot"
  )

ggsave(file.path(dir_fig, "figC_prophet_vs_mejor.png"), gC, width = 8.5, height = 8.5, dpi = 300)

#-------------------------------------------------------------------------------
# Coeficientes de Spearman

leer_escenario <- function(f) {
  m <- str_match(basename(f), "^resultados_(.+)_(\\d+)\\.rds$")
  if (is.na(m[1, 1])) return(NULL)
  
  r <- readRDS(f)
  
  imp <- as_tibble(r$Resumen_Imputacion) %>%
    transmute(Metodo = as.character(Metodo), RMSE_Imp)
  
  pro <- as_tibble(r$Resumen_Pronostico) %>%
    filter(Modelo %in% c("ARIMA", "ETS")) %>%
    transmute(Metodo = as.character(Metodo), modelo = as.character(Modelo),
              h = as.integer(Horizonte), RMSE_Pro)
  
  inner_join(imp, pro, by = "Metodo") %>%
    mutate(mec_cod = m[1, 2], pct = as.integer(m[1, 3]), .before = 1)
}

res <- map(archivos , leer_escenario) %>% bind_rows()%>%
  mutate(
    mecanismo = coalesce(unname(etiquetas_mec[mec_cod]), mec_cod),
    mecanismo = factor(mecanismo, levels = unique(c(orden_mec, mecanismo)))
  )

rankings <- res %>%
  group_by(mecanismo, pct, modelo, h) %>%
  mutate(
    rango_imp = rank(RMSE_Imp, ties.method = "average"),
    rango_pro = rank(RMSE_Pro, ties.method = "average")
  ) %>%
  ungroup() %>%
  arrange(mecanismo, pct, modelo, h, rango_imp) %>%
  select(mecanismo, pct, modelo, h, Metodo, RMSE_Imp, rango_imp, RMSE_Pro, rango_pro)

tops <- rankings %>%
  group_by(mecanismo, pct, modelo, h) %>%
  summarise(
    n_metodos      = n(),
    top_imputacion = list(Metodo[order(RMSE_Imp)]),
    top_pronostico = list(Metodo[order(RMSE_Pro)]),
    .groups = "drop"
  )

spearman <- rankings %>%
  group_by(mecanismo, pct, modelo, h) %>%
  summarise(
    n_metodos = n(),
    rho       = cor(rango_imp, rango_pro, method = "spearman"),
    mejor_imp = Metodo[which.min(RMSE_Imp)],
    mejor_pro = Metodo[which.min(RMSE_Pro)],
    .groups = "drop"
  )

saveRDS(list(rankings = rankings, tops = tops, spearman = spearman),
        file.path(dir_res, "rankings_spearman.rds"))

mean(spearman$rho)

lim_rho      <- c(0, 1)   
umbral_texto <- 0.83
digitos      <- 3

sp <- readRDS(file.path(dir_res, "rankings_spearman.rds"))$spearman

sp <- sp %>%
  mutate(
    mecanismo = coalesce(unname(etiquetas_mec[as.character(mecanismo)]), as.character(mecanismo)),
    mecanismo = factor(mecanismo, levels = rev(unique(c(orden_mec, mecanismo)))),
    modelo    = factor(modelo, levels = c("ARIMA", "ETS")),
    h_lab     = factor(paste0("h = ", h), levels = paste0("h = ", sort(unique(h))))
  )

fmt <- function(x) formatC(x, format = "f", digits = digitos, decimal.mark = ",")

mapa_spearman <- function(p) {
  d <- filter(sp, pct == p)
  
  ggplot(d, aes(h_lab, mecanismo, fill = rho)) +
    geom_tile(colour = "white", linewidth = 0.6) +
    geom_text(aes(label = fmt(rho), colour = abs(rho) > umbral_texto), size = 3) +
    scale_colour_manual(values = c("FALSE" = "black", "TRUE" = "white"), guide = "none") +
    scale_fill_distiller(
      palette = "RdBu", direction = 1, limits = lim_rho,
      breaks = c(-1, -0.5, 0, 0.5, 1), labels = c("-1", "-0,5", "0", "0,5", "1"),
      name = "Spearman"
    ) +
    facet_grid(. ~ modelo) +
    scale_x_discrete(position = "top") +
    labs(x = NULL, y = NULL) +
    theme_minimal(base_size = 10) +
    theme(
      panel.grid      = element_blank(),
      strip.text.x    = element_text(face = "bold"),
      strip.placement = "outside",
      axis.text.x     = element_text(face = "bold"),
      plot.caption    = element_text(hjust = 0, size = 8),
      legend.position = "right"
    )
}

dir.create(dir_fig, showWarnings = FALSE)
for (p in c(10, 25, 40)) {
  ggsave(file.path(dir_fig, sprintf("fig_spearman%02d.png", p)),
         mapa_spearman(p), width = 7.5, height = 3.6, dpi = 300)
}
