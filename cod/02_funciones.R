# ------------------------------------------------------------
# 1. CARGAR DATOS
# ------------------------------------------------------------

source("01_Importar_Datos.R")

# ============================================================
# 2. RESUMEN GENERAL DE LISTAS DE ESPERA
# ============================================================

tabla_resumen_general <- tibble(
  `Tipo de atención` = c("Consulta externa", "Quirúrgica"),
  `Casos pendientes` = c(384940, 213402),
  `Días promedio de espera` = c(564, 447)
)

# Comparación de indicadores entre ambos tipos de atención
comparacion_general <- tibble(
  Indicador = c("Casos pendientes", "Días promedio de espera"),
  Consulta_externa = c(384940, 564),
  Quirurgica = c(213402, 447)
) %>%
  mutate(
    Diferencia = Consulta_externa - Quirurgica,
    Variacion_porcentual = Diferencia / Quirurgica * 100
  )

# ------------------------------------------------------------
# 3. DEFINIR TABLAS
# ------------------------------------------------------------

tabla_region_ce <- region_ce %>%
  filter(!is.na(Mes)) %>%
  rename(Region = Mes) %>%
  mutate(
    `Días promedio` = as.numeric(`Días promedio`)
  )

tabla_region_q <- region_q

# Porcentaje que se lleva cada region del total de datos
distribucion_ce <- tabla_region_ce %>%
  transmute(
    Region,
    Casos_Consulta_externa = `Casos pendientes`,
    Porcentaje_Consulta_externa =
      `Casos pendientes` /
      tabla_resumen_general$`Casos pendientes`[
        tabla_resumen_general$`Tipo de atención` == "Consulta externa"
      ] * 100
  )

distribucion_q <- tabla_region_q %>%
  transmute(
    Region,
    Casos_Quirurgicos = `Casos pendientes`,
    Porcentaje_Quirurgico =
      `Casos pendientes` /
      tabla_resumen_general$`Casos pendientes`[
        tabla_resumen_general$`Tipo de atención` == "Quirúrgica"
      ] * 100
  )

tabla_distribucion_total <- full_join(
  distribucion_ce,
  distribucion_q,
  by = "Region"
) %>%
  bind_rows(
    tibble(
      Region = "Total general",
      Casos_Consulta_externa = sum(.$Casos_Consulta_externa, na.rm = TRUE),
      Porcentaje_Consulta_externa = sum(.$Porcentaje_Consulta_externa, na.rm = TRUE),
      Casos_Quirurgicos = sum(.$Casos_Quirurgicos, na.rm = TRUE),
      Porcentaje_Quirurgico = sum(.$Porcentaje_Quirurgico, na.rm = TRUE)
    )
  ) %>%
  mutate(
    Porcentaje_Consulta_externa = scales::percent(
      Porcentaje_Consulta_externa / 100,
      accuracy = 0.1
    ),
    Porcentaje_Quirurgico = scales::percent(
      Porcentaje_Quirurgico / 100,
      accuracy = 0.1
    )
  )

# ============================================================
# 4. ANÁLISIS REGIONAL
# ============================================================

# Guardar por separado los hospitales nacionales
hospitales_nacionales <- tabla_region_q %>%
  filter(Region == "Hospitales Nacionales y Centros Especializados")

hospitales_nacionales_ce <- tabla_region_ce %>%
  filter(Region == "Hospitales Nacionales y Centros Especializados")

# Separar las regiones territoriales de consulta externa

analisis_regional_ce <- tabla_region_ce %>%
  filter(Region != "Hospitales Nacionales y Centros Especializados") %>%
  mutate(
    Porcentaje_casos = `Casos pendientes` /
      sum(`Casos pendientes`) * 100
  )



# Diferencia entre el mayor y menor tiempo de espera
brecha_dias_regional_ce <- max(
  analisis_regional_ce$`Días promedio`,
  na.rm = TRUE
) - min(
  analisis_regional_ce$`Días promedio`,
  na.rm = TRUE
)

# Diferencia entre la mayor y menor cantidad de casos
brecha_casos_regional_ce <- max(
  analisis_regional_ce$`Casos pendientes`,
  na.rm = TRUE
) - min(
  analisis_regional_ce$`Casos pendientes`,
  na.rm = TRUE
)

# Participación de cada región territorial en el total regional
analisis_regional <- tabla_region_q %>%
  filter(Region != "Hospitales Nacionales y Centros Especializados") %>%
  mutate(
    Porcentaje_casos = `Casos pendientes` /
      sum(`Casos pendientes`) * 100
  )

# Tres regiones con más casos pendientes
regiones_mas_casos <- analisis_regional %>%
  arrange(desc(`Casos pendientes`)) %>%
  slice_head(n = 3)

# Tres regiones con menos casos pendientes
regiones_menos_casos <- analisis_regional %>%
  arrange(`Casos pendientes`) %>%
  slice_head(n = 3)

# Tres regiones con mayor tiempo promedio de espera
regiones_mayor_espera <- analisis_regional %>%
  arrange(desc(`Días promedio`)) %>%
  slice_head(n = 3)

# Tres regiones con menor tiempo promedio de espera
regiones_menor_espera <- analisis_regional %>%
  arrange(`Días promedio`) %>%
  slice_head(n = 3)

# Diferencia entre el mayor y menor tiempo de espera
brecha_dias_regional <- max(
  analisis_regional$`Días promedio`,
  na.rm = TRUE
) - min(
  analisis_regional$`Días promedio`,
  na.rm = TRUE
)

# Diferencia entre la mayor y menor cantidad de casos
brecha_casos_regional <- max(
  analisis_regional$`Casos pendientes`,
  na.rm = TRUE
) - min(
  analisis_regional$`Casos pendientes`,
  na.rm = TRUE
)

# Calcular estadísticos descriptivos regionales
calcular_estadisticos_regionales <- function(datos) {
  
  casos <- datos$`Casos pendientes`
  dias <- datos$`Días promedio`
  
  tibble(
    Indicador = c("Casos pendientes", "Días promedio de espera"),
    
    Media = c(mean(casos), mean(dias)),
    
    Mediana = c(median(casos), median(dias)),
    
    Desviacion_estandar = c(sd(casos), sd(dias)),
    
    Minimo = c(min(casos), min(dias)),
    
    Maximo = c(max(casos), max(dias)),
    
    Rango = c(
      max(casos) - min(casos),
      max(dias) - min(dias)
    ),
    
    Coeficiente_variacion = c(
      sd(casos) / mean(casos) * 100,
      sd(dias) / mean(dias) * 100
    )
  )
}

# Estadísticos de consulta externa
estadisticos_regionales_ce <- calcular_estadisticos_regionales(
  analisis_regional_ce
)

# Estadísticos de intervenciones quirúrgicas
estadisticos_regionales_q <- calcular_estadisticos_regionales(
  analisis_regional
)

# Comparación estadística entre servicios
comparacion_estadistica_regional <- bind_rows(
  estadisticos_regionales_ce %>%
    mutate(Servicio = "Consulta externa"),
  
  estadisticos_regionales_q %>%
    mutate(Servicio = "Intervenciones quirúrgicas")
) %>%
  select(
    Servicio,
    Indicador,
    Media,
    Mediana,
    Desviacion_estandar,
    Minimo,
    Maximo,
    Rango,
    Coeficiente_variacion
  ) %>%
  mutate(
    Coeficiente_variacion = scales::percent(
      Coeficiente_variacion / 100,
      accuracy = 0.1
    )
  )

# DISTRIBUCIÓN DE CASOS PENDIENTES POR REGIÓN
tabla_distribucion_regional <- full_join(
  analisis_regional_ce %>%
    select(
      Region,
      Casos_CE = `Casos pendientes`,
      Porcentaje_CE = Porcentaje_casos
    ),
  analisis_regional %>%
    select(
      Region,
      Casos_Q = `Casos pendientes`,
      Porcentaje_Q = Porcentaje_casos
    ),
  by = "Region"
) %>%
  arrange(Region) %>%
  mutate(
    Porcentaje_CE = scales::percent(Porcentaje_CE / 100, accuracy = 0.1),
    Porcentaje_Q = scales::percent(Porcentaje_Q / 100, accuracy = 0.1)
  )
# ============================================================
# 5. GRÁFICOS
# ============================================================

# 1. Gráfico genberal de casos pendientes por tipo de atención
grafico_resumen <- ggplot2::ggplot(
  tabla_resumen_general,
  ggplot2::aes(
    x = `Tipo de atención`,
    y = `Casos pendientes`,
    fill = `Tipo de atención`
  )
) +
  ggplot2::geom_col(width = 0.6) +
  ggplot2::geom_text(
    ggplot2::aes(
      label = format(
        `Casos pendientes`,
        big.mark = ".",
        scientific = FALSE
      )
    ),
    hjust = -0.15,
    size = 4
  ) +
  ggplot2::coord_flip() +
  ggplot2::scale_fill_manual(
    values = c(
      "Consulta externa" = "#A64B43",
      "Quirúrgica" = "#D9A66F"
    )
  ) +
  ggplot2::scale_y_continuous(
    labels = scales::label_number(
      big.mark = ".",
      decimal.mark = ","
    ),
    expand = ggplot2::expansion(mult = c(0, 0.18))
  ) +
  ggplot2::labs(
    title = "Casos pendientes según tipo de atención",
    x = NULL,
    y = "Cantidad de casos",
    fill = "Tipo de atención"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.position = "none",
    plot.title = ggplot2::element_text(
      face = "bold",
      hjust = 0.5
    )
  )


# 2. Función para construir gráficos regionales
crear_grafico_regional <- function(datos, variable, titulo,
                                   etiqueta_eje, color) {
  
  ggplot2::ggplot(
    datos,
    ggplot2::aes(
      x = reorder(Region, .data[[variable]]),
      y = .data[[variable]]
    )
  ) +
    ggplot2::geom_col(
      fill = color,
      width = 0.65
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = scales::comma(
          .data[[variable]],
          big.mark = ".",
          decimal.mark = ","
        )
      ),
      vjust = -0.4,
      size = 3.3
    ) +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0, 0.12)),
      labels = scales::label_comma(
        big.mark = ".",
        decimal.mark = ","
      )
    ) +
    ggplot2::labs(
      title = titulo,
      x = "Región",
      y = etiqueta_eje
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold",
        size = 12
      ),
      axis.text.x = ggplot2::element_text(
        angle = 35,
        hjust = 1
      ),
      panel.grid.major.x = ggplot2::element_blank()
    )
}



# CONSULTA EXTERNA

# Casos pendientes por región
grafico_casos_ce <- crear_grafico_regional(
  analisis_regional_ce,
  "Casos pendientes",
  "Casos pendientes por región - Consulta externa",
  "Cantidad de casos pendientes",
  "#A64B43"
)

# Días promedio de espera por región
grafico_dias_ce <- crear_grafico_regional(
  analisis_regional_ce,
  "Días promedio",
  "Días promedio de espera por región - Consulta externa",
  "Días promedio de espera",
  "#D9A66F"
)


# INTERVENCIONES QUIRÚRGICAS

# Casos pendientes por región
grafico_casos_q <- crear_grafico_regional(
  analisis_regional,
  "Casos pendientes",
  "Casos pendientes por región - Intervenciones quirúrgicas",
  "Cantidad de casos pendientes",
  "#A64B43"
)

# Días promedio de espera por región
grafico_dias_q <- crear_grafico_regional(
  analisis_regional,
  "Días promedio",
  "Días promedio de espera por región - Intervenciones quirúrgicas",
  "Días promedio de espera",
  "#D9A66F"
)