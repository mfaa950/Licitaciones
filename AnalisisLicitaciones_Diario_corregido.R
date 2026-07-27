# ============================================================
# BÚSQUEDA DIARIA DE LICITACIONES ARCE + INFORME WORD
# Ejecutar diariamente a las 12:00 (hora de Uruguay)
# ============================================================

options(stringsAsFactors = FALSE)
Sys.setenv(TZ = "America/Montevideo")

paquetes <- c(
  "httr2", "rvest", "dplyr", "stringr", "purrr",
  "openxlsx", "lubridate", "officer", "flextable"
)

faltantes <- paquetes[
  !vapply(paquetes, requireNamespace, logical(1), quietly = TRUE)
]

if (length(faltantes) > 0) {
  install.packages(faltantes, repos = "https://cloud.r-project.org")
}

suppressPackageStartupMessages({
  library(httr2)
  library(rvest)
  library(dplyr)
  library(stringr)
  library(purrr)
  library(openxlsx)
  library(lubridate)
  library(officer)
  library(flextable)
})

# ------------------------------------------------------------
# CONFIGURACIÓN
# ------------------------------------------------------------

base <- "https://www.comprasestatales.gub.uy"

tipos <- c(
  "LA" = "Licitación Abreviada",
  "LP" = "Licitación Pública"
)

# Se consulta ayer y hoy en ARCE, pero luego se conservan únicamente
# las publicaciones realizadas durante las últimas 24 horas.
momento_ejecucion <- now(tzone = "America/Montevideo")
momento_desde <- momento_ejecucion - hours(24*7)

fecha_desde <- format(as.Date(momento_desde), "%Y-%m-%d")
fecha_hasta <- format(as.Date(momento_ejecucion), "%Y-%m-%d")

sufijo_fecha <- format(as.Date(momento_ejecucion), "%Y-%m-%d")

archivo_excel_completo <- paste0("Licitaciones_ARCE_", sufijo_fecha, ".xlsx")
archivo_excel_filtrado <- paste0("Licitaciones_relevantes_", sufijo_fecha, ".xlsx")
archivo_word <- paste0("Informe_licitaciones_", sufijo_fecha, ".docx")

# ------------------------------------------------------------
# FUNCIONES DE DESCARGA Y EXTRACCIÓN
# ------------------------------------------------------------

leer_html <- function(url) {
  request(url) |>
    req_user_agent(
      "Mozilla/5.0 (Windows NT 10.0; Win64; x64) ",
      "AppleWebKit/537.36 Chrome/124 Safari/537.36"
    ) |>
    req_headers(
      Accept = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
      `Accept-Language` = "es-UY,es;q=0.9,en;q=0.8"
    ) |>
    req_timeout(30) |>
    req_retry(max_tries = 3) |>
    req_perform() |>
    resp_body_string() |>
    read_html()
}

armar_url <- function(tipo, page = 1) {
  paste0(
    base,
    "/consultas/buscar/",
    "tipo-pub/ALL/",
    "tipo-compra/", tipo, "/",
    "tipo-doc/C/",
    "tipo-fecha/PUB/",
    "filtro-cat/CAT/",
    "orden/ORD_PUB/",
    "tipo-orden/DESC/",
    "rango-fecha/", fecha_desde, "_", fecha_hasta, "/",
    "page/", page
  )
}

limpiar_fecha <- function(x) {
  ifelse(
    is.na(x),
    NA_character_,
    str_replace_all(x, fixed("&sol;"), "/")
  )
}

extraer_fecha_publicacion <- function(txt) {
  m <- str_match(
    txt,
    regex(
      "Fecha\\s+Publicaci[oó]n:\\s*(\\d{2}(?:/|&sol;)\\d{2}(?:/|&sol;)\\d{4}\\s+\\d{2}:\\d{2})\\s*hs",
      ignore_case = TRUE
    )
  )[, 2]
  limpiar_fecha(m)
}

extraer_fecha_apertura <- function(txt) {
  m <- str_match(
    txt,
    regex(
      "Acto\\s+de\\s+Apertura:\\s*(\\d{2}(?:/|&sol;)\\d{2}(?:/|&sol;)\\d{4}\\s+\\d{2}:\\d{2})\\s*hs",
      ignore_case = TRUE
    )
  )[, 2]
  limpiar_fecha(m)
}

extraer_recepcion_ofertas <- function(txt) {
  m <- str_match(
    txt,
    regex(
      "Recepci[oó]n\\s+de\\s+ofertas\\s+hasta:\\s*(\\d{2}(?:/|&sol;)\\d{2}(?:/|&sol;)\\d{4}\\s+\\d{2}:\\d{2})\\s*hs",
      ignore_case = TRUE
    )
  )[, 2]
  limpiar_fecha(m)
}

extraer_objeto <- function(txt) {
  if (is.na(txt)) return(NA_character_)

  txt2 <- txt |>
    str_replace_all("&sol;", "/") |>
    str_replace_all("\uFEFF", " ") |>
    str_replace_all("\\r", "\n")

  partes <- str_split(
    txt2,
    regex("Recepci[oó]n de ofertas hasta:", ignore_case = TRUE),
    simplify = TRUE
  )

  if (length(partes) == 0) return(NA_character_)

  lineas <- str_split(partes[1], "\n")[[1]]
  lineas <- str_squish(lineas)
  lineas <- lineas[lineas != ""]

  if (length(lineas) == 0) return(NA_character_)

  objeto <- tail(lineas, 1)

  if (str_detect(
    objeto,
    regex("Licitaci[oó]n|Consulta de Publicaciones|^\\|", ignore_case = TRUE)
  )) {
    return(NA_character_)
  }

  objeto
}

extraer_numero <- function(titulo, tipo_nombre) {
  m <- str_match(
    titulo,
    regex(paste0(tipo_nombre, "\\s+([^|\\n\\r]+)"), ignore_case = TRUE)
  )
  ifelse(is.na(m[, 2]), NA_character_, str_squish(m[, 2]))
}

extraer_organismo <- function(titulo) {
  m <- str_match(titulo, "\\|\\s*(.+)$")
  ifelse(is.na(m[, 2]), NA_character_, str_squish(m[, 2]))
}

leer_detalle <- function(link) {
  Sys.sleep(0.25)

  tryCatch({
    doc <- leer_html(link)
    txt <- html_text2(doc)

    list(
      fecha_publicado = extraer_fecha_publicacion(txt),
      fecha_apertura = extraer_fecha_apertura(txt),
      fecha_recepcion_ofertas = extraer_recepcion_ofertas(txt),
      objeto = extraer_objeto(txt)
    )
  }, error = function(e) {
    message("No se pudo leer: ", link, " | ", conditionMessage(e))
    list(
      fecha_publicado = NA_character_,
      fecha_apertura = NA_character_,
      fecha_recepcion_ofertas = NA_character_,
      objeto = NA_character_
    )
  })
}

scrapear_tipo <- function(tipo_codigo, tipo_nombre) {
  message("Procesando ", tipo_nombre, "...")

  resultados <- list()
  vistos <- character()

  for (page in 1:300) {
    message("  Página ", page)
    url <- armar_url(tipo_codigo, page)

    doc <- tryCatch(leer_html(url), error = function(e) NULL)
    if (is.null(doc)) break

    links <- doc |>
      html_elements(xpath = "//a[contains(@href, 'mostrar-llamado')]")

    if (length(links) == 0) break

    tabla_links <- tibble(
      titulo = links |> html_text2(),
      link = links |> html_attr("href")
    ) |>
      mutate(
        link = if_else(
          str_starts(link, "http"),
          link,
          paste0(base, link)
        )
      ) |>
      distinct(link, .keep_all = TRUE) |>
      filter(!link %in% vistos)

    if (nrow(tabla_links) == 0) break
    vistos <- c(vistos, tabla_links$link)

    for (i in seq_len(nrow(tabla_links))) {
      detalle <- leer_detalle(tabla_links$link[i])

      resultados[[length(resultados) + 1]] <- tibble(
        tipo = tipo_nombre,
        tipo_codigo = tipo_codigo,
        numero_llamado = extraer_numero(tabla_links$titulo[i], tipo_nombre),
        organismo_unidad = extraer_organismo(tabla_links$titulo[i]),
        fecha_publicado = detalle$fecha_publicado,
        fecha_apertura = detalle$fecha_apertura,
        fecha_recepcion_ofertas = detalle$fecha_recepcion_ofertas,
        objeto = detalle$objeto,
        link = tabla_links$link[i]
      )
    }
  }

  if (length(resultados) == 0) {
    message("  No se encontraron llamados para ", tipo_nombre, ".")

    return(tibble(
      tipo = character(),
      tipo_codigo = character(),
      numero_llamado = character(),
      organismo_unidad = character(),
      fecha_publicado = character(),
      fecha_apertura = character(),
      fecha_recepcion_ofertas = character(),
      objeto = character(),
      link = character()
    ))
  }

  bind_rows(resultados)
}

# ------------------------------------------------------------
# OBTENCIÓN Y LIMPIEZA
# ------------------------------------------------------------

datos <- imap_dfr(tipos, ~ scrapear_tipo(.y, .x))

datos_limpios <- datos |>
  mutate(
    numero_llamado = str_replace_all(
      numero_llamado,
      fixed("&sol;"),
      "/"
    ),
    fecha_publicado_dt = dmy_hm(
      fecha_publicado,
      tz = "America/Montevideo",
      quiet = TRUE
    ),
    fecha_apertura_dt = dmy_hm(
      fecha_apertura,
      tz = "America/Montevideo",
      quiet = TRUE
    ),
    objeto = if_else(
      is.na(objeto) | objeto == "",
      "No informado",
      objeto
    )
  ) |>
  filter(
    is.na(fecha_publicado_dt) |
      (
        fecha_publicado_dt >= momento_desde &
          fecha_publicado_dt <= momento_ejecucion
      )
  ) |>
  distinct(link, .keep_all = TRUE) |>
  arrange(organismo_unidad, fecha_apertura_dt, fecha_publicado_dt) |>
  select(
    tipo,
    tipo_codigo,
    numero_llamado,
    organismo_unidad,
    fecha_publicado,
    fecha_apertura,
    fecha_recepcion_ofertas,
    objeto,
    link
  )

datos_relevantes <- datos_limpios |>
  filter(
    str_detect(
      organismo_unidad,
      regex(
        "^Ministerio de Transporte y Obras P[uú]blicas$|^Intendencia",
        ignore_case = TRUE
      )
    )
  ) |>
  arrange(
    organismo_unidad,
    dmy_hm(fecha_apertura, quiet = TRUE),
    dmy_hm(fecha_publicado, quiet = TRUE)
  )

# ------------------------------------------------------------
# EXPORTACIÓN A EXCEL
# ------------------------------------------------------------

write.xlsx(
  datos_limpios,
  archivo_excel_completo,
  overwrite = TRUE
)

write.xlsx(
  datos_relevantes,
  archivo_excel_filtrado,
  overwrite = TRUE
)

# ------------------------------------------------------------
# GENERACIÓN DEL INFORME WORD
# ------------------------------------------------------------

crear_tabla_organismo <- function(datos_org) {
  tabla <- datos_org |>
    transmute(
      Llamado = paste(tipo_codigo, numero_llamado),
      Objeto = objeto,
      Publicación = fecha_publicado,
      Apertura = fecha_apertura
    )

  flextable(tabla) |>
    theme_vanilla() |>
    bold(part = "header") |>
    fontsize(size = 8, part = "all") |>
    width(j = "Llamado", width = 1.15) |>
    width(j = "Objeto", width = 4.7) |>
    width(j = "Publicación", width = 1.25) |>
    width(j = "Apertura", width = 1.25) |>
    valign(valign = "top", part = "all") |>
    align(j = c("Publicación", "Apertura"), align = "center", part = "all") |>
    set_table_properties(layout = "fixed", width = 1)
}

cantidad_llamados <- nrow(datos_relevantes)
cantidad_organismos <- n_distinct(datos_relevantes$organismo_unidad)

doc <- read_docx()

doc <- doc |>
  body_add_par("INFORME DE LLAMADOS", style = "Title") |>
  body_add_par("Informe de llamados por organismo", style = "heading 1") |>
  body_add_par(
    paste0(
      "Detalle de ", cantidad_llamados,
      " llamados agrupados en ", cantidad_organismos,
      " organismos"
    )
  ) |>
  body_add_par(
    paste0(
      "Período de publicación: ",
      format(momento_desde, "%d/%m/%Y %H:%M"),
      " al ",
      format(momento_ejecucion, "%d/%m/%Y %H:%M")
    )
  ) |>
  body_add_par(
    "Orden de los llamados: por organismo y fecha de apertura"
  )

if (cantidad_llamados == 0) {
  doc <- doc |>
    body_add_par("") |>
    body_add_par(
      "No se encontraron llamados de Intendencias ni del Ministerio de Transporte y Obras Públicas publicados durante las últimas 24 horas.",
      style = "Normal"
    )
} else {
  organismos <- unique(datos_relevantes$organismo_unidad)

  for (organismo in organismos) {
    datos_org <- datos_relevantes |>
      filter(organismo_unidad == organismo)

    doc <- doc |>
      body_add_par(organismo, style = "heading 2") |>
      body_add_par(
        paste0(
          nrow(datos_org),
          ifelse(nrow(datos_org) == 1, " llamado", " llamados")
        )
      ) |>
      body_add_flextable(crear_tabla_organismo(datos_org)) |>
      body_add_par("")
  }
}

# Pie de página con numeración
seccion <- prop_section(
  page_size = page_size(orient = "portrait"),
  page_margins = page_mar(
    top = 0.55,
    bottom = 0.55,
    left = 0.6,
    right = 0.6
  ),
  type = "continuous"
)

doc <- body_set_default_section(doc, seccion)

print(doc, target = archivo_word)

# ------------------------------------------------------------
# RESUMEN DE EJECUCIÓN
# ------------------------------------------------------------

message("Proceso terminado correctamente.")
message("Período exacto: ", momento_desde, " a ", momento_ejecucion)
message("Total de llamados encontrados: ", nrow(datos_limpios))
message("Llamados relevantes: ", cantidad_llamados)
message("Organismos relevantes: ", cantidad_organismos)
message("Excel completo: ", archivo_excel_completo)
message("Excel filtrado: ", archivo_excel_filtrado)
message("Informe Word: ", archivo_word)
