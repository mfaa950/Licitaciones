# install.packages(c("httr2", "rvest", "dplyr", "stringr", "purrr", "openxlsx", "lubridate"))

library(httr2)
library(rvest)
library(dplyr)
library(stringr)
library(purrr)
library(openxlsx)
library(lubridate)

base <- "https://www.comprasestatales.gub.uy"

fecha_desde <- "2026-07-20"
fecha_hasta <- "2026-07-27"

tipos <- c(
  "LA" = "Licitación Abreviada",
  "LP" = "Licitación Pública"
)

archivo_salida <- file.path(getwd(), "licitaciones_ARCE_2026_04_LA_LP.xlsx")

leer_html <- function(url) {
  request(url) |>
    req_user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 Chrome/124 Safari/537.36") |>
    req_headers(
      Accept = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
      `Accept-Language` = "es-UY,es;q=0.9,en;q=0.8"
    ) |>
    req_timeout(30) |>
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
  ifelse(is.na(x), NA_character_, str_replace_all(x, fixed("&sol;"), "/"))
}

extraer_fecha_publicacion_detalle <- function(txt) {
  m <- str_match(
    txt,
    regex(
      "Fecha\\s+Publicaci[oó]n:\\s*(\\d{2}(?:/|&sol;)\\d{2}(?:/|&sol;)\\d{4}\\s+\\d{2}:\\d{2})\\s*hs",
      ignore_case = TRUE
    )
  )[, 2]
  
  limpiar_fecha(m)
}

extraer_fecha_apertura_detalle <- function(txt) {
  m <- str_match(
    txt,
    regex(
      "Acto\\s+de\\s+Apertura:\\s*(\\d{2}(?:/|&sol;)\\d{2}(?:/|&sol;)\\d{4}\\s+\\d{2}:\\d{2})\\s*hs",
      ignore_case = TRUE
    )
  )[, 2]
  
  limpiar_fecha(m)
}

extraer_recepcion_ofertas_detalle <- function(txt) {
  m <- str_match(
    txt,
    regex(
      "Recepci[oó]n\\s+de\\s+ofertas\\s+hasta:\\s*(\\d{2}(?:/|&sol;)\\d{2}(?:/|&sol;)\\d{4}\\s+\\d{2}:\\d{2})\\s*hs",
      ignore_case = TRUE
    )
  )[, 2]
  
  limpiar_fecha(m)
}

extraer_objeto_detalle <- function(txt) {
  txt2 <- txt |>
    str_replace_all("\uFEFF", " ") |>
    str_squish()
  
  m <- str_match(
    txt2,
    regex(
      "Consulta de Publicaciones\\s+.*?\\|\\s*[^\\n]+\\s+.*?\\|\\s*[^\\n]+\\s+(.+?)\\s+Recepci[oó]n\\s+de\\s+ofertas\\s+hasta:",
      ignore_case = TRUE
    )
  )[, 2]
  
  ifelse(is.na(m), NA_character_, str_squish(m))
}

extraer_numero <- function(txt, tipo_nombre) {
  m <- str_match(txt, paste0(tipo_nombre, "\\s+([^|\\n\\r]+)"))
  ifelse(is.na(m[, 2]), NA_character_, str_squish(m[, 2]))
}

extraer_organismo <- function(txt) {
  m <- str_match(txt, "\\|\\s*(.+)$")
  ifelse(is.na(m[, 2]), NA_character_, str_squish(m[, 2]))
}

leer_detalle <- function(link) {
  Sys.sleep(0.3)
  
  tryCatch({
    doc <- leer_html(link)
    txt <- html_text2(doc)
    
    list(
      fecha_publicado = extraer_fecha_publicacion_detalle(txt),
      fecha_apertura = extraer_fecha_apertura_detalle(txt),
      fecha_recepcion_ofertas = extraer_recepcion_ofertas_detalle(txt),
      objeto = extraer_objeto_detalle(txt),
      texto_detalle_crudo = txt
    )
    
  }, error = function(e) {
    list(
      fecha_publicado = NA_character_,
      fecha_apertura = NA_character_,
      fecha_recepcion_ofertas = NA_character_,
      objeto = NA_character_,
      texto_detalle_crudo = NA_character_
    )
  })
}

scrapear_tipo <- function(tipo_codigo, tipo_nombre) {
  message("Procesando ", tipo_nombre, "...")
  
  resultados <- list()
  vistos <- character()
  
  for (page in 1:300) {
    url <- armar_url(tipo_codigo, page)
    message("  Página ", page)
    
    doc <- tryCatch(leer_html(url), error = function(e) NULL)
    if (is.null(doc)) break
    
    links <- doc |>
      html_elements(xpath = "//a[contains(@href, 'mostrar-llamado')]")
    
    if (length(links) == 0) break
    
    titulos <- links |> html_text2()
    hrefs <- links |> html_attr("href")
    hrefs <- ifelse(str_starts(hrefs, "http"), hrefs, paste0(base, hrefs))
    
    tabla_links <- tibble(
      titulo = titulos,
      link = hrefs
    ) |>
      distinct(link, .keep_all = TRUE)
    
    tabla_links <- tabla_links |>
      filter(!link %in% vistos)
    
    if (nrow(tabla_links) == 0) break
    
    vistos <- c(vistos, tabla_links$link)
    
    for (i in seq_len(nrow(tabla_links))) {
      titulo <- tabla_links$titulo[i]
      link <- tabla_links$link[i]
      
      detalle <- leer_detalle(link)
      
      resultados[[length(resultados) + 1]] <- tibble(
        tipo = tipo_nombre,
        tipo_codigo = tipo_codigo,
        numero_llamado = extraer_numero(titulo, tipo_nombre),
        organismo_unidad = extraer_organismo(titulo),
        fecha_publicado = detalle$fecha_publicado,
        fecha_apertura = detalle$fecha_apertura,
        fecha_recepcion_ofertas = detalle$fecha_recepcion_ofertas,
        objeto = detalle$objeto,
        link = link
      )
    }
  }
  
  bind_rows(resultados)
}

datos <- imap_dfr(tipos, ~scrapear_tipo(.y, .x))

datos_limpios <- datos |>
  mutate(
    fecha_publicado_date = dmy_hm(fecha_publicado)
  ) |>
  filter(
    is.na(fecha_publicado_date) |
      (
        as.Date(fecha_publicado_date) >= ymd(fecha_desde) &
          as.Date(fecha_publicado_date) <= ymd(fecha_hasta)
      )
  ) |>
  distinct(link, .keep_all = TRUE) |>
  arrange(tipo, fecha_publicado_date, numero_llamado) |>
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

wb <- createWorkbook()
addWorksheet(wb, "licitaciones")

writeData(wb, "licitaciones", datos_limpios)
setColWidths(wb, "licitaciones", cols = 1:ncol(datos_limpios), widths = "auto")
freezePane(wb, "licitaciones", firstRow = TRUE)

saveWorkbook(wb, archivo_salida, overwrite = TRUE)

message("Listo. Archivo generado en: ", archivo_salida)
message("Filas exportadas: ", nrow(datos_limpios))

library(httr2)
library(rvest)
library(stringr)

###############################################
df<-read.xlsx("licitaciones_ARCE_2026_04_LA_LP.xlsx")

link <- df$link[1]   # tomo un solo link para probar

doc <- request(link) |>
  req_user_agent("Mozilla/5.0") |>
  req_perform() |>
  resp_body_string() |>
  read_html()

txt <- html_text2(doc)

cat(substr(txt, 1, 5000))


library(purrr)

resultados <- map(df$link, function(link) {
  
  tryCatch({
    request(link) |>
      req_user_agent("Mozilla/5.0") |>
      req_perform() |>
      resp_body_string() |>
      read_html() |>
      html_text2()
    
  }, error = function(e) NA)
})

############################################

df <- read.xlsx("licitaciones_ARCE_2026_04_LA_LP.xlsx")

limpiar_fecha <- function(x) {
  ifelse(is.na(x), NA_character_, str_replace_all(x, fixed("&sol;"), "/"))
}

leer_texto_llamado <- function(link) {
  tryCatch({
    request(link) |>
      req_user_agent("Mozilla/5.0") |>
      req_timeout(30) |>
      req_perform() |>
      resp_body_string() |>
      read_html() |>
      html_text2()
  }, error = function(e) NA_character_)
}

extraer_fecha_publicacion <- function(txt) {
  m <- str_match(
    txt,
    regex("Fecha\\s+Publicaci[oó]n:\\s*(\\d{2}(?:/|&sol;)\\d{2}(?:/|&sol;)\\d{4}\\s+\\d{2}:\\d{2})\\s*hs",
          ignore_case = TRUE)
  )[,2]
  limpiar_fecha(m)
}

extraer_fecha_apertura <- function(txt) {
  m <- str_match(
    txt,
    regex("Acto\\s+de\\s+Apertura:\\s*(\\d{2}(?:/|&sol;)\\d{2}(?:/|&sol;)\\d{4}\\s+\\d{2}:\\d{2})\\s*hs",
          ignore_case = TRUE)
  )[,2]
  limpiar_fecha(m)
}

extraer_recepcion_ofertas <- function(txt) {
  m <- str_match(
    txt,
    regex("Recepci[oó]n\\s+de\\s+ofertas\\s+hasta:\\s*(\\d{2}(?:/|&sol;)\\d{2}(?:/|&sol;)\\d{4}\\s+\\d{2}:\\d{2})\\s*hs",
          ignore_case = TRUE)
  )[,2]
  limpiar_fecha(m)
}

extraer_objeto <- function(txt) {
  
  txt2 <- txt |>
    str_replace_all("&sol;", "/") |>
    str_replace_all("\uFEFF", " ") |>
    str_replace_all("\\r", "\n")
  
  # 1. Cortar todo antes de "Recepción de ofertas"
  partes <- str_split(txt2, "Recepci[oó]n de ofertas hasta:", simplify = TRUE)
  
  if (length(partes) == 0) return(NA_character_)
  
  bloque <- partes[1]
  
  # 2. Separar en líneas
  lineas <- str_split(bloque, "\n")[[1]]
  
  # 3. Limpiar
  lineas <- str_trim(lineas)
  lineas <- lineas[lineas != ""]
  
  # 4. El objeto es la última línea antes de "Recepción..."
  objeto <- tail(lineas, 1)
  
  # 5. Filtrar basura por seguridad
  if (str_detect(objeto, "Licitaci[oó]n|\\|")) {
    return(NA_character_)
  }
  
  objeto
}

# 1) Traer texto de todos los links
df$texto_detalle <- map_chr(df$link, function(l) {
  Sys.sleep(0.25)
  leer_texto_llamado(l)
})

# 2) Extraer datos desde cada texto
df_final <- df |>
  mutate(
    fecha_publicado = map_chr(texto_detalle, extraer_fecha_publicacion),
    fecha_apertura = map_chr(texto_detalle, extraer_fecha_apertura),
    fecha_recepcion_ofertas = map_chr(texto_detalle, extraer_recepcion_ofertas),
    objeto = map_chr(texto_detalle, extraer_objeto)
  ) |>
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


df_final <- df_final |>
  mutate(
    numero_llamado = str_replace_all(numero_llamado, fixed("&sol;"), "/"),
    organismo_unidad = str_trim(str_remove(numero_llamado, "^\\S+\\s*")),
    numero_llamado = str_extract(numero_llamado, "^\\S+"),
    MTOP_Intendencia = if_else(
      str_detect(
        organismo_unidad,
        regex("^Ministerio de Transporte y Obras Públicas$|^Intendencia", ignore_case = TRUE)
      ),
      "Si",
      "No"
    )
  )

# 3) Exportar Excel corregido
write.xlsx(
  df_final,
  "licitaciones_ARCE_2026_04_LA_LP_CORREGIDO.xlsx",
  overwrite = TRUE
)

message("Listo: licitaciones_ARCE_2026_04_LA_LP_CORREGIDO.xlsx")
