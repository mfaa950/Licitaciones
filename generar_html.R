# Genera el informe HTML y ejecuta previamente la consulta principal
Sys.setenv(TZ = "America/Montevideo")
source("AnalisisLicitaciones_Diario_corregido.R", encoding = "UTF-8")

dir.create("docs", showWarnings = FALSE, recursive = TRUE)

esc_html <- function(x) {
  x <- ifelse(is.na(x), "", as.character(x))
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x
}

filas_html <- function(df) {
  if (nrow(df) == 0) return("<p class='sin-resultados'>No se encontraron llamados relevantes para el período consultado.</p>")

  bloques <- character()
  for (org in unique(df$organismo_unidad)) {
    d <- df[df$organismo_unidad == org, , drop = FALSE]
    filas <- vapply(seq_len(nrow(d)), function(i) {
      llamado <- paste(d$tipo_codigo[i], d$numero_llamado[i])
      paste0(
        "<tr>",
        "<td>", esc_html(llamado), "</td>",
        "<td>", esc_html(d$objeto[i]), "</td>",
        "<td>", esc_html(d$fecha_publicado[i]), "</td>",
        "<td>", esc_html(d$fecha_apertura[i]), "</td>",
        "<td><a href='", esc_html(d$link[i]), "' target='_blank' rel='noopener'>Ver llamado</a></td>",
        "</tr>"
      )
    }, character(1))

    bloques <- c(bloques, paste0(
      "<section class='organismo'>",
      "<h2>", esc_html(org), "</h2>",
      "<p class='cantidad'>", nrow(d), ifelse(nrow(d) == 1, " llamado", " llamados"), "</p>",
      "<div class='tabla-wrap'><table>",
      "<thead><tr><th>Llamado</th><th>Objeto</th><th>Publicación</th><th>Apertura</th><th>Enlace</th></tr></thead>",
      "<tbody>", paste(filas, collapse = "\n"), "</tbody></table></div>",
      "</section>"
    ))
  }
  paste(bloques, collapse = "\n")
}

actualizado <- format(Sys.time(), "%d/%m/%Y %H:%M", tz = "America/Montevideo")
periodo <- paste0(format(fecha_inicio, "%d/%m/%Y"), " al ", format(hoy, "%d/%m/%Y"))
contenido <- filas_html(datos_relevantes)

html <- paste0(
"<!doctype html><html lang='es'><head><meta charset='utf-8'>",
"<meta name='viewport' content='width=device-width, initial-scale=1'>",
"<title>Informe diario de licitaciones</title>",
"<style>",
":root{font-family:Arial,Helvetica,sans-serif;color:#182230;background:#f4f6f8}",
"body{margin:0}.contenedor{max-width:1200px;margin:auto;padding:24px}",
"header{background:#153f68;color:white;padding:28px;border-radius:14px;margin-bottom:20px}",
"h1{margin:0 0 8px;font-size:30px}header p{margin:5px 0;opacity:.95}",
".resumen{display:flex;gap:12px;flex-wrap:wrap;margin:18px 0}",
".tarjeta{background:white;padding:14px 18px;border-radius:10px;box-shadow:0 2px 8px rgba(0,0,0,.07)}",
".organismo{background:white;margin:18px 0;padding:20px;border-radius:12px;box-shadow:0 2px 8px rgba(0,0,0,.07)}",
"h2{margin:0;color:#153f68}.cantidad{margin:6px 0 14px;color:#5b6673}",
".tabla-wrap{overflow-x:auto}table{border-collapse:collapse;width:100%;min-width:850px}",
"th,td{padding:11px;border-bottom:1px solid #dde3e8;text-align:left;vertical-align:top}",
"th{background:#edf3f8;color:#153f68}a{color:#0b62a4;font-weight:bold;text-decoration:none}",
"a:hover{text-decoration:underline}.sin-resultados{background:white;padding:22px;border-radius:12px}",
"footer{text-align:center;color:#697582;padding:20px;font-size:13px}",
"</style></head><body><div class='contenedor'>",
"<header><h1>Informe diario de licitaciones</h1>",
"<p>Período de publicación: ", periodo, "</p>",
"<p>Actualizado: ", actualizado, " (Uruguay)</p></header>",
"<div class='resumen'>",
"<div class='tarjeta'><strong>", nrow(datos_relevantes), "</strong><br>Llamados relevantes</div>",
"<div class='tarjeta'><strong>", dplyr::n_distinct(datos_relevantes$organismo_unidad), "</strong><br>Organismos</div>",
"</div>", contenido,
"<footer>Generado automáticamente desde Compras Estatales de Uruguay.</footer>",
"</div></body></html>"
)

writeLines(html, "docs/index.html", useBytes = TRUE)
message("HTML generado: docs/index.html")
