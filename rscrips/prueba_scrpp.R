library(httr2)
library(rvest)
library(jsonlite)

BASE_URL <- "http://siel.stps.gob.mx:303/ibmcognos/cgi-bin/cognos.cgi"

# ── Función para parsear la respuesta XML/estado de Cognos ──────────────────
parsear_estado <- function(contenido) {
  # Extraer el JSON dentro del tag <xml><state>...</state></xml>
  json_str <- gsub(".*<state>(.*)</state>.*", "\\1", contenido)
  json_str <- gsub("&quot;", '"', json_str)
  tryCatch(fromJSON(json_str), error = function(e) list(m_sStatus = "error"))
}

# ── Función de polling CORREGIDA ─────────────────────────────────────────────
hacer_polling <- function(conversation_token, cafcontextid,
                          max_intentos = 30, espera_seg = 4) {
  
  for (i in seq_len(max_intentos)) {
    cat(sprintf("  Intento %d/%d...\n", i, max_intentos))
    Sys.sleep(espera_seg)
    
    resp <- request(BASE_URL) |>
      req_method("POST") |>
      req_options(ssl_verifypeer = FALSE) |>
      req_body_form(
        b_action             = "cognosViewer",
        `cv.catchLogOnFault` = "true",
        `cv.id`              = "_NS_",
        `cv.responseFormat`  = "data",
        `cv.showFaultPage`   = "true",
        `ui.action`          = "forward",      # <-- mismo que la petición inicial
        `ui.conversation`    = conversation_token,
        `ui.cafcontextid`    = cafcontextid,
        `ui.object`          = "/content/folder[@name='Sitio STPS']/folder[@name='3. Revisiones del salario contractual']/report[@name='3.2.1 Resumen Jurisdicción Federal']",
        `ui.objectClass`     = "report",
        `ui.primaryAction`   = "run"
      ) |>
      req_perform()
    
    contenido <- resp |> resp_body_string()
    
    # ── Caso 1: llegaron tablas HTML directamente ──
    if (grepl("<table", contenido, ignore.case = TRUE)) {
      cat("  ✅ Tablas encontradas en respuesta HTML\n")
      return(list(status = "complete", contenido = contenido))
    }
    
    # ── Caso 2: sigue en estado "working" con nuevo token ──
    if (grepl("m_sStatus", contenido)) {
      estado <- parsear_estado(contenido)
      cat(sprintf("  Estado: %s\n", estado$m_sStatus))
      
      # Actualizar token si viene uno nuevo
      if (!is.null(estado$m_sConversation) && nchar(estado$m_sConversation) > 0) {
        conversation_token <<- estado$m_sConversation
        cat("  Token actualizado\n")
      }
      
      if (estado$m_sStatus == "complete") {
        # A veces "complete" viene sin tablas y hay que hacer un GET final
        cat("  Estado complete — intentando GET final...\n")
        resp_final <- request(BASE_URL) |>
          req_method("POST") |>
          req_options(ssl_verifypeer = FALSE) |>
          req_body_form(
            b_action            = "cognosViewer",
            `cv.catchLogOnFault`= "true",
            `cv.id`             = "_NS_",
            `cv.responseFormat` = "data",
            `ui.action`         = "render",    # pedir el render final
            `ui.conversation`   = conversation_token,
            `ui.cafcontextid`   = cafcontextid
          ) |>
          req_perform()
        return(list(status = "complete", 
                    contenido = resp_final |> resp_body_string()))
      }
      
      if (estado$m_sStatus %in% c("fault", "error")) {
        cat("  ❌ Error del servidor\n")
        # Imprimir contenido completo para diagnosticar
        cat(substr(contenido, 1, 800), "\n")
        return(list(status = "error", contenido = contenido))
      }
    } else {
      # Respuesta inesperada — imprimir para diagnosticar
      cat("  ⚠️  Respuesta no reconocida:\n")
      cat(substr(contenido, 1, 400), "\n")
    }
  }
  
  list(status = "timeout", contenido = NULL)
}

# ── PASO 1: Primera petición POST (igual que antes) ─────────────────────────
cat("Enviando petición inicial...\n")

resp_inicial <- request(BASE_URL) |>
  req_method("POST") |>
  req_options(ssl_verifypeer = FALSE) |>
  req_body_form(
    `_promptControl`       = "prompt",
    b_action               = "cognosViewer",
    `cv.catchLogOnFault`   = "true",
    `cv.id`                = "_NS_",
    `cv.objectPermissions` = "execute read traverse",
    `cv.responseFormat`    = "data",
    `cv.showFaultPage`     = "true",
    errURL = "/ibmcognos/cgi-bin/cognos.cgi?b_action=xts.run&m=portal/cc.xts&m_folder=iAB3120B47B544DC8A38675A3C1DB8A9C",
    executionParameters    = EXEC_PARAMS,
    m_tracking             = M_TRACKING,
    p_Mes                  = p_mes_xml,
    p_Nacional             = "<selectChoices></selectChoices>",
    `run.prompt`           = "false",
    `ui.action`            = "forward",
    `ui.backURL`           = "/ibmcognos/cgi-bin/cognos.cgi?b_action=xts.run&m=portal/cc.xts&m_folder=iAB3120B47B544DC8A38675A3C1DB8A9C",
    `ui.cafcontextid`      = UI_CAFCONTEXTID,
    `ui.conversation`      = UI_CONVERSATION,
    `ui.object`            = "/content/folder[@name='Sitio STPS']/folder[@name='3. Revisiones del salario contractual']/report[@name='3.2.1 Resumen Jurisdicción Federal']",
    `ui.objectClass`       = "report",
    `ui.primaryAction`     = "run"
  ) |>
  req_perform()

contenido_inicial <- resp_inicial |> resp_body_string()

# ── PASO 2: Extraer token de la primera respuesta y hacer polling ────────────
estado_inicial <- parsear_estado(contenido_inicial)
cat("Estado inicial:", estado_inicial$m_sStatus, "\n")

nuevo_token <- estado_inicial$m_sConversation
cat("Nuevo token conversación:", substr(nuevo_token, 1, 40), "...\n")

# ── PASO 3: Polling hasta obtener datos ─────────────────────────────────────
resultado <- hacer_polling(
  conversation_token = nuevo_token,
  cafcontextid       = UI_CAFCONTEXTID,
  max_intentos       = 20,
  espera_seg         = 4
)

# ── PASO 4: Parsear tablas HTML ──────────────────────────────────────────────
if (resultado$status == "complete") {
  html_resp <- resultado$contenido |> read_html()
  tablas    <- html_resp |> html_elements("table")
  cat("Tablas encontradas:", length(tablas), "\n")
  
  lista_dfs <- lapply(tablas, function(t) {
    tryCatch(html_table(t, fill = TRUE), error = function(e) NULL)
  })
  lista_dfs  <- Filter(Negate(is.null), lista_dfs)
  lista_dfs  <- Filter(function(df) ncol(df) >= 2 & nrow(df) >= 2, lista_dfs)
  
  tabla_final <- lista_dfs[[which.max(sapply(lista_dfs, nrow))]]
  
  print(head(tabla_final))
  write.csv(tabla_final, "revisiones_salariales_stps.csv",
            row.names = FALSE, fileEncoding = "UTF-8")
  cat("✅ Guardado en revisiones_salariales_stps.csv\n")
  
} else {
  cat("❌ No se obtuvieron datos. Status:", resultado$status, "\n")
  cat(substr(resultado$contenido, 1, 500), "\n")
}
