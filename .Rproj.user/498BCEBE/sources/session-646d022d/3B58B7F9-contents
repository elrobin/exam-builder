library(shiny)
library(bslib)
library(DBI)
library(RSQLite)
library(DT)

# ── Configuración ─────────────────────────────────────────────────────────────
DB_PATH <- "preguntas.db"   # cambia la ruta si quieres guardarlo en otro sitio

# ── Helper: texto vacío → NA ──────────────────────────────────────────────────
txt <- function(x) {
  v <- trimws(as.character(x))
  if (length(v) == 0 || is.na(v) || v == "") NA_character_ else v
}

# ── Helper: NULL → valor por defecto ─────────────────────────────────────────
`%||%` <- function(a, b) if (!is.null(a) && !is.na(a)) a else b

# ── Inicializar BD ────────────────────────────────────────────────────────────
init_db <- function(path) {
  con <- dbConnect(SQLite(), path)
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS preguntas (
      id                 INTEGER PRIMARY KEY AUTOINCREMENT,
      asignatura         TEXT NOT NULL DEFAULT '',
      tema               TEXT,
      enunciado          TEXT NOT NULL,
      opcion_a           TEXT NOT NULL,
      opcion_b           TEXT NOT NULL,
      opcion_c           TEXT NOT NULL,
      opcion_d           TEXT NOT NULL,
      respuesta_correcta TEXT NOT NULL CHECK(respuesta_correcta IN ('A','B','C','D')),
      dificultad         TEXT CHECK(dificultad IN ('Baja','Media','Alta')),
      anyo               INTEGER,
      notas              TEXT,
      created_at         TEXT DEFAULT (datetime('now'))
    )
  ")
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS examenes (
      id         INTEGER PRIMARY KEY AUTOINCREMENT,
      nombre     TEXT NOT NULL,
      asignatura TEXT,
      fecha      TEXT DEFAULT (date('now'))
    )
  ")
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS usos (
      pregunta_id INTEGER NOT NULL REFERENCES preguntas(id),
      examen_id   INTEGER NOT NULL REFERENCES examenes(id),
      PRIMARY KEY (pregunta_id, examen_id)
    )
  ")
  dbDisconnect(con)
}

init_db(DB_PATH)
get_con <- function() dbConnect(SQLite(), DB_PATH)

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- page_navbar(
  title = "📚 Banco de Preguntas",
  theme = bs_theme(bootswatch = "flatly", base_font = font_google("Inter")),
  fillable = FALSE,

  # ── Pestaña 1: Nueva pregunta ───────────────────────────────────────────────
  nav_panel(
    title = "✏️ Nueva pregunta",
    div(style = "max-width:860px; margin:auto; padding-top:1rem;",
      card(
        card_header("Añadir pregunta al banco"),
        layout_column_wrap(
          width = 1/2,
          textInput("asignatura", "Asignatura *", placeholder = "Ej: DAT"),
          textInput("tema", "Tema", placeholder = "Ej: Citas y referencias")
        ),
        textAreaInput("enunciado", "Enunciado *", width = "100%", rows = 3,
                      placeholder = "Escribe aquí la pregunta..."),
        layout_column_wrap(
          width = 1/2,
          textAreaInput("opcion_a", "Opción A *", rows = 2),
          textAreaInput("opcion_b", "Opción B *", rows = 2),
          textAreaInput("opcion_c", "Opción C *", rows = 2),
          textAreaInput("opcion_d", "Opción D *", rows = 2)
        ),
        layout_column_wrap(
          width = 1/3,
          selectInput("respuesta_correcta", "Respuesta correcta *",
                      choices = c("A", "B", "C", "D")),
          selectInput("dificultad", "Dificultad",
                      choices = c("— sin especificar —" = "", "Baja", "Media", "Alta")),
          numericInput("anyo", "Año de elaboración",
                       value = as.integer(format(Sys.Date(), "%Y")),
                       min = 2000, max = 2100, step = 1)
        ),
        textAreaInput("notas", "Notas (opcional)", rows = 2,
                      placeholder = "Comentarios internos, fuente, etc."),
        actionButton("btn_guardar", "💾 Guardar pregunta",
                     class = "btn-primary btn-lg")
      )
    )
  ),

  # ── Pestaña 2: Banco ────────────────────────────────────────────────────────
  nav_panel(
    title = "🗃️ Banco",
    layout_sidebar(
      sidebar = sidebar(
        width = 220,
        strong("Filtros"),
        selectInput("filtro_asig", "Asignatura", choices = NULL),
        selectInput("filtro_tema", "Tema",       choices = NULL),
        selectInput("filtro_dif",  "Dificultad",
                    choices = c("Todas" = "", "Baja", "Media", "Alta")),
        hr(),
        strong("Examen en curso"),
        verbatimTextOutput("n_seleccionadas", placeholder = TRUE),
        actionButton("btn_add", "➕ Añadir selección al examen",
                     class = "btn-success btn-sm", width = "100%"),
        br(),
        actionButton("btn_editar", "✏️ Editar pregunta seleccionada",
                     class = "btn-outline-primary btn-sm", width = "100%"),
        br(),
        actionButton("btn_limpiar", "🗑️ Limpiar selección",
                     class = "btn-outline-danger btn-sm", width = "100%")
      ),
      card(
        card_header("Preguntas — selecciona las que quieras añadir al examen"),
        DTOutput("tabla_banco")
      )
    )
  ),

  # ── Pestaña 3: Crear examen ─────────────────────────────────────────────────
  nav_panel(
    title = "📝 Crear examen",
    layout_column_wrap(
      width = 1/2,
      card(
        card_header("Configuración del examen"),
        textInput("nombre_examen", "Nombre del examen *",
                  placeholder = "Ej: DAT 2025/26 – Bloque 1"),
        textInput("asig_examen", "Asignatura"),
        hr(),
        actionButton("btn_guardar_examen", "✅ Registrar examen y guardar usos",
                     class = "btn-primary")
      ),
      card(
        card_header("Preguntas seleccionadas"),
        DTOutput("tabla_seleccion")
      )
    ),
    card(
      card_header("Texto listo para pegar en Word"),
      p(class = "text-muted",
        "La respuesta correcta NO aparece aquí (es el examen del alumno); se guarda en la BD."),
      actionButton("btn_generar", "🔄 Generar texto", class = "btn-secondary"),
      br(), br(),
      verbatimTextOutput("texto_word", placeholder = TRUE)
    )
  ),

  # ── Pestaña 4: Historial ────────────────────────────────────────────────────
  nav_panel(
    title = "📊 Historial",
    layout_column_wrap(
      width = 1/2,
      card(
        card_header("Exámenes registrados"),
        DTOutput("tabla_examenes")
      ),
      card(
        card_header("Preguntas más repetidas"),
        DTOutput("tabla_top_uso")
      )
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  rv <- reactiveValues(
    seleccionadas  = integer(0),
    refresh_banco  = 0L,
    edit_id = NULL
  )

  # ── Actualizar filtros ────────────────────────────────────────────────────
  observeEvent(rv$refresh_banco, {
    con <- get_con()
    asigs <- dbGetQuery(con, "SELECT DISTINCT asignatura FROM preguntas
                               WHERE asignatura != '' ORDER BY asignatura")$asignatura
    temas <- dbGetQuery(con, "SELECT DISTINCT tema FROM preguntas
                               WHERE tema IS NOT NULL AND tema != '' ORDER BY tema")$tema
    dbDisconnect(con)
    updateSelectInput(session, "filtro_asig",
                      choices  = c("Todas" = "", asigs),
                      selected = isolate(input$filtro_asig))
    updateSelectInput(session, "filtro_tema",
                      choices  = c("Todos" = "", temas),
                      selected = isolate(input$filtro_tema))
  }, ignoreNULL = FALSE)

  # ── Guardar nueva pregunta ────────────────────────────────────────────────
  observeEvent(input$btn_guardar, {
    req(input$enunciado, input$opcion_a, input$opcion_b,
        input$opcion_c, input$opcion_d, input$asignatura)

    anyo_val <- tryCatch(as.integer(input$anyo), warning = function(e) NA_integer_)
    if (length(anyo_val) == 0 || is.na(anyo_val)) anyo_val <- NA_integer_

    con <- get_con()
    dbExecute(con,
      "INSERT INTO preguntas
         (asignatura, tema, enunciado, opcion_a, opcion_b, opcion_c, opcion_d,
          respuesta_correcta, dificultad, anyo, notas)
       VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
      params = list(
        txt(input$asignatura),
        txt(input$tema),
        txt(input$enunciado),
        txt(input$opcion_a),
        txt(input$opcion_b),
        txt(input$opcion_c),
        txt(input$opcion_d),
        input$respuesta_correcta,
        txt(input$dificultad),
        anyo_val,
        txt(input$notas)
      )
    )
    dbDisconnect(con)

    showNotification("✅ Pregunta guardada.", type = "message", duration = 3)
    rv$refresh_banco <- rv$refresh_banco + 1L

    updateTextAreaInput(session, "enunciado", value = "")
    updateTextAreaInput(session, "opcion_a",  value = "")
    updateTextAreaInput(session, "opcion_b",  value = "")
    updateTextAreaInput(session, "opcion_c",  value = "")
    updateTextAreaInput(session, "opcion_d",  value = "")
    updateTextInput(session,     "tema",      value = "")
    updateTextAreaInput(session, "notas",     value = "")
  })

  # ── Datos del banco ───────────────────────────────────────────────────────
  preguntas_bd <- reactive({
    rv$refresh_banco
    con <- get_con()
    df <- dbGetQuery(con, "
      SELECT p.id,
             p.asignatura,
             p.tema,
             p.enunciado,
             p.respuesta_correcta AS respuesta,
             p.dificultad,
             p.anyo,
             COUNT(u.examen_id) AS usos
      FROM preguntas p
      LEFT JOIN usos u ON p.id = u.pregunta_id
      GROUP BY p.id
      ORDER BY p.id DESC
    ")
    dbDisconnect(con)
    df
  })

  preguntas_filtradas <- reactive({
    df <- preguntas_bd()
    if (!is.null(input$filtro_asig) && nchar(input$filtro_asig))
      df <- df[df$asignatura == input$filtro_asig, ]
    if (!is.null(input$filtro_tema) && nchar(input$filtro_tema))
      df <- df[!is.na(df$tema) & df$tema == input$filtro_tema, ]
    if (!is.null(input$filtro_dif)  && nchar(input$filtro_dif))
      df <- df[!is.na(df$dificultad) & df$dificultad == input$filtro_dif, ]
    df
  })

  output$tabla_banco <- renderDT({
    df <- preguntas_filtradas()
    df$enunciado <- ifelse(nchar(df$enunciado) > 90,
                           paste0(substr(df$enunciado, 1, 90), "…"), df$enunciado)
    datatable(df, selection = "multiple", rownames = FALSE,
              options = list(pageLength = 20, scrollX = TRUE, dom = "fti"),
              colnames = c("ID","Asignatura","Tema","Enunciado","R✓","Dif.","Año","Usos")) |>
      formatStyle("usos", backgroundColor =
                    styleInterval(c(0, 2), c("white", "#fff3cd", "#f8d7da")))
  })

  # ── Añadir al examen en curso ─────────────────────────────────────────────
  observeEvent(input$btn_add, {
    filas <- input$tabla_banco_rows_selected
    if (!length(filas)) {
      showNotification("Selecciona al menos una pregunta.", type = "warning")
      return()
    }
    nuevas <- preguntas_filtradas()$id[filas]
    rv$seleccionadas <- unique(c(rv$seleccionadas, nuevas))
    showNotification(paste0("➕ ", length(nuevas), " añadida(s). Total: ",
                            length(rv$seleccionadas)), type = "message", duration = 3)
  })

  observeEvent(input$btn_limpiar, {
    rv$seleccionadas  <- integer(0)
    rv$texto_generado <- ""
  })

  # ── Abrir modal de edición ────────────────────────────────────────────────
  observeEvent(input$btn_editar, {
    filas <- input$tabla_banco_rows_selected
    if (length(filas) != 1) {
      showNotification("Selecciona exactamente una pregunta para editar.", type = "warning")
      return()
    }
    id_sel <- preguntas_filtradas()$id[filas]
    con <- get_con()
    q <- dbGetQuery(con, "SELECT * FROM preguntas WHERE id = ?", params = list(id_sel))
    dbDisconnect(con)

    showModal(modalDialog(
      title  = paste0("Editar pregunta #", id_sel),
      size   = "l",
      footer = tagList(
        modalButton("Cancelar"),
        actionButton("btn_guardar_edicion", "💾 Guardar cambios", class = "btn-primary")
      ),
      rv$edit_id <- id_sel,
      layout_column_wrap(
        width = 1/2,
        textInput("edit_asignatura", "Asignatura *", value = q$asignatura),
        textInput("edit_tema",       "Tema",         value = q$tema %||% "")
      ),
      textAreaInput("edit_enunciado", "Enunciado *", value = q$enunciado,
                    width = "100%", rows = 3),
      layout_column_wrap(
        width = 1/2,
        textAreaInput("edit_opcion_a", "Opción A *", value = q$opcion_a, rows = 2),
        textAreaInput("edit_opcion_b", "Opción B *", value = q$opcion_b, rows = 2),
        textAreaInput("edit_opcion_c", "Opción C *", value = q$opcion_c, rows = 2),
        textAreaInput("edit_opcion_d", "Opción D *", value = q$opcion_d, rows = 2)
      ),
      layout_column_wrap(
        width = 1/3,
        selectInput("edit_respuesta", "Respuesta correcta *",
                    choices  = c("A","B","C","D"),
                    selected = q$respuesta_correcta),
        selectInput("edit_dificultad", "Dificultad",
                    choices  = c("— sin especificar —" = "", "Baja","Media","Alta"),
                    selected = if (is.na(q$dificultad)) "" else q$dificultad),
        numericInput("edit_anyo", "Año",
                     value = if (is.na(q$anyo)) as.integer(format(Sys.Date(),"%Y")) else q$anyo,
                     min = 2000, max = 2100, step = 1)
      ),
      textAreaInput("edit_notas", "Notas", value = q$notas %||% "", rows = 2)
    ))
  })

  # ── Guardar edición ───────────────────────────────────────────────────────
  observeEvent(input$btn_guardar_edicion, {
    req(input$edit_enunciado, input$edit_opcion_a, input$edit_opcion_b,
        input$edit_opcion_c,  input$edit_opcion_d, input$edit_asignatura)

    id_edit  <- as.integer(rv$edit_id)
    anyo_val <- tryCatch(as.integer(input$edit_anyo), warning = function(e) NA_integer_)
    if (length(anyo_val) == 0 || is.na(anyo_val)) anyo_val <- NA_integer_

    con <- get_con()
    dbExecute(con,
      "UPDATE preguntas SET
         asignatura = ?, tema = ?, enunciado = ?,
         opcion_a = ?, opcion_b = ?, opcion_c = ?, opcion_d = ?,
         respuesta_correcta = ?, dificultad = ?, anyo = ?, notas = ?
       WHERE id = ?",
      params = list(
        txt(input$edit_asignatura),
        txt(input$edit_tema),
        txt(input$edit_enunciado),
        txt(input$edit_opcion_a),
        txt(input$edit_opcion_b),
        txt(input$edit_opcion_c),
        txt(input$edit_opcion_d),
        input$edit_respuesta,
        txt(input$edit_dificultad),
        anyo_val,
        txt(input$edit_notas),
        id_edit
      )
    )
    dbDisconnect(con)

    removeModal()
    showNotification("✅ Pregunta actualizada.", type = "message", duration = 3)
    rv$refresh_banco <- rv$refresh_banco + 1L
  })

  output$n_seleccionadas <- renderText({
    n <- length(rv$seleccionadas)
    if (n == 0) "Sin preguntas aún" else paste0(n, " pregunta(s)")
  })

  # ── Preguntas seleccionadas ───────────────────────────────────────────────
  preguntas_sel_df <- reactive({
    ids <- rv$seleccionadas
    if (!length(ids)) return(data.frame())
    con <- get_con()
    df <- dbGetQuery(con, sprintf(
      "SELECT id, tema, enunciado, respuesta_correcta AS respuesta,
              dificultad, opcion_a, opcion_b, opcion_c, opcion_d
       FROM preguntas WHERE id IN (%s)",
      paste(ids, collapse = ",")))
    dbDisconnect(con)
    df[match(ids, df$id), ]
  })

  output$tabla_seleccion <- renderDT({
    df <- preguntas_sel_df()
    if (!nrow(df))
      return(datatable(data.frame(Mensaje = "Sin preguntas seleccionadas")))
    mostrar <- df[, c("id","tema","enunciado","respuesta","dificultad")]
    mostrar$enunciado <- ifelse(nchar(mostrar$enunciado) > 70,
                                paste0(substr(mostrar$enunciado, 1, 70), "…"),
                                mostrar$enunciado)
    datatable(mostrar, rownames = FALSE, selection = "none",
              options = list(pageLength = 10, dom = "ti"),
              colnames = c("ID","Tema","Enunciado","R✓","Dif."))
  })

  # ── Generar texto Word ────────────────────────────────────────────────────
  observeEvent(input$btn_generar, {
    df <- preguntas_sel_df()
    if (!nrow(df)) {
      showNotification("No hay preguntas seleccionadas.", type = "warning")
      return()
    }
    lineas <- character(0)
    for (i in seq_len(nrow(df))) {
      r <- df[i, ]
      lineas <- c(lineas,
        paste0("**", i, ". ", r$enunciado, "**"),
        paste0("- [ ] ", r$opcion_a),
        paste0("- [ ] ", r$opcion_b),
        paste0("- [ ] ", r$opcion_c),
        paste0("- [ ] ", r$opcion_d),
        "")
    }
    rv$texto_generado <- paste(lineas, collapse = "\n")
  })

  output$texto_word <- renderText({ rv$texto_generado })

  # ── Guardar examen ────────────────────────────────────────────────────────
  observeEvent(input$btn_guardar_examen, {
    req(input$nombre_examen)
    ids <- rv$seleccionadas
    if (!length(ids)) {
      showNotification("No hay preguntas seleccionadas.", type = "warning")
      return()
    }
    con <- get_con()
    dbExecute(con,
      "INSERT INTO examenes (nombre, asignatura) VALUES (?, ?)",
      params = list(trimws(input$nombre_examen), txt(input$asig_examen)))
    exam_id <- dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id
    for (pid in ids)
      dbExecute(con,
        "INSERT OR IGNORE INTO usos (pregunta_id, examen_id) VALUES (?, ?)",
        params = list(pid, exam_id))
    dbDisconnect(con)

    showNotification(paste0("✅ '", input$nombre_examen, "' guardado con ",
                            length(ids), " preguntas."), type = "message", duration = 5)
    rv$refresh_banco  <- rv$refresh_banco + 1L
    rv$seleccionadas  <- integer(0)
    rv$texto_generado <- ""
    updateTextInput(session, "nombre_examen", value = "")
    updateTextInput(session, "asig_examen",   value = "")
  })

  # ── Historial ─────────────────────────────────────────────────────────────
  output$tabla_examenes <- renderDT({
    rv$refresh_banco
    con <- get_con()
    df <- dbGetQuery(con, "
      SELECT e.id, e.nombre, e.asignatura, e.fecha,
             COUNT(u.pregunta_id) AS n_preguntas
      FROM examenes e
      LEFT JOIN usos u ON e.id = u.examen_id
      GROUP BY e.id ORDER BY e.id DESC")
    dbDisconnect(con)
    datatable(df, rownames = FALSE,
              options = list(pageLength = 10, dom = "ti"),
              colnames = c("ID","Nombre","Asignatura","Fecha","Nº preguntas"))
  })

  output$tabla_top_uso <- renderDT({
    rv$refresh_banco
    con <- get_con()
    df <- dbGetQuery(con, "
      SELECT p.id, p.asignatura, p.tema,
             CASE WHEN LENGTH(p.enunciado) > 80
                  THEN SUBSTR(p.enunciado,1,80)||'…' ELSE p.enunciado END AS enunciado,
             COUNT(u.examen_id) AS usos
      FROM preguntas p
      INNER JOIN usos u ON p.id = u.pregunta_id
      GROUP BY p.id ORDER BY usos DESC LIMIT 30")
    dbDisconnect(con)
    datatable(df, rownames = FALSE,
              options = list(pageLength = 10, dom = "ti"),
              colnames = c("ID","Asignatura","Tema","Enunciado","Usos")) |>
      formatStyle("usos", backgroundColor =
                    styleInterval(c(1, 3), c("#d4edda", "#fff3cd", "#f8d7da")))
  })
}

shinyApp(ui, server)
