library(shiny)
library(bslib)
library(highcharter)
library(dplyr)
library(tidyr)
library(arrow)
library(shinyWidgets)

# ── Source modules ──────────────────────────────────────────────────────────
source("R/globals.R")
source("R/calculations.R")
source("R/data_utils.R")
source("R/mod_dashboard.R")
source("R/mod_entrate_uscite.R")
source("R/mod_vista_annuale.R")
source("R/mod_simulazione.R")

# ── Theme ───────────────────────────────────────────────────────────────────
tema <- bs_theme(
  version    = 5,
  bg         = "#f0f2f5",
  fg         = "#212529",
  primary    = "#1e3a5f",
  secondary  = "#6c757d",
  success    = "#27ae60",
  danger     = "#e74c3c",
  warning    = "#f5a623",
  base_font  = font_google("Inter"),
  heading_font = font_google("Inter")
)

# ── UI ───────────────────────────────────────────────────────────────────────
ui <- page_navbar(

  id    = "nav",

  title = tags$span(
    tags$i(class = "bi bi-coin-stack"),
    " MoneyPlan"
  ),

  theme = tema,

  header = tagList(
    tags$link(
      rel  = "stylesheet",
      href = "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.3/font/bootstrap-icons.min.css"
    ),
    tags$link(rel = "stylesheet", href = "style.css")
  ),

  nav_panel("Dashboard",      mod_dashboard_ui("dash")),
  nav_panel("Entrate/Uscite", mod_entrate_uscite_ui("eu")),
  nav_panel("Vista Annuale",  mod_vista_annuale_ui("va")),
  nav_panel("Simulazione",    mod_simulazione_ui("sim")),

  nav_spacer(),

  nav_item(
    actionButton(
      "btn_cap_main",
      icon("gear"),
      class = "btn btn-warning btn-sm rounded-circle",
      style = "width:36px;height:36px;padding:0;",
      title = "Configurazione"
    )
  )

)

# ── Server ───────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # ── Shared state ────────────────────────────────────────────────────────────
  rv <- reactiveValues(
    entrate           = load_entrate(),
    uscite            = load_uscite(),
    capitale_iniziale = load_capitale(),
    tipi              = load_tipi(),
    tipi_entrate      = character(0),
    tipi_uscite       = character(0),
    colori_uscite     = character(0)
  )

  # Derive tipi_entrate, tipi_uscite, colori_uscite from rv$tipi
  observe({
    tipi             <- rv$tipi
    rv$tipi_entrate  <- tipi |> filter(tipo == "entrata") |> pull(nome)
    rv$tipi_uscite   <- tipi |> filter(tipo == "uscita")  |> pull(nome)
    tu               <- tipi |> filter(tipo == "uscita")
    rv$colori_uscite <- setNames(tu$colore, tu$nome)
  })

  # ── Shared computations ─────────────────────────────────────────────────────
  mensile     <- reactive(df_mensile(rv$entrate, rv$uscite))
  capitale_df <- reactive(df_capitale(mensile(), rv$capitale_iniziale))

  # ── Configurazione — modal ─────────────────────────────────────────────────
  observeEvent(input$btn_cap_main, {
    showModal(modalDialog(
      title      = "Configurazione",
      easyClose  = TRUE,
      size       = "l",
      scrollable = TRUE,
      footer     = modalButton("Chiudi"),

      # ── 1. Capitale ─────────────────────────────────────────────────────────
      h6(class = "fw-semibold border-bottom pb-2 mb-3", "Capitale di partenza"),
      div(
        class = "d-flex align-items-end gap-3 mb-4",
        div(
          style = "flex:1",
          numericInputIcon(
            "cap_main_val",
            label = "Importo (€)",
            value = rv$capitale_iniziale,
            min   = 0,
            step  = 100,
            icon  = list(icon("euro-sign"))
          )
        ),
        div(
          style = "padding-bottom:1px",
          actionButton("ok_cap_main", "Salva", class = "btn btn-primary")
        )
      ),

      # ── 2. Crea tipologia ───────────────────────────────────────────────────
      h6(class = "fw-semibold border-bottom pb-2 mb-3", "Aggiungi tipologia"),
      div(
        class = "row g-2 align-items-end mb-4",
        div(
          class = "col-auto",
          pickerInput(
            "tipo_nuovo",
            label   = "Tipo",
            choices = c("Entrata" = "entrata", "Uscita" = "uscita"),
            options = pickerOptions(style = "btn-outline-secondary")
          )
        ),
        div(
          class = "col",
          textInput("nome_nuovo", "Nome", placeholder = "es. Freelance")
        ),
        div(
          class = "col-auto",
          div(
            style = "margin-top:1.65rem",
            actionButton("crea_tipo", "Crea", class = "btn btn-success")
          )
        )
      ),

      # ── 3. Lista tipologie ──────────────────────────────────────────────────
      h6(class = "fw-semibold border-bottom pb-2 mb-2", "Tipologie esistenti"),
      uiOutput("lista_tipi_modal")
    ))
  })

  # Lista tipologie nel modal (aggiornata reattivamente)
  output$lista_tipi_modal <- renderUI({
    tipi <- rv$tipi

    build_row <- function(n, tipo_val) {
      div(
        class = "d-flex align-items-center gap-2 py-1",
        style = "border-bottom:1px solid #f0f2f5",
        span(class = "flex-grow-1 small", n),
        tags$button(
          type    = "button",
          class   = "btn btn-sm btn-outline-danger py-0 px-2",
          onclick = sprintf(
            "Shiny.setInputValue('del_tipo',{tipo:'%s',nome:'%s'},{priority:'event'})",
            tipo_val, n
          ),
          icon("trash")
        )
      )
    }

    te <- tipi |> filter(tipo == "entrata") |> pull(nome) |> sort()
    tu <- tipi |> filter(tipo == "uscita")  |> pull(nome) |> sort()

    div(
      class = "row g-3",
      div(
        class = "col-6",
        div(class = "text-success fw-semibold small mb-1",
            icon("arrow-up"), " Entrate"),
        if (length(te) > 0)
          lapply(te, build_row, tipo_val = "entrata")
        else
          p(class = "text-muted small mb-0", "Nessuna tipologia")
      ),
      div(
        class = "col-6",
        div(class = "text-danger fw-semibold small mb-1",
            icon("arrow-down"), " Uscite"),
        if (length(tu) > 0)
          lapply(tu, build_row, tipo_val = "uscita")
        else
          p(class = "text-muted small mb-0", "Nessuna tipologia")
      )
    )
  })

  # ── Salva capitale ─────────────────────────────────────────────────────────
  observeEvent(input$ok_cap_main, {
    val <- input$cap_main_val
    req(!is.null(val), val >= 0)
    rv$capitale_iniziale <- val
    save_capitale(val)
    showNotification("Capitale salvato.", type = "message")
  })

  # ── Crea tipologia ─────────────────────────────────────────────────────────
  observeEvent(input$crea_tipo, {
    tipo_val <- input$tipo_nuovo
    nome_val <- trimws(input$nome_nuovo)

    if (nchar(nome_val) == 0) {
      showNotification("Inserisci un nome per la tipologia.", type = "warning")
      return()
    }
    if (grepl("'", nome_val, fixed = TRUE)) {
      showNotification("Il nome non può contenere apostrofi.", type = "warning")
      return()
    }

    esistenti <- rv$tipi |> filter(tipo == tipo_val) |> pull(nome)
    if (nome_val %in% esistenti) {
      showNotification("Questa tipologia esiste già.", type = "warning")
      return()
    }

    colore   <- next_colore(tipo_val, rv$tipi)
    rv$tipi  <- bind_rows(rv$tipi, tibble(tipo = tipo_val, nome = nome_val, colore = colore))
    save_tipi(rv$tipi)
    updateTextInput(session, "nome_nuovo", value = "")
    showNotification(paste0("Tipologia '", nome_val, "' aggiunta."), type = "message")
  })

  # ── Elimina tipologia (con cascade delete dei dati) ────────────────────────
  observeEvent(input$del_tipo, {
    req(input$del_tipo)
    tipo_val <- input$del_tipo$tipo
    nome_val <- input$del_tipo$nome

    rv$tipi <- rv$tipi |> filter(!(tipo == tipo_val & nome == nome_val))
    save_tipi(rv$tipi)

    if (tipo_val == "entrata") {
      rv$entrate <- rv$entrate |> filter(tipologia != nome_val)
      save_entrate(rv$entrate)
    } else {
      rv$uscite <- rv$uscite |> filter(tipologia != nome_val)
      save_uscite(rv$uscite)
    }

    showNotification(
      paste0("'", nome_val, "' eliminata con i relativi dati."),
      type = "message"
    )
  })

  # ── Modules ─────────────────────────────────────────────────────────────────
  mod_dashboard_server("dash", rv, mensile, capitale_df)
  mod_entrate_uscite_server("eu", rv)
  mod_vista_annuale_server("va", rv, mensile, capitale_df)
  mod_simulazione_server("sim",  rv, mensile, capitale_df)
}

shinyApp(ui, server)
