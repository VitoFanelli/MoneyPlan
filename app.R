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

  # Capital button top-right
  nav_item(
    actionButton(
      "btn_cap_main",
      icon("gear"),
      class = "btn btn-warning btn-sm rounded-circle",
      style = "width:36px;height:36px;padding:0;",
      title = "Capitale iniziale"
    )
  )

)

# ── Server ───────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # ── Shared state ────────────────────────────────────────────────────────────
  rv <- reactiveValues(
    entrate          = load_entrate(),
    uscite           = load_uscite(),
    capitale_iniziale = load_capitale()
  )

  # ── Shared computations ─────────────────────────────────────────────────────
  mensile     <- reactive(df_mensile(rv$entrate, rv$uscite))
  capitale_df <- reactive(df_capitale(mensile(), rv$capitale_iniziale))

  # ── Capitale iniziale — global button ──────────────────────────────────────
  observeEvent(input$btn_cap_main, {
    showModal(modalDialog(
      title     = "Capitale iniziale",
      easyClose = TRUE,
      footer    = tagList(
        modalButton("Annulla"),
        actionButton("ok_cap_main", "Salva", class = "btn btn-primary")
      ),
      numericInputIcon(
        "cap_main_val",
        label = "Capitale di partenza (€)",
        value = rv$capitale_iniziale,
        min   = 0,
        step  = 100,
        icon  = list(icon("euro-sign"))
      )
    ))
  })

  observeEvent(input$ok_cap_main, {

    val <- input$cap_main_val
    req(!is.null(val), val >= 0)
    rv$capitale_iniziale <- val
    save_capitale(val)
    removeModal()

  })

  # ── Modules ─────────────────────────────────────────────────────────────────
  mod_dashboard_server("dash", rv, mensile, capitale_df)
  mod_entrate_uscite_server("eu", rv)
  mod_vista_annuale_server("va", rv, mensile, capitale_df)
  mod_simulazione_server("sim",  rv, mensile, capitale_df)
}

shinyApp(ui, server)
