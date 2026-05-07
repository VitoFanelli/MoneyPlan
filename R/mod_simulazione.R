mod_simulazione_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "container-fluid py-4",
    h4(class = "fw-semibold mb-3", "Simulazione scenari"),

    # Scenario cards
    div(
      class = "row g-3 mb-4",
      div(
        class = "col-md-5",
        div(
          class = "scenario-card scenario-card--base",
          div(class = "scenario-label", "Scenario base"),
          div(class = "row mt-2",
            div(class = "col-6",
              div(class = "sc-sub", "Capitale fine anno"),
              div(class = "sc-val", textOutput(ns("base_cap"), inline = TRUE))
            ),
            div(class = "col-6",
              div(class = "sc-sub", "Risparmio"),
              div(class = "sc-val", textOutput(ns("base_risp"), inline = TRUE))
            )
          )
        )
      ),
      div(
        class = "col-md-5",
        div(
          class = "scenario-card scenario-card--sim",
          div(class = "scenario-label", "Simulazione attiva"),
          div(class = "row mt-2",
            div(class = "col-6",
              div(class = "sc-sub", "Capitale fine anno"),
              div(class = "sc-val", textOutput(ns("sim_cap"), inline = TRUE))
            ),
            div(class = "col-6",
              div(class = "sc-sub", "Risparmio"),
              div(class = "sc-val", textOutput(ns("sim_risp"), inline = TRUE))
            )
          )
        )
      )
    ),

    # Editor + Chart
    div(
      class = "row g-3",

      # Left: editor
      div(
        class = "col-lg-5",
        card(
          card_header("Modifica simulazione"),
          div(
            class = "p-3",

            h6(class = "text-success mb-2", icon("arrow-up"), " Entrate mensili"),
            uiOutput(ns("editor_e")),

            tags$hr(),

            h6(class = "text-danger mb-2", icon("arrow-down"), " Uscite mensili"),
            uiOutput(ns("editor_u")),

            div(
              class = "d-flex gap-2 mt-3",
              actionButton(ns("reset_sim"), "↺ Reset", class = "btn btn-outline-secondary"),
              actionButton(ns("ricalcola"),  "▶ Ricalcola", class = "btn btn-warning fw-bold flex-fill")
            )
          )
        )
      ),

      # Right: comparison + delta
      div(
        class = "col-lg-7",
        card(
          class = "mb-3",
          card_header("Confronto scenari"),
          highchartOutput(ns("chart_confronto"), height = "280px")
        ),
        card(
          card_header("Riepilogo variazioni"),
          uiOutput(ns("riepilogo"))
        )
      )
    )
  )
}

mod_simulazione_server <- function(id, rv, mensile, capitale_df) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive: average per categoria from actual data
    base_e_avgs <- reactive({
      avg_mensile_per_categoria(rv$entrate)
    })
    base_u_avgs <- reactive({
      avg_mensile_per_categoria(rv$uscite)
    })

    # Simulation state (initialized from base averages)
    sim_vals <- reactiveValues(e = NULL, u = NULL, computed = FALSE)

    observe({
      # Initialize sim vals when base changes and sim not yet set
      if (!sim_vals$computed) {
        be <- base_e_avgs()
        bu <- base_u_avgs()
        te <- rv$tipi_entrate
        tu <- rv$tipi_uscite
        e_init <- setNames(
          sapply(te, function(t) { v <- be[t]; if (length(v)==1 && !is.na(v)) as.numeric(v) else 0 }),
          te
        )
        u_init <- setNames(
          sapply(tu, function(t) { v <- bu[t]; if (length(v)==1 && !is.na(v)) as.numeric(v) else 0 }),
          tu
        )
        sim_vals$e <- e_init
        sim_vals$u <- u_init
        sim_vals$computed <- TRUE
      }
    })

    # Reset manuale
    observeEvent(input$reset_sim, {
      sim_vals$computed <- FALSE
    })

    # Reset automatico quando cambiano le tipologie
    observeEvent({rv$tipi_entrate; rv$tipi_uscite}, {
      sim_vals$computed <- FALSE
    }, ignoreInit = TRUE)

    # Ricalcola — read input values and update sim_vals
    observeEvent(input$ricalcola, {
      te <- rv$tipi_entrate
      tu <- rv$tipi_uscite
      new_e <- setNames(
        sapply(te, function(t) {
          val <- input[[paste0("sim_e_", t)]]
          if (is.null(val) || is.na(val)) 0 else as.numeric(val)
        }),
        te
      )
      new_u <- setNames(
        sapply(tu, function(t) {
          val <- input[[paste0("sim_u_", t)]]
          if (is.null(val) || is.na(val)) 0 else as.numeric(val)
        }),
        tu
      )
      sim_vals$e <- new_e
      sim_vals$u <- new_u
    })

    # Computed sim mensile
    sim_mensile_rv <- reactive({
      req(sim_vals$e, sim_vals$u)
      sim_df_mensile(sim_vals$e, sim_vals$u)
    })

    sim_capitale_rv <- reactive({
      df_capitale(sim_mensile_rv(), rv$capitale_iniziale)
    })

    # ── KPIs ──────────────────────────────────────────────────────────────────
    output$base_cap <- renderText({
      fmt_eur(capitale_df()$capitale[12])
    })
    output$base_risp <- renderText({
      fmt_eur(sum(mensile()$saldo))
    })
    output$sim_cap <- renderText({
      req(sim_vals$computed)
      fmt_eur(sim_capitale_rv()$capitale[12])
    })
    output$sim_risp <- renderText({
      req(sim_vals$computed)
      fmt_eur(sum(sim_mensile_rv()$saldo))
    })

    # ── Editor Entrate ─────────────────────────────────────────────────────────
    output$editor_e <- renderUI({
      req(sim_vals$e)
      be <- base_e_avgs()
      te <- rv$tipi_entrate
      div(
        lapply(te, function(t) {
          base_v <- if (!is.null(be[t]) && !is.na(be[t])) round(be[t]) else 0
          sim_v  <- if (!is.null(sim_vals$e[[t]])) round(sim_vals$e[[t]]) else 0
          div(
            class = "sim-row",
            span(class = "sim-label", t),
            span(class = "sim-from text-muted small",
              if (base_v > 0) paste0("€ ", base_v) else "—"),
            span(class = "sim-arrow", "→"),
            div(
              class = "sim-to",
              numericInput(
                ns(paste0("sim_e_", t)),
                label  = NULL,
                value  = sim_v,
                min    = 0,
                step   = 50,
                width  = "110px"
              )
            )
          )
        })
      )
    })

    # ── Editor Uscite ──────────────────────────────────────────────────────────
    output$editor_u <- renderUI({
      req(sim_vals$u)
      bu <- base_u_avgs()
      tu <- rv$tipi_uscite
      div(
        lapply(tu, function(t) {
          base_v <- if (!is.null(bu[t]) && !is.na(bu[t])) round(bu[t]) else 0
          sim_v  <- if (!is.null(sim_vals$u[[t]])) round(sim_vals$u[[t]]) else 0
          div(
            class = "sim-row",
            span(class = "sim-label", t),
            span(class = "sim-from text-muted small",
              if (base_v > 0) paste0("€ ", base_v) else "—"),
            span(class = "sim-arrow", "→"),
            div(
              class = "sim-to",
              numericInput(
                ns(paste0("sim_u_", t)),
                label  = NULL,
                value  = sim_v,
                min    = 0,
                step   = 50,
                width  = "110px"
              )
            )
          )
        })
      )
    })

    # ── Comparison chart ───────────────────────────────────────────────────────
    output$chart_confronto <- renderHighchart({
      req(sim_vals$computed)
      base <- capitale_df()
      sim  <- sim_capitale_rv()

      highchart() |>
        hc_chart(type = "line") |>
        hc_xAxis(categories = MESI_BREVI) |>
        hc_yAxis(title = list(text = "€"), labels = list(format = "€{value}")) |>
        hc_add_series(
          name      = "Scenario base",
          data      = base$capitale,
          color     = "#1e3a5f",
          marker    = list(enabled = TRUE, radius = 4),
          dashStyle = "Solid"
        ) |>
        hc_add_series(
          name      = "Simulazione",
          data      = sim$capitale,
          color     = "#f5a623",
          marker    = list(enabled = TRUE, radius = 4),
          dashStyle = "ShortDash"
        ) |>
        hc_tooltip(shared = TRUE, valuePrefix = "€ ", valueDecimals = 0) |>
        hc_legend(enabled = TRUE) |>
        hc_credits(enabled = FALSE)
    })

    # ── Riepilogo ─────────────────────────────────────────────────────────────
    output$riepilogo <- renderUI({
      req(sim_vals$computed)
      be  <- base_e_avgs()
      bu  <- base_u_avgs()
      se  <- sim_vals$e
      su  <- sim_vals$u

      # Total comparisons
      base_tot_e <- sum(mensile()$tot_e)
      base_tot_u <- sum(mensile()$tot_u)
      sim_tot_e  <- sum(se) * 12
      sim_tot_u  <- sum(su) * 12

      # Worst month (most negative delta)
      base_cap <- capitale_df()
      sim_cap  <- sim_capitale_rv()
      deltas   <- sim_cap$capitale - base_cap$capitale
      worst_i  <- which.min(deltas)

      div(
        class = "p-3",
        if (min(deltas) < 0) {
          div(
            class = "alert alert-warning mb-3 py-2",
            tags$b(paste0(MESI_LUNGHI[worst_i], " ⚠")),
            br(),
            tags$small(
              sprintf(
                "Capitale sim. - base: %s%s",
                if (deltas[worst_i] < 0) "−" else "+",
                fmt_eur(abs(deltas[worst_i]))
              )
            )
          )
        } else NULL,

        div(class = "riep-row",
          span("Entrate totali"),
          span(
            class = "text-muted small",
            sprintf("%s → ", fmt_eur(base_tot_e))
          ),
          span(
            class = if (sim_tot_e >= base_tot_e) "text-success fw-bold" else "text-danger fw-bold",
            fmt_eur(sim_tot_e)
          )
        ),
        div(class = "riep-row",
          span("Uscite totali"),
          span(
            class = "text-muted small",
            sprintf("%s → ", fmt_eur(base_tot_u))
          ),
          span(
            class = if (sim_tot_u <= base_tot_u) "text-success fw-bold" else "text-danger fw-bold",
            fmt_eur(sim_tot_u)
          )
        ),
        div(class = "riep-row mt-2",
          span("Risparmio simulato"),
          span(class = "fw-bold", fmt_eur(sum(sim_mensile_rv()$saldo)))
        )
      )
    })
  })
}
