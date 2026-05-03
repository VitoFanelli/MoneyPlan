mod_vista_annuale_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "container-fluid py-4",
    div(
      class = "d-flex align-items-center justify-content-between mb-3",
      h4(class = "fw-semibold mb-0", "Vista Annuale"),
      div(
        class = "cap-init-box",
        span(class = "cap-init-label", "Capitale iniziale"),
        actionButton(
          ns("edit_cap"),
          textOutput(ns("cap_init_lbl"), inline = TRUE),
          class = "btn btn-outline-secondary btn-sm ms-2"
        )
      )
    ),

    # KPI summary
    div(
      class = "row g-3 mb-4",
      div(class = "col-sm-6 col-lg-3",
        div(class = "va-kpi",
          div(class = "va-kpi-label", "Totale entrate"),
          div(class = "va-kpi-value text-success", textOutput(ns("tot_e")))
        )
      ),
      div(class = "col-sm-6 col-lg-3",
        div(class = "va-kpi",
          div(class = "va-kpi-label", "Totale uscite"),
          div(class = "va-kpi-value text-danger", textOutput(ns("tot_u")))
        )
      ),
      div(class = "col-sm-6 col-lg-3",
        div(class = "va-kpi",
          div(class = "va-kpi-label", "Risparmio totale"),
          div(class = "va-kpi-value", textOutput(ns("risp")))
        )
      ),
      div(class = "col-sm-6 col-lg-3",
        div(class = "va-kpi va-kpi--accent",
          div(class = "va-kpi-label", "Capitale finale"),
          div(class = "va-kpi-value", textOutput(ns("cap_fin")))
        )
      )
    ),

    # Month grid
    uiOutput(ns("mese_grid"))
  )
}

mod_vista_annuale_server <- function(id, rv, mensile, capitale_df) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$cap_init_lbl <- renderText(fmt_eur(rv$capitale_iniziale))

    output$tot_e <- renderText({
      fmt_eur(sum(mensile()$tot_e))
    })
    output$tot_u <- renderText({
      fmt_eur(sum(mensile()$tot_u))
    })
    output$risp <- renderText({
      s <- sum(mensile()$saldo)
      col <- col_saldo(s)
      fmt_eur(s)
    })
    output$cap_fin <- renderText({
      fmt_eur(capitale_df()$capitale[12])
    })

    # Month grid
    output$mese_grid <- renderUI({
      m   <- mensile()
      cap <- capitale_df()

      rows <- lapply(seq(1, 12, by = 4), function(start) {
        cols <- lapply(start:min(start + 3, 12), function(i) {
          saldo_i <- m$saldo[i]
          cap_i   <- cap$capitale[i]
          is_cur  <- (i == mese_corrente)
          is_fut  <- (i >= mese_corrente)
          col_s   <- col_saldo(saldo_i)
          pct     <- if (m$tot_e[i] > 0)
            min(100, round(m$tot_u[i] / m$tot_e[i] * 100))
          else 0

          div(
            class = "col",
            div(
              class = paste("month-card", if (is_cur) "month-card--current"),
              # Header
              div(
                class = "month-card-header",
                span(class = "month-name", MESI_BREVI[i]),
                NULL
              ),
              # Body
              div(class = "month-row",
                span(class = "month-row-label", "Entrate"),
                span(class = "month-row-val text-success",
                  if (m$tot_e[i] > 0) fmt_eur(m$tot_e[i]) else "—")
              ),
              div(class = "month-row",
                span(class = "month-row-label", "Uscite"),
                span(class = "month-row-val text-danger",
                  if (m$tot_u[i] > 0) fmt_eur(m$tot_u[i]) else "—")
              ),
              tags$hr(class = "my-2"),
              div(class = "month-row",
                span(class = "month-row-label fw-semibold", "Saldo"),
                span(
                  class = "month-row-val fw-bold",
                  style = paste0("color:", col_s),
                  if (is_fut && m$tot_e[i] == 0 && m$tot_u[i] == 0)
                    "previsto"
                  else
                    fmt_saldo(saldo_i)
                )
              ),
              # Progress bar (uscite / entrate)
              if (m$tot_e[i] > 0 || m$tot_u[i] > 0) {
                div(
                  class = "progress mt-2",
                  style = "height: 5px;",
                  div(
                    class = paste(
                      "progress-bar",
                      if (pct <= 80) "bg-success"
                      else if (pct <= 100) "bg-warning"
                      else "bg-danger"
                    ),
                    style = paste0("width:", min(pct, 100), "%"),
                    role  = "progressbar"
                  )
                )
              } else NULL,
              # Capitale
              div(
                class = "month-cap mt-2",
                "Capitale: ",
                span(class = "fw-semibold", fmt_eur(cap_i))
              )
            )
          )
        })
        div(class = "row row-cols-4 g-3 mb-3", cols)
      })
      tagList(rows)
    })

    # Edit capitale iniziale
    observeEvent(input$edit_cap, {
      showModal(modalDialog(
        title = "Capitale iniziale",
        easyClose = TRUE,
        footer = tagList(
          modalButton("Annulla"),
          actionButton(ns("salva_cap"), "Salva", class = "btn btn-primary")
        ),
        numericInputIcon(
          ns("cap_input"),
          label = "Capitale di partenza (€)",
          value = rv$capitale_iniziale,
          min   = 0,
          step  = 100,
          icon  = list(icon("euro-sign"))
        )
      ))
    })

    observeEvent(input$salva_cap, {
      val <- input$cap_input
      req(!is.null(val), val >= 0)
      rv$capitale_iniziale <- val
      save_capitale(val)
      removeModal()
    })
  })
}
