mod_entrate_uscite_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "container-fluid py-4",
    h4(class = "mb-3 fw-semibold", "Entrate & Uscite — Anno"),
    uiOutput(ns("accordion"))
  )
}

mod_entrate_uscite_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    mese_aperto <- reactiveVal(mese_corrente)

    # ── Accordion ─────────────────────────────────────────────────────────────
    output$accordion <- renderUI({
      m_ap <- mese_aperto()
      lapply(1:12, function(i) {
        e_m <- rv$entrate |> filter(mese == i)
        u_m <- rv$uscite  |> filter(mese == i)
        tot_e <- sum(e_m$importo)
        tot_u <- sum(u_m$importo)
        is_open <- identical(m_ap, i)
        is_cur  <- (i == mese_corrente)

        header <- div(
          class = "acc-header",
          onclick = sprintf(
            "event.stopPropagation(); Shiny.setInputValue('%s', %d, {priority:'event'})",
            ns("toggle"), i
          ),
          div(
            class = "acc-title",
            span(if (is_open) "▼ " else "▶ "),
            span(MESI_LUNGHI[i]),
            if (is_cur) span(class = "badge-corrente ms-2", "corrente") else NULL
          ),
          div(
            class = "acc-totals",
            if (tot_e > 0) span(class = "text-success me-3", paste0("↑ ", fmt_eur(tot_e))) else NULL,
            if (tot_u > 0) span(class = "text-danger",  paste0("↓ ", fmt_eur(tot_u))) else NULL
          )
        )

        body <- if (is_open) {
          # Entrate list
          e_rows <- if (nrow(e_m) > 0) {
            lapply(seq_len(nrow(e_m)), function(j) {
              r <- e_m[j, ]
              div(
                class = "item-row",
                span(class = "item-tipo", r$tipologia),
                div(
                  class = "item-actions",
                  span(class = "item-importo text-success", fmt_eur(r$importo)),
                  tags$button(
                    type  = "button",
                    class = "btn btn-sm btn-outline-danger btn-del",
                    onclick = sprintf(
                      "Shiny.setInputValue('%s','%s',{priority:'event'})",
                      ns("del_e"), r$id
                    ),
                    icon("trash")
                  )
                )
              )
            })
          } else list(p(class = "text-muted small mb-1", "Nessuna entrata"))

          # Uscite list
          u_rows <- if (nrow(u_m) > 0) {
            lapply(seq_len(nrow(u_m)), function(j) {
              r <- u_m[j, ]
              div(
                class = "item-row",
                span(class = "item-tipo", r$tipologia),
                div(
                  class = "item-actions",
                  span(class = "item-importo text-danger", fmt_eur(r$importo)),
                  tags$button(
                    type  = "button",
                    class = "btn btn-sm btn-outline-danger btn-del",
                    onclick = sprintf(
                      "Shiny.setInputValue('%s','%s',{priority:'event'})",
                      ns("del_u"), r$id
                    ),
                    icon("trash")
                  )
                )
              )
            })
          } else list(p(class = "text-muted small mb-1", "Nessuna uscita"))

          div(
            class = "acc-body",
            div(
              class = "row g-4",
              div(
                class = "col-md-6",
                h6(class = "text-uppercase small text-muted mb-2", "Entrate"),
                e_rows,
                tags$button(
                  type  = "button",
                  class = "btn btn-add mt-2",
                  onclick = sprintf(
                    "event.stopPropagation(); Shiny.setInputValue('%s',%d,{priority:'event'})",
                    ns("open_e"), i
                  ),
                  "+ Aggiungi entrata"
                )
              ),
              div(
                class = "col-md-6",
                h6(class = "text-uppercase small text-muted mb-2", "Uscite"),
                u_rows,
                tags$button(
                  type  = "button",
                  class = "btn btn-add btn-add--red mt-2",
                  onclick = sprintf(
                    "event.stopPropagation(); Shiny.setInputValue('%s',%d,{priority:'event'})",
                    ns("open_u"), i
                  ),
                  "+ Aggiungi uscita"
                )
              )
            )
          )
        } else NULL

        div(
          class = paste("acc-item mb-2", if (is_open) "acc-item--open"),
          header,
          body
        )
      })
    })

    # Toggle
    observeEvent(input$toggle, {
      if (identical(mese_aperto(), input$toggle)) mese_aperto(NULL) else mese_aperto(input$toggle)
    })

    # ── Modal Entrata ──────────────────────────────────────────────────────────
    observeEvent(input$open_e, {
      if (length(rv$tipi_entrate) == 0) {
        showNotification("Aggiungi prima una tipologia di entrata nella Configurazione.", type = "warning")
        return()
      }
      mese_pre <- input$open_e
      showModal(modalDialog(
        title = tags$h5(class = "modal-title-custom", "Nuova Entrata"),
        easyClose = TRUE,
        size = "l",
        footer = tagList(
          modalButton("Annulla"),
          actionButton(ns("ok_e"), "Conferma", class = "btn btn-primary")
        ),
        div(
          pickerInput(
            inputId  = ns("tipo_e"),
            label    = "Tipologia",
            choices  = sort(rv$tipi_entrate),
            selected = head(rv$tipi_entrate, 1),
            multiple = FALSE,
            options  = pickerOptions(
              liveSearch  = FALSE,
              style       = "btn-outline-primary"
            )
          ),
          div(class = "mt-3",
            numericInputIcon(
              inputId = ns("imp_e"),
              label   = "Importo (€)",
              value   = 0,
              min     = 0,
              step    = 10,
              icon    = list(icon("euro-sign"))
            )
          ),
          div(class = "mt-3",
            tags$label(class = "form-label fw-semibold", "Mesi"),
            checkboxGroupButtons(
              inputId  = ns("mesi_e"),
              label    = NULL,
              choices  = setNames(as.character(1:12), MESI_BREVI),
              selected = as.character(mese_pre),
              status   = "outline-primary",
              size     = "sm",
              width    = "100%"
            )
          )
        )
      ))
    })

    observeEvent(input$ok_e, {
      mesi_sel <- as.integer(input$mesi_e)
      tipo     <- input$tipo_e
      importo  <- input$imp_e

      if (length(mesi_sel) == 0 || is.null(tipo) || is.null(importo) || importo <= 0) {
        showNotification("Compila tutti i campi correttamente.", type = "warning")
        return()
      }

      nuovi <- tibble(
        id       = paste0("e_", as.numeric(Sys.time()), "_", mesi_sel),
        tipologia = tipo,
        mese     = mesi_sel,
        importo  = as.numeric(importo)
      )
      rv$entrate <- bind_rows(rv$entrate, nuovi)
      save_entrate(rv$entrate)
      removeModal()
    })

    # ── Modal Uscita ───────────────────────────────────────────────────────────
    observeEvent(input$open_u, {
      if (length(rv$tipi_uscite) == 0) {
        showNotification("Aggiungi prima una tipologia di uscita nella Configurazione.", type = "warning")
        return()
      }
      mese_pre <- input$open_u
      showModal(modalDialog(
        title = tags$h5(class = "modal-title-custom", "Nuova Uscita"),
        easyClose = TRUE,
        size = "l",
        footer = tagList(
          modalButton("Annulla"),
          actionButton(ns("ok_u"), "Conferma", class = "btn btn-primary")
        ),
        div(
          pickerInput(
            inputId  = ns("tipo_u"),
            label    = "Tipologia",
            choices  = sort(rv$tipi_uscite),
            selected = head(rv$tipi_uscite, 1),
            multiple = FALSE,
            options  = pickerOptions(
              liveSearch  = FALSE,
              style       = "btn-outline-primary"
            )
          ),
          div(class = "mt-3",
            numericInputIcon(
              inputId = ns("imp_u"),
              label   = "Importo (€)",
              value   = 0,
              min     = 0,
              step    = 10,
              icon    = list(icon("euro-sign"))
            )
          ),
          div(class = "mt-3",
            tags$label(class = "form-label fw-semibold", "Mesi"),
            checkboxGroupButtons(
              inputId  = ns("mesi_u"),
              label    = NULL,
              choices  = setNames(as.character(1:12), MESI_BREVI),
              selected = as.character(mese_pre),
              status   = "outline-primary",
              size     = "sm",
              width    = "100%"
            )
          )
        )
      ))
    })

    observeEvent(input$ok_u, {
      mesi_sel <- as.integer(input$mesi_u)
      tipi     <- input$tipo_u
      importo  <- input$imp_u

      if (length(mesi_sel) == 0 || length(tipi) == 0 || is.null(importo) || importo <= 0) {
        showNotification("Compila tutti i campi correttamente.", type = "warning")
        return()
      }

      tipo <- tipi[1]  # single category per record
      nuovi <- tibble(
        id        = paste0("u_", as.numeric(Sys.time()), "_", mesi_sel),
        tipologia = tipo,
        mese      = mesi_sel,
        importo   = as.numeric(importo)
      )
      rv$uscite <- bind_rows(rv$uscite, nuovi)
      save_uscite(rv$uscite)
      removeModal()
    })

    # ── Delete ────────────────────────────────────────────────────────────────
    observeEvent(input$del_e, {
      req(input$del_e)
      rv$entrate <- rv$entrate |> filter(id != input$del_e)
      save_entrate(rv$entrate)
    })

    observeEvent(input$del_u, {
      req(input$del_u)
      rv$uscite <- rv$uscite |> filter(id != input$del_u)
      save_uscite(rv$uscite)
    })
  })
}
