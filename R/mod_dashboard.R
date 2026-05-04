mod_dashboard_ui <- function(id) {

  ns     <- NS(id)
  m_prec <- mese_corrente - 1

  cap_label <- if (m_prec < 1)
    "Capitale corrente"
  else
    paste0("Capitale corrente (", MESI_LUNGHI[m_prec], ")")

  risp_label <- if (m_prec < 1)
    "Risparmio mensile"
  else if (m_prec == 1)
    paste0("Risparmio mensile (", MESI_BREVI[1], ")")
  else
    paste0("Risparmio mensile (", MESI_BREVI[1], " - ", MESI_BREVI[m_prec], ")")

  div(
    class = "container-fluid py-4",

    # ── KPI banner ────────────────────────────────────────────────────────────
    div(
      class = "kpi-banner mb-4",
      div(
        class = "kpi-main",
        div(class = "kpi-label", cap_label),
        div(class = "kpi-value", textOutput(ns("cap_txt"), inline = TRUE)),
        div(class = "kpi-sub mt-1", textOutput(ns("var_txt"), inline = TRUE))
      ),
      div(
        class = "kpi-side",
        div(
          class = "kpi-box",
          div(class = "kpi-label", risp_label),
          div(class = "kpi-value-sm", textOutput(ns("risp_txt"), inline = TRUE)),
          div(class = "kpi-hint", "media anno")
        ),
        div(
          class = "kpi-box",
          div(class = "kpi-label", "Stima capitale fine anno"),
          div(class = "kpi-value-sm", textOutput(ns("stima_txt"), inline = TRUE))
        )
      )
    ),

    # ── Card per mese: entrate/uscite + saldo ─────────────────────────────────
    card(
      class = "mb-3",
      card_header("Entrate / Uscite per mese"),
      uiOutput(ns("mese_cards"))
    ),

    # ── Capitale trend ────────────────────────────────────────────────────────
    card(
      card_header("Andamento capitale"),
      highchartOutput(ns("chart_line"), height = "280px")
    ),

    # ── Tabella voci per mese ─────────────────────────────────────────────────
    card(
      class = "mt-3",
      card_header("Dettaglio voci per mese"),
      div(style = "overflow-x:auto", uiOutput(ns("tabella_voci")))
    )
  )
}

mod_dashboard_server <- function(id, rv, mensile, capitale_df) {
  moduleServer(id, function(input, output, session) {

    output$cap_txt <- renderText({
      fmt_eur(capitale_attuale(capitale_df(), rv$capitale_iniziale))
    })

    output$var_txt <- renderText({
      v <- capitale_attuale(capitale_df(), rv$capitale_iniziale) - rv$capitale_iniziale
      icona <- if (v >= 0) "▲ +" else "▼ "
      paste0(icona, fmt_eur(abs(v)), " da inizio anno")
    })

    output$risp_txt <- renderText({
      fmt_eur(media_risparmio(mensile()))
    })

    output$stima_txt <- renderText({
      cap <- df_capitale(mensile(), rv$capitale_iniziale)
      fmt_eur(cap$capitale[12])
    })

    # Card mensili: barre entrate/uscite + saldo
    output$mese_cards <- renderUI({
      m       <- mensile()
      max_val <- max(c(m$tot_e, m$tot_u, 1), na.rm = TRUE)

      cards <- lapply(1:12, function(i) {
        e     <- m$tot_e[i]
        u     <- m$tot_u[i]
        s     <- m$saldo[i]
        pct_e <- round(max(e, 0) / max_val * 100)
        pct_u <- round(max(u, 0) / max_val * 100)
        col_s <- col_saldo(s)
        is_cur <- (i == mese_corrente)
        is_fut <- (i > mese_corrente)

        border_style <- if (is_cur)
          "border:2px solid #1e3a5f; border-radius:8px; background:white; padding:10px; box-shadow:0 2px 6px rgba(30,58,95,0.15)"
        else
          "border:1px solid #dee2e6; border-radius:8px; background:white; padding:10px"

        div(
          class = "col-6 col-md-4 col-lg-3 col-xl-2",
          div(
            style = border_style,
            # Intestazione mese
            div(
              class = "text-center mb-2",
              style = if (is_cur) "font-weight:700; color:#1e3a5f" else "font-weight:600; color:#495057",
              MESI_BREVI[i],
              if (is_cur) tags$span(
                style = "font-size:0.6rem; color:#1e3a5f; margin-left:4px", "cur."
              ) else if (is_fut) tags$span(
                style = "font-size:0.6rem; color:#adb5bd; margin-left:4px", "prev."
              ) else NULL
            ),
            # Barra Entrate
            div(
              class = "d-flex align-items-center gap-1 mb-1",
              tags$span(style = "font-size:0.65rem; color:#27ae60; min-width:10px; font-weight:600", "E"),
              div(
                style = "flex:1; height:7px; background:#e9ecef; border-radius:4px; overflow:hidden",
                div(style = paste0("width:", pct_e, "%; height:100%; background:#27ae60"))
              ),
              tags$span(style = "font-size:0.65rem; color:#27ae60; text-align:right; min-width:38px", fmt_eur_compact(e))
            ),
            # Barra Uscite
            div(
              class = "d-flex align-items-center gap-1 mb-2",
              tags$span(style = "font-size:0.65rem; color:#e74c3c; min-width:10px; font-weight:600", "U"),
              div(
                style = "flex:1; height:7px; background:#e9ecef; border-radius:4px; overflow:hidden",
                div(style = paste0("width:", pct_u, "%; height:100%; background:#e74c3c"))
              ),
              tags$span(style = "font-size:0.65rem; color:#e74c3c; text-align:right; min-width:38px", fmt_eur_compact(u))
            ),
            # Saldo
            tags$hr(style = "margin:6px 0; border-color:#dee2e6"),
            div(
              class = "text-center fw-bold",
              style = paste0("font-size:0.85rem; color:", col_s),
              paste0("€ ", fmt_saldo(s))
            )
          )
        )
      })

      div(class = "row g-2 p-2", cards)
    })

    # Line chart
    output$chart_line <- renderHighchart({
      cap <- capitale_df()

      effettivo  <- cap$capitale
      previsto   <- cap$capitale
      effettivo[cap$mese >= mese_corrente]       <- NA
      previsto[cap$mese < (mese_corrente - 1)]  <- NA

      highchart() |>
        hc_chart(type = "line") |>
        hc_xAxis(categories = MESI_BREVI) |>
        hc_yAxis(
          title  = list(text = "€"),
          labels = list(format = "€{value}")
        ) |>
        hc_add_series(
          name      = "Effettivo",
          data      = effettivo,
          color     = "#1e3a5f",
          marker    = list(enabled = TRUE, radius = 5),
          dashStyle = "Solid"
        ) |>
        hc_add_series(
          name      = "Previsto",
          data      = previsto,
          color     = "#3498db",
          marker    = list(enabled = TRUE, radius = 4),
          dashStyle = "ShortDash"
        ) |>
        hc_tooltip(shared = TRUE, valuePrefix = "€ ", valueDecimals = 0) |>
        hc_legend(enabled = TRUE) |>
        hc_credits(enabled = FALSE)
    })

    # Tabella voci per mese
    output$tabella_voci <- renderUI({

      entrate <- rv$entrate
      uscite  <- rv$uscite

      e_agg <- if (nrow(entrate) > 0)
        entrate |> group_by(tipologia, mese) |> summarise(importo = sum(importo), .groups = "drop")
      else
        tibble(tipologia = character(), mese = integer(), importo = numeric())

      u_agg <- if (nrow(uscite) > 0)
        uscite |> group_by(tipologia, mese) |> summarise(importo = sum(importo), .groups = "drop")
      else
        tibble(tipologia = character(), mese = integer(), importo = numeric())

      tipi_e <- e_agg |> group_by(tipologia) |> summarise(tot = sum(importo), .groups = "drop") |> arrange(desc(tot)) |> pull(tipologia)
      tipi_u <- u_agg |> group_by(tipologia) |> summarise(tot = sum(importo), .groups = "drop") |> arrange(desc(tot)) |> pull(tipologia)

      if (length(tipi_e) == 0 && length(tipi_u) == 0) {
        return(p(class = "text-muted p-3", "Nessun dato disponibile."))
      }

      tot_uscite_annuo <- sum(u_agg$importo, na.rm = TRUE)

      make_header <- function() {
        tags$tr(
          tags$th(style = "min-width:130px", "Voce"),
          lapply(MESI_BREVI, function(m) tags$th(class = "text-center", style = "min-width:60px", m)),
          tags$th(class = "text-center", style = "min-width:80px; border-left:2px solid #dee2e6", "Totale"),
          tags$th(class = "text-center", style = "min-width:50px; color:#6c757d", "%")
        )
      }

      make_section_hdr <- function(label, bg, txt_class, icon_cls) {
        tags$tr(
          tags$td(
            colspan = 15,
            class   = paste("fw-semibold small py-1 px-2", txt_class),
            style   = paste0("background:", bg, "; border-top:2px solid #dee2e6"),
            tags$i(class = paste(icon_cls, "me-1")), label
          )
        )
      }

      make_row <- function(t, agg_df, color_style, tot_ref = NULL) {
        row_data <- agg_df |> filter(tipologia == t)
        cells <- lapply(1:12, function(m) {
          v <- row_data |> filter(mese == m) |> pull(importo)
          if (length(v) == 0)
            tags$td(class = "text-center text-muted small", "—")
          else
            tags$td(class = "text-center", style = color_style, fmt_eur(v))
        })
        totale <- sum(row_data$importo, na.rm = TRUE)
        pct_cell <- if (!is.null(tot_ref) && tot_ref > 0) {
          pct <- round(totale / tot_ref * 100, 1)
          tags$td(
            class = "text-center text-muted small",
            style = "border-left:1px solid #dee2e6",
            paste0(pct, "%")
          )
        } else {
          tags$td(class = "text-center text-muted small", style = "border-left:1px solid #dee2e6", "—")
        }
        tags$tr(
          tags$td(t),
          cells,
          tags$td(
            class = "text-center fw-bold",
            style = paste0(color_style, "; border-left:2px solid #dee2e6"),
            fmt_eur(totale)
          ),
          pct_cell
        )
      }

      col_e <- "color:#27ae60; font-weight:500"
      col_u <- "color:#e74c3c; font-weight:500"

      tbody_rows <- list()

      if (length(tipi_e) > 0) {
        tbody_rows <- c(
          tbody_rows,
          list(make_section_hdr("Entrate", "#f0faf4", "text-success", "bi bi-arrow-up-circle-fill")),
          lapply(tipi_e, make_row, agg_df = e_agg, color_style = col_e, tot_ref = sum(e_agg$importo, na.rm = TRUE))
        )
      }

      if (length(tipi_u) > 0) {
        tbody_rows <- c(
          tbody_rows,
          list(make_section_hdr("Uscite", "#fdf0f0", "text-danger", "bi bi-arrow-down-circle-fill")),
          lapply(tipi_u, make_row, agg_df = u_agg, color_style = col_u, tot_ref = tot_uscite_annuo)
        )
      }

      tags$table(
        class = "table table-sm table-hover table-bordered mb-0",
        style = "font-size:0.85rem",
        tags$thead(class = "table-light", make_header()),
        do.call(tags$tbody, tbody_rows)
      )

    })

  })

}
