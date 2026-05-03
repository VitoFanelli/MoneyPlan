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

    # ── Charts ────────────────────────────────────────────────────────────────
    div(
      class = "row g-3 mb-3",
      div(
        class = "col-lg-8",
        card(
          full_screen = FALSE,
          card_header("Entrate vs Uscite mensili"),
          highchartOutput(ns("chart_bar"), height = "320px")
        )
      ),
      div(
        class = "col-lg-4",
        card(
          full_screen = FALSE,
          card_header("Distribuzione uscite"),
          highchartOutput(ns("chart_donut"), height = "320px")
        )
      )
    ),

    # ── Saldo strip ───────────────────────────────────────────────────────────
    card(
      class = "mb-3",
      card_header("Saldo per mese"),
      uiOutput(ns("saldo_strip"))
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

    # Bar chart
    output$chart_bar <- renderHighchart({
      m <- mensile()
      highchart() |>
        hc_chart(type = "column") |>
        hc_xAxis(categories = MESI_BREVI, crosshair = TRUE) |>
        hc_yAxis(title = list(text = "€"), labels = list(format = "€{value}")) |>
        hc_plotOptions(column = list(groupPadding = 0.1, borderRadius = 3)) |>
        hc_add_series(name = "Entrate", data = m$tot_e, color = "#27ae60") |>
        hc_add_series(name = "Uscite",  data = m$tot_u, color = "#e74c3c") |>
        hc_tooltip(shared = TRUE, valuePrefix = "€ ", valueDecimals = 0) |>
        hc_legend(enabled = TRUE, align = "right", verticalAlign = "top") |>
        hc_credits(enabled = FALSE)
    })

    # Donut chart
    output$chart_donut <- renderHighchart({
      cat_df <- uscite_per_categoria(rv$uscite)
      if (nrow(cat_df) == 0) {
        return(highchart() |>
          hc_title(text = "Nessun dato", style = list(color = "#adb5bd")) |>
          hc_credits(enabled = FALSE))
      }
      colori <- unname(rv$colori_uscite[cat_df$tipologia])
      colori[is.na(colori)] <- "#95a5a6"

      hc_data <- lapply(seq_len(nrow(cat_df)), function(i) {
        list(name = cat_df$tipologia[i], y = cat_df$tot[i], color = colori[i])
      })

      highchart() |>
        hc_chart(type = "pie") |>
        hc_plotOptions(pie = list(
          innerSize = "58%",
          dataLabels = list(enabled = FALSE),
          showInLegend = TRUE
        )) |>
        hc_add_series(
          name = "Uscite",
          data = hc_data
        ) |>
        hc_legend(enabled = TRUE, layout = "vertical", align = "right", verticalAlign = "middle") |>
        hc_tooltip(pointFormat = "<b>{point.name}</b>: € {point.y:,.0f}<br/>({point.percentage:.1f}%)") |>
        hc_credits(enabled = FALSE)
    })

    # Saldo strip
    output$saldo_strip <- renderUI({
      m  <- mensile()
      tiles <- lapply(1:12, function(i) {
        s   <- m$saldo[i]
        col <- col_saldo(s)
        is_cur <- (i == mese_corrente)
        is_fut <- (i >= mese_corrente)
        div(
          class = paste("saldo-tile", if (is_cur) "saldo-tile--current"),
          div(class = "tile-mese", MESI_BREVI[i]),
          div(class = "tile-valore", style = paste0("color:", col), fmt_saldo(s)),
          if (is_fut) div(class = "tile-prev", "prev.") else NULL
        )
      })
      div(class = "saldo-strip", tiles)
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

      make_header <- function() {
        tags$tr(
          tags$th(style = "min-width:130px", "Voce"),
          lapply(MESI_BREVI, function(m) tags$th(class = "text-center", style = "min-width:60px", m)),
          tags$th(class = "text-center", style = "min-width:80px; border-left:2px solid #dee2e6", "Totale")
        )
      }

      make_section_hdr <- function(label, bg, txt_class, icon_cls) {
        tags$tr(
          tags$td(
            colspan = 14,
            class   = paste("fw-semibold small py-1 px-2", txt_class),
            style   = paste0("background:", bg, "; border-top:2px solid #dee2e6"),
            tags$i(class = paste(icon_cls, "me-1")), label
          )
        )
      }

      make_row <- function(t, agg_df, color_style) {
        row_data <- agg_df |> filter(tipologia == t)
        cells <- lapply(1:12, function(m) {
          v <- row_data |> filter(mese == m) |> pull(importo)
          if (length(v) == 0)
            tags$td(class = "text-center text-muted small", "—")
          else
            tags$td(class = "text-center", style = color_style, fmt_eur(v))
        })
        totale <- sum(row_data$importo, na.rm = TRUE)
        tags$tr(
          tags$td(t),
          cells,
          tags$td(
            class = "text-center fw-bold",
            style = paste0(color_style, "; border-left:2px solid #dee2e6"),
            fmt_eur(totale)
          )
        )
      }

      col_e <- "color:#27ae60; font-weight:500"
      col_u <- "color:#e74c3c; font-weight:500"

      tbody_rows <- list()

      if (length(tipi_e) > 0) {
        tbody_rows <- c(
          tbody_rows,
          list(make_section_hdr("Entrate", "#f0faf4", "text-success", "bi bi-arrow-up-circle-fill")),
          lapply(tipi_e, make_row, agg_df = e_agg, color_style = col_e)
        )
      }

      if (length(tipi_u) > 0) {
        tbody_rows <- c(
          tbody_rows,
          list(make_section_hdr("Uscite", "#fdf0f0", "text-danger", "bi bi-arrow-down-circle-fill")),
          lapply(tipi_u, make_row, agg_df = u_agg, color_style = col_u)
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
