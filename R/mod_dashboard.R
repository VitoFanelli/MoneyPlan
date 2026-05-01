mod_dashboard_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "container-fluid py-4",

    # ── KPI banner ────────────────────────────────────────────────────────────
    div(
      class = "kpi-banner mb-4",
      div(
        class = "kpi-main",
        div(class = "kpi-label", "Capitale corrente"),
        div(class = "kpi-value", textOutput(ns("cap_txt"), inline = TRUE)),
        div(class = "kpi-sub mt-1", textOutput(ns("var_txt"), inline = TRUE))
      ),
      div(
        class = "kpi-side",
        div(
          class = "kpi-box",
          div(class = "kpi-label", "Risparmio mensile"),
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
    )
  )
}

mod_dashboard_server <- function(id, rv, mensile, capitale_df) {
  moduleServer(id, function(input, output, session) {

    output$cap_txt <- renderText({
      fmt_eur(capitale_attuale(capitale_df()))
    })

    output$var_txt <- renderText({
      v <- capitale_attuale(capitale_df()) - rv$capitale_iniziale
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
      colori <- unname(COLORI_USCITE[cat_df$tipologia])
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
        is_fut <- (i > mese_corrente)
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
      effettivo[cap$mese > mese_corrente] <- NA
      previsto[cap$mese < mese_corrente]  <- NA

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
  })
}
