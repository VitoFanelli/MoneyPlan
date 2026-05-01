empty_entrate <- function() {
  tibble(id = character(), tipologia = character(), mese = integer(), importo = numeric())
}

empty_uscite <- function() {
  tibble(id = character(), tipologia = character(), mese = integer(), importo = numeric())
}

df_mensile <- function(entrate, uscite) {
  agg_e <- if (nrow(entrate) > 0)
    entrate |> group_by(mese) |> summarise(tot_e = sum(importo), .groups = "drop")
  else
    tibble(mese = integer(), tot_e = numeric())

  agg_u <- if (nrow(uscite) > 0)
    uscite |> group_by(mese) |> summarise(tot_u = sum(importo), .groups = "drop")
  else
    tibble(mese = integer(), tot_u = numeric())

  tibble(mese = 1:12) |>
    left_join(agg_e, by = "mese") |>
    left_join(agg_u, by = "mese") |>
    mutate(
      tot_e = replace_na(tot_e, 0),
      tot_u = replace_na(tot_u, 0),
      saldo = tot_e - tot_u
    )
}

df_capitale <- function(mensile, cap_init) {
  mensile |> mutate(capitale = cap_init + cumsum(saldo))
}

media_risparmio <- function(mensile) {
  dati <- mensile |> filter(mese <= mese_corrente, tot_e > 0 | tot_u > 0)
  if (nrow(dati) == 0) return(0)
  mean(dati$saldo)
}

capitale_attuale <- function(capitale_df) {
  capitale_df$capitale[min(mese_corrente, 12)]
}

uscite_per_categoria <- function(uscite) {
  if (nrow(uscite) == 0) return(tibble(tipologia = character(), tot = numeric()))
  uscite |>
    group_by(tipologia) |>
    summarise(tot = sum(importo), .groups = "drop") |>
    arrange(desc(tot))
}

avg_mensile_per_categoria <- function(df) {
  if (nrow(df) == 0) return(setNames(numeric(0), character(0)))
  res <- df |>
    group_by(tipologia) |>
    summarise(avg = sum(importo) / 12, .groups = "drop")
  setNames(res$avg, res$tipologia)
}

sim_df_mensile <- function(sim_e, sim_u) {
  # sim_e/sim_u: named numeric (tipologia -> monthly_importo)
  tot_e <- if (length(sim_e) > 0) sum(unlist(sim_e), na.rm = TRUE) else 0
  tot_u <- if (length(sim_u) > 0) sum(unlist(sim_u), na.rm = TRUE) else 0
  tibble(mese = 1:12, tot_e = tot_e, tot_u = tot_u, saldo = tot_e - tot_u)
}
