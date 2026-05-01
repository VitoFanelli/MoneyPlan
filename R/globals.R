MESI_BREVI  <- c(
  "Gen", "Feb", "Mar", "Apr", "Mag", "Giu", "Lug", "Ago", "Set", "Ott", "Nov", "Dic"
)

MESI_LUNGHI <- c(
  "Gennaio",
  "Febbraio",
  "Marzo",
  "Aprile",
  "Maggio",
  "Giugno",
  "Luglio",
  "Agosto",
  "Settembre",
  "Ottobre",
  "Novembre",
  "Dicembre"
)

TIPO_ENTRATE <- c("Stipendio", "Extra")

COLORI_USCITE <- c(
  Alimentari  = "#e74c3c",
  Automobile  = "#e67e22",
  Mutuo       = "#c0392b",
  Scuola      = "#8e44ad",
  Sport       = "#27ae60",
  Vacanze     = "#2980b9",
  Svago       = "#f1c40f",
  Videogames  = "#16a085",
  Utenze      = "#e67e22",
  Abbonamenti = "#f39c12",
  Casa        = "#d35400"
)

PALETTE_COLORI <- c(
  "#e74c3c", "#3498db", "#2ecc71", "#f1c40f", "#9b59b6",
  "#e67e22", "#1abc9c", "#e91e63", "#00bcd4", "#ff5722",
  "#8bc34a", "#673ab7", "#ff9800", "#607d8b", "#795548",
  "#f06292", "#4fc3f7", "#a5d6a7", "#ffcc02", "#ce93d8"
)

mese_corrente <- as.integer(format(Sys.Date(), "%m"))

fmt_eur <- function(x) {
  if (is.null(x) || (length(x) == 1 && is.na(x))) return("€ —")
  paste0("€ ", formatC(round(x), format = "d", big.mark = "."))
}

fmt_saldo <- function(x) {
  if (is.null(x) || (length(x) == 1 && is.na(x))) return("—")
  s <- formatC(abs(round(x)), format = "d", big.mark = ".")
  if (x >= 0) paste0("+", s) else paste0("-", s)
}

col_saldo <- function(x) {
  if (is.null(x) || is.na(x) || x == 0) "#6c757d"
  else if (x > 0) "#27ae60"
  else "#e74c3c"
}
