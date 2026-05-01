MESI_BREVI  <- c("Gen","Feb","Mar","Apr","Mag","Giu","Lug","Ago","Set","Ott","Nov","Dic")
MESI_LUNGHI <- c("Gennaio","Febbraio","Marzo","Aprile","Maggio","Giugno",
                 "Luglio","Agosto","Settembre","Ottobre","Novembre","Dicembre")

TIPO_ENTRATE <- c("Stipendio","Bonus","Welfare","Extra")
TIPO_USCITE  <- c("Alimentari","Automobile","Mutuo","Scuola","Sport","Vacanze","Svago","Videogames",
                  "Utenze","Abbonamenti","Casa")

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
