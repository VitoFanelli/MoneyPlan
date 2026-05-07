DATA_DIR <- normalizePath("data", mustWork = FALSE)

.ensure_dir <- function() {
  if (!dir.exists(DATA_DIR)) dir.create(DATA_DIR, recursive = TRUE)
}

.fpath <- function(name) file.path(DATA_DIR, paste0(name, ".feather"))

# Use arrow for feather I/O (modern replacement of feather package)
.read_f  <- function(path) arrow::read_feather(path)
.write_f <- function(df, path) arrow::write_feather(df, path)

load_entrate <- function() {
  p <- .fpath("entrate")
  if (!file.exists(p)) return(empty_entrate())
  tryCatch({
    df <- .read_f(p)
    df$mese <- as.integer(df$mese)
    df$importo <- as.numeric(df$importo)
    df
  }, error = function(e) empty_entrate())
}

load_uscite <- function() {
  p <- .fpath("uscite")
  if (!file.exists(p)) return(empty_uscite())
  tryCatch({
    df <- .read_f(p)
    df$mese <- as.integer(df$mese)
    df$importo <- as.numeric(df$importo)
    df
  }, error = function(e) empty_uscite())
}

load_capitale <- function() {
  p <- .fpath("config")
  if (!file.exists(p)) return(10000)
  tryCatch({
    df <- .read_f(p)
    as.numeric(df$capitale_iniziale[1])
  }, error = function(e) 10000)
}

save_entrate <- function(df) {
  .ensure_dir()
  .write_f(df, .fpath("entrate"))
}

save_uscite <- function(df) {
  .ensure_dir()
  .write_f(df, .fpath("uscite"))
}

save_capitale <- function(cap) {
  .ensure_dir()
  .write_f(tibble(capitale_iniziale = as.numeric(cap)), .fpath("config"))
}

# ── Tipologie ────────────────────────────────────────────────────────────────

default_tipi <- function() {
  n_e <- length(TIPO_ENTRATE)
  dplyr::bind_rows(
    tibble::tibble(
      tipo   = "entrata",
      nome   = TIPO_ENTRATE,
      colore = PALETTE_COLORI[(seq_len(n_e) - 1L) %% length(PALETTE_COLORI) + 1L]
    ),
    tibble::tibble(
      tipo   = "uscita",
      nome   = names(COLORI_USCITE),
      colore = unname(COLORI_USCITE)
    )
  )
}

next_colore <- function(tipo_val, tipi_df) {
  usati <- tipi_df |> dplyr::filter(tipo == tipo_val) |> dplyr::pull(colore)
  for (c in PALETTE_COLORI) {
    if (!(c %in% usati)) return(c)
  }
  PALETTE_COLORI[(length(usati)) %% length(PALETTE_COLORI) + 1L]
}

load_tipi <- function() {
  p <- .fpath("tipi")
  if (!file.exists(p)) return(default_tipi())
  tryCatch({
    df <- .read_f(p)
    df$tipo   <- as.character(df$tipo)
    df$nome   <- as.character(df$nome)
    df$colore <- as.character(df$colore)
    df
  }, error = function(e) default_tipi())
}

save_tipi <- function(df) {
  .ensure_dir()
  .write_f(df, .fpath("tipi"))
}
