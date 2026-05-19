DATA_DIR <- normalizePath("data", mustWork = FALSE)

.ensure_dir <- function() {
  if (!dir.exists(DATA_DIR)) dir.create(DATA_DIR, recursive = TRUE)
}

.fpath     <- function(name) file.path(DATA_DIR, paste0(name, ".rds"))
.fpath_old <- function(name) file.path(DATA_DIR, paste0(name, ".feather"))

.read_f  <- function(path) readRDS(path)
.write_f <- function(df, path) saveRDS(df, path)

# Silently try to read legacy feather file (ignores errors)
.read_feather_legacy <- function(path) {
  tryCatch(arrow::read_feather(path), error = function(e) NULL)
}

# ── Empty frames ──────────────────────────────────────────────────────────────

empty_entrate <- function() {
  tibble::tibble(
    id        = character(0),
    tipologia = character(0),
    mese      = integer(0),
    importo   = numeric(0)
  )
}

empty_uscite <- function() {
  tibble::tibble(
    id        = character(0),
    tipologia = character(0),
    mese      = integer(0),
    importo   = numeric(0)
  )
}

# ── Load / Save ───────────────────────────────────────────────────────────────

load_entrate <- function() {
  rds_p <- .fpath("entrate")
  if (file.exists(rds_p)) {
    tryCatch({
      df <- readRDS(rds_p)
      df$mese    <- as.integer(df$mese)
      df$importo <- as.numeric(df$importo)
      return(df)
    }, error = function(e) NULL)
  }
  # migrate from feather
  old_p <- .fpath_old("entrate")
  if (file.exists(old_p)) {
    df <- .read_feather_legacy(old_p)
    if (!is.null(df)) {
      df$mese    <- as.integer(df$mese)
      df$importo <- as.numeric(df$importo)
      .ensure_dir()
      saveRDS(df, rds_p)
      return(df)
    }
  }
  empty_entrate()
}

load_uscite <- function() {
  rds_p <- .fpath("uscite")
  if (file.exists(rds_p)) {
    tryCatch({
      df <- readRDS(rds_p)
      df$mese    <- as.integer(df$mese)
      df$importo <- as.numeric(df$importo)
      return(df)
    }, error = function(e) NULL)
  }
  # migrate from feather
  old_p <- .fpath_old("uscite")
  if (file.exists(old_p)) {
    df <- .read_feather_legacy(old_p)
    if (!is.null(df)) {
      df$mese    <- as.integer(df$mese)
      df$importo <- as.numeric(df$importo)
      .ensure_dir()
      saveRDS(df, rds_p)
      return(df)
    }
  }
  empty_uscite()
}

load_capitale <- function() {
  rds_p <- .fpath("config")
  if (file.exists(rds_p)) {
    tryCatch({
      df <- readRDS(rds_p)
      return(as.numeric(df$capitale_iniziale[1]))
    }, error = function(e) NULL)
  }
  # migrate from feather
  old_p <- .fpath_old("config")
  if (file.exists(old_p)) {
    df <- .read_feather_legacy(old_p)
    if (!is.null(df)) {
      val <- as.numeric(df$capitale_iniziale[1])
      .ensure_dir()
      saveRDS(tibble::tibble(capitale_iniziale = val), rds_p)
      return(val)
    }
  }
  10000
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
  .write_f(tibble::tibble(capitale_iniziale = as.numeric(cap)), .fpath("config"))
}

# ── Tipologie ─────────────────────────────────────────────────────────────────

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

# Rebuild tipi from existing entry data when the saved file is unavailable
.reconstruct_tipi <- function() {
  base <- default_tipi()

  read_any <- function(name) {
    rds_p <- .fpath(name)
    if (file.exists(rds_p)) {
      df <- tryCatch(readRDS(rds_p), error = function(e) NULL)
      if (!is.null(df)) return(df)
    }
    old_p <- .fpath_old(name)
    if (file.exists(old_p)) {
      df <- .read_feather_legacy(old_p)
      if (!is.null(df)) return(df)
    }
    NULL
  }

  e_df <- read_any("entrate")
  u_df <- read_any("uscite")

  e_types <- if (!is.null(e_df)) unique(as.character(e_df$tipologia)) else character(0)
  u_types <- if (!is.null(u_df)) unique(as.character(u_df$tipologia)) else character(0)

  result <- base
  for (nm in setdiff(e_types, result$nome[result$tipo == "entrata"])) {
    result <- dplyr::bind_rows(
      result,
      tibble::tibble(tipo = "entrata", nome = nm, colore = next_colore("entrata", result))
    )
  }
  for (nm in setdiff(u_types, result$nome[result$tipo == "uscita"])) {
    result <- dplyr::bind_rows(
      result,
      tibble::tibble(tipo = "uscita", nome = nm, colore = next_colore("uscita", result))
    )
  }
  result
}

load_tipi <- function() {
  rds_p <- .fpath("tipi")
  if (file.exists(rds_p)) {
    tryCatch({
      df <- readRDS(rds_p)
      df$tipo   <- as.character(df$tipo)
      df$nome   <- as.character(df$nome)
      df$colore <- as.character(df$colore)
      return(df)
    }, error = function(e) NULL)
  }
  # migrate from feather
  old_p <- .fpath_old("tipi")
  if (file.exists(old_p)) {
    df <- .read_feather_legacy(old_p)
    if (!is.null(df)) {
      df$tipo   <- as.character(df$tipo)
      df$nome   <- as.character(df$nome)
      df$colore <- as.character(df$colore)
      .ensure_dir()
      saveRDS(df, rds_p)
      return(df)
    }
  }
  # last resort: reconstruct from entry data and persist
  df <- .reconstruct_tipi()
  tryCatch({
    .ensure_dir()
    saveRDS(df, rds_p)
  }, error = function(e) NULL)
  df
}

save_tipi <- function(df) {
  .ensure_dir()
  .write_f(df, .fpath("tipi"))
}
