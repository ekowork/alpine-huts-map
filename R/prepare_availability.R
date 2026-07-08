library(readxl)
library(jsonlite)
library(stringr)
library(stringi)
library(purrr)
library(dplyr)

slugify_hut_id <- function(raw_hut) {
  x <- trimws(as.character(raw_hut))

  replacements <- c(
    "ß" = "ss", "ẞ" = "ss",
    "ä" = "a", "ö" = "o", "ü" = "u",
    "Ä" = "a", "Ö" = "o", "Ü" = "u"
  )

  x <- stringr::str_replace_all(x, replacements)
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- tolower(x)
  x <- stringr::str_replace_all(x, "[^a-z0-9]+", "-")
  x <- stringr::str_remove(x, "^-+")
  x <- stringr::str_remove(x, "-+$")
  x
}

split_hut_country <- function(raw_hut) {
  raw_hut <- trimws(as.character(raw_hut))
  m <- stringr::str_match(raw_hut, "^(.*?),\\s*([A-Z]{2})$")

  if (!is.na(m[1, 1])) {
    list(name = m[1, 2], country = m[1, 3])
  } else {
    list(name = raw_hut, country = NA_character_)
  }
}

parse_calendar_date <- function(month_header, day) {
  if (is.na(month_header) || month_header == "" || is.na(day) || day == "") {
    return(NA_character_)
  }

  mh <- trimws(as.character(month_header))
  m <- stringr::str_match(mh, "^(\\d{1,2})/(\\d{4})$")

  if (is.na(m[1, 1])) {
    stop("Unexpected month_header: ", month_header)
  }

  month <- as.integer(m[1, 2])
  year <- as.integer(m[1, 3])
  day_val <- as.integer(day)

  res_date <- tryCatch(
    as.Date(sprintf("%04d-%02d-%02d", year, month, day_val)),
    error = function(e) NA
  )

  if (is.na(res_date)) return(NA_character_)
  format(res_date, "%Y-%m-%d")
}

normalize_status <- function(raw_status, free_places, disabled) {
  raw_status <- tolower(trimws(as.character(raw_status)))
  if (is.na(raw_status)) raw_status <- ""

  free_clean <- suppressWarnings(as.integer(free_places))
  if (length(free_clean) == 0 || is.na(free_places) || free_places == "") {
    free_clean <- NA_integer_
  }

  if (raw_status == "error") {
    return(list(status = "error", level = "error", free = free_clean))
  }

  is_disabled <- !is.na(disabled) && (
    isTRUE(disabled) || tolower(as.character(disabled)) == "true"
  )

  if (is_disabled) {
    return(list(status = "closed", level = "closed", free = free_clean))
  }

  if (raw_status == "unknown" || is.na(free_clean)) {
    return(list(status = "unknown", level = "unknown", free = NA_integer_))
  }

  if (free_clean <= 0) {
    return(list(status = "full", level = "full", free = 0L))
  }

  if (free_clean <= 3) {
    return(list(status = "available", level = "low", free = free_clean))
  }

  if (free_clean <= 9) {
    return(list(status = "available", level = "medium", free = free_clean))
  }

  list(status = "available", level = "high", free = free_clean)
}

null_if_na <- function(x) {
  if (length(x) == 0 || is.na(x)) NULL else x
}

build_availability_outputs <- function(df_rows) {
  generated_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")

  long_rows <- list()
  errors_list <- list()
  huts_list <- list()

  if (nrow(df_rows) == 0) {
    return(list(
      long_rows = tibble::tibble(),
      json = list(
        generated_at = generated_at,
        date_from = NULL,
        date_to = NULL,
        errors = list(),
        huts = list()
      )
    ))
  }

  for (i in seq_len(nrow(df_rows))) {
    r <- df_rows[i, ]

    raw_hut <- r$Hut
    if (is.na(raw_hut) || trimws(as.character(raw_hut)) == "") next

    raw_hut <- trimws(as.character(raw_hut))
    hut_id <- slugify_hut_id(raw_hut)

    hut_info <- split_hut_country(raw_hut)
    hut_name <- hut_info$name
    country <- hut_info$country

    raw_status <- if ("status" %in% names(r)) r$status else NA_character_
    raw_status_chr <- tolower(trimws(as.character(raw_status)))
    if (is.na(raw_status_chr)) raw_status_chr <- ""

    if (raw_status_chr == "error") {
      err_msg <- if ("error_message" %in% names(r)) r$error_message else NA_character_
      if (!hut_id %in% names(errors_list)) errors_list[[hut_id]] <- list()

      errors_list[[hut_id]][[length(errors_list[[hut_id]]) + 1]] <- list(
        hut = raw_hut,
        message = null_if_na(err_msg)
      )
      next
    }

    month_header <- if ("month_header" %in% names(r)) r$month_header else NA_character_
    day <- if ("day" %in% names(r)) r$day else NA_integer_
    iso_date <- parse_calendar_date(month_header, day)

    if (is.na(iso_date)) next

    free_places <- if ("free_places" %in% names(r)) r$free_places else NA_integer_
    disabled <- if ("disabled" %in% names(r)) r$disabled else NA
    norm <- normalize_status(raw_status, free_places, disabled)

    if (!hut_id %in% names(huts_list)) {
      huts_list[[hut_id]] <- list(
        hut = raw_hut,
        name = hut_name,
        country = null_if_na(country),
        calendar = list()
      )
    }

    day_obj <- list(
      free = if (is.na(norm$free)) NULL else norm$free,
      status = norm$status,
      level = norm$level
    )

    if ("raw_text" %in% names(r) && !is.na(r$raw_text)) day_obj$raw_text <- as.character(r$raw_text)
    if ("aria" %in% names(r) && !is.na(r$aria)) day_obj$aria <- as.character(r$aria)

    huts_list[[hut_id]]$calendar[[iso_date]] <- day_obj

    long_rows[[length(long_rows) + 1]] <- tibble::tibble(
      hut_id = hut_id,
      hut_name = hut_name,
      country = if (is.na(country)) "" else country,
      date = iso_date,
      free_places = if (is.na(norm$free)) NA_integer_ else norm$free,
      status = norm$status,
      level = norm$level,
      raw_hut = raw_hut,
      raw_status = if (is.na(raw_status)) "" else as.character(raw_status),
      raw_text = if ("raw_text" %in% names(r) && !is.na(r$raw_text)) as.character(r$raw_text) else "",
      aria = if ("aria" %in% names(r) && !is.na(r$aria)) as.character(r$aria) else "",
      disabled = if ("disabled" %in% names(r) && !is.na(r$disabled)) as.character(r$disabled) else "",
      color = if ("color" %in% names(r) && !is.na(r$color)) as.character(r$color) else ""
    )
  }

  if (length(long_rows) > 0) {
    df_long <- dplyr::bind_rows(long_rows)
    all_dates <- sort(unique(df_long$date))
  } else {
    df_long <- tibble::tibble(
      hut_id = character(), hut_name = character(), country = character(), date = character(),
      free_places = integer(), status = character(), level = character(), raw_hut = character(),
      raw_status = character(), raw_text = character(), aria = character(), disabled = character(),
      color = character()
    )
    all_dates <- character()
  }

  for (hut_id in names(huts_list)) {
    cal <- huts_list[[hut_id]]$calendar

    available_days <- purrr::keep(names(cal), function(d) {
      identical(cal[[d]]$status, "available") &&
        !is.null(cal[[d]]$free) &&
        !is.na(cal[[d]]$free) &&
        cal[[d]]$free > 0
    })
    available_days <- sort(available_days)

    days_full <- sum(purrr::map_lgl(cal, ~ identical(.x$status, "full")))
    days_unknown <- sum(purrr::map_lgl(cal, ~ identical(.x$status, "unknown")))

    if (length(available_days) > 0) {
      next_avail_date <- available_days[1]
      next_avail_free <- cal[[next_avail_date]]$free
      all_free_counts <- purrr::map_int(available_days, ~ cal[[.x]]$free)
      max_free <- max(all_free_counts)
      total_free <- sum(all_free_counts)
    } else {
      next_avail_date <- NULL
      next_avail_free <- NULL
      max_free <- NULL
      total_free <- 0L
    }

    huts_list[[hut_id]]$summary <- list(
      days_total = length(cal),
      days_available = length(available_days),
      days_full = days_full,
      days_unknown = days_unknown,
      next_available_date = next_avail_date,
      next_available_free = next_avail_free,
      max_free_places = max_free,
      total_free_place_days = total_free
    )
  }

  if (length(huts_list) > 0) {
    huts_list <- huts_list[order(names(huts_list))]
  }

  availability_json <- list(
    generated_at = generated_at,
    date_from = if (length(all_dates) > 0) all_dates[1] else NULL,
    date_to = if (length(all_dates) > 0) all_dates[length(all_dates)] else NULL,
    errors = errors_list,
    huts = huts_list
  )

  list(long_rows = df_long, json = availability_json)
}

prepare_availability <- function(
  in_xlsx = file.path("data", "calendar_results.xlsx"),
  out_json = file.path("docs", "availability.json"),
  out_csv = file.path("data", "availability_long.csv")
) {
  if (!file.exists(in_xlsx)) {
    stop("Vstupní soubor neexistuje: ", in_xlsx)
  }

  dir.create(dirname(out_json), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(out_csv), recursive = TRUE, showWarnings = FALSE)

  df_raw <- readxl::read_xlsx(in_xlsx, sheet = 1)
  outputs <- build_availability_outputs(df_raw)

  readr::write_csv(outputs$long_rows, out_csv, na = "")

  writeLines(
    jsonlite::toJSON(outputs$json, auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null"),
    out_json,
    useBytes = TRUE
  )

  num_errors <- sum(purrr::map_int(outputs$json$errors, length))

  cat("Input rows:", nrow(df_raw), "\n")
  cat("Calendar rows written:", nrow(outputs$long_rows), "\n")
  cat("Huts:", length(outputs$json$huts), "\n")
  cat("Date range:", outputs$json$date_from, "–", outputs$json$date_to, "\n")
  cat("Errors:", num_errors, "\n")
  cat("Wrote:", out_json, "\n")
  cat("Wrote:", out_csv, "\n")

  invisible(outputs)
}

if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  in_xlsx <- if (length(args) >= 1) args[1] else file.path("data", "calendar_results.xlsx")
  out_json <- if (length(args) >= 2) args[2] else file.path("docs", "availability.json")
  out_csv <- if (length(args) >= 3) args[3] else file.path("data", "availability_long.csv")

  prepare_availability(in_xlsx, out_json, out_csv)
}
