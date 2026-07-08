
dir.create("docs", showWarnings = FALSE)

availability <- data.frame(
  hut = c("Adamek-Hütte, AT", "Aarbiwak SAC, CH"),
  date = c("2026-07-15", "2026-07-15"),
  available = c(TRUE, FALSE),
  beds = c(4, 0)
)

jsonlite::write_json(
  availability,
  "docs/availability.json",
  pretty = TRUE,
  auto_unbox = TRUE
)

jsonlite::write_json(
  list(
    last_run = as.character(Sys.time()),
    status = "test"
  ),
  "docs/last_run.json",
  pretty = TRUE,
  auto_unbox = TRUE
)
