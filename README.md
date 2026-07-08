# Alpine Hut Availability Map

**Live map:** https://ekowork.github.io/alpine-huts-map/
**Hut locations:** [open `docs/huts.geojson`](docs/huts.geojson)  

A small private project that shows indicative availability for selected Alpine mountain huts on an interactive map.

The data is scraped from [hut-reservation.org](https://www.hut-reservation.org/) and displayed as a static web map hosted via GitHub Pages.

This project is experimental. The data may be incomplete, outdated, or wrong. The official reservation system of each hut is always the source of truth.

## Live map

https://ekowork.github.io/alpine-huts-map/

## What this project does

The workflow is:

1. Read the list of huts from `data/chaty.xlsx`.
2. Log in to `hut-reservation.org`.
3. Scrape availability calendars for the selected huts.
4. Save the raw scraping output.
5. Convert the results into map-ready JSON.
6. Update the static map in `docs/`.
7. Commit the updated availability data back to the repository.

The map itself is a static HTML/JavaScript page. No backend server is needed for viewing the map.

## Repository structure

```text
.
├── .github/workflows/
│   └── scrape.yml                  # GitHub Actions workflow for automated scraping
├── R/
│   ├── volna_mista_na_chatach_v2.R  # Main scraper
│   └── prepare_availability.R      # Converts scraper output to JSON/CSV
├── data/
│   ├── chaty.xlsx                  # Input list of huts
│   ├── availability_long.csv       # Latest availability in long format
│   └── history/                    # Daily archived snapshots
├── docs/
│   ├── index.html                  # Static web map
│   ├── availability.json           # Latest map data
│   ├── huts.geojson                # Hut locations
│   └── config.js                   # Map configuration
└── README.md
```

## Input data

The main input file is:

```text
data/chaty.xlsx
```

It should contain at least a column called:

```text
huts
```

Example values:

```text
Adamek-Hütte, AT
Preintalerhütte, AT
Dachstein-Südwandhütte, AT
```

The scraper uses these names to search for huts in the reservation system.

## Outputs

The scraper creates or updates:

```text
data/calendar_results.xlsx
data/availability_long.csv
docs/availability.json
data/history/availability_YYYY-MM-DD.csv.gz
```

### `docs/availability.json`

This is the main file used by the web map.

### `data/availability_long.csv`

A flat table with the latest scraped availability.

### `data/history/`

Daily compressed CSV snapshots. These are useful for debugging, checking past runs, or recovering from a broken scrape.

## Running locally

From the root of the repository, set the required environment variables.

On Windows PowerShell:

```powershell
$env:HUT_EMAIL="your_email"
$env:HUT_PASSWORD="your_password"
$env:N_MONTHS_TO_SCRAPE="2"
```

For a small test run:

```powershell
$env:MAX_HUTS="3"
```

Then run:

```powershell
Rscript R/volna_mista_na_chatach_v2.R
```

If `Rscript` is not available in PowerShell, use the full path, for example:

```powershell
& "C:\Program Files\R\R-4.4.3\bin\Rscript.exe" R/volna_mista_na_chatach_v2.R
```

## Testing with only a few huts

To test only the first three huts from `data/chaty.xlsx`:

```powershell
$env:MAX_HUTS="3"
Rscript R/volna_mista_na_chatach_v2.R
```

To run all huts, leave `MAX_HUTS` unset.

## GitHub Actions

The automated scraper is defined in:

```text
.github/workflows/scrape.yml
```

It can be run manually from the GitHub Actions tab and also runs automatically on a schedule.

Required GitHub repository secrets:

```text
HUT_EMAIL
HUT_PASSWORD
```

Set them in:

```text
Settings → Secrets and variables → Actions
```

The workflow uses these secrets to log in to `hut-reservation.org`.

## GitHub Pages

The map is served from the `docs/` folder.

Recommended GitHub Pages settings:

```text
Source: Deploy from a branch
Branch: main
Folder: /docs
```

## Configuration

The number of months to scrape is controlled by:

```text
N_MONTHS_TO_SCRAPE
```

Example:

```text
N_MONTHS_TO_SCRAPE=2
```

The number of huts to scrape can be limited with:

```text
MAX_HUTS
```

Example:

```text
MAX_HUTS=3
```

If `MAX_HUTS` is not set, the scraper processes all huts in `data/chaty.xlsx`.

## Important limitations

This is not an official availability source.

The scraper may fail if:

- the reservation website changes,
- the login flow changes,
- hut names do not match the reservation system,
- a hut has a non-standard booking setup,
- the website is temporarily unavailable,
- the calendar format changes.

Always verify availability directly in the official reservation system before making travel plans.

## Reporting issues

If you find a mistake, missing hut, wrong availability, or broken map behavior, please open an issue:

https://github.com/ekowork/alpine-huts-map/issues

## Disclaimer

This is a small private project created for convenience and experimentation. It is not affiliated with hut-reservation.org, Alpenverein, SAC, CAI, or any hut operator.

The official reservation system of each hut is always the authoritative source.
