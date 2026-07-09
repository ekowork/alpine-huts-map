library(dplyr)
library(purrr)
library(stringr)
library(tibble)
library(readr)
library(readxl)
library(writexl)
library(jsonlite)
library(chromote)
library(lubridate)

# ------------------------------------------------------------
# Nastavení
# ------------------------------------------------------------

calendar_results <- tibble()

# Smoke-test defaults: 3 chaty × 1 měsíc.
# Pro celý běh později nastav MAX_HUTS=0 a N_MONTHS_TO_SCRAPE=2.
n_months_to_scrape <- as.integer(Sys.getenv("N_MONTHS_TO_SCRAPE", "2"))

max_huts_raw <- Sys.getenv("MAX_HUTS", "")

max_huts <- if (nzchar(max_huts_raw)) {
  as.integer(max_huts_raw)
} else {
  NA_integer_
}

input_huts_path <- Sys.getenv("INPUT_HUTS_XLSX", file.path("data", "chaty.xlsx"))
calendar_results_path <- Sys.getenv("CALENDAR_RESULTS_XLSX", file.path("data", "calendar_results.xlsx"))
availability_json_path <- Sys.getenv("AVAILABILITY_JSON", file.path("docs", "availability.json"))
availability_long_csv_path <- Sys.getenv("AVAILABILITY_LONG_CSV", file.path("data", "availability_long.csv"))

dir.create(dirname(calendar_results_path), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(availability_json_path), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(availability_long_csv_path), recursive = TRUE, showWarnings = FALSE)

# huts to check are define below after accessing the website!
results <- data.frame(
  Hut = character(),
  Available = logical(),
  Message = character(),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------
# Helper funkce
# ------------------------------------------------------------

jsq <- function(x) {
  jsonlite::toJSON(x, auto_unbox = TRUE)
}

press_key <- function(b, key, code, vk, pause = 0.05) {
  b$Input$dispatchKeyEvent(
    type = "rawKeyDown",
    key = key,
    code = code,
    windowsVirtualKeyCode = vk,
    nativeVirtualKeyCode = vk
  )
  
  Sys.sleep(pause)
  
  b$Input$dispatchKeyEvent(
    type = "keyUp",
    key = key,
    code = code,
    windowsVirtualKeyCode = vk,
    nativeVirtualKeyCode = vk
  )
}

wait_for_js <- function(b, condition_js, timeout = 25, interval = 0.25) {
  start <- Sys.time()
  
  repeat {
    out <- tryCatch({
      b$Runtime$evaluate(sprintf("
        (function() {
          try {
            return !!(%s);
          } catch(e) {
            return false;
          }
        })()
      ", condition_js))
    }, error = function(e) NULL)
    
    ok <- tryCatch(
      isTRUE(out$result$value),
      error = function(e) FALSE
    )
    
    if (ok) return(TRUE)
    
    if (as.numeric(difftime(Sys.time(), start, units = "secs")) > timeout) {
      stop("Timeout while waiting for: ", condition_js)
    }
    
    Sys.sleep(interval)
  }
}

click_button_text <- function(b, text) {
  b$Runtime$evaluate(sprintf("
    (function() {
      const wanted = %s;

      const btn = Array.from(document.querySelectorAll('button'))
        .find(el => {
          const txt = (el.textContent || '').trim().toUpperCase();
          const disabled =
            el.disabled ||
            el.getAttribute('aria-disabled') === 'true' ||
            String(el.className || '').toLowerCase().includes('disabled');

          return txt === wanted.toUpperCase() && !disabled;
        });

      if (!btn) {
        throw new Error('Enabled button not found: ' + wanted);
      }

      btn.click();
    })()
  ", jsq(text)))
}


debug_hut_options <- function(b) {
  out <- b$Runtime$evaluate("
    JSON.stringify((function() {
      const norm = s => (s || '').replace(/\\s+/g, ' ').trim();

      const options = Array.from(document.querySelectorAll(
        '.cdk-overlay-container mat-option, .cdk-overlay-container .mat-mdc-option, [role=\"option\"]'
      )).map((el, i) => ({
        i: i,
        text: norm(el.innerText || el.textContent),
        visible: !!(el.offsetWidth || el.offsetHeight || el.getClientRects().length)
      }));

      return options;
    })())
  ")
  
  jsonlite::fromJSON(out$result$value)
}


click_away <- function(b) {
  b$Runtime$evaluate("
    (function() {
      if (document.activeElement) {
        document.activeElement.dispatchEvent(new Event('input', { bubbles: true }));
        document.activeElement.dispatchEvent(new Event('change', { bubbles: true }));
        document.activeElement.blur();
      }

      document.body.dispatchEvent(new MouseEvent('mousedown', {
        bubbles: true,
        clientX: 20,
        clientY: 20
      }));

      document.body.dispatchEvent(new MouseEvent('mouseup', {
        bubbles: true,
        clientX: 20,
        clientY: 20
      }));

      document.body.click();
    })()
  ")
}

mouse_click <- function(b, x, y, pause = 0.15) {
  b$Input$dispatchMouseEvent(
    type = "mouseMoved",
    x = x,
    y = y
  )
  
  Sys.sleep(0.05)
  
  b$Input$dispatchMouseEvent(
    type = "mousePressed",
    x = x,
    y = y,
    button = "left",
    clickCount = 1
  )
  
  Sys.sleep(pause)
  
  b$Input$dispatchMouseEvent(
    type = "mouseReleased",
    x = x,
    y = y,
    button = "left",
    clickCount = 1
  )
}

get_all_dropdown_options <- function(b) {
  # 1. Zaměříme a "klikneme" do inputu pro výběr chaty, aby se menu otevřelo
  b$Runtime$evaluate("
    (function() {
      const input = document.querySelector('input[placeholder=\"Hütte auswählen\"]');
      if (!input) throw new Error('Input pro chaty nenalezen!');
      
      input.focus();
      input.dispatchEvent(new MouseEvent('click', { bubbles: true }));
      
      // Občas je potřeba i zmáčknout šipku dolů, aby se našeptávač probudil
      input.dispatchEvent(new KeyboardEvent('keydown', { key: 'ArrowDown', bubbles: true }));
    })()
  ")
  
  # 2. Počkáme, dokud se v DOMu neobjeví alespoň jedna položka menu
  wait_for_js(
    b,
    "document.querySelectorAll('.cdk-overlay-container mat-option, [role=\"option\"]').length > 0",
    timeout = 15
  )
  
  # 3. Sesbíráme všechny viditelné texty z menu a pošleme je zpět jako JSON
  out <- b$Runtime$evaluate("
    JSON.stringify((function() {
      const options = Array.from(document.querySelectorAll(
        '.cdk-overlay-container mat-option, .cdk-overlay-container .mat-mdc-option, [role=\"option\"]'
      ));
      
      return options
        .map(el => (el.innerText || el.textContent || '').trim())
        .filter(text => text.length > 0);
    })())
  ")
  
  # 4. Zavřeme menu (kliknutím někam jinam), ať nám tam nepřekáží pro další kroky
  click_away(b)
  
  # 5. Převedeme JSON zpět na R vektor a vrátíme
  return(jsonlite::fromJSON(out$result$value))
}

ctrl_a_backspace <- function(b) {
  b$Input$dispatchKeyEvent(
    type = "rawKeyDown",
    key = "Control",
    code = "ControlLeft",
    windowsVirtualKeyCode = 17,
    nativeVirtualKeyCode = 17,
    modifiers = 2
  )
  
  b$Input$dispatchKeyEvent(
    type = "rawKeyDown",
    key = "a",
    code = "KeyA",
    windowsVirtualKeyCode = 65,
    nativeVirtualKeyCode = 65,
    modifiers = 2
  )
  
  b$Input$dispatchKeyEvent(
    type = "keyUp",
    key = "a",
    code = "KeyA",
    windowsVirtualKeyCode = 65,
    nativeVirtualKeyCode = 65,
    modifiers = 2
  )
  
  b$Input$dispatchKeyEvent(
    type = "keyUp",
    key = "Control",
    code = "ControlLeft",
    windowsVirtualKeyCode = 17,
    nativeVirtualKeyCode = 17
  )
  
  Sys.sleep(0.1)
  press_key(b, "Backspace", "Backspace", 8)
}

click_enabled_button_physical <- function(b, text, timeout = 25) {
  wait_for_js(
    b,
    sprintf("
      Array.from(document.querySelectorAll('button')).some(el => {
        const txt = (el.textContent || '').trim().toUpperCase();
        const disabled =
          el.disabled ||
          el.getAttribute('aria-disabled') === 'true' ||
          String(el.className || '').toLowerCase().includes('disabled');
        const visible = !!(el.offsetWidth || el.offsetHeight || el.getClientRects().length);

        return visible && txt === %s.toUpperCase() && !disabled;
      })
    ", jsq(text)),
    timeout = timeout
  )
  
  out <- b$Runtime$evaluate(sprintf("
    JSON.stringify((function() {
      const wanted = %s.toUpperCase();

      const btn = Array.from(document.querySelectorAll('button')).find(el => {
        const txt = (el.textContent || '').trim().toUpperCase();
        const disabled =
          el.disabled ||
          el.getAttribute('aria-disabled') === 'true' ||
          String(el.className || '').toLowerCase().includes('disabled');
        const visible = !!(el.offsetWidth || el.offsetHeight || el.getClientRects().length);

        return visible && txt === wanted && !disabled;
      });

      if (!btn) return { found: false };

      const r = btn.getBoundingClientRect();

      return {
        found: true,
        text: (btn.textContent || '').trim(),
        x: r.left + r.width / 2,
        y: r.top + r.height / 2
      };
    })())
  ", jsq(text)))
  
  obj <- jsonlite::fromJSON(out$result$value, simplifyVector = FALSE)
  
  if (!isTRUE(obj$found)) {
    stop("Button not found or disabled: ", text)
  }
  
  cat("Klikám na tlačítko:", obj$text, "\n")
  mouse_click(b, obj$x, obj$y)
  Sys.sleep(0.8)
}

slow_insert_text <- function(b, text, delay = 0.12) {
  chars <- strsplit(text, "", fixed = TRUE)[[1]]
  
  for (ch in chars) {
    b$Input$insertText(text = ch)
    Sys.sleep(delay)
  }
}

select_hut_robust <- function(b, hut, timeout = 30) {
  
  hut_search <- sub(",.*$", "", hut)
  hut_search <- sub("-Hütte$", "", hut_search)
  hut_search <- trimws(hut_search)
  
  cat("Hledám chatu přes:", hut_search, "\n")
  
  # Fokus do pole pro výběr chaty
  wait_for_js(
    b,
    "document.querySelector('input[placeholder=\"Hütte auswählen\"]')",
    timeout = timeout
  )
  
  b$Runtime$evaluate("
    (function() {
      const x = document.querySelector('input[placeholder=\"Hütte auswählen\"]');
      if (!x) throw new Error('Hütte auswählen input not found');
      x.focus();
    })()
  ")
  
  Sys.sleep(0.3)
  ctrl_a_backspace(b)
  Sys.sleep(0.3)
  
  # Pomalu napíšeme jen hledací fragment, ne celý název
  slow_insert_text(b, hut_search, delay = 0.14)
  
  # Počkáme na položku v autocomplete
  wait_for_js(
    b,
    sprintf("
      Array.from(document.querySelectorAll(
        '.cdk-overlay-container mat-option, .cdk-overlay-container .mat-mdc-option, [role=\"option\"]'
      )).some(el => {
        const txt = (el.innerText || el.textContent || '').trim().toLowerCase();
        const visible = !!(el.offsetWidth || el.offsetHeight || el.getClientRects().length);
        return visible && txt.length > 0 && txt.includes(%s);
      })
    ", jsq(tolower(hut_search))),
    timeout = timeout
  )
  
  # Najdeme položku a klikneme fyzicky doprostřed
  out <- b$Runtime$evaluate(sprintf("
    JSON.stringify((function() {
      const wanted = %s.toLowerCase();
      const norm = s => (s || '').replace(/\\s+/g, ' ').trim();

      const options = Array.from(document.querySelectorAll(
        '.cdk-overlay-container mat-option, .cdk-overlay-container .mat-mdc-option, [role=\"option\"]'
      )).filter(el => {
        const txt = norm(el.innerText || el.textContent);
        const visible = !!(el.offsetWidth || el.offsetHeight || el.getClientRects().length);
        return visible && txt.length > 0;
      });

      const option = options.find(el => {
        const txt = norm(el.innerText || el.textContent).toLowerCase();
        return txt.includes(wanted);
      }) || options[0];

      if (!option) {
        return {
          found: false,
          options: options.map(el => norm(el.innerText || el.textContent))
        };
      }

      option.scrollIntoView({ block: 'center' });

      const r = option.getBoundingClientRect();

      return {
        found: true,
        text: norm(option.innerText || option.textContent),
        x: r.left + r.width / 2,
        y: r.top + r.height / 2,
        options: options.map(el => norm(el.innerText || el.textContent))
      };
    })())
  ", jsq(hut_search)))
  
  obj <- jsonlite::fromJSON(out$result$value, simplifyVector = FALSE)
  
  if (!isTRUE(obj$found)) {
    stop("Nenašel jsem položku chaty. Options: ", paste(unlist(obj$options), collapse = " | "))
  }
  
  cat("Vybraná položka:", obj$text, "\n")
  mouse_click(b, obj$x, obj$y)
  
  Sys.sleep(1.2)
}


open_calendar <- function(b, timeout = 20) {
  # POZOR: nehledáme podle textu (datum/date/von/bis) - ten je u jednotlivých
  # chat podle jejich nastaveného jazyka (např. FR), ale podle CSS tříd,
  # které generuje Angular Material knihovna a jsou stejné ve všech jazycích.
  wait_for_js(
    b,
    "
    Array.from(document.querySelectorAll(
      '.mat-datepicker-toggle, mat-datepicker-toggle, input[matdatepicker], input.mat-date-range-input-inner'
    )).some(el => !!(el.offsetWidth || el.offsetHeight || el.getClientRects().length))
    ",
    timeout = timeout
  )
  
  out <- b$Runtime$evaluate("
    JSON.stringify((function() {
      const visible = el => !!(el.offsetWidth || el.offsetHeight || el.getClientRects().length);

      // 1. Primárně: ikonka kalendáře (mat-datepicker-toggle) - jazykově neutrální.
      const toggle = Array.from(document.querySelectorAll(
        '.mat-datepicker-toggle, mat-datepicker-toggle'
      )).find(visible);

      if (toggle) {
        const btn = toggle.querySelector('button') || toggle;
        const r = btn.getBoundingClientRect();
        return { found: true, x: r.left + r.width / 2, y: r.top + r.height / 2, via: 'toggle' };
      }

      // 2. Fallback: datumový input přes Angular Material atributy/třídy (ne text).
      const input = Array.from(document.querySelectorAll(
        'input[matdatepicker], input.mat-date-range-input-inner, input.mat-datepicker-input'
      )).find(el => visible(el) && !el.disabled);

      if (!input) return { found: false };

      const field = input.closest('mat-form-field') || input.parentElement;
      const iconButton = field
        ? field.querySelector('button, [mat-icon-button]')
        : null;

      const target = iconButton || input;
      const r = target.getBoundingClientRect();

      return { found: true, x: r.left + r.width / 2, y: r.top + r.height / 2, via: 'input-fallback' };
    })())
  ")
  
  obj <- jsonlite::fromJSON(out$result$value, simplifyVector = FALSE)
  
  if (!isTRUE(obj$found)) {
    stop("Nenašel jsem datumové pole / ikonku kalendáře.")
  }
  
  mouse_click(b, obj$x, obj$y)
  
  wait_for_js(
    b,
    "
    document.querySelector('mat-calendar') ||
    document.querySelector('.mat-datepicker-content') ||
    document.querySelector('.mat-calendar-body')
    ",
    timeout = timeout
  )
  
  Sys.sleep(0.5)
}

wait_for_calendar_counts <- function(b, timeout = 8, interval = 0.5) {
  start <- Sys.time()

  repeat {
    out <- b$Runtime$evaluate("
      JSON.stringify((function() {
        const norm = s => (s || '').replace(/\\s+/g, ' ').trim();

        const cells = Array.from(document.querySelectorAll('.mat-calendar-body-cell'))
          .map(el => {
            const txt = norm(el.innerText || el.textContent);
            const cls = String(el.className || '');
            const disabled =
              el.getAttribute('aria-disabled') === 'true' ||
              cls.toLowerCase().includes('disabled');

            const nums = txt.match(/\\d+/g) || [];

            return {
              text: txt,
              disabled: disabled,
              n_nums: nums.length
            };
          })
          .filter(x => x.text.length > 0);

        const enabled = cells.filter(x => !x.disabled);
        const enabled_with_counts = enabled.filter(x => x.n_nums >= 2);

        return {
          n_cells: cells.length,
          n_enabled: enabled.length,
          n_enabled_with_counts: enabled_with_counts.length
        };
      })())
    ")

    state <- jsonlite::fromJSON(out$result$value, simplifyVector = FALSE)

    if (
      state$n_cells > 0 &&
      (
        state$n_enabled == 0 ||
        state$n_enabled_with_counts > 0
      )
    ) {
      return(TRUE)
    }

    if (as.numeric(difftime(Sys.time(), start, units = "secs")) > timeout) {
      cat(
        "Pozor: availability čísla se nestihla načíst.",
        "enabled =", state$n_enabled,
        "with_counts =", state$n_enabled_with_counts,
        "\n"
      )
      return(FALSE)
    }

    Sys.sleep(interval)
  }
}

scrape_visible_calendar_month <- function(b) {
  out <- b$Runtime$evaluate("
    JSON.stringify((function() {
      const norm = s => (s || '').replace(/\\s+/g, ' ').trim();

      const headerEl =
        document.querySelector('.mat-calendar-period-button') ||
        document.querySelector('.mat-calendar-header button') ||
        document.querySelector('mat-calendar-header');

      const header = norm(headerEl ? (headerEl.innerText || headerEl.textContent) : '');

      const cells = Array.from(document.querySelectorAll(
         '.mat-calendar-body-cell'
      )).map((el, i) => {
        const txt = norm(el.innerText || el.textContent);
        const aria = el.getAttribute('aria-label') || '';

        const cls = String(el.className || '');
        const disabled =
          el.getAttribute('aria-disabled') === 'true' ||
          cls.toLowerCase().includes('disabled');

        const style = window.getComputedStyle(el);
        const r = el.getBoundingClientRect();

        return {
          i: i,
          text: txt,
          aria: aria,
          className: cls,
          disabled: disabled,
          color: style.color,
          x: r.left + r.width / 2,
          y: r.top + r.height / 2
        };
      }).filter(x => x.text.length > 0);

      return {
        header: header,
        cells: cells
      };
    })())
  ")
  
  raw <- jsonlite::fromJSON(out$result$value, simplifyVector = FALSE)
  
  header <- raw$header
  cells <- raw$cells
  
  if (length(cells) == 0) {
    return(tibble::tibble())
  }
  
  tibble::tibble(
    month_header = header,
    raw_text = purrr::map_chr(cells, ~ .x$text),
    aria = purrr::map_chr(cells, ~ .x$aria %||% ""),
    disabled = purrr::map_lgl(cells, ~ isTRUE(.x$disabled)),
    color = purrr::map_chr(cells, ~ .x$color %||% "")
  ) %>%
    mutate(
      # typicky bude text v buňce něco jako "1 6", "2 31", "3 0"
      nums = stringr::str_extract_all(raw_text, "\\d+"),
      day = purrr::map_int(nums, ~ if (length(.x) >= 1) as.integer(.x[1]) else NA_integer_),
      free_places = purrr::map_int(nums, ~ if (length(.x) >= 2) as.integer(.x[2]) else NA_integer_),
      status = case_when(
        disabled ~ "disabled",
        !is.na(free_places) & free_places == 0 ~ "full",
        !is.na(free_places) & free_places > 0 ~ "available",
        TRUE ~ "unknown"
      )
    ) %>%
    select(month_header, day, free_places, status, raw_text, aria, disabled, color)
}

next_calendar_month <- function(b, timeout = 25) {
  
  old_state <- b$Runtime$evaluate("
    JSON.stringify((function() {
      const norm = s => (s || '').replace(/\\s+/g, ' ').trim();
      const calendar =
        document.querySelector('.mat-datepicker-content') ||
        document.querySelector('mat-calendar') ||
        document.querySelector('.mat-calendar') ||
        document;
      const headerEl = calendar.querySelector('.mat-calendar-period-button');
      const cells = Array.from(calendar.querySelectorAll('.mat-calendar-body-cell'))
        .map(el => norm(el.innerText || el.textContent))
        .join('|');
      return {
        header: norm(headerEl ? (headerEl.innerText || headerEl.textContent) : ''),
        cells: cells
      };
    })())
  ")
  old_state <- jsonlite::fromJSON(old_state$result$value, simplifyVector = FALSE)
  cat("Kalendář před klikem:", old_state$header, "\n")
  
  click_result <- b$Runtime$evaluate("
    JSON.stringify((function() {
      const norm = s => (s || '').replace(/\\s+/g, ' ').trim();

      const visible = el => {
        if (!el) return false;
        const r = el.getBoundingClientRect();
        const style = window.getComputedStyle(el);
        return r.width > 0 && r.height > 0 &&
               style.display !== 'none' && style.visibility !== 'hidden';
      };

      // OPRAVA: přesná detekce disabled, ne substring match na 'disabled'
      // (třída 'mat-mdc-button-disabled-interactive' NENÍ skutečně disabled tlačítko)
      const disabled = el => {
        return el.disabled === true ||
               el.hasAttribute('disabled') ||
               el.getAttribute('aria-disabled') === 'true';
      };

      const calendar =
        Array.from(document.querySelectorAll('.mat-datepicker-content, mat-calendar, .mat-calendar'))
          .find(visible) || document;

      let nextBtn = Array.from(calendar.querySelectorAll('.mat-calendar-next-button'))
        .find(el => visible(el) && !disabled(el));

      if (nextBtn) {
        nextBtn.click();
        return { found: true, method: 'mat-calendar-next-button', text: norm(nextBtn.innerText || nextBtn.textContent), aria: nextBtn.getAttribute('aria-label') || '' };
      }

      const periodButton = calendar.querySelector('.mat-calendar-period-button');
      if (!periodButton) {
        return { found: false, reason: 'period button not found' };
      }
      const pr = periodButton.getBoundingClientRect();

      const clickableSel = 'button, a, [role=\"button\"], [tabindex]';
      const candidates = Array.from(calendar.querySelectorAll(clickableSel))
        .filter(el => el !== periodButton)
        .filter(el => !el.closest('.mat-calendar-body'))
        .filter(visible)
        .filter(el => !disabled(el))
        .map(el => {
          const r = el.getBoundingClientRect();
          return {
            el, tag: el.tagName, text: norm(el.innerText || el.textContent),
            aria: el.getAttribute('aria-label') || '',
            className: String(el.className || ''),
            x: r.left + r.width / 2, y: r.top + r.height / 2
          };
        })
        .filter(c => Math.abs(c.y - (pr.top + pr.height / 2)) < 10 && c.x > pr.right);

      if (candidates.length === 0) {
        return {
          found: false,
          reason: 'no clickable element right of period button in same row',
          headerHTML: (calendar.querySelector('mat-calendar-header') || calendar).outerHTML.slice(0, 2000)
        };
      }

      const picked =
        candidates.find(c =>
          c.className.toLowerCase().includes('next') ||
          c.aria.toLowerCase().includes('next') ||
          c.aria.toLowerCase().includes('näch') ||
          c.aria.toLowerCase().includes('weiter')
        ) || candidates.sort((a, b) => a.x - b.x)[0];

      const target = picked.el.closest('button, a, [role=\"button\"]') || picked.el;
      target.click();

      return { found: true, method: 'fallback', tag: picked.tag, text: picked.text, aria: picked.aria, className: picked.className };
    })())
  ")
  
  res <- jsonlite::fromJSON(click_result$result$value, simplifyVector = FALSE)
  
  if (!isTRUE(res$found)) {
    cat("Nepodařilo se najít next element:\n")
    print(res)
    stop("Nenašel jsem tlačítko pro další měsíc.")
  }
  
  cat("Kliknuto přes:", res$method, "| text =", res$text %||% "", "| aria =", res$aria %||% "", "\n")
  
  Sys.sleep(1)
  
  wait_for_js(
    b,
    sprintf("
      (function() {
        const norm = s => (s || '').replace(/\\s+/g, ' ').trim();
        const calendar =
          document.querySelector('.mat-datepicker-content') ||
          document.querySelector('mat-calendar') ||
          document.querySelector('.mat-calendar') ||
          document;
        const headerEl = calendar.querySelector('.mat-calendar-period-button');
        const header = norm(headerEl ? (headerEl.innerText || headerEl.textContent) : '');
        const cells = Array.from(calendar.querySelectorAll('.mat-calendar-body-cell'))
          .map(el => norm(el.innerText || el.textContent))
          .join('|');
        return header !== %s || cells !== %s;
      })()
    ", jsq(old_state$header), jsq(old_state$cells)),
    timeout = timeout
  )
  
  new_header <- b$Runtime$evaluate("
    (function() {
      const calendar =
        document.querySelector('.mat-datepicker-content') ||
        document.querySelector('mat-calendar') ||
        document.querySelector('.mat-calendar') ||
        document;
      const headerEl = calendar.querySelector('.mat-calendar-period-button');
      return ((headerEl || {}).innerText || '').trim();
    })()
  ")$result$value
  
  cat("Kalendář po kliku:", new_header, "\n")
  
  TRUE
}


with_retries <- function(expr, attempts = 3, sleep_sec = 3, label = "akce") {
  last_error <- NULL
  
  for (i in seq_len(attempts)) {
    cat(sprintf("\nPokus %d/%d: %s\n", i, attempts, label))
    
    out <- tryCatch(
      {
        force(expr)
      },
      error = function(e) {
        last_error <<- e
        cat("Chyba:", e$message, "\n")
        NULL
      }
    )
    
    if (!is.null(out)) {
      return(out)
    }
    
    Sys.sleep(sleep_sec)
  }
  
  stop(sprintf(
    "Selhalo po %d pokusech: %s | poslední chyba: %s",
    attempts,
    label,
    last_error$message
  ))
}

go_to_reservation_list <- function(b, timeout = 40) {
  
  b$Page$navigate("https://www.hut-reservation.org/reservation/list")
  Sys.sleep(2)
  
  # Kdyby stránka zůstala ve špatném stavu, zkusíme reload
  ok <- tryCatch({
    wait_for_js(
      b,
      "
      Array.from(document.querySelectorAll('button')).some(el => {
        const txt = (el.textContent || '').trim().toUpperCase();
        return txt === 'NEUE RESERVIERUNG';
      })
      ",
      timeout = timeout
    )
    TRUE
  }, error = function(e) FALSE)
  
  if (!ok) {
    cat("NEUE RESERVIERUNG nenalezeno, zkouším reload...\n")
    b$Page$reload(ignoreCache = TRUE)
    Sys.sleep(4)
    
    wait_for_js(
      b,
      "
      Array.from(document.querySelectorAll('button')).some(el => {
        const txt = (el.textContent || '').trim().toUpperCase();
        return txt === 'NEUE RESERVIERUNG';
      })
      ",
      timeout = timeout
    )
  }
  
  TRUE
}

# ------------------------------------------------------------
# Otevření prohlížeče a automatický login
# ------------------------------------------------------------

get_required_env <- function(name) {
  value <- Sys.getenv(name)
  if (!nzchar(value)) {
    stop("Missing required environment variable: ", name)
  }
  value
}

hut_email <- get_required_env("HUT_EMAIL")
hut_password <- get_required_env("HUT_PASSWORD")


b <- ChromoteSession$new()

# Lokálně si klidně nech zobrazit browser, v GitHub Actions ne.
if (Sys.getenv("GITHUB_ACTIONS") != "true") {
  b$view()
}

fill_input <- function(b, selector_js, value, timeout = 30) {
  wait_for_js(b, selector_js, timeout = timeout)

  b$Runtime$evaluate(sprintf("
    (function() {
      const el = %s;
      if (!el) throw new Error('Input not found');

      el.focus();
      el.value = '';

      el.dispatchEvent(new Event('input', { bubbles: true }));
      el.dispatchEvent(new Event('change', { bubbles: true }));
    })()
  ", selector_js))

  Sys.sleep(0.2)
  b$Input$insertText(text = value)

  b$Runtime$evaluate(sprintf("
    (function() {
      const el = %s;
      if (!el) throw new Error('Input not found after typing');

      el.dispatchEvent(new Event('input', { bubbles: true }));
      el.dispatchEvent(new Event('change', { bubbles: true }));
      el.blur();
    })()
  ", selector_js))

  Sys.sleep(0.2)
}

auto_login <- function(b, email, password, timeout = 40, max_attempts = 3) {
  
  email_selector <- "
    document.querySelector('input[type=\"email\"]') ||
    document.querySelector('input[name=\"email\"]') ||
    document.querySelector('input[autocomplete=\"username\"]') ||
    document.querySelector('input[formcontrolname=\"email\"]') ||
    Array.from(document.querySelectorAll('input')).find(el => {
      const meta = (
        (el.placeholder || '') + ' ' +
        (el.name || '') + ' ' +
        (el.id || '') + ' ' +
        (el.outerHTML || '')
      ).toLowerCase();
      return meta.includes('mail') || meta.includes('email') || meta.includes('user') || meta.includes('login');
    })
  "
  
  password_selector <- "
    document.querySelector('input[type=\"password\"]') ||
    document.querySelector('input[name=\"password\"]') ||
    document.querySelector('input[autocomplete=\"current-password\"]') ||
    document.querySelector('input[formcontrolname=\"password\"]')
  "
  
  login_once <- function(attempt) {
    cat(sprintf("\nLogin pokus %s/%s\n", attempt, max_attempts))
    
    cat("Login: otevírám login stránku...\n")
    b$Page$navigate("https://www.hut-reservation.org/login")
    Sys.sleep(3)
    
    # Některé běhy se tváří načteně, ale Angular/formulář je rozbitý.
    # Proto při druhém a dalším pokusu uděláme ještě reload.
    if (attempt > 1) {
      cat("Login: hard reload stránky...\n")
      try(b$Page$reload(ignoreCache = TRUE), silent = TRUE)
      Sys.sleep(4)
    }
    
    cat("Login: vyplňuju email...\n")
    fill_input(b, email_selector, email, timeout = timeout)
    Sys.sleep(0.8)
    
    cat("Login: klikám NEXT...\n")
    click_enabled_button_physical(b, "NEXT", timeout = timeout)
    Sys.sleep(2)
    
    cat("Login: čekám na password field...\n")
    wait_for_js(b, password_selector, timeout = timeout)
    
    cat("Login: vyplňuju password...\n")
    fill_input(b, password_selector, password, timeout = timeout)
    Sys.sleep(0.8)
    
    cat("Login: klikám LOGIN...\n")
    click_enabled_button_physical(b, "LOGIN", timeout = timeout)
    Sys.sleep(6)
    
    cat("Login: ověřuju přihlášení přes seznam rezervací...\n")
    
    # Tohle je důležité: někdy login proběhne, ale appka zůstane divně viset.
    # Zkusíme reservation list, když selže, reloadneme a zkusíme ještě jednou.
    ok <- tryCatch({
      go_to_reservation_list(b, timeout = timeout)
      TRUE
    }, error = function(e) {
      cat("Login: první ověření selhalo, reloaduju a zkouším znovu...\n")
      cat("Důvod:", conditionMessage(e), "\n")
      
      try(b$Page$reload(ignoreCache = TRUE), silent = TRUE)
      Sys.sleep(5)
      
      go_to_reservation_list(b, timeout = timeout)
      TRUE
    })
    
    ok
  }
  
  last_error <- NULL
  
  for (attempt in seq_len(max_attempts)) {
    ok <- tryCatch({
      login_once(attempt)
    }, error = function(e) {
      last_error <<- e
      cat(sprintf("Login pokus %s/%s selhal.\n", attempt, max_attempts))
      cat("Důvod:", conditionMessage(e), "\n")
      FALSE
    })
    
    if (isTRUE(ok)) {
      cat("Login OK.\n")
      return(TRUE)
    }
    
    cat("Login: čistím stav před dalším pokusem...\n")
    try(b$Page$navigate("about:blank"), silent = TRUE)
    Sys.sleep(2)
  }
  
  stop(
    "Login selhal i po ", max_attempts, " pokusech. Poslední chyba: ",
    if (!is.null(last_error)) conditionMessage(last_error) else "neznámá chyba"
  )
}
auto_login(b, hut_email, hut_password)

                 
# ------------------------------------------------------------
# JAKE CHATY JSOU DOSTUPNE K VYHLEDAVANI
# ------------------------------------------------------------

# Přejdi na stránku s novou rezervací
#go_to_reservation_list(b, timeout = 40)
#click_button_text(b, "NEUE RESERVIERUNG")

# Počkej na input
#wait_for_js(b, "document.querySelector('input[placeholder=\"Hütte auswählen\"]')")

# Vytáhni si seznam!
#dostupne_chaty <- get_all_dropdown_options(b)

# Vypiš si je do konzole, ať víš, z čeho vybírat
#print("Seznam chat v menu:")
#print(dostupne_chaty)



# tedka se podivame jake si vybereme do hledani. napriklad pouze AT

#chaty = data.frame(huts = dostupne_chaty) %>%
 # separate(
 #   col = huts, 
 #   into = c("hut_name", "country"), 
 #   sep = ", ", 
 #   remove = FALSE, # FALSE znamená, že ti tam zůstane i ten původní spojený sloupec
 #   extra = "merge", 
 #   fill = "right"
 # )

#chaty %>% write_xlsx("data/chaty.xlsx")
if (!file.exists(input_huts_path)) {
  stop(
    "Nenacházím vstupní soubor s chatami: ", input_huts_path,
    "\nDej chaty.xlsx do složky data/ nebo nastav INPUT_HUTS_XLSX."
  )
}

chaty <- readxl::read_xlsx(input_huts_path)

if (!"huts" %in% names(chaty)) {
  stop("Soubor ", input_huts_path, " musí obsahovat sloupec `huts` s názvy typu 'Adamek-Hütte, AT'.")
}

raw_test_huts <- Sys.getenv("TEST_HUTS", "")

huts_to_check <- if (nzchar(raw_test_huts)) {
  trimws(strsplit(raw_test_huts, "\\|")[[1]])
} else {
  chaty %>%
    pull(huts) %>%
    as.character()
}

huts_to_check <- huts_to_check[nzchar(huts_to_check) & !is.na(huts_to_check)]
huts_to_check <- unique(huts_to_check)

if (!is.na(max_huts) && max_huts > 0) {
  huts_to_check <- head(huts_to_check, max_huts)
}

cat("\n=== SMOKE TEST NASTAVENÍ ===\n")
cat("Soubor chat:", input_huts_path, "\n")
cat("Počet chat:", length(huts_to_check), "\n")
cat("Počet měsíců:", n_months_to_scrape, "\n")
cat("Vybrané chaty:\n")
print(huts_to_check)

if (length(huts_to_check) == 0) {
  stop("Nemám žádné chaty ke kontrole. Zkontroluj data/chaty.xlsx nebo TEST_HUTS.")
}

# ------------------------------------------------------------
# Hlavní smyčka
# ------------------------------------------------------------

start_time <- Sys.time()
total_huts <- length(huts_to_check)
counter <- 1

for (hut in huts_to_check) {
  
  # --- PROGRESS BAR A VÝPOČET ČASU ---
  now <- Sys.time()
  elapsed_secs <- as.numeric(difftime(now, start_time, units = "secs"))
  
  if (counter == 1) {
    eta_str <- "počítám..."
  } else {
    avg_time <- elapsed_secs / (counter - 1)
    remaining_huts <- total_huts - counter + 1
    eta_secs <- avg_time * remaining_huts
    eta_str <- paste0(round(eta_secs), " s")
  }
  
  cat(sprintf(
    "\n[%d/%d] Kontroluji: %s | Uplynulo: %d s | Odhad do konce: %s\n",
    counter, total_huts, hut, round(elapsed_secs), eta_str
  ))
  # -----------------------------------
  
  out <- tryCatch({
    
    with_retries({
      
      go_to_reservation_list(b, timeout = 10)
      
      click_enabled_button_physical(b, "NEUE RESERVIERUNG", timeout = 20)
      
      select_hut_robust(b, hut, timeout = 15)
      
      click_enabled_button_physical(b, "OK", timeout = 10)
      
      # POZOR: nekontrolujeme německá slova v textu (formulář chaty může být
      # v jazyce, který má chata nastavený - např. FR), ale strukturální
      # prvky Angular Material knihovny, které jsou stejné bez ohledu na jazyk.
      wait_for_js(
        b,
        "
      (function() {
        return !!(
          document.querySelector('.mat-datepicker-toggle') ||
          document.querySelector('mat-datepicker-toggle') ||
          document.querySelector('input[matdatepicker]') ||
          document.querySelector('input.mat-date-range-input-inner') ||
          document.querySelectorAll('.mat-form-field').length > 0
        );
      })()
      ",
        timeout = 12
      )
      
      Sys.sleep(2)
      
      open_calendar(b, timeout = 10)

      Sys.sleep(0.5)
      
      hut_months <- list()
      
      for (m in seq_len(n_months_to_scrape)) {
        
        cat("Scrapuji kalendář:", hut, "| měsíc", m, "\n")
        
        wait_for_calendar_counts(b, timeout = 5)

        one_month <- scrape_visible_calendar_month(b) %>%
          mutate(
            Hut = hut,
            month_index = m,
            .before = 1
          )
        
        hut_months[[m]] <- one_month
        
        if (m < n_months_to_scrape) {
          # with_retries({
          #   next_calendar_month(b, timeout = 25)
          # }, attempts = 3, sleep_sec = 2, label = paste("další měsíc:", hut))
          next_calendar_month(b, timeout = 25)
        }
      }
      
      bind_rows(hut_months)
      
    }, attempts = 3, sleep_sec = 4, label = paste("celá chata:", hut))
    
  }, error = function(e) {
    
    tibble(
      Hut = hut,
      month_index = NA_integer_,
      month_header = NA_character_,
      day = NA_integer_,
      free_places = NA_integer_,
      status = "error",
      raw_text = NA_character_,
      aria = NA_character_,
      disabled = NA,
      color = NA_character_,
      error_message = e$message
    )
    
  })
  
  calendar_results <- bind_rows(calendar_results, out)
  
  print(print(
  out %>%
    select(Hut, month_index, month_header, day, free_places, status) %>%
    group_by(month_index, month_header) %>%
    slice_head(n = 5) %>%
    ungroup()),
  n = Inf)
  
  counter <- counter + 1
}

# ------------------------------------------------------------
# Výsledek
# ------------------------------------------------------------

cat("\n=== HOTOVO ===\n")

calendar_results_clean <- calendar_results %>%
  select(Hut, month_index, month_header, day, free_places, status, raw_text, aria, disabled, color) %>%
  arrange(Hut, month_index, day)

print(calendar_results_clean)

writexl::write_xlsx(calendar_results, calendar_results_path)
cat("Zapsáno:", calendar_results_path, "\n")

source(file.path("R", "prepare_availability.R"))

prepare_availability(
  in_xlsx = calendar_results_path,
  out_json = availability_json_path,
  out_csv = availability_long_csv_path
)

cat("Zapsáno:", availability_json_path, "\n")
cat("Zapsáno:", availability_long_csv_path, "\n")


# ------------------------------------------------------------
# Denní archiv výsledků
# ------------------------------------------------------------

history_dir <- file.path("data", "history")
dir.create(history_dir, recursive = TRUE, showWarnings = FALSE)

run_date <- format(Sys.Date(), "%Y-%m-%d")

history_file <- file.path(
  history_dir,
  paste0("availability_", run_date, ".csv.gz")
)

availability_long_history <- readr::read_csv(
  availability_long_csv_path,
  show_col_types = FALSE
) %>%
  dplyr::mutate(scraped_at = Sys.time())

readr::write_csv(
  availability_long_history,
  history_file
)

cat("Historický snapshot uložen:", history_file, "\n")

