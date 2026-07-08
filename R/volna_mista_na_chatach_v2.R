library(chromote)
library(tidyverse)
library(jsonlite)

# ------------------------------------------------------------
# Nastavení
# ------------------------------------------------------------

calendar_results <- tibble()
n_months_to_scrape <- 2

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
  wait_for_js(
    b,
    "
    Array.from(document.querySelectorAll('input')).some(el => {
      const visible = !!(el.offsetWidth || el.offsetHeight || el.getClientRects().length);
      const meta = (
        (el.placeholder || '') + ' ' +
        (el.getAttribute('aria-label') || '') + ' ' +
        (el.outerHTML || '')
      ).toLowerCase();

      return visible && !el.disabled &&
             (meta.includes('datum') || meta.includes('date') || meta.includes('von') || meta.includes('bis'));
    })
    ",
    timeout = timeout
  )
  
  out <- b$Runtime$evaluate("
    JSON.stringify((function() {
      const inputs = Array.from(document.querySelectorAll('input')).filter(el => {
        const visible = !!(el.offsetWidth || el.offsetHeight || el.getClientRects().length);
        const meta = (
          (el.placeholder || '') + ' ' +
          (el.getAttribute('aria-label') || '') + ' ' +
          (el.outerHTML || '')
        ).toLowerCase();

        return visible && !el.disabled &&
               (meta.includes('datum') || meta.includes('date') || meta.includes('von') || meta.includes('bis'));
      });

      const input = inputs[0];
      if (!input) return { found: false };

      const field = input.closest('mat-form-field') || input.parentElement;
      const iconButton = field
        ? field.querySelector('button, .mat-datepicker-toggle button, [mat-icon-button]')
        : null;

      const target = iconButton || input;
      const r = target.getBoundingClientRect();

      return {
        found: true,
        x: r.left + r.width / 2,
        y: r.top + r.height / 2
      };
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
# Otevření prohlížeče a ruční login
# ------------------------------------------------------------

b <- ChromoteSession$new()
b$view()

b$Page$navigate("https://www.hut-reservation.org/login")

readline(
  prompt = "Přihlas se. Jakmile uvidíš seznam svých rezervací, zmáčkni ENTER pro pokračování..."
)

# ------------------------------------------------------------
# JAKE CHATY JSOU DOSTUPNE K VYHLEDAVANI
# ------------------------------------------------------------

# Přejdi na stránku s novou rezervací
go_to_reservation_list(b, timeout = 40)
click_button_text(b, "NEUE RESERVIERUNG")

# Počkej na input
wait_for_js(b, "document.querySelector('input[placeholder=\"Hütte auswählen\"]')")

# Vytáhni si seznam!
dostupne_chaty <- get_all_dropdown_options(b)

# Vypiš si je do konzole, ať víš, z čeho vybírat
print("Seznam chat v menu:")
print(dostupne_chaty)



# tedka se podivame jake si vybereme do hledani. napriklad pouze AT

chaty = data.frame(huts = dostupne_chaty) %>%
  separate(
    col = huts, 
    into = c("hut_name", "country"), 
    sep = ", ", 
    remove = FALSE, # FALSE znamená, že ti tam zůstane i ten původní spojený sloupec
    extra = "merge", 
    fill = "right"
  )

chaty %>% write_xlsx("chaty.xlsx")
library(readxl)
chaty = read_xlsx("chaty.xlsx")

huts_to_check <- chaty %>% 
#  filter(country== "AT") %>%
  pull(huts) 

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
      
      go_to_reservation_list(b, timeout = 40)
      
      click_enabled_button_physical(b, "NEUE RESERVIERUNG", timeout = 30)
      
      select_hut_robust(b, hut, timeout = 35)
      
      click_enabled_button_physical(b, "OK", timeout = 30)
      
      wait_for_js(
        b,
        "
      (function() {
        const txt = (document.body.innerText || '').toLowerCase();

        return txt.includes('verfügbarkeit prüfen') ||
               txt.includes('datum von') ||
               txt.includes('anzahl personen') ||
               txt.includes('matratzenlager') ||
               txt.includes('mehrbettzimmer') ||
               txt.includes('zweierzimmer');
      })()
      ",
        timeout = 50
      )
      
      Sys.sleep(1.5)
      
      open_calendar(b, timeout = 30)
      
      hut_months <- list()
      
      for (m in seq_len(n_months_to_scrape)) {
        
        cat("Scrapuji kalendář:", hut, "| měsíc", m, "\n")
        
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
  
  print(out %>% select(Hut, month_header, day, free_places, status))
  
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

write_xlsx(calendar_results, "calendar_results.xlsx")




####----------------------------------------------------------######
# ted delam to ze transformuju ty vyscrapovana data do json
####----------------------------------------------------------######

library(readxl)
library(jsonlite)
library(stringr)
library(stringi)
library(purrr)

# --- Pomocné funkce ---

slugify_hut_id <- function(raw_hut) {
  # Odstranění mezer na začátku a konci
  x <- trimws(raw_hut)
  
  # Specifické německé náhrady (jako v Pythonu)
  replacements <- c("ß" = "ss", "ẞ" = "ss",
                    "ä" = "a", "ö" = "o", "ü" = "u",
                    "Ä" = "a", "Ö" = "o", "Ü" = "u")
  x <- str_replace_all(x, replacements)
  
  # Převod na ASCII (odstranění zbylé diakritiky)
  x <- stri_trans_general(x, "Latin-ASCII")
  # Na malá písmena
  x <- tolower(x)
  # Náhrada ne-alfanumerických znaků pomlčkou
  x <- str_replace_all(x, "[^a-z0-9]+", "-")
  # Oříznutí pomlček z krajů
  x <- str_remove(x, "^-+")
  x <- str_remove(x, "-+$")
  return(x)
}

split_hut_country <- function(raw_hut) {
  raw_hut <- trimws(raw_hut)
  # Match struktury: "Jméno, AT"
  m <- str_match(raw_hut, "^(.*?),\\s*([A-Z]{2})$")
  
  if (!is.na(m[1, 1])) {
    return(list(name = m[1, 2], country = m[1, 3]))
  } else {
    return(list(name = raw_hut, country = NA_character_))
  }
}

parse_calendar_date <- function(month_header, day) {
  if (is.na(month_header) || month_header == "" || is.na(day) || day == "") {
    return(NA_character_)
  }
  
  mh <- trimws(as.character(month_header))
  m <- str_match(mh, "^(\\d{1,2})/(\\d{4})$")
  
  if (is.na(m[1, 1])) {
    stop(paste("Unexpected month_header:", month_header))
  }
  
  month <- as.integer(m[1, 2])
  year <- as.integer(m[1, 3])
  day_val <- as.integer(day)
  
  # Sestavení ISO data
  res_date <- tryCatch({
    as.Date(paste(year, month, day_val, sep = "-"))
  }, error = function(e) NA)
  
  if (is.na(res_date)) return(NA_character_)
  return(format(res_date, "%Y-%m-%d"))
}

normalize_status <- function(raw_status, free_places, disabled) {
  raw_status <- tolower(trimws(as.character(raw_status)))
  
  if (is.na(free_places) || free_places == "") {
    free_clean <- NA_integer_
  } else {
    free_clean <- as.integer(free_places)
  }
  
  if (length(raw_status) > 0 && raw_status == "error") {
    return(list(status = "error", level = "error", free = free_clean))
  }
  
  # V R může být disabled logická hodnota nebo text. Ošetříme obojí.
  is_disabled <- !is.na(disabled) && (disabled == TRUE || tolower(as.character(disabled)) == "true")
  if (is_disabled) {
    return(list(status = "closed", level = "closed", free = free_clean))
  }
  
  if (length(raw_status) > 0 && raw_status == "unknown" || is.na(free_clean)) {
    return(list(status = "unknown", level = "unknown", free = NA_integer_))
  }
  
  if (free_clean <= 0) {
    return(list(status = "full", level = "full", free = 0))
  }
  
  if (free_clean <= 3) {
    return(list(status = "available", level = "low", free = free_clean))
  }
  if (free_clean <= 9) {
    return(list(status = "available", level = "medium", free = free_clean))
  }
  return(list(status = "available", level = "high", free = free_clean))
}

# --- Hlavní processing logiky ---

build_outputs <- function(df_rows) {
  generated_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
  
  long_rows <- list()
  errors_list <- list()
  huts_list <- list()
  
  # Pokud je tabulka prázdná
  if (nrow(df_rows) == 0) {
    return(list(long_rows = data.frame(), json = list()))
  }
  
  # Iterace přes řádky (v R převedeme na list pro zachování logiky z Pythonu)
  for (i in seq_len(nrow(df_rows))) {
    r <- df_rows[i, ]
    
    raw_hut <- r$Hut
    if (is.na(raw_hut) || trimws(raw_hut) == "") next
    
    raw_hut <- trimws(as.character(raw_hut))
    hut_id <- slugify_hut_id(raw_hut)
    
    hut_info <- split_hut_country(raw_hut)
    hut_name <- hut_info$name
    country <- hut_info$country
    
    raw_status <- if ("status" %in% names(r)) r$status else NA
    
    # Scraper error handling
    if (!is.na(raw_status) && tolower(trimws(as.character(raw_status))) == "error") {
      err_msg <- if ("error_message" %in% names(r)) r$error_message else NA
      if (!hut_id %in% names(errors_list)) errors_list[[hut_id]] <- list()
      
      errors_list[[hut_id]][[length(errors_list[[hut_id]]) + 1]] <- list(
        hut = raw_hut,
        message = if (is.na(err_msg)) NULL else err_msg
      )
      next
    }
    
    # Parsování kalendářního dne
    month_header <- if ("month_header" %in% names(r)) r$month_header else NA
    day <- if ("day" %in% names(r)) r$day else NA
    iso_date <- parse_calendar_date(month_header, day)
    
    if (is.na(iso_date)) next
    
    # Normalizace stavů
    free_places <- if ("free_places" %in% names(r)) r$free_places else NA
    disabled <- if ("disabled" %in% names(r)) r$disabled else NA
    norm <- normalize_status(raw_status, free_places, disabled)
    
    # Příprava objektu pro huts JSON
    if (!hut_id %in% names(huts_list)) {
      huts_list[[hut_id]] <- list(
        hut = raw_hut,
        name = hut_name,
        country = if (is.na(country)) NULL else country,
        calendar = list()
      )
    }
    
    day_obj <- list(
      free = if (is.na(norm$free)) NULL else norm$free,
      status = norm$status,
      level = norm$level
    )
    
    if ("raw_text" %in% names(r) && !is.na(r$raw_text)) day_obj$raw_text <- r$raw_text
    if ("aria" %in% names(r) && !is.na(r$aria)) day_obj$aria <- r$aria
    
    huts_list[[hut_id]]$calendar[[iso_date]] <- day_obj
    
    # Příprava řádku pro CSV plochou tabulku
    long_rows[[length(long_rows) + 1]] <- list(
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
  
  # Převod listu řádků na data.frame
  if (length(long_rows) > 0) {
    df_long <- bind_rows(map(long_rows, as.data.frame, stringsAsFactors = FALSE))
    all_dates <- sort(unique(df_long$date))
  } else {
    df_long <- data.frame()
    all_dates <- c()
  }
  
  # Výpočet agregací / summary pro každou chatu
  for (hut_id in names(huts_list)) {
    cal <- huts_list[[hut_id]]$calendar
    
    # Filtrace volných dní
    available_days <- keep(names(cal), function(d) {
      cal[[d]]$status == "available" && !is.null(cal[[d]]$free) && cal[[d]]$free > 0
    })
    available_days <- sort(available_days)
    
    days_full <- sum(map_lgl(cal, ~ .x$status == "full"))
    days_unknown <- sum(map_lgl(cal, ~ .x$status == "unknown"))
    
    if (length(available_days) > 0) {
      next_avail_date <- available_days[1]
      next_avail_free <- cal[[next_avail_date]]$free
      all_free_counts <- map_int(available_days, ~ cal[[.x]]$free)
      max_free <- max(all_free_counts)
      total_free <- sum(all_free_counts)
    } else {
      next_avail_date <- NULL
      next_avail_free <- NULL
      max_free <- NULL
      total_free <- 0
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
  
  # Seřazení chat abecedně podle klíče (hut_id)
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
  
  return(list(long_rows = df_long, json = availability_json))
}

# pomocná funkce pro rychlé spojení listů do data.frame (podobně jako dplyr::bind_rows)
bind_rows <- function(l) {
  do.call(rbind, l)
}

# --- Main Funkce ---

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  
  in_xlsx  <- if (length(args) >= 1) args[1] else "calendar_results.xlsx"
  out_json <- if (length(args) >= 2) args[2] else "availability.json"
  out_csv  <- if (length(args) >= 3) args[3] else "availability_long.csv"
  
  if (!file.exists(in_xlsx)) {
    stop(paste("Vstupní soubor neexistuje:", in_xlsx))
  }
  
  # Načtení prvního sheetu xlsx
  df_raw <- read_excel(in_xlsx, sheet = 1)
  
  # Spuštění transformace
  outputs <- build_outputs(df_raw)
  
  long_rows <- outputs$long_rows
  availability_json <- outputs$json
  
  # Zápis CSV
  if (nrow(long_rows) > 0) {
    write.csv(long_rows, out_csv, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  } else {
    # Prázdné CSV s hlavičkou
    headers <- data.frame(hut_id=c(), hut_name=c(), country=c(), date=c(), free_places=c(), 
                          status=c(), level=c(), raw_hut=c(), raw_status=c(), raw_text=c(), 
                          aria=c(), disabled=c(), color=c())
    write.csv(headers, out_csv, row.names = FALSE)
  }
  
  # Zápis JSON (auto_unbox = TRUE zajistí, že se skalární hodnoty neuloží jako pole [val])
  writeLines(
    toJSON(availability_json, auto_unbox = TRUE, pretty = TRUE),
    out_json,
    useBytes = TRUE
  )
  
  # Konzoly logy jako v Pythonu
  num_errors <- sum(map_int(availability_json$errors, length))
  
  cat(paste("Input rows:", nrow(df_raw), "\n"))
  cat(paste("Calendar rows written:", nrow(long_rows), "\n"))
  cat(paste("Huts:", length(availability_json$huts), "\n"))
  cat(paste("Date range:", availability_json$date_from, "–", availability_json$date_to, "\n"))
  cat(paste("Errors:", num_errors, "\n"))
  cat(paste("Wrote:", out_json, "\n"))
  cat(paste("Wrote:", out_csv, "\n"))
}

main()























