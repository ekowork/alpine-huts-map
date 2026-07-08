#!/usr/bin/env python3
"""
Convert scraper output calendar_results.xlsx into:
  1) availability_long.csv  - flat/debug table
  2) availability.json      - compact structure for the Leaflet popup calendar

Usage:
  python prepare_availability.py calendar_results.xlsx availability.json availability_long.csv

No external Python packages required.
"""

from __future__ import annotations

import csv
import json
import re
import sys
import unicodedata
import zipfile
import xml.etree.ElementTree as ET
from collections import defaultdict
from datetime import datetime, date
from pathlib import Path
from typing import Any

NS = {"a": "http://schemas.openxmlformats.org/spreadsheetml/2006/main"}


def _excel_col_to_idx(cell_ref: str) -> tuple[int, int]:
    m = re.match(r"([A-Z]+)(\d+)", cell_ref)
    if not m:
        raise ValueError(f"Bad Excel cell reference: {cell_ref}")
    col = 0
    for ch in m.group(1):
        col = col * 26 + ord(ch) - 64
    return col - 1, int(m.group(2)) - 1


def _read_shared_strings(z: zipfile.ZipFile) -> list[str]:
    if "xl/sharedStrings.xml" not in z.namelist():
        return []
    root = ET.fromstring(z.read("xl/sharedStrings.xml"))
    out = []
    for si in root.findall("a:si", NS):
        texts = [t.text or "" for t in si.findall(".//a:t", NS)]
        out.append("".join(texts))
    return out


def _cell_value(cell: ET.Element, shared_strings: list[str]) -> Any:
    cell_type = cell.attrib.get("t")

    if cell_type == "inlineStr":
        return "".join(t.text or "" for t in cell.findall(".//a:t", NS))

    v = cell.find("a:v", NS)
    if v is None:
        return None

    raw = v.text
    if raw is None:
        return None

    if cell_type == "s":
        return shared_strings[int(raw)]
    if cell_type == "b":
        return bool(int(raw))
    if cell_type == "str":
        return raw

    # Numeric cell. Preserve integer-looking values as int.
    try:
        if "." in raw:
            num = float(raw)
            return int(num) if num.is_integer() else num
        return int(raw)
    except ValueError:
        return raw


def read_first_sheet_xlsx(path: str | Path) -> list[dict[str, Any]]:
    """Read the first worksheet into a list of dict rows."""
    with zipfile.ZipFile(path) as z:
        shared_strings = _read_shared_strings(z)
        # For this scraper output the first worksheet is sheet1.xml.
        sheet_xml = z.read("xl/worksheets/sheet1.xml")

    root = ET.fromstring(sheet_xml)

    rows: list[list[Any]] = []
    max_col = 0

    for row in root.findall(".//a:sheetData/a:row", NS):
        row_idx = int(row.attrib["r"]) - 1
        values_by_col: dict[int, Any] = {}

        for cell in row.findall("a:c", NS):
            col_idx, _ = _excel_col_to_idx(cell.attrib["r"])
            values_by_col[col_idx] = _cell_value(cell, shared_strings)
            max_col = max(max_col, col_idx)

        while len(rows) <= row_idx:
            rows.append([])
        rows[row_idx] = [values_by_col.get(i) for i in range(max_col + 1)]

    if not rows:
        return []

    headers = [str(x).strip() if x is not None else "" for x in rows[0]]
    dict_rows = []
    for row in rows[1:]:
        row = row + [None] * (len(headers) - len(row))
        dict_rows.append(dict(zip(headers, row)))
    return dict_rows


def slugify_hut_id(raw_hut: str) -> str:
    """Make a stable hut_id, e.g. 'Adolf-Noßberger-Hütte, AT' -> 'adolf-nossberger-hutte-at'."""
    x = raw_hut.strip()
    replacements = {
        "ß": "ss", "ẞ": "ss",
        "ä": "a", "ö": "o", "ü": "u",
        "Ä": "a", "Ö": "o", "Ü": "u",
    }
    for old, new in replacements.items():
        x = x.replace(old, new)
    x = unicodedata.normalize("NFKD", x).encode("ascii", "ignore").decode("ascii")
    x = x.lower()
    x = re.sub(r"[^a-z0-9]+", "-", x).strip("-")
    return x


def split_hut_country(raw_hut: str) -> tuple[str, str | None]:
    """Split 'Adamek-Hütte, AT' -> ('Adamek-Hütte', 'AT')."""
    m = re.match(r"^(.*?),\s*([A-Z]{2})$", raw_hut.strip())
    if m:
        return m.group(1), m.group(2)
    return raw_hut.strip(), None


def parse_calendar_date(month_header: Any, day: Any) -> str | None:
    """Convert month_header='7/2026', day=14 into '2026-07-14'."""
    if month_header in (None, "") or day in (None, ""):
        return None

    mh = str(month_header).strip()
    m = re.match(r"^(\d{1,2})/(\d{4})$", mh)
    if not m:
        raise ValueError(f"Unexpected month_header: {month_header!r}")

    month = int(m.group(1))
    year = int(m.group(2))
    return date(year, month, int(day)).isoformat()


def normalize_status(raw_status: Any, free_places: Any, disabled: Any) -> tuple[str, str, int | None]:
    """
    Return (status, level, free_places_clean).

    status = semantic state used in the popup
    level  = visual bucket used for colors
    """
    raw_status = str(raw_status or "").strip().lower()

    if free_places in (None, ""):
        free_clean = None
    else:
        free_clean = int(free_places)

    if raw_status == "error":
        return "error", "error", free_clean

    if disabled is True:
        return "closed", "closed", free_clean

    if raw_status == "unknown" or free_clean is None:
        return "unknown", "unknown", None

    if free_clean <= 0:
        return "full", "full", 0

    # Keep status semantically available; use level for color intensity.
    if free_clean <= 3:
        return "available", "low", free_clean
    if free_clean <= 9:
        return "available", "medium", free_clean
    return "available", "high", free_clean


def build_outputs(rows: list[dict[str, Any]]) -> tuple[list[dict[str, Any]], dict[str, Any]]:
    generated_at = datetime.now().replace(microsecond=0).isoformat()

    long_rows: list[dict[str, Any]] = []
    errors: dict[str, list[dict[str, Any]]] = defaultdict(list)
    huts: dict[str, dict[str, Any]] = {}

    for r in rows:
        raw_hut = r.get("Hut")
        if not raw_hut:
            continue

        raw_hut = str(raw_hut).strip()
        hut_id = slugify_hut_id(raw_hut)
        hut_name, country = split_hut_country(raw_hut)
        raw_status = r.get("status")

        # Scraper-level error rows do not represent a calendar day.
        if str(raw_status).lower() == "error":
            errors[hut_id].append({
                "hut": raw_hut,
                "message": r.get("error_message"),
            })
            continue

        iso_date = parse_calendar_date(r.get("month_header"), r.get("day"))
        if iso_date is None:
            continue

        status, level, free_clean = normalize_status(raw_status, r.get("free_places"), r.get("disabled"))

        huts.setdefault(hut_id, {
            "hut": raw_hut,
            "name": hut_name,
            "country": country,
            "calendar": {},
        })

        day_obj = {
            "free": free_clean,
            "status": status,
            "level": level,
        }

        # Keep debug fields only if useful for later troubleshooting.
        if r.get("raw_text") is not None:
            day_obj["raw_text"] = r.get("raw_text")
        if r.get("aria") is not None:
            day_obj["aria"] = r.get("aria")

        huts[hut_id]["calendar"][iso_date] = day_obj

        long_rows.append({
            "hut_id": hut_id,
            "hut_name": hut_name,
            "country": country,
            "date": iso_date,
            "free_places": free_clean,
            "status": status,
            "level": level,
            "raw_hut": raw_hut,
            "raw_status": raw_status,
            "raw_text": r.get("raw_text"),
            "aria": r.get("aria"),
            "disabled": r.get("disabled"),
            "color": r.get("color"),
        })

    all_dates = sorted({r["date"] for r in long_rows})

    # Add summaries for popup / marker coloring.
    for hut_id, h in huts.items():
        calendar = h["calendar"]
        available = [
            (d, v["free"])
            for d, v in calendar.items()
            if v.get("status") == "available" and isinstance(v.get("free"), int) and v.get("free") > 0
        ]
        available_sorted = sorted(available)
        h["summary"] = {
            "days_total": len(calendar),
            "days_available": len(available_sorted),
            "days_full": sum(1 for v in calendar.values() if v.get("status") == "full"),
            "days_unknown": sum(1 for v in calendar.values() if v.get("status") == "unknown"),
            "next_available_date": available_sorted[0][0] if available_sorted else None,
            "next_available_free": available_sorted[0][1] if available_sorted else None,
            "max_free_places": max((free for _, free in available_sorted), default=None),
            "total_free_place_days": sum(free for _, free in available_sorted),
        }

    availability_json = {
        "generated_at": generated_at,
        "date_from": all_dates[0] if all_dates else None,
        "date_to": all_dates[-1] if all_dates else None,
        "errors": dict(errors),
        "huts": dict(sorted(huts.items())),
    }

    return long_rows, availability_json


def write_long_csv(rows: list[dict[str, Any]], path: str | Path) -> None:
    fieldnames = [
        "hut_id", "hut_name", "country", "date", "free_places", "status", "level",
        "raw_hut", "raw_status", "raw_text", "aria", "disabled", "color",
    ]
    with open(path, "w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(rows)


def main() -> None:
    in_xlsx = Path(sys.argv[1]) if len(sys.argv) > 1 else Path("calendar_results.xlsx")
    out_json = Path(sys.argv[2]) if len(sys.argv) > 2 else Path("availability.json")
    out_csv = Path(sys.argv[3]) if len(sys.argv) > 3 else Path("availability_long.csv")

    rows = read_first_sheet_xlsx(in_xlsx)
    long_rows, availability_json = build_outputs(rows)

    write_long_csv(long_rows, out_csv)
    with open(out_json, "w", encoding="utf-8") as f:
        json.dump(availability_json, f, ensure_ascii=False, indent=2, sort_keys=True)

    print(f"Input rows: {len(rows)}")
    print(f"Calendar rows written: {len(long_rows)}")
    print(f"Huts: {len(availability_json['huts'])}")
    print(f"Date range: {availability_json['date_from']} – {availability_json['date_to']}")
    print(f"Errors: {sum(len(v) for v in availability_json['errors'].values())}")
    print(f"Wrote: {out_json}")
    print(f"Wrote: {out_csv}")


if __name__ == "__main__":
    main()
