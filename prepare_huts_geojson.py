#!/usr/bin/env python3
"""
Convert geocoded hut table into huts.geojson for the Leaflet/Mapy.com map.

Expected input columns can include:
  huts, hut_name, country, lat, lon, status, formatted_address, query_used

The important part is the `huts` column, e.g. "Adamek-Hütte, AT".
It is converted to the same stable `hut_id` as prepare_availability.py:
  "Adamek-Hütte, AT" -> "adamek-hutte-at"

Usage:
  python prepare_huts_geojson.py "Untitled spreadsheet.xlsx" huts.geojson
  python prepare_huts_geojson.py huts.csv huts.geojson

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
from collections import Counter
from pathlib import Path
from typing import Any

NS = {"a": "http://schemas.openxmlformats.org/spreadsheetml/2006/main"}


def slugify_hut_id(raw_hut: str) -> str:
    """Make a stable hut_id, e.g. 'Adolf-Noßberger-Hütte, AT' -> 'adolf-nossberger-hutte-at'."""
    x = str(raw_hut or "").strip()
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
    m = re.match(r"^(.*?),\s*([A-Z]{2})$", str(raw_hut or "").strip())
    if m:
        return m.group(1), m.group(2)
    return str(raw_hut or "").strip(), None


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

    try:
        if "." in raw:
            num = float(raw)
            return int(num) if num.is_integer() else num
        return int(raw)
    except ValueError:
        return raw


def read_first_sheet_xlsx(path: str | Path) -> list[dict[str, Any]]:
    """Read the first worksheet in an .xlsx file into dict rows. Minimal stdlib reader."""
    path = Path(path)
    with zipfile.ZipFile(path) as z:
        shared_strings = _read_shared_strings(z)

        workbook = ET.fromstring(z.read("xl/workbook.xml"))
        first_sheet = workbook.find(".//a:sheets/a:sheet", NS)
        if first_sheet is None:
            raise ValueError("No worksheets found in workbook.")

        # Most simple xlsx files store first sheet as sheet1.xml. This also matches your geocoded table.
        sheet_xml_name = "xl/worksheets/sheet1.xml"
        if sheet_xml_name not in z.namelist():
            candidates = sorted(x for x in z.namelist() if x.startswith("xl/worksheets/sheet") and x.endswith(".xml"))
            if not candidates:
                raise ValueError("No worksheet XML files found in workbook.")
            sheet_xml_name = candidates[0]

        sheet_xml = z.read(sheet_xml_name)

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
    out: list[dict[str, Any]] = []
    for row in rows[1:]:
        row = row + [None] * (len(headers) - len(row))
        out.append(dict(zip(headers, row)))
    return out


def read_csv(path: str | Path) -> list[dict[str, Any]]:
    path = Path(path)
    text = path.read_text(encoding="utf-8-sig")
    sample = text[:4096]
    try:
        dialect = csv.Sniffer().sniff(sample, delimiters=",;\t")
    except csv.Error:
        dialect = csv.excel
    with path.open("r", encoding="utf-8-sig", newline="") as f:
        return list(csv.DictReader(f, dialect=dialect))


def read_table(path: str | Path) -> list[dict[str, Any]]:
    path = Path(path)
    suffix = path.suffix.lower()
    if suffix == ".xlsx":
        return read_first_sheet_xlsx(path)
    if suffix in {".csv", ".txt", ".tsv"}:
        return read_csv(path)
    raise ValueError(f"Unsupported input file type: {suffix}. Use .xlsx or .csv/.tsv.")


def clean_key(key: str) -> str:
    return str(key or "").strip().lower().replace(" ", "_")


def normalize_row_keys(row: dict[str, Any]) -> dict[str, Any]:
    return {clean_key(k): v for k, v in row.items()}


def get_first(row: dict[str, Any], keys: list[str], default: Any = None) -> Any:
    for key in keys:
        if key in row and row[key] not in (None, ""):
            return row[key]
    return default


def parse_float(value: Any) -> float | None:
    if value in (None, ""):
        return None
    if isinstance(value, (int, float)):
        return float(value)
    value = str(value).strip().replace(",", ".")
    try:
        return float(value)
    except ValueError:
        return None


def build_geojson(rows: list[dict[str, Any]]) -> tuple[dict[str, Any], dict[str, Any]]:
    features: list[dict[str, Any]] = []
    skipped: list[dict[str, Any]] = []
    seen_ids: set[str] = set()
    status_counts: Counter[str] = Counter()
    duplicate_ids: Counter[str] = Counter()

    for i, raw_row in enumerate(rows, start=2):  # row 1 is header in the source file
        row = normalize_row_keys(raw_row)

        raw_hut = get_first(row, ["huts", "hut", "raw_hut", "name"])
        if not raw_hut:
            skipped.append({"row": i, "reason": "missing huts/Hut column value"})
            continue
        raw_hut = str(raw_hut).strip()

        hut_id = slugify_hut_id(raw_hut)
        if not hut_id:
            skipped.append({"row": i, "hut": raw_hut, "reason": "empty hut_id after slugify"})
            continue

        if hut_id in seen_ids:
            duplicate_ids[hut_id] += 1
            skipped.append({"row": i, "hut": raw_hut, "hut_id": hut_id, "reason": "duplicate hut_id"})
            continue
        seen_ids.add(hut_id)

        lat = parse_float(get_first(row, ["lat", "latitude"]))
        lon = parse_float(get_first(row, ["lon", "lng", "long", "longitude"]))
        if lat is None or lon is None:
            skipped.append({"row": i, "hut": raw_hut, "hut_id": hut_id, "reason": "missing lat/lon"})
            continue
        if not (-90 <= lat <= 90 and -180 <= lon <= 180):
            skipped.append({"row": i, "hut": raw_hut, "hut_id": hut_id, "reason": f"lat/lon out of range: {lat}, {lon}"})
            continue

        fallback_name, fallback_country = split_hut_country(raw_hut)
        name = str(get_first(row, ["hut_name", "name"], fallback_name)).strip()
        country = get_first(row, ["country"], fallback_country)
        country = str(country).strip() if country not in (None, "") else None

        geocode_status = get_first(row, ["status", "geocode_status"], None)
        geocode_status = str(geocode_status).strip() if geocode_status not in (None, "") else None
        status_counts[geocode_status or ""] += 1

        properties = {
            "hut_id": hut_id,
            "hut": raw_hut,
            "name": name,
            "country": country,
            "geocode_status": geocode_status,
            "formatted_address": get_first(row, ["formatted_address", "address"]),
            "query_used": get_first(row, ["query_used", "query"]),
        }

        # Preserve optional columns if you add them later.
        optional_columns = [
            "reservation_url", "url", "website", "notes", "source", "provider"
        ]
        for col in optional_columns:
            value = get_first(row, [col])
            if value not in (None, ""):
                properties[col] = value

        # If only generic url/website is present, also expose reservation_url for the map popup.
        if "reservation_url" not in properties:
            for alt in ("url", "website"):
                if alt in properties:
                    properties["reservation_url"] = properties[alt]
                    break

        features.append({
            "type": "Feature",
            "properties": properties,
            "geometry": {
                "type": "Point",
                # GeoJSON requires [longitude, latitude].
                "coordinates": [lon, lat],
            },
        })

    features.sort(key=lambda f: (f["properties"].get("country") or "", f["properties"].get("name") or ""))

    geojson = {
        "type": "FeatureCollection",
        "features": features,
    }

    report = {
        "input_rows": len(rows),
        "features_written": len(features),
        "skipped_rows": len(skipped),
        "skipped_examples": skipped[:20],
        "geocode_status_counts": dict(status_counts),
        "duplicate_hut_ids": dict(duplicate_ids),
    }
    return geojson, report


def main() -> None:
    in_path = Path(sys.argv[1]) if len(sys.argv) > 1 else Path("huts.csv")
    out_path = Path(sys.argv[2]) if len(sys.argv) > 2 else Path("huts.geojson")

    rows = read_table(in_path)
    geojson, report = build_geojson(rows)

    with out_path.open("w", encoding="utf-8") as f:
        json.dump(geojson, f, ensure_ascii=False, indent=2)

    print(f"Input rows: {report['input_rows']}")
    print(f"Features written: {report['features_written']}")
    print(f"Skipped rows: {report['skipped_rows']}")
    print(f"Geocode statuses: {report['geocode_status_counts']}")
    if report["duplicate_hut_ids"]:
        print(f"Duplicate hut_ids skipped: {report['duplicate_hut_ids']}")
    if report["skipped_examples"]:
        print("Skipped examples:")
        for item in report["skipped_examples"][:10]:
            print(f"  - {item}")
    print(f"Wrote: {out_path}")


if __name__ == "__main__":
    main()
