# ── 006_extract_pdf_tables.py ────────────────────────────────────────────────
# Extrae Cuadros 1-4 de las Tarjetas Semanales CIS (167 PDFs, 2020-2026)
# y los acumula en 4 CSVs históricos.
#
# Uso:
#   python rscrips/006_extract_pdf_tables.py              # extracción completa
#   python rscrips/006_extract_pdf_tables.py --incremental # solo PDFs nuevos
#   python rscrips/006_extract_pdf_tables.py --test-single "CIS_2026Febrero_28.pdf"
# ─────────────────────────────────────────────────────────────────────────────

import re
import sys
import logging
import argparse
from pathlib import Path
from datetime import date

import pandas as pd
import pdfplumber

# ── RUTAS ─────────────────────────────────────────────────────────────────────

PROJECT_ROOT = Path(__file__).parent.parent
PDF_DIR      = PROJECT_ROOT / "Tarjetas semanales de Contratación y Conflictividad Colectiva"
OUTPUT_DIR   = PROJECT_ROOT / "csvs"
LOG_PATH     = OUTPUT_DIR / "extraction_log.txt"

REFORMA_CUTOFF = date(2022, 1, 1)

PYTHON_PATH = Path(__file__).parent.parent  # solo para documentación

# ── CATÁLOGOS ─────────────────────────────────────────────────────────────────

MESES = {
    "enero": 1, "febrero": 2, "marzo": 3, "abril": 4,
    "mayo": 5, "junio": 6, "julio": 7, "agosto": 8,
    "septiembre": 9, "octubre": 10, "noviembre": 11, "diciembre": 12,
}

CUADRO_KEYWORDS = {
    1: ["PRINCIPALES REVISIONES SALARIALES", "CUADRO 1"],
    2: ["POR INSTANCIA", "CUADRO 2"],
    3: ["CONVENIOS DONDE SE NEGOCIARON", "CUADRO 3"],
    4: ["ESTALLADAS VIGENTES", "CUADRO 4"],
}

# ── SCHEMAS CANÓNICOS ─────────────────────────────────────────────────────────

C1_COLS = [
    "fecha_reporte", "version_formato", "empresa", "entidad",
    "tipo_revision", "propiedad", "central_obrera", "instancia",
    "rama_actividad", "trabajadores", "pct_sindicalizados",
    "salario_diario", "nominal_pct", "real_pct",
]
C2_COLS = [
    "fecha_reporte", "version_formato", "periodo", "instancia",
    "revisiones_anio_anterior", "revisiones_anio_actual",
    "nominal_anio_anterior", "real_anio_anterior",
    "nominal_anio_actual", "real_anio_actual",
    "trabajadores_anio_anterior", "trabajadores_anio_actual",
]
C3_COLS = [
    "fecha_reporte", "version_formato", "empresa", "entidad",
    "propiedad", "rama", "central", "num_trabajadores",
    "bono_forma_pago", "bono_meta",
]
C4_COLS = [
    "fecha_reporte", "version_formato", "empresa", "entidad", "municipio",
    "sindicato", "propiedad", "rama", "central", "causa",
    "instancia", "fecha_inicio", "duracion_dias", "trabajadores",
]

SCHEMA_MAP = {1: C1_COLS, 2: C2_COLS, 3: C3_COLS, 4: C4_COLS}

OUTPUT_FILES = {
    1: OUTPUT_DIR / "cuadro1_revisiones_acumulado.csv",
    2: OUTPUT_DIR / "cuadro2_instancias_acumulado.csv",
    3: OUTPUT_DIR / "cuadro3_bonos_acumulado.csv",
    4: OUTPUT_DIR / "cuadro4_huelgas_vigentes_acumulado.csv",
}

# ── TABLE SETTINGS ────────────────────────────────────────────────────────────

SETTINGS_LINES = {
    "vertical_strategy": "lines",
    "horizontal_strategy": "lines",
    "snap_tolerance": 3,
    "join_tolerance": 3,
    "edge_min_length": 10,
}
SETTINGS_TEXT = {
    "vertical_strategy": "text",
    "horizontal_strategy": "text",
    "snap_tolerance": 3,
    "join_tolerance": 3,
}

# ── LOGGING ───────────────────────────────────────────────────────────────────

def setup_logging(log_path: Path):
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
    fmt = "%(asctime)s | %(levelname)-8s | %(message)s"
    datefmt = "%Y-%m-%d %H:%M:%S"
    logging.basicConfig(
        level=logging.INFO,
        format=fmt,
        datefmt=datefmt,
        handlers=[
            logging.FileHandler(log_path, encoding="utf-8", mode="a"),
            logging.StreamHandler(sys.stdout),
        ],
    )

logger = logging.getLogger(__name__)

# ── PARSEO DE FECHAS ──────────────────────────────────────────────────────────

def parse_filename_date(stem: str) -> "date | None":
    """
    Parsea la fecha del nombre de archivo (sin extensión).

    Formatos soportados:
      CIS_{YYYY}S?{Mes}_{DD}      (nuevo con guion bajo, 2022+)
      CIS_{YYYY}S?{Mes}{DD}       (nuevo sin guion bajo, mid-2021)
      CIS_{YYYY}S?{Mes}_Cierre    (cierre de año, sin día → último día del mes)
      CIS-{DD}[sep]{Mes}[sep]{YYYY}  (viejo, 2020-2021)

    Edge cases:
      - CIS-20 Febrero2020.docx  → strip .docx primero
      - CIS_2025SOctubre_09      → S typo antes del mes
      - CIS_2022Diciembre_Cierre → sin día numérico → último día del mes
      - CIS-12 Noviembre-2020 (002) → strip sufijo (002)
      - Septiembre (tiene e simple en filenames) → usar [^\\W\\d_] con UNICODE
    """
    import datetime as _dt

    # Strip .docx si está en el stem
    clean = re.sub(r"\.docx$", "", stem, flags=re.IGNORECASE)
    # Strip sufijos como " (002)"
    clean = re.sub(r"\s*\(\d+\)\s*$", "", clean).strip()

    # Patrón de letras que incluye caracteres acentuados (usar \w con UNICODE
    # es más robusto que rangos de código en Python re)
    LETRA = r"[^\W\d_]"

    def _resolve_month(month_s: str) -> "int | None":
        """Busca el mes; si no lo encuentra, intenta quitando S inicial (typo SOctubre)."""
        mn = MESES.get(month_s.lower())
        if mn:
            return mn
        # Typo: 'S' extra antes del nombre real (ej. SOctubre → Octubre)
        if month_s.upper().startswith("S"):
            return MESES.get(month_s[1:].lower())
        return None

    # ── Formato nuevo con guion bajo: CIS_{YYYY}{Mes}_{DD} ───────────────────
    m = re.match(
        rf"CIS_(\d{{4}})({LETRA}+)_(\d{{1,2}})$",
        clean, re.IGNORECASE | re.UNICODE,
    )
    if m:
        year_s, month_s, day_s = m.group(1), m.group(2), m.group(3)
        month_num = _resolve_month(month_s)
        if month_num:
            try:
                return date(int(year_s), month_num, int(day_s))
            except ValueError:
                pass

    # ── Formato nuevo SIN guion bajo: CIS_{YYYY}{Mes}{DD} ────────────────────
    # Ej: CIS_2021Agosto12
    m = re.match(
        rf"CIS_(\d{{4}})({LETRA}+?)(\d{{1,2}})$",
        clean, re.IGNORECASE | re.UNICODE,
    )
    if m:
        year_s, month_s, day_s = m.group(1), m.group(2), m.group(3)
        month_num = _resolve_month(month_s)
        if month_num:
            try:
                return date(int(year_s), month_num, int(day_s))
            except ValueError:
                pass

    # ── Formato nuevo con "Cierre": CIS_{YYYY}{Mes}_Cierre ───────────────────
    m = re.match(
        rf"CIS_(\d{{4}})({LETRA}+)_Cierre$",
        clean, re.IGNORECASE | re.UNICODE,
    )
    if m:
        year_s, month_s = m.group(1), m.group(2)
        month_num = _resolve_month(month_s)
        if month_num:
            next_month = date(int(year_s), month_num % 12 + 1, 1) if month_num < 12 \
                         else date(int(year_s) + 1, 1, 1)
            last_day = (next_month - _dt.timedelta(days=1)).day
            return date(int(year_s), month_num, last_day)

    # ── Formato viejo: CIS-{DD}[sep]{Mes}[sep]{YYYY} ─────────────────────────
    m = re.match(
        rf"CIS-(\d{{1,2}})[\s\-]?({LETRA}+)[\-\s]?(\d{{4}})$",
        clean, re.IGNORECASE | re.UNICODE,
    )
    if m:
        day_s, month_s, year_s = m.group(1), m.group(2), m.group(3)
        month_num = MESES.get(month_s.lower())
        if month_num:
            try:
                return date(int(year_s), month_num, int(day_s))
            except ValueError:
                pass

    return None


def detect_version_formato(fecha: date) -> str:
    return "pre_reforma" if fecha < REFORMA_CUTOFF else "post_reforma"


# ── DETECCIÓN DE PÁGINAS POR CUADRO ──────────────────────────────────────────

def _page_contains(page_text: str, keywords: list[str]) -> bool:
    text_up = (page_text or "").upper()
    return any(kw.upper() in text_up for kw in keywords)


def find_cuadro_pages(pdf: "pdfplumber.PDF", cuadro_num: int) -> list[int]:
    """
    Devuelve índices (0-based) de las páginas que pertenecen al cuadro.
    Para Cuadro 4 (multi-página) acumula hasta encontrar indicador de fin.
    """
    target_kws   = CUADRO_KEYWORDS[cuadro_num]
    next_kws     = CUADRO_KEYWORDS.get(cuadro_num + 1, [])
    collecting   = False
    pages_found  = []

    for i, page in enumerate(pdf.pages):
        try:
            text = page.extract_text() or ""
        except Exception:
            text = ""

        if not collecting:
            if _page_contains(text, target_kws):
                collecting = True
                pages_found.append(i)
        else:
            # Cuadros 1-3: solo una página cada uno
            if cuadro_num < 4:
                break
            # Cuadro 4: continuar hasta encontrar el siguiente cuadro o fin
            if next_kws and _page_contains(text, next_kws):
                break
            # Indicadores de fin del cuadro 4
            if "FUENTE:" in text.upper() and len(pages_found) >= 2:
                pages_found.append(i)
                break
            pages_found.append(i)

    return pages_found


# ── EXTRACCIÓN DE TABLAS ──────────────────────────────────────────────────────

def extract_tables_from_pages(pages: list, filename: str, cuadro_num: int) -> list[list]:
    """Extrae filas crudas de las páginas. Intenta 'lines', fallback a 'text'."""
    all_rows = []
    for page in pages:
        tables = []
        try:
            tables = page.extract_tables(SETTINGS_LINES)
        except Exception:
            pass

        if not tables:
            try:
                tables = page.extract_tables(SETTINGS_TEXT)
                if tables:
                    logger.debug("%s | cuadro%d | usó estrategia 'text'", filename, cuadro_num)
            except Exception:
                pass

        for tbl in tables:
            all_rows.extend(tbl)

    return all_rows


def clean_raw_table(raw_rows: list[list]) -> list[list]:
    """Limpia whitespace, None → '', elimina filas vacías."""
    cleaned = []
    for row in raw_rows:
        if row is None:
            continue
        clean_row = [
            str(cell).strip().replace("\n", " ") if cell is not None else ""
            for cell in row
        ]
        if any(c != "" for c in clean_row):
            cleaned.append(clean_row)
    return cleaned


def _remove_duplicate_headers(rows: list[list], header_cell: str) -> list[list]:
    """Elimina filas que repiten el encabezado (aparecen en saltos de página)."""
    return [r for r in rows if not (r and r[0].strip().upper() == header_cell.upper())]


# ── NORMALIZACIÓN POR CUADRO ──────────────────────────────────────────────────

def _empty_df(cols: list[str]) -> pd.DataFrame:
    return pd.DataFrame(columns=cols)


def normalize_cuadro1(raw_rows: list[list], fecha: date, filename: str) -> pd.DataFrame:
    rows = clean_raw_table(raw_rows)
    # Encontrar la fila de encabezado (la que contiene "Empresa")
    data_start = 0
    for i, row in enumerate(rows):
        row_text = " ".join(row).lower()
        if "empresa" in row_text:
            data_start = i + 1
            break

    data_rows = rows[data_start:]
    if not data_rows:
        logger.warning("%s | cuadro1 | sin filas de datos", filename)
        return _empty_df(C1_COLS)

    records = []
    ver = detect_version_formato(fecha)
    fecha_s = fecha.isoformat()

    for row in data_rows:
        # Saltar filas de notas al pie (empiezan con número o "FUENTE")
        if not row:
            continue
        first = row[0].strip()
        if re.match(r"^\d\s", first) or "FUENTE" in first.upper() or "NOTA" in first.upper():
            continue
        # Saltar si parece footer/total
        if first.upper().startswith("TOTAL"):
            continue

        # Mapeo posicional — los PDFs tienen 12+ columnas
        # Posiciones: 0=empresa, 1=entidad, 2=tipo_rev, 3=propiedad,
        #             4=central, 5=instancia, 6=rama, 7=trabajadores,
        #             8=pct_sind, 9=salario, 10=nominal, 11=real
        def get(i): return row[i].strip() if i < len(row) else ""

        rec = {
            "fecha_reporte":      fecha_s,
            "version_formato":    ver,
            "empresa":            get(0),
            "entidad":            get(1),
            "tipo_revision":      get(2),
            "propiedad":          get(3),
            "central_obrera":     get(4),
            "instancia":          get(5),
            "rama_actividad":     get(6),
            "trabajadores":       get(7),
            "pct_sindicalizados": get(8),
            "salario_diario":     get(9),
            "nominal_pct":        get(10),
            "real_pct":           get(11),
        }
        # Solo guardar si tiene al menos nombre de empresa
        if rec["empresa"]:
            records.append(rec)

    if not records:
        logger.warning("%s | cuadro1 | 0 registros válidos", filename)
        return _empty_df(C1_COLS)

    return pd.DataFrame(records, columns=C1_COLS)


def normalize_cuadro2(raw_rows: list[list], fecha: date, filename: str) -> pd.DataFrame:
    rows = clean_raw_table(raw_rows)
    if not rows:
        logger.warning("%s | cuadro2 | sin filas", filename)
        return _empty_df(C2_COLS)

    ver = detect_version_formato(fecha)
    fecha_s = fecha.isoformat()

    # La tabla tiene 2 secciones de período separadas por una fila de encabezado
    # Detectar filas "TOTAL", "A) CONCILIADAS", "B) RATIFICADAS"
    records = []
    periodo_actual = ""
    for row in rows:
        first = row[0].strip().upper() if row else ""

        # Detectar período
        if "1° AL" in first or "1 AL" in first or "FEBRERO" in first:
            periodo_actual = " ".join(row).strip()
            continue
        if "1° DE ENERO" in first or "1 DE ENERO" in first or "ENERO AL" in first:
            periodo_actual = " ".join(row).strip()
            continue
        if "ENERO" in first and "FEBRERO" in first:
            periodo_actual = " ".join(row).strip()
            continue

        # Detectar instancia
        instancia = ""
        if first.startswith("TOTAL"):
            instancia = "TOTAL"
        elif "CONCILIADA" in first:
            instancia = "Conciliadas"
        elif "RATIFICADA" in first:
            instancia = "Ratificadas"

        if instancia:
            def get(i): return row[i].strip() if i < len(row) else ""
            rec = {
                "fecha_reporte":           fecha_s,
                "version_formato":         ver,
                "periodo":                 periodo_actual,
                "instancia":               instancia,
                "revisiones_anio_anterior": get(1),
                "revisiones_anio_actual":   get(2),
                "nominal_anio_anterior":    get(3),
                "real_anio_anterior":       get(4),
                "nominal_anio_actual":      get(5),
                "real_anio_actual":         get(6),
                "trabajadores_anio_anterior": get(7) if len(row) > 7 else "",
                "trabajadores_anio_actual":   get(8) if len(row) > 8 else "",
            }
            records.append(rec)

    if not records:
        logger.warning("%s | cuadro2 | 0 registros de instancias detectados", filename)
        return _empty_df(C2_COLS)

    return pd.DataFrame(records, columns=C2_COLS)


def normalize_cuadro3(raw_rows: list[list], fecha: date, filename: str) -> pd.DataFrame:
    rows = clean_raw_table(raw_rows)
    # Encontrar inicio de datos
    data_start = 0
    for i, row in enumerate(rows):
        row_text = " ".join(row).lower()
        if "empresa" in row_text and ("bono" in row_text or "productividad" in row_text):
            data_start = i + 1
            break

    data_rows = rows[data_start:]
    if not data_rows:
        logger.info("%s | cuadro3 | sin bonos de productividad este período", filename)
        return _empty_df(C3_COLS)

    ver = detect_version_formato(fecha)
    fecha_s = fecha.isoformat()
    records = []

    for row in data_rows:
        if not row:
            continue
        first = row[0].strip()
        if not first or "FUENTE" in first.upper() or re.match(r"^\d\s", first):
            continue

        def get(i): return row[i].strip() if i < len(row) else ""

        rec = {
            "fecha_reporte":   fecha_s,
            "version_formato": ver,
            "empresa":         get(0),
            "entidad":         get(1),
            "propiedad":       get(2),
            "rama":            get(3),
            "central":         get(4),
            "num_trabajadores": get(5),
            "bono_forma_pago": get(6),
            "bono_meta":       get(7),
        }
        if rec["empresa"]:
            records.append(rec)

    if not records:
        logger.info("%s | cuadro3 | 0 registros (semana sin bonos)", filename)
        return _empty_df(C3_COLS)

    return pd.DataFrame(records, columns=C3_COLS)


def _classify_causa(causa_text: str) -> str:
    """Clasifica texto fragmentado de causa en categorías canónicas."""
    t = causa_text.lower()
    if "contrato ley" in t:
        return "Violación de contrato ley"
    if "firma" in t:
        return "Firma de contrato"
    if "anual" in t or "salarial" in t:
        return "Revisión salarial"
    if "reparto" in t:
        return "Reparto de utilidades"
    if "revisión de" in t or "revision de" in t:
        return "Revisión de contrato"
    if "violación de" in t or "violacion de" in t:
        return "Violación de contrato"
    return causa_text.strip()


# Bounds X por defecto (fallback si la detección dinámica falla).
# El orden de las columnas en el PDF es:
# empresa | entidad+municipio | sindicato | propiedad | rama | central |
# causa | instancia | fecha_inicio | duracion_dias | trabajadores
_C4_COL_NAMES = [
    "empresa", "entidad_municipio", "sindicato", "propiedad", "rama",
    "central", "causa", "instancia", "fecha_inicio", "duracion_dias",
    "trabajadores",
]
_C4_COL_BOUNDS_FALLBACK = {
    "empresa":           (18, 96),
    "entidad_municipio": (96, 198),
    "sindicato":         (198, 286),
    "propiedad":         (286, 328),
    "rama":              (328, 366),
    "central":           (366, 393),
    "causa":             (393, 434),
    "instancia":         (434, 473),
    "fecha_inicio":      (473, 515),
    "duracion_dias":     (515, 545),
    "trabajadores":      (545, 612),
}

_DATE_RE = re.compile(r"^\d{2}/\d{2}/\d{4}$")
_MULTI_ENT_RE = re.compile(r"^m[áa]s\s+de\s+una\s+entidad", re.IGNORECASE)


def _split_entidad_municipio(raw: str) -> tuple[str, str]:
    """
    Separa el texto crudo de la celda 'Entidad Federativa y Municipio' del PDF
    en dos campos: entidad y municipio.

    Casos:
      "Tamaulipas, Matamoros"               → ("Tamaulipas", "Matamoros")
      "Más de una entidad, Cuauhtémoc"      → ("Más de una entidad", "Cuauhtémoc")
      "Más de una entidad"                  → ("Más de una entidad", "")
      "Baja California, Sierra Mojada"      → ("Baja California", "Sierra Mojada")
      "Coahuila"                            → ("Coahuila", "")
    """
    s = " ".join((raw or "").split()).strip().strip(",").strip()
    if not s:
        return "", ""
    if _MULTI_ENT_RE.match(s):
        rest = _MULTI_ENT_RE.sub("", s, count=1).lstrip(", ").strip()
        return "Más de una entidad", rest
    parts = s.split(",", 1)
    if len(parts) == 1:
        return parts[0].strip(), ""
    return parts[0].strip(), parts[1].strip()


def _detect_c4_layout(page) -> "tuple[tuple, dict] | None":
    """
    Detecta el área tabular del Cuadro 4 en una página y los cortes X de columnas.

    `find_tables` suele devolver muchas mini-tablas (una por fila visual del
    PDF). Se toman las filas con 11 celdas (firma del Cuadro 4), se promedian
    sus bounds X para tener cortes robustos, y se une el bbox vertical de
    todas las mini-tablas para cubrir el área completa.

    Devuelve (bbox, col_bounds) o None si no se detecta el Cuadro 4.
    """
    try:
        tables = page.find_tables(SETTINGS_LINES) or []
    except Exception:
        tables = []

    eleven_cell_rows = []  # filas con 11 celdas
    bboxes = []            # bboxes de las tablas que contienen al menos una fila de 11

    for tbl in tables:
        has_eleven = False
        for r in tbl.rows:
            cells = [c for c in r.cells if c is not None]
            if len(cells) == 11:
                eleven_cell_rows.append(cells)
                has_eleven = True
        if has_eleven:
            bboxes.append(tbl.bbox)

    if not eleven_cell_rows or not bboxes:
        return None

    # Promediar cortes X de las filas de 11 celdas (más robusto que tomar una sola)
    col_bounds: dict[str, tuple[float, float]] = {}
    for i, name in enumerate(_C4_COL_NAMES):
        x0s = [row[i][0] for row in eleven_cell_rows]
        x1s = [row[i][2] for row in eleven_cell_rows]
        col_bounds[name] = (sum(x0s) / len(x0s), sum(x1s) / len(x1s))

    x0 = min(b[0] for b in bboxes)
    top = min(b[1] for b in bboxes)
    x1 = max(b[2] for b in bboxes)
    bottom = max(b[3] for b in bboxes)
    return (x0, top, x1, bottom), col_bounds


def _extract_cuadro4_by_words(pages: list, fecha: date, filename: str) -> pd.DataFrame:
    """
    Extrae Cuadro 4 usando posiciones de palabras (word-level extraction).

    Procesa cada página independientemente: detecta la región tabular y los
    cortes de columna con `page.find_tables()`, filtra palabras a la región
    detectada (excluye logo/título/pie de página/notas), y usa las fechas
    DD/MM/YYYY de la columna fecha_inicio como ancla para delimitar bandas
    por registro. La fila de encabezado (etiquetas "Empresa", "Sindicato"…)
    se excluye automáticamente porque queda arriba del primer ancla de fecha.
    """
    ver = detect_version_formato(fecha)
    fecha_s = fecha.isoformat()

    records: list[dict] = []

    for page in pages:
        layout = _detect_c4_layout(page)
        if layout is None:
            col_bounds = dict(_C4_COL_BOUNDS_FALLBACK)
        else:
            _bbox, col_bounds = layout

        try:
            words = page.extract_words()
        except Exception:
            words = []
        if not words:
            continue

        fecha_lo, fecha_hi = col_bounds["fecha_inicio"]
        date_anchors = sorted(
            [w for w in words
             if fecha_lo <= w["x0"] < fecha_hi
             and _DATE_RE.match(w["text"].strip())],
            key=lambda w: w["top"],
        )
        if not date_anchors:
            continue

        # Pitch típico entre filas (mediana). Solo se usa como fallback para
        # los casos con un solo registro. En general, se usa el pitch local.
        if len(date_anchors) >= 2:
            diffs = sorted(date_anchors[i + 1]["top"] - date_anchors[i]["top"]
                           for i in range(len(date_anchors) - 1))
            median_pitch = diffs[len(diffs) // 2]
        else:
            median_pitch = 40.0

        # Piso absoluto: el encabezado de página (logo + título repetido)
        # ocupa los primeros ~40pt. Cualquier dato real está abajo de eso.
        HEADER_SAFE_Y = 40.0

        for idx, anchor in enumerate(date_anchors):
            y_date = anchor["top"]
            # Banda superior:
            #   - registros intermedios: punto medio con el ancla previa
            #   - PRIMER registro: 45% del pitch al siguiente (captura wrap
            #     de sindicato/empresa arriba del ancla) con piso en
            #     HEADER_SAFE_Y para excluir encabezado de página
            if idx > 0:
                y_top = (date_anchors[idx - 1]["top"] + y_date) / 2
            else:
                next_pitch = (date_anchors[1]["top"] - y_date
                              if len(date_anchors) > 1 else median_pitch)
                y_top = max(HEADER_SAFE_Y, y_date - next_pitch * 0.45)
            # Banda inferior:
            #   - registros intermedios: punto medio con el ancla siguiente
            #   - ÚLTIMO registro: 50% del pitch al anterior abajo del ancla
            if idx < len(date_anchors) - 1:
                y_bot = (y_date + date_anchors[idx + 1]["top"]) / 2
            else:
                prev_pitch = (y_date - date_anchors[-2]["top"]
                              if len(date_anchors) > 1 else median_pitch)
                y_bot = y_date + prev_pitch * 0.5

            band_words = [w for w in words if y_top <= w["top"] <= y_bot]

            def col_text(col_name: str) -> str:
                lo, hi = col_bounds[col_name]
                col_words = sorted(
                    [w for w in band_words if lo <= w["x0"] < hi],
                    key=lambda w: (w["top"], w["x0"]),
                )
                return " ".join(w["text"].strip() for w in col_words).strip()

            def col_text_near(col_name: str, tol: float = 5.0) -> str:
                lo, hi = col_bounds[col_name]
                col_words = sorted(
                    [w for w in words
                     if lo <= w["x0"] < hi and abs(w["top"] - y_date) < tol],
                    key=lambda w: (w["top"], w["x0"]),
                )
                return " ".join(w["text"].strip() for w in col_words).strip()

            empresa_raw           = col_text("empresa")
            entidad_municipio_raw = col_text("entidad_municipio")
            sindicato_raw         = col_text("sindicato")
            propiedad_raw         = col_text("propiedad")
            rama_raw              = col_text("rama")
            central_raw           = col_text("central")
            causa_raw             = col_text("causa")
            instancia_raw         = col_text_near("instancia")
            fecha_inicio          = anchor["text"].strip()
            duracion_raw          = col_text_near("duracion_dias")
            trabajadores_raw      = col_text_near("trabajadores")

            for inst in ["CFCRL-TLFAC", "JFCA"]:
                causa_raw = causa_raw.replace(inst, "")

            causa_clean = _classify_causa(causa_raw)
            entidad, municipio = _split_entidad_municipio(entidad_municipio_raw)

            records.append({
                "fecha_reporte":   fecha_s,
                "version_formato": ver,
                "empresa":         " ".join(empresa_raw.split()).strip(),
                "entidad":         entidad,
                "municipio":       municipio,
                "sindicato":       " ".join(sindicato_raw.split()).strip(),
                "propiedad":       " ".join(propiedad_raw.split()).strip(),
                "rama":            " ".join(rama_raw.split()).strip(),
                "central":         " ".join(central_raw.split()).strip(),
                "causa":           causa_clean,
                "instancia":       instancia_raw,
                "fecha_inicio":    fecha_inicio,
                "duracion_dias":   duracion_raw.replace(",", ""),
                "trabajadores":    trabajadores_raw.replace(",", "").rstrip("*"),
            })

    if not records:
        logger.info("%s | cuadro4 | sin registros extraídos por word-position", filename)
        return _empty_df(C4_COLS)

    logger.info("%s | cuadro4 | %d registros extraídos por word-position", filename, len(records))
    return pd.DataFrame(records, columns=C4_COLS)


def normalize_cuadro4(raw_rows: list[list], fecha: date, filename: str,
                      pages: "list | None" = None) -> pd.DataFrame:
    """
    Normaliza Cuadro 4. Si se pasan objetos de página (pages), usa extracción
    por posición de palabras (más robusta). Si no, usa el método posicional
    original como fallback.
    """
    # Método preferido: word-position (requiere objetos de página pdfplumber)
    if pages is not None:
        df = _extract_cuadro4_by_words(pages, fecha, filename)
        if not df.empty:
            return df
        logger.warning("%s | cuadro4 | word-position falló, usando fallback posicional", filename)

    # ── Fallback: método posicional original (para compatibilidad) ────────────
    rows = clean_raw_table(raw_rows)
    rows = _remove_duplicate_headers(rows, "Empresa")
    rows = _remove_duplicate_headers(rows, "EMPRESA")

    data_start = 0
    for i, row in enumerate(rows):
        row_text = " ".join(row).lower()
        if "empresa" in row_text and ("sindicato" in row_text or "causa" in row_text):
            data_start = i + 1
            break

    data_rows = rows[data_start:]
    if not data_rows:
        logger.info("%s | cuadro4 | sin huelgas vigentes", filename)
        return _empty_df(C4_COLS)

    ver = detect_version_formato(fecha)
    fecha_s = fecha.isoformat()
    records = []

    for row in data_rows:
        if not row:
            continue
        first = row[0].strip()
        if not first or "FUENTE" in first.upper() or "NOTA" in first.upper():
            continue
        if re.match(r"^[12]\s", first):
            continue

        def get(i): return row[i].strip() if i < len(row) else ""

        entidad, municipio = _split_entidad_municipio(get(1))
        n = len(row)
        if n >= 11:
            rec = {
                "fecha_reporte":   fecha_s,
                "version_formato": ver,
                "empresa":         get(0),
                "entidad":         entidad,
                "municipio":       municipio,
                "sindicato":       get(2),
                "propiedad":       get(3),
                "rama":            get(4),
                "central":         get(5),
                "causa":           get(6),
                "instancia":       get(7),
                "fecha_inicio":    get(8),
                "duracion_dias":   get(9),
                "trabajadores":    get(10),
            }
        else:
            rec = {
                "fecha_reporte":   fecha_s,
                "version_formato": ver,
                "empresa":         get(0),
                "entidad":         entidad,
                "municipio":       municipio,
                "sindicato":       get(2),
                "propiedad":       get(3),
                "rama":            get(4),
                "central":         get(5),
                "causa":           get(6),
                "instancia":       get(7),
                "fecha_inicio":    get(8),
                "duracion_dias":   pd.NA,
                "trabajadores":    get(9) if n >= 10 else "",
            }

        if rec["empresa"]:
            records.append(rec)

    if not records:
        logger.info("%s | cuadro4 | 0 registros válidos", filename)
        return _empty_df(C4_COLS)

    return pd.DataFrame(records, columns=C4_COLS)


# ── PROCESAMIENTO DE UN PDF ───────────────────────────────────────────────────

def process_single_pdf(pdf_path: Path) -> dict:
    """
    Procesa un PDF y retorna dict con DataFrames para cada cuadro.
    Todos los errores son capturados; nunca lanza excepciones.
    """
    filename = pdf_path.name
    stem = pdf_path.stem
    # Manejar doble extensión .docx.pdf
    if stem.lower().endswith(".docx"):
        stem = Path(stem).stem

    fecha = parse_filename_date(stem)
    result = {
        "fecha": fecha,
        "filename": filename,
        "cuadro1": None,
        "cuadro2": None,
        "cuadro3": None,
        "cuadro4": None,
        "errors": [],
    }

    if fecha is None:
        msg = f"No se pudo parsear fecha del filename: {filename}"
        logger.warning(msg)
        result["errors"].append(msg)
        return result

    try:
        with pdfplumber.open(pdf_path) as pdf:
            for cuadro_num in [1, 2, 3, 4]:
                try:
                    pages_idx = find_cuadro_pages(pdf, cuadro_num)
                    if not pages_idx:
                        logger.warning("%s | cuadro%d | header no encontrado en el PDF",
                                       filename, cuadro_num)
                        result[f"cuadro{cuadro_num}"] = _empty_df(SCHEMA_MAP[cuadro_num])
                        continue

                    pages = [pdf.pages[i] for i in pages_idx]
                    raw_rows = extract_tables_from_pages(pages, filename, cuadro_num)

                    if cuadro_num == 1:
                        df = normalize_cuadro1(raw_rows, fecha, filename)
                    elif cuadro_num == 2:
                        df = normalize_cuadro2(raw_rows, fecha, filename)
                    elif cuadro_num == 3:
                        df = normalize_cuadro3(raw_rows, fecha, filename)
                    else:
                        df = normalize_cuadro4(raw_rows, fecha, filename, pages=pages)

                    result[f"cuadro{cuadro_num}"] = df
                    logger.debug("%s | cuadro%d | %d filas extraídas",
                                 filename, cuadro_num, len(df))

                except Exception as e:
                    msg = f"{filename} | cuadro{cuadro_num} | ERROR: {e}"
                    logger.error(msg)
                    result["errors"].append(msg)
                    result[f"cuadro{cuadro_num}"] = _empty_df(SCHEMA_MAP[cuadro_num])

    except Exception as e:
        msg = f"{filename} | ERROR al abrir PDF: {e}"
        logger.error(msg)
        result["errors"].append(msg)

    return result


# ── INCREMENTAL: FECHAS YA PROCESADAS ────────────────────────────────────────

def load_existing_dates(_output_dir: Path) -> set[str]:
    """Lee fecha_reporte de los CSVs existentes y devuelve el conjunto."""
    existing = set()
    for fpath in OUTPUT_FILES.values():
        if fpath.exists():
            try:
                df = pd.read_csv(fpath, usecols=["fecha_reporte"])
                existing.update(df["fecha_reporte"].dropna().astype(str).tolist())
            except Exception:
                pass
    return existing


# ── ACUMULACIÓN Y ESCRITURA ───────────────────────────────────────────────────

def accumulate_results(all_results: list[dict], append: bool = False):
    """Concatena DataFrames y escribe los 4 CSVs acumulados."""
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

    for cuadro_num, out_path in OUTPUT_FILES.items():
        key = f"cuadro{cuadro_num}"
        cols = SCHEMA_MAP[cuadro_num]

        new_frames = [r[key] for r in all_results if r[key] is not None and not r[key].empty]

        if not new_frames:
            logger.info("cuadro%d | ningún nuevo dato para escribir", cuadro_num)
            continue

        new_df = pd.concat(new_frames, ignore_index=True)

        if append and out_path.exists():
            try:
                existing_df = pd.read_csv(out_path, dtype=str)
                combined = pd.concat([existing_df, new_df.astype(str)], ignore_index=True)
            except Exception:
                combined = new_df
        else:
            combined = new_df

        # Deduplicar y ordenar
        if cuadro_num == 4:
            dedup_cols = ["fecha_reporte", "empresa", "fecha_inicio", "causa", "trabajadores"]
        else:
            dedup_cols = ["fecha_reporte"] + [c for c in cols[2:3]]
        combined = combined.drop_duplicates(subset=dedup_cols)
        combined = combined.sort_values("fecha_reporte").reset_index(drop=True)

        combined.to_csv(out_path, index=False, encoding="utf-8-sig")
        logger.info("cuadro%d | %d filas escritas -> %s", cuadro_num, len(combined), out_path.name)


# ── MAIN ──────────────────────────────────────────────────────────────────────

def main():
    setup_logging(LOG_PATH)

    parser = argparse.ArgumentParser(description="Extrae cuadros 1-4 de PDFs CIS")
    parser.add_argument("--incremental", action="store_true",
                        help="Solo procesar PDFs no presentes en los CSVs existentes")
    parser.add_argument("--test-single", metavar="FILENAME",
                        help="Procesar un solo PDF y mostrar resultado en stdout")
    args = parser.parse_args()

    # ── Test de un solo archivo ───────────────────────────────────────────────
    if args.test_single:
        pdf_path = PDF_DIR / args.test_single
        if not pdf_path.exists():
            print(f"ERROR: Archivo no encontrado: {pdf_path}")
            sys.exit(1)
        logger.info("=== TEST SINGLE: %s ===", args.test_single)
        result = process_single_pdf(pdf_path)
        print(f"\nFecha parseada: {result['fecha']}")
        for n in [1, 2, 3, 4]:
            df = result[f"cuadro{n}"]
            if df is not None:
                print(f"\n-- Cuadro {n} ({len(df)} filas) --")
                print(df.to_string())
        if result["errors"]:
            print("\nErrores:", result["errors"])
        return

    # ── Extracción completa o incremental ────────────────────────────────────
    pdf_files = sorted(PDF_DIR.glob("*.pdf"))
    total_pdfs = len(pdf_files)
    logger.info("=== INICIO EXTRACCIÓN | %d PDFs encontrados ===", total_pdfs)

    # Filtrar por modo incremental
    if args.incremental:
        existing_dates = load_existing_dates(OUTPUT_DIR)
        logger.info("Modo incremental: %d fechas ya procesadas", len(existing_dates))

        def needs_processing(p: Path) -> bool:
            stem = p.stem
            if stem.lower().endswith(".docx"):
                stem = Path(stem).stem
            d = parse_filename_date(stem)
            return d is None or d.isoformat() not in existing_dates

        pdf_files = [p for p in pdf_files if needs_processing(p)]
        logger.info("PDFs nuevos a procesar: %d", len(pdf_files))

    if not pdf_files:
        logger.info("Ningún PDF nuevo para procesar.")
        return

    # Procesar
    all_results = []
    processed = failed = 0

    for pdf_path in pdf_files:
        result = process_single_pdf(pdf_path)
        all_results.append(result)
        if result["errors"] and result["fecha"] is None:
            failed += 1
        else:
            processed += 1

    logger.info("=== RESUMEN | procesados: %d | con errores parciales: %d | sin fecha: %d ===",
                processed, sum(1 for r in all_results if r["errors"]), failed)

    accumulate_results(all_results, append=args.incremental)

    logger.info("=== EXTRACCIÓN COMPLETADA ===")


if __name__ == "__main__":
    main()
