# Insumos — Negociación laboral

Qué es cada archivo, de dónde sale, quién lo consume y en qué parte del informe aterriza.
Antes de generar gráficas conviene correr el preflight, que dice si cada insumo cubre el mes:

```bash
Rscript -e "source('scripts/000_config.R'); verificar_insumos()"
```

---

## `stps_poligonos/` — Portal Polígonos STPS

Descarga **manual** mensual desde `http://siel.stps.gob.mx:303/ibmcognos` (sistema IBM Cognos).
El scraping sigue sin resolverse; ver los borradores en `scripts/borradores/`.

| Archivo | Contenido | Consumen | Sección del informe |
|---|---|---|---|
| `negociaciones_stata.xlsx` | Archivo maestro. Hojas: `j_federal`, `j_local`, `j_federal_anual`, `j_local_anual`, `entidad`, `empresas`, `huelgas`, `emplaza` | `002`, `004`, `005`, `007`, `009` | Jur. federal · Jur. local · Huelgas |
| `negociaciones_central.xlsx` | Por central obrera. 7 centrales × 2 hojas (mensual + anual): `ctm`, `croc`, `crom`, `sna_asa`, `otras`, `ind_ct`, `no_esp` | `003`, `009` | Incremento real por central obrera |
| `5.2.5 Emplazamientos a Huelgas por Entidad Federativa.xlsx` | Emplazamientos por entidad y mes. Encabezado de 9 filas (`skip = 9`) | `006` | Emplazamientos a huelga por entidad |
| `incrementos_sector.xlsx` | Serie histórica de incrementos por sector. Encabezado de 8 filas (`skip = 8`) | `010` | — (complementaria) |

**Rezago estructural de la jurisdicción local.** La hoja `j_local` va varios meses atrás de
`j_federal`: el informe de junio 2026 reporta local a **febrero 2026**. No es un error de descarga.
El script `002` lo maneja tomando el mes del último dato local disponible, y el preflight lo reporta
como "rezago normal" en vez de marcarlo desactualizado.

---

## `stps_scian/` — Negociaciones por sector de actividad

Un archivo por mes: `negociaciones_scian_{mes} {año}.xlsx`, hoja `Cuadro (2)`. Contiene la tabla de
13 sectores + total con incremento real, personas involucradas, distribución y número de
negociaciones, ya ordenada de mayor a menor incremento.

> **El encabezado interno no es confiable.** El archivo de junio 2026 dice *"MARZO 2024"* en la
> celda de título. **El nombre del archivo es la única fuente para fechar este insumo**, y por eso
> `ruta_scian_mes()` en `scripts/000_config.R` lo resuelve por nombre, nunca por contenido.

El espaciado de los nombres es inconsistente (`junio  2025` con dos espacios convive con
`junio 2026` con uno); `ruta_scian_mes()` tolera ambos.

Alimenta la **tabla por sector de actividad** del informe. Hoy esa tabla se transcribe a mano:
generarla con código es el pendiente `011_tabla_sectores.R`.

---

## `cis_pdf/` — Tarjetas Semanales de Contratación y Conflictividad Colectiva

183 PDFs, 2020–2026. **Confidenciales**: la carpeta está en `.gitignore`.
Llegan por correo institucional; se depositan aquí tal cual, sin renombrar.

Formatos de nombre que el extractor reconoce:

| Época | Patrón |
|---|---|
| 2020–2021 | `CIS-{día}[sep]{Mes}[sep]{año}.pdf` |
| 2022+ | `CIS_{año}{Mes}_{día}.pdf` |
| mediados de 2021 | `CIS_{año}{Mes}{día}.pdf` (sin guion bajo) |
| octubre 2025 | `CIS_{año}SOctubre_{día}.pdf` (typo "S", detectado automáticamente) |
| cierre de año | `CIS_{año}{Mes}_Cierre.pdf` (usa el último día del mes) |

**Frontera de la reforma:** los PDFs anteriores a 2022-01-01 se marcan `version_formato =
"pre_reforma"`; el campo `instancia` cambia de `JFCA` a `CFCRL-TLFAC` en esa fecha.

---

## `cis_csv/` — Salida del extractor de PDFs

Genera `scripts/001_extraccion_pdf_cis.py`. **Confidenciales**, también en `.gitignore`.

```bash
python scripts/001_extraccion_pdf_cis.py --incremental      # solo tarjetas nuevas
python scripts/001_extraccion_pdf_cis.py                    # reextracción completa
python scripts/001_extraccion_pdf_cis.py --test-single "CIS_2026Febrero_28.pdf"
```

| Archivo | Cuadro | Contenido | Consumen |
|---|---|---|---|
| `cuadro1_revisiones_acumulado.csv` | 1 | Principales revisiones: empresa, central, trabajadores, incremento nominal/real | — |
| `cuadro2_instancias_acumulado.csv` | 2 | Conciliadas vs. ratificadas, comparativo anual | — |
| `cuadro3_bonos_acumulado.csv` | 3 | Convenios con bonos de productividad | — |
| `cuadro4_huelgas_vigentes_acumulado.csv` | 4 | Huelgas federales activas: empresa, entidad, municipio, sindicato, causa, duración, trabajadores | `007`, `008` |
| `extraction_log.txt` | — | Advertencias y errores de la extracción | auditoría |

Todos traen `fecha_reporte` (la tarjeta de origen), así que los scripts filtran por corte.

**Calidad:** las tablas multi-columna de estos PDFs fragmentan filas al extraer. Las filas con
empresa vacía son artefactos de layout y se filtran solas. Para auditar, revisar
`extraction_log.txt`.

**Cuadro 4 — entidad y municipio.** La columna del PDF "Entidad Federativa y Municipio" se parte en
dos campos:

- `"Tamaulipas, Matamoros"` → `entidad="Tamaulipas"`, `municipio="Matamoros"`
- `"Más de una entidad, Cuauhtémoc"` → `entidad="Más de una entidad"`, `municipio="Cuauhtémoc"`
- `"Más de una entidad"` solo → `municipio=""`

---

## `catalogos/` — Referencias geográficas

| Archivo | Origen | Consumen |
|---|---|---|
| `municipios_centroides.csv` | Centroides lon/lat por (entidad, municipio) | `008` |
| `catun_municipio/AGEEML_202631880673_utf8.csv` | Catálogo único de municipios INEGI (AGEEML) | `008` |
| `catun_municipio/AGEEML_*.{xlsx,txt,dbf}` | Mismo catálogo en otros formatos, sin uso | — |
| `Municipios.dbf` | Descarga INEGI cruda, sin uso | — |

Los mapas de entidad (`005`, `006`) no usan estos catálogos: toman la geometría de
`rnaturalearth` / `rnaturalearthhires`.

---

## `word_huelgas_vigentes/` — Documentos de la Dirección Técnica

`HUELGAS VIGENTES EN LA JURISDICCIÓN FEDERAL_{mes}_{año}.docx`, uno por mes. **Confidenciales**,
en `.gitignore`. Referencia de consulta; ningún script los lee (la misma información sale del
Cuadro 4 de las tarjetas CIS, que sí es procesable).

---

## Insumos retirados

En `_archivo/insumos_manuales/`, fuera del pipeline desde la reorganización de julio 2026:

| Archivo | Por qué salió |
|---|---|
| `HUELGAS.xlsx` | Llenado manual. Sus cifras 2007–2026 eran idénticas a las columnas `huelgas` / `huelgas_vigentes` de la hoja `huelgas` del archivo maestro. Al migrar `007` a esa hoja, la gráfica salió byte-idéntica. |
| `Libro1.xlsx` | Llenado manual de huelgas por causa. Sustituido por el Cuadro 4 de las tarjetas CIS, que es registro nominal caso por caso. |

En `_archivo/insumos_viejos/`: vintages anteriores del archivo maestro, la carpeta `coahuila/` y
salidas de intentos de scraping.
