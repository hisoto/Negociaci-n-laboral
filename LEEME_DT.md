# Negociación laboral — cómo correrlo

Genera las gráficas y tablas de la sección **Negociaciones contractuales** del informe
mensual de la Dirección Técnica.

Contacto: Héctor Iván Soto Parra — CAEL, CONASAMI.

---

## La primera vez en esta computadora

1. Abrir **`Negociacion_laboral.Rproj`** (Positron o RStudio). Esto ancla las rutas: el
   proyecto se puede mover de carpeta o de disco sin tocar código, pero hay que abrirlo
   desde el `.Rproj`.

2. Correr una sola vez:

   ```
   Rscript scripts/000_requisitos.R
   ```

   Instala los paquetes y revisa las tres cosas que fallan callado al cambiar de equipo:

   - **`rnaturalearthhires`** no está en CRAN. Sin él fallan los mapas (005 y 006).
   - **La tipografía Noto Sans.** Si falta, las gráficas se generan igual pero con otra
     letra, fuera del canon DT 2026, y solo avisan con un *warning*. Si el script dice
     `FALTA`, instalarla antes de publicar nada.
   - **La carpeta compartida de la DT.** Si no aparece, las copias externas se omiten y
     las gráficas se quedan solo en `graphs/`.

---

## Cada mes

1. **Dejar los insumos en su lugar.** Los baja Héctor del portal Polígonos de la STPS
   (IBM Cognos) y llegan por correo; el detalle de cada archivo está en
   [`inputs/README.md`](inputs/README.md). Si no llegaron, el paso 2 lo dice.

2. **Editar dos líneas** al inicio de `scripts/Master_informe_negociaciones.R`:

   ```r
   anio_interes <- 2026L
   mes_interes  <- 6L
   ```

   Es lo único que cambia cada mes.

3. **Correr el master:**

   ```
   Rscript scripts/Master_informe_negociaciones.R
   ```

   O, desde Positron: `source(here::here("scripts", "Master_informe_negociaciones.R"))`.

---

## Cómo leer el arranque

Antes de generar nada, el master imprime la cobertura de los insumos. Ejemplo:

```
── Cobertura de insumos · mes objetivo: junio 2026 ──

  negociaciones_stata / j_federal      002 · 901       OK
  negociaciones_stata / j_local        002             rezago normal (02-2026)
  SCIAN del mes                        011 · 902       OK
```

| Estado | Qué significa |
|---|---|
| `OK` | El insumo cubre el mes objetivo |
| `rezago normal (mm-aaaa)` | **No es error.** La jurisdicción local y el SCIAN los publica la STPS con meses de retraso; el informe lo dice explícitamente ("fecha de los últimos datos disponibles") |
| `DESACTUALIZADO (mm-aaaa)` | El archivo es de un mes anterior. **Detenerse aquí**: correr así publica el mes pasado con la fecha de este |
| `FALTA / ilegible` | El archivo no está o cambió de formato |

Al terminar, el resumen lista cada paso con OK o ERROR. Un paso que falla no detiene a los
demás: se revisa ese y se vuelve a correr.

---

## Qué produce

**Gráficas** — PNG en `graphs/NN_tema/` y SVG en la carpeta `graphs/` de la DT, que es de
donde las toma el Word. Son 15:

| Script | Archivos |
|---|---|
| `002` | `ts_juris_federal` · `ts_juris_local` · `barras_{revisiones,trabajadores}_{federal,local}` |
| `003` | `ts_centrales` |
| `004` | `empresas` |
| `005` | `mapa_incremento` · `mapa_solo_incremento` · `bar_incremento` |
| `006` | `mapa_emplazamientos` · `mapa_solo_emplazamientos` |
| `007` | `bar_huelgas` · `bar_huelgas_causa` |

Todas con sufijo `_{año}m{mes}`, por ejemplo `ts_centrales_2026m06.svg`.

**Tablas** — CSV en `outputs/tablas/` y copia en `bases/` de la DT:

| Script | Archivo | Dónde va en el informe |
|---|---|---|
| `011` | `tabla_sectores_{per}.csv` | Tabla de sectores de actividad (SCIAN) |
| `013` | `tabla_huelgas_vigentes_{per}.csv` | Anexo estadístico, listado de huelgas vigentes |

---

## Cosas que conviene saber

- **`901_extra_mir.R` necesita el INPC** de `comportamiento_precios`. Lo busca primero en
  `bases/inpc.csv` de la DT y, si no está, en el proyecto hermano. Es un extra: no corre
  en el master del informe.

- **Los aumentos del salario mínimo** viven en `inputs/salario_minimo_aumentos.csv`. Cada
  1 de enero se agrega una fila; no se toca ningún script.

- **El Excel del SCIAN trae basura de vez en cuando.** En junio de 2026 el número de
  negociaciones del primer sector venía como `"Sa"` en lugar de `2`. El script `011` lo
  detecta, lo reconstruye desde el porcentaje y lo avisa en consola; además verifica que
  los sectores sumen el Total. Si esa verificación falla, **no publicar la tabla** sin
  mirar la hoja.

- **El título interno del Excel del SCIAN miente.** El de junio 2026 dice "MARZO 2024". El
  archivo se fecha por su nombre, nunca por el contenido.

- **Para hacer pruebas sin escribir en la carpeta compartida:**

  ```
  Rscript -e "Sys.setenv(CNSM_COPIAR_DT='false'); source('scripts/Master_informe_negociaciones.R')"
  ```

- **`proyectosDT` es carpeta compartida de la Dirección Técnica.** El pipeline solo agrega
  y sobrescribe sus propios archivos del mes. No borrar nada de ahí.
