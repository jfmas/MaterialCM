# MaterialCM

Código en R del artículo **«Mapeo batimétrico en aguas costeras con imágenes de
satélite: un estudio de caso en el Golfo de México»** (Mas y Pérez Vega),
enviado a *Ciencias Marinas*.

El estudio estima la batimetría de la zona costera de Chelem / Puerto Progreso
(Yucatán, México) con imágenes Landsat 8/9, Sentinel-2 y SuperDove (PlanetScope)
aplicando la transformación de ratio de Stumpf et al. (2003), y evalúa el efecto
del sensor, la turbidez y el preprocesamiento sobre la precisión.

El grueso del análisis se apoya en el paquete de R **satbathy**, desarrollado
para este trabajo: <https://github.com/jfmas/satbathy>. Este repositorio
contiene los guiones que **usan** ese paquete para reproducir las tablas y
figuras del artículo, más los guiones de adquisición y preparación de datos.

## Requisitos

- R ≥ 4.5 y el paquete **satbathy** (`remotes::install_github("jfmas/satbathy")`).
- Paquetes: `terra`, `sf`, `lme4`, `httr2`, `curl`, `fuzzyjoin`, `lubridate`,
  `TTR`, `waterquality`, `CDSE`.
- Credenciales, **exportadas como variables de entorno** (los guiones nunca las
  guardan en texto):
  - Copernicus Data Space (Sentinel-2): `CDSE_ID`, `CDSE_SECRET`.
  - Planet (SuperDove): `PL_API_KEY`.
- Los datos in situ (sondeos con ecosonda) y las imágenes no se incluyen aquí
  (ver *Data availability* en el artículo); se obtienen con los guiones de
  adquisición.

> **Nota:** varios guiones fijan la carpeta de trabajo con `setwd(...)` a la ruta
> del autor. Ajústala a tu entorno. Los guiones de `analisis_satbathy/` se
> ejecutan desde la raíz del proyecto y se llaman entre sí con rutas relativas
> (`source("analisis_satbathy/config_imagenes.R")`).

## Estructura

```
Scripts_Bati/        adquisición de imágenes y preparación de sondeos
analisis_satbathy/   análisis batimétrico con satbathy y generación de figuras
```

## Flujo de trabajo

### 1. Adquisición de imágenes
| Guion | Función |
|-------|---------|
| `Scripts_Bati/baja_sentinel_CDSE.R` | Descarga Sentinel-2 (Copernicus Data Space) sobre el área, con filtro de nubes. |
| `Scripts_Bati/baja_sentinel_CDSE_03a05.R` | Igual, para el catálogo marzo–mayo 2025 usado en la comparación multisensor. |
| `Scripts_Bati/busca_planet.R` | Busca escenas SuperDove (Planet Data API) que cubren los transectos. |
| `Scripts_Bati/lista_planet_solicitud.R` | Filtra las escenas candidatas por calidad, % utilizable, nubes y cobertura. |
| `Scripts_Bati/pide_planet.R` | Pide y descarga las escenas (Planet Orders API), con descargas reanudables. |

Las imágenes Landsat 8/9 se descargan de EarthExplorer.

### 2. Selección de fechas por turbidez (Fig. 4)
| Guion | Función |
|-------|---------|
| `Scripts_Bati/Yuc_analiza_turbidez_serieS.R` | Serie anual del índice de turbidez de Frohn y Autrey (2009) sobre Sentinel-2, para identificar el periodo más transparente. |
| `analisis_satbathy/turbidity_timeseries.R` | Reproduce la serie de turbidez con satbathy → **Figura 4**. |

### 3. Preparación de los sondeos
| Guion | Función |
|-------|---------|
| `Scripts_Bati/procesa_radardat.R` | Corrige la profundidad de la ecosonda con el nivel del mar del mareógrafo de Progreso y normaliza a un nivel de referencia común. |
| `Scripts_Bati/checa_con_semar.R` | Verifica la congruencia con el modelo batimétrico de SEMAR y une los sondeos en un solo conjunto. |
| `analisis_satbathy/selecciona_sondeos.R` | Construye `puntos_seleccionados.gpkg` (1789 sondeos) descartando la franja costera que cae en NA en ≥40 % de las imágenes. |

### 4. Procesamiento de las imágenes SuperDove
| Guion | Función |
|-------|---------|
| `analisis_satbathy/procesa_planet_nuevas.R` | Compone las escenas SuperDove (producto SR de 8 bandas → 4 bandas de reflectancia 0–1) una imagen por fecha, con su máscara UDM2. |
| `analisis_satbathy/procesa_planet_0308.R` | Igual, para la fecha de estudio del 8 de marzo. |
| `analisis_satbathy/enriquece_catalogo_planet.R` | Añade metadatos (hora UTC, % utilizable/nube, cobertura, calidad) al catálogo Planet. |

### 5. Análisis batimétrico (paquete satbathy)
| Guion | Función | Salida en el artículo |
|-------|---------|-----------------------|
| `analisis_satbathy/config_imagenes.R` | Configuración central: imágenes, bandas, niveles de marea, y utilidades (`puntos_sondeo`, `loto`, `ic_rmse`). | — |
| `analisis_satbathy/mask_helpers.R` | Enmascarado por sensor (QA de Landsat, SCL de Sentinel-2, UDM2 de Planet + NDWI) y la cadena `prepara()` (máscara → descarte de blancos brillantes → filtro). | — |
| `analisis_satbathy/define_comunes.R` | Conjunto común de sondeos válido en las imágenes de estudio. | — |
| `analisis_satbathy/recalculo_4m.R` | Error por imagen con validación *leave-one-transect-out* (LOTO), estratos de profundidad, sensibilidad al filtro y efecto del glint. | **Tabla 3** |
| `analisis_satbathy/cobertura_por_imagen.R` | Sondeos válidos por estrato de 2 m en cada imagen del catálogo. | — |
| `analisis_satbathy/aplica_admision.R` | Aplica el criterio de admisión por cobertura estratificada (≥15 sondeos/estrato entre 2 y 16 m). | **Tabla 4**, **Figura 9**, tabla de excluidas |
| `analisis_satbathy/recalculo_fig9_4m.R` | Recálculo LOTO del catálogo completo (Landsat, Sentinel-2 y SuperDove, marzo–mayo 2025) × 4 combinaciones de preprocesamiento. | datos de **Figura 9** |
| `analisis_satbathy/analisis_estadistico_3sensores.R` | Modelos lineales de efectos mixtos con los tres sensores sobre las imágenes admitidas. | análisis estadístico |

### 6. Figuras
| Guion | Figura |
|-------|--------|
| `analisis_satbathy/turbidity_timeseries.R` | **Figura 4** — serie de turbidez |
| `analisis_satbathy/make_fig6_v2.R` | **Figura 6** — coeficiente de determinación frente a la profundidad máxima |
| `analisis_satbathy/make_fig7_v2.R` | **Figura 7** — dispersión ratio–profundidad con el umbral |
| `analisis_satbathy/make_fig8_residuales.R` | **Figura 8** — mapas batimétricos con residuales |
| `analisis_satbathy/aplica_admision.R` | **Figura 9** — R² frente a la turbidez |

Las figuras 1–3 y 5 (área de estudio, diagrama de flujo, transectos e imagen de
ratio) se elaboran con guiones de mapa y con satbathy.

## Cita

Mas JF, Pérez Vega A. Mapeo batimétrico en aguas costeras con imágenes de
satélite: un estudio de caso en el Golfo de México. *Ciencias Marinas*.

## Licencia

Ver [LICENSE](LICENSE).
