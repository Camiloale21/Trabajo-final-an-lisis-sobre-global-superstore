# ¿Cuánto cuesta un descuento? Efecto del descuento sobre el margen en Global Superstore (2011–2014)

Proyecto final de la asignatura **Analítica de Datos** (Administración de Empresas).

## Pregunta de negocio

¿En qué medida el nivel de descuento determina el margen de utilidad de las ventas de Global Superstore entre 2011 y 2014, controlando por el precio, la cantidad, la categoría, el mercado, el segmento y la logística? ¿A partir de qué nivel de descuento las ventas dejan de ser rentables?

## Principales hallazgos

| Hallazgo | Resultado |
|---|---|
| Variación del margen explicada solo por el descuento (M1) | 71,6 % |
| Relación descuento–margen | No lineal: la pérdida de margen se acelera (β de descuento² = −3,12; p < 0,001) |
| Costo de +1 p.p. de descuento | −1,30 p.p. de margen con un descuento del 20 %; −2,55 p.p. con uno del 40 % |
| Descuento de equilibrio (margen = 0) | 28,0 % en promedio (31,7 % en EE. UU., 24,9 % en África) |
| ¿El descuento aumenta las unidades por línea? | No (p = 0,259) |

## Estructura del repositorio

```
global-superstore-descuentos/
├── README.md                         <- este archivo
├── global-superstore-descuentos.Rproj
├── data/
│   └── raw/
│       └── Global Superstore Retail Dataset.csv   <- datos originales (sin modificar)
├── R/
│   └── analisis_descuentos.R         <- script único: limpieza, EDA, modelos, pruebas, figuras
├── output/                           <- se genera al ejecutar el script
│   ├── tablas/                       <- tablas estadísticas y econométricas (.csv)
│   ├── figuras/                      <- gráficas (.png)
│   └── modelos.txt                   <- resúmenes completos de los modelos
└── informe/
    └── informe_final.pdf             <- informe del proyecto
```

## Datos

- **Fuente:** *Global Superstore Retail Dataset*, https://github.com/Chukwuebukadotme/Global-Superstore-Retail-Dataset (originalmente publicado en Kaggle).
- **Unidad de observación:** línea de pedido (un producto dentro de un pedido).
- **Tamaño:** 51.290 filas × 27 columnas; pedidos entre el 01/01/2011 y el 31/12/2014.
- **Limpieza:** no hay valores faltantes ni duplicados. Se excluye 1 línea con ventas iguales a 0 porque el margen no está definido. Se descarta la columna `记录数`, un contador que vale 1 en todas las filas. Los valores atípicos **no** se eliminan; se usan errores estándar robustos y pruebas de robustez.

## Cómo reproducir el análisis

1. Instalar [R](https://cran.r-project.org/) (versión 4.1 o superior) y [RStudio](https://posit.co/download/rstudio-desktop/).
2. Clonar o descargar este repositorio.
3. Abrir `global-superstore-descuentos.Rproj` en RStudio. Esto fija el directorio de trabajo en la raíz del proyecto.
4. Abrir `R/analisis_descuentos.R` y pulsar **Source** (Ctrl/Cmd + Shift + S).

El script instala automáticamente los paquetes que falten (`readr`, `dplyr`, `tidyr`, `ggplot2`, `scales`, `sandwich`, `lmtest`, `car`) y genera todas las salidas en `output/`. La ejecución tarda menos de un minuto.

## Metodología (resumen)

| Modelo | Especificación | Propósito |
|---|---|---|
| M1 | margen ~ descuento | Regresión lineal simple |
| M2 | M1 + log(precio de lista) + cantidad + categoría + segmento + mercado + prioridad + envío + año | Regresión múltiple (*ceteris paribus*) |
| **M3** | M2 + descuento² | **Modelo principal**: no linealidad |
| M4 | M3 + descuento × categoría | Heterogeneidad por categoría |
| M5 | margen ~ 1/(1 − descuento) + controles | Especificación derivada de la teoría |
| MB | log(cantidad) ~ descuento + controles | ¿Los descuentos aumentan el volumen? |

- Estimación por MCO con errores estándar robustos HC1 (White), dado que Breusch-Pagan rechaza la homocedasticidad.
- Diagnóstico: Breusch-Pagan, RESET de Ramsey, Jarque-Bera, Durbin-Watson, GVIF y errores agrupados por pedido.
- Descuento de equilibrio: raíz del modelo cuadrático con los controles en sus medias; intervalos por el método delta.

## Autores

- Camilo Alejandro Hernández Moreno
- Sofía Castiblanco Aguilera

Pontificia Universidad Javeriana · Analítica de Datos · Profesor: Juan Nicolás Velásquez Rey

## Nota sobre el uso de inteligencia artificial

Se utilizó Claude (Anthropic), un modelo de inteligencia artificial, como apoyo para formular la pregunta problema y para desarrollar el código en R. Los autores revisaron el código y los resultados, y son responsables del contenido del proyecto.
