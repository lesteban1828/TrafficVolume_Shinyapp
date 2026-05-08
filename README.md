# Dashboard Shiny Modificado y Corregido

## Incluye
- `global.R` corregido
- `ui.R`
- `server.R` corregido para estructura Shiny clásica
- `www/styles.css` con tipografía más grande
- soporte para fondo GIF en inicio

## Para usar el GIF de fondo

Coloca tu archivo aquí:

```text
www/city_traffic.gif
```

Si no agregas el GIF, la app funciona igual; solo no se verá la animación de fondo.

## Ejecutar

Abre la carpeta en RStudio y ejecuta:

```r
shiny::runApp(".")
```
