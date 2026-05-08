# 🚦 Predicción del Volumen de Tráfico — Dashboard Interactivo en Shiny

<div align="center">

![R](https://img.shields.io/badge/R-%3E%3D4.4-276DC3?style=flat-square\&logo=r\&logoColor=white)
![Shiny](https://img.shields.io/badge/Shiny-dashboard-4EA5D9?style=flat-square)
![Machine Learning](https://img.shields.io/badge/Machine-Learning-00C853?style=flat-square)
![Universidad del Norte](https://img.shields.io/badge/UniNorte-Barranquilla%202026-C0392B?style=flat-square)

### Dashboard interactivo para análisis y predicción de tráfico vehicular utilizando Machine Learning

**Juan Esteban García Gómez · Luis Esteban Mariño**
*Ciencia de Datos — Universidad del Norte, Barranquilla 2026*

</div>

---

# 🚗 ¿De qué trata este proyecto?

Este proyecto fue desarrollado como una aplicación interactiva en **R Shiny** con el objetivo de analizar y predecir el volumen de tráfico vehicular utilizando técnicas de **aprendizaje automático supervisado**.

La aplicación integra variables relacionadas con:

* condiciones climáticas
* variables temporales
* factores ambientales
* fechas especiales y festivos
* comportamiento histórico del tráfico

Con esta información, el sistema es capaz de identificar patrones y generar predicciones sobre el flujo vehicular en diferentes momentos del día.

El dashboard fue diseñado no solo para mostrar resultados, sino también para ofrecer una experiencia visual moderna e interactiva inspirada en interfaces de inteligencia artificial y sistemas urbanos inteligentes.

---

# 🖥️ ¿Qué incluye el dashboard?

| Sección                        | Descripción                                              |
| ------------------------------ | -------------------------------------------------------- |
| 🏠 Inicio                      | Pantalla principal interactiva con diseño visual moderno |
| 📖 Contexto                    | Problema, objetivos y justificación del proyecto         |
| 📊 Exploración de datos        | Distribuciones, correlaciones y comportamiento temporal  |
| 🤖 Modelos de Machine Learning | Comparación entre algoritmos predictivos                 |
| 📈 Predicción                  | Simulación de predicciones de tráfico                    |
| 🚦 Aplicaciones reales         | Casos de uso en movilidad urbana inteligente             |

---

# 🧠 Modelos implementados

Para encontrar el mejor modelo predictivo se entrenaron y compararon diferentes algoritmos de regresión:

| Modelo               | Función                                                |
| -------------------- | ------------------------------------------------------ |
| 🌳 Árbol de Decisión | Modelo simple e interpretable basado en reglas         |
| 🌲 Random Forest     | Ensamble robusto de múltiples árboles                  |
| 📍 KNN               | Predicción basada en vecinos cercanos                  |
| ⚡ XGBoost            | Gradient Boosting optimizado                           |
| 📈 Ridge             | Regresión lineal con regularización L2                 |
| 📉 Lasso             | Regresión lineal con selección automática de variables |
| 🧮 SVM               | Support Vector Regression                              |

Cada modelo fue evaluado mediante métricas como:

* RMSE
* MAE
* R²
* MAPE
* Validación cruzada

---

# 🚀 ¿Cómo ejecutar el proyecto en tu computador?

## Paso 1 — Instalar R y RStudio

Si aún no los tienes instalados:

### Descargar R

[https://cran.r-project.org/](https://cran.r-project.org/)

### Descargar RStudio

[https://posit.co/download/rstudio-desktop/](https://posit.co/download/rstudio-desktop/)

---

## Paso 2 — Descargar el proyecto

### Opción A — Clonar el repositorio con Git

```bash
git clone https://github.com/lesteban1828/TrafficVolume_Shinyapp.git
cd TrafficVolume_Shinyapp
```

### Opción B — Descargar ZIP

1. Haz clic en el botón verde:

```text
<> Code
```

2. Selecciona:

```text
Download ZIP
```

3. Extrae la carpeta en tu computador.

---

## Paso 3 — Instalar las librerías necesarias

Abre RStudio y ejecuta el siguiente código:

```r
install.packages(c(
  "shiny",
  "plotly",
  "htmltools",
  "dplyr",
  "ggplot2",
  "readr",
  "DT",
  "scales"
))
```

La primera instalación puede tardar algunos minutos dependiendo del computador.

---

## Paso 4 — Verificar la estructura del proyecto

La carpeta debe quedar organizada así:

```text
TrafficVolume_Shinyapp/
│
├── global.R
├── ui.R
├── server.R
├── README.md
├── .gitignore
│
├── data/
│   └── Traffic.csv
│
└── www/
    ├── styles.css
    ├── traffic_bg.jpeg
    ├── Semaforo.mp4
    ├── carretera_noche.mp4
```

---

## Paso 5 — Ejecutar la aplicación

### Desde RStudio

1. Abre el archivo:

```text
ShinyappFinal_TrafficVolume.Rproj
```

2. Haz clic en:

```text
▶ Run App
```

---

### Desde la consola de R

También puedes ejecutar:

```r
shiny::runApp(".")
```

La aplicación se abrirá automáticamente en tu navegador.

---

# 🎨 Diseño visual

La interfaz fue diseñada utilizando:

* glassmorphism
* gradientes modernos
* efectos neon
* tipografía moderna
* videos de fondo
* diseño responsive

Todo el diseño busca transmitir una estética tecnológica relacionada con movilidad urbana e inteligencia artificial.

---

# 🌆 Aplicaciones reales del proyecto

## 🚦 Semáforos inteligentes

El modelo puede utilizarse para ajustar dinámicamente los tiempos de los semáforos dependiendo de las condiciones futuras del tráfico.

---

## 🗺️ Navegación predictiva

La solución podría integrarse con aplicaciones como:

* Google Maps
* Waze
* Uber

para sugerir rutas basadas en tráfico futuro y no únicamente en tráfico actual.

---

## 🌱 Impacto urbano

Este tipo de sistemas puede ayudar a:

* reducir congestiones
* disminuir emisiones contaminantes
* optimizar tiempos de desplazamiento
* mejorar la planeación vial
* apoyar ciudades inteligentes

---

# 📦 Librerías utilizadas

| Paquete     | Función                                |
| ----------- | -------------------------------------- |
| `shiny`     | Framework principal de la aplicación   |
| `plotly`    | Visualizaciones interactivas           |
| `ggplot2`   | Creación de gráficas                   |
| `dplyr`     | Transformación y manipulación de datos |
| `readr`     | Lectura del dataset                    |
| `DT`        | Tablas interactivas                    |
| `htmltools` | Componentes HTML personalizados        |
| `scales`    | Formato de escalas y ejes              |

---

# 📊 Dataset utilizado

El proyecto trabaja con un dataset de tráfico urbano que contiene variables como:

* temperatura
* lluvia
* nieve
* nubosidad
* hora del día
* día de la semana
* mes
* festivos
* clima
* volumen vehicular

Estas variables fueron utilizadas para construir y evaluar los modelos predictivos.

---

# 🌐 Publicar la app online

La aplicación puede desplegarse fácilmente utilizando:

```text
shinyapps.io
```

Instalar:

```r
install.packages("rsconnect")
```

Conectar la cuenta:

```r
rsconnect::setAccountInfo(
  name = "TU_USUARIO",
  token = "TU_TOKEN",
  secret = "TU_SECRET"
)
```

Publicar la app:

```r
rsconnect::deployApp()
```

---

# 👨‍💻 Autores

| Nombre                    | Programa                                 |
| ------------------------- | ---------------------------------------- |
| Juan Esteban García Gómez | Ciencia de Datos — Universidad del Norte |
| Luis Esteban Mariño       | Ciencia de Datos — Universidad del Norte |

**Universidad del Norte · Barranquilla, Colombia · 2026**

---

# ❓ Problemas frecuentes

## La aplicación no encuentra el dataset

Verifica que exista el archivo:

```text
data/Traffic.csv
```

---

## No cargan imágenes o videos

Asegúrate de que todos los archivos multimedia estén dentro de:

```text
www/
```

---

## Error instalando paquetes

Actualiza R desde:

[https://cran.r-project.org/](https://cran.r-project.org/)

---

## La aplicación tarda en abrir

Los archivos `.mp4` pueden aumentar el tiempo de carga inicial.

---

<div align="center">

### Desarrollado con ❤️ en R Shiny · Universidad del Norte · 2026

</div>
