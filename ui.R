# ============================================================
#  ui.R
# ============================================================

FONTS <- tags$head(
  tags$link(rel="preconnect", href="https://fonts.googleapis.com"),
  tags$link(rel="stylesheet",
    href="https://fonts.googleapis.com/css2?family=Space+Grotesk:wght@400;500;600;700;800;900&family=Inter:wght@300;400;500;600;700;800&display=swap")
)

ui <- fluidPage(
  FONTS,
  tags$link(rel="stylesheet", href="styles.css"),
  NAVBAR,
  uiOutput("page")
)
