# ============================================================
# server.R
# ============================================================

server <- function(input, output, session) {

  `%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

  current_page <- reactiveVal("home")
  go <- function(page) current_page(page)

  # Navegación — navbar
  observeEvent(input$n_home, go("home"))
  observeEvent(input$n_ctx,  go("ctx"))
  observeEvent(input$n_eda,  go("eda"))
  observeEvent(input$n_mod,  go("mod"))
  observeEvent(input$n_sim,  go("sim"))
  observeEvent(input$n_sim2, go("sim"))
  observeEvent(input$n_ins,  go("ins"))

  # Navegación — botones hero
  observeEvent(input$hb_ctx, go("ctx"))
  observeEvent(input$hb_eda, go("eda"))
  observeEvent(input$hb_mod, go("mod"))
  observeEvent(input$hb_sim, go("sim"))
  observeEvent(input$hb_ins, go("ins"))

  # Navegación — stack chips hero
  observeEvent(input$sc_ctx, go("ctx"))
  observeEvent(input$sc_eda, go("eda"))
  observeEvent(input$sc_sim, go("sim"))
  observeEvent(input$sc_mod, go("mod"))
  observeEvent(input$sc_ins, go("ins"))

  # Navegación — mini nav cards hero
  observeEvent(input$nc_ctx, go("ctx"))
  observeEvent(input$nc_eda, go("eda"))
  observeEvent(input$nc_sim, go("sim"))
  observeEvent(input$nc_mod, go("mod"))
  observeEvent(input$nc_ins, go("ins"))

  # Navegación — botones en páginas internas
  observeEvent(input$ctx_eda,  go("eda"))
  observeEvent(input$ctx_sim,  go("sim"))
  observeEvent(input$ins_mod,  go("mod"))
  observeEvent(input$ins_sim,  go("sim"))

  output$page <- renderUI({
    switch(
      current_page(),
      home = pageHome(),
      ctx  = pageContexto(),
      eda  = pageEDA(),
      mod  = pageModelos(),
      sim  = pageSimulador(),
      ins  = pageInsights(),
      pageHome()
    )
  })

  # ── EDA ────────────────────────────────────────────────────
  output$eda_beh <- renderPlotly({ fig_beh(input$eda_var %||% "hour") })
  output$eda_tgt <- renderPlotly({ fig_tgt(input$eda_var %||% "hour") })

  output$eda_desc_txt <- renderUI({
    var  <- input$eda_var %||% "hour"
    meta <- EDA_META[[var]]
    div(style=paste0("color:",MUTED,";font-size:13px;font-family:'Inter',sans-serif;margin-top:10px;"),
        paste0(meta$desc, " · Tipo: ", meta$unit))
  })

  output$eda_corr <- renderUI({
    var  <- input$eda_var %||% "hour"
    meta <- EDA_META[[var]]
    val  <- if (!is.null(meta$corr)) meta$corr else 0
    col  <- if (val >= 0) PRIMARY else DANGER
    div(style=paste0("font-size:38px;font-weight:800;font-family:'Space Grotesk',sans-serif;color:",col,";margin-top:8px;"),
        sprintf("%+.3f", val))
  })

  output$eda_dir <- renderUI({
    var  <- input$eda_var %||% "hour"
    meta <- EDA_META[[var]]
    val  <- if (!is.null(meta$corr)) meta$corr else 0
    dir  <- if (val >= 0) "positiva" else "negativa"
    str  <- if (abs(val) >= 0.5) "fuerte" else if (abs(val) >= 0.15) "moderada" else "débil"
    div(style=paste0("font-size:12px;color:",MUTED,";font-family:'Inter',sans-serif;"),
        paste0("relación ", dir, " ", str, " con traffic_volume"))
  })

  output$eda_insight <- renderUI({
    var  <- input$eda_var %||% "hour"
    meta <- EDA_META[[var]]
    val  <- if (!is.null(meta$corr)) meta$corr else 0
    dir  <- if (val >= 0) "positiva" else "negativa"
    str  <- if (abs(val) >= 0.5) "fuerte" else if (abs(val) >= 0.15) "moderada" else "débil"
    div(style=paste0("font-size:13px;color:",MUTED,";line-height:1.65;font-family:'Inter',sans-serif;"),
      tags$span("Interpretación: ", style=paste0("color:",FG,";font-weight:700;")),
      paste0(meta$title, " muestra una correlación Spearman ", sprintf("%+.3f", val), " con el target. "),
      paste0("Esto sugiere una relación ", dir, " y ", str,
             "; debe leerse junto con el gráfico comparativo, porque algunas variables tienen comportamiento no lineal o categórico.")
    )
  })

  # ── EDA complementario ─────────────────────────────────────
  output$s_vif     <- renderPlotly({ fig_vif() })
  output$s_out     <- renderPlotly({ fig_outliers() })
  output$s_corr    <- renderPlotly({ fig_corr() })
  output$s_weather <- renderPlotly({ fig_weather() })

  # ── Modelos ────────────────────────────────────────────────
  output$m_r2    <- renderPlotly({ fig_r2() })
  output$m_err   <- renderPlotly({ fig_err() })
  output$m_ov    <- renderPlotly({ fig_overfit() })
  output$m_imp   <- renderPlotly({ fig_imp() })
  output$m_ridge <- renderPlotly({ fig_ridge() })

  # ── Simulador ──────────────────────────────────────────────
  pred <- reactive({
    estimate(
      hour    = input$p_h    %||% 8,
      dow     = as.numeric(input$p_d   %||% 0),
      month   = as.numeric(input$p_m   %||% 5),
      holiday = as.numeric(input$p_hol %||% 0),
      temp    = input$p_temp %||% 288,
      humidity= input$p_hum  %||% 70,
      ws      = input$p_ws   %||% 3,
      vis     = input$p_vis  %||% 5,
      poll    = input$p_pol  %||% 120,
      rain    = input$p_rai  %||% 0,
      snow    = input$p_sno  %||% 0,
      clouds  = input$p_clo  %||% 20,
      weather = input$p_wt   %||% "Clear",
      wind_dir= input$p_wd   %||% 320
    )
  })

  output$pred_v <- renderText({
    format(pred(), big.mark=".", decimal.mark=",")
  })

  output$pred_gauge <- renderPlotly({ fig_gauge(pred()) })

  output$pred_band <- renderUI({
    v <- pred()
    if (v >= 5000) {
      col <- DANGER; txt <- "Demanda alta"
    } else if (v >= 2800) {
      col <- ACCENT; txt <- "Demanda media"
    } else {
      col <- PRIMARY; txt <- "Demanda baja"
    }
    div(style=paste0(
      "display:inline-flex;padding:9px 14px;border-radius:999px;",
      "background:",col,"18;color:",col,
      ";border:1px solid ",col,"45;font-weight:800;margin-bottom:20px;",
      "font-family:'Inter',sans-serif;font-size:13px;"
    ), txt)
  })

  output$pred_txt <- renderText({
    h <- input$p_h %||% 8
    paste0("La estimación responde principalmente al patrón horario (",
           sprintf("%02d:00", as.integer(h)),
           "), el día seleccionado y las condiciones climáticas. ",
           "Este simulador reproduce el comportamiento esperado del modelo final ",
           "para presentar la lógica de predicción al usuario.")
  })
}
