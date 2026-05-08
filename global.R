# ============================================================
#  global.R
# ============================================================
library(shiny)
library(plotly)
library(htmltools)

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

# ── Design tokens ──────────────────────────────────────────
BG      <- "#080d16"
CARD    <- "#0f1623"
CARD2   <- "#111827"
BORDER  <- "#1a2840"
FG      <- "#e4f0f8"
MUTED   <- "#6b8fac"
PRIMARY <- "#00e599"
ACCENT  <- "#f97316"
DANGER  <- "#ef4444"
CYAN    <- "#22d3ee"
H <- "'Space Grotesk', system-ui, sans-serif"
B <- "'Inter', system-ui, sans-serif"

# ── Datos de tráfico ───────────────────────────────────────
HOUR_VOL   <- c(650,420,320,280,320,760,2400,4900,5400,4100,3600,3800,
                4000,4100,4400,4900,5800,5500,4200,3000,2300,1800,1300,900)
DOW_LABELS <- c("Lun","Mar","Mié","Jue","Vie","Sáb","Dom")
DOW_VOL    <- c(4350,4500,4520,4480,4400,3100,2400)
MON_LABELS <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
MON_VOL    <- c(3100,3250,3400,3500,3550,3600,3450,3500,3600,3550,3300,3150)

CORR_F <- c("hour_cos","hour","hour_sin","day_of_week","temperature",
            "clouds_all","humidity","month","wind_direction","wind_speed")
CORR_V <- c(-0.7421,0.3453,-0.1673,-0.1571,0.1285,0.0299,0.0144,-0.0135,0.0125,0.0117)

WEATHER_C  <- c("Clear","Clouds","Mist","Haze","Rain","Drizzle","Snow","Thunderstorm","Fog")
WEATHER_V  <- c(3650,3450,3100,3050,2950,2900,2400,2200,2100)

FEAT_NAMES <- c("hour_cos","hour_sin","day_of_week","temperature","weather_desc_enc",
                "weather_type_enc","month","humidity","wind_direction","air_pollution_index",
                "visibility_in_miles","clouds_all","wind_speed","rain_p_h","snow_p_h","is_holiday_bin")
FEAT_VALS  <- c(0.382,0.161,0.148,0.087,0.062,0.041,0.031,0.022,0.018,
                0.014,0.012,0.009,0.006,0.003,0.002,0.001)

RIDGE_F <- c("hour_cos","hour_sin","day_of_week","temperature","month",
             "weather_type_enc","humidity","wind_speed","is_holiday_bin","weather_desc_enc")
RIDGE_C <- c(-1518.3,-468.7,-311.4,76.9,-49.6,-41.4,-26.1,22.2,-18.0,-16.7)

VIF_F <- c("temperature","humidity","hour","wind_direction","visibility_in_miles",
           "month","air_pollution_index","wind_speed","day_of_week","clouds_all","hour_sin","hour_cos")
VIF_V <- c(42.96,19.08,9.31,5.16,4.74,4.56,4.39,3.90,3.20,2.67,2.47,1.03)

OUTLIER_V <- c("rain_p_h","snow_p_h","wind_speed","air_pollution_index","humidity","clouds_all","traffic_volume")
OUTLIER_P <- c(12.4,8.7,4.2,2.1,1.8,0.9,0.3)

MODELS_DATA <- list(
  list(name="XGBoost",           r2=0.9522, rmse=437.0,  mae=257.2, cv=0.9491, tr=0.9623),
  list(name="Random Forest",     r2=0.9456, rmse=466.3,  mae=268.0, cv=0.9421, tr=0.9693),
  list(name="Arbol de Decision", r2=0.9312, rmse=524.1,  mae=297.6, cv=0.9200, tr=0.9485),
  list(name="KNN",               r2=0.8129, rmse=864.5,  mae=560.3, cv=0.8050, tr=0.8529),
  list(name="SVM (SVR)",         r2=0.8041, rmse=884.5,  mae=575.1, cv=0.7980, tr=0.8012),
  list(name="Ridge",             r2=0.6812, rmse=1128.3, mae=871.0, cv=0.6750, tr=0.6820),
  list(name="Lasso",             r2=0.6812, rmse=1128.3, mae=871.1, cv=0.6748, tr=0.6819)
)

EDA_META <- list(
  hour        = list(title="Hora del dia",       unit="TMP", desc="Hora extraida de date_time (0-23)",              corr= 0.345),
  day_of_week = list(title="Dia de semana",       unit="TMP", desc="Patron semanal: laborales vs fin de semana",     corr=-0.157),
  month       = list(title="Mes",                 unit="TMP", desc="Estacionalidad mensual del volumen vehicular",   corr=-0.013),
  temperature = list(title="Temperatura (K)",     unit="NUM", desc="Temperatura registrada en Kelvin",               corr= 0.129),
  humidity    = list(title="Humedad (%)",         unit="NUM", desc="Humedad relativa del ambiente",                  corr= 0.014),
  wind_speed  = list(title="Velocidad del viento",unit="NUM", desc="Velocidad del viento registrada por hora",       corr= 0.012),
  visibility  = list(title="Visibilidad",         unit="NUM", desc="Visibilidad estimada en millas",                 corr= 0.004),
  air_pol     = list(title="Contaminacion",       unit="NUM", desc="Indice de contaminacion atmosferica",            corr=-0.003),
  clouds_all  = list(title="Nubosidad (%)",       unit="NUM", desc="Porcentaje de cobertura de nubes",               corr= 0.030),
  weather     = list(title="Tipo de clima",       unit="CAT", desc="Categoria general del clima observado",          corr=-0.013),
  holiday     = list(title="Festivo",             unit="CAT", desc="Indica si el registro corresponde a un festivo", corr=-0.038)
)

NUM_RANGES <- list(
  temperature = c(250,305,288,7), humidity   = c(20,100,72,17),
  wind_speed  = c(0,13,3.1,1.8),  visibility = c(1,10,5.4,2.2),
  air_pol     = c(0,300,150,70),  clouds_all = c(0,100,42,32)
)

TARGET_EFFECT <- list(
  temperature=5.3, humidity=2.0, wind_speed=18.0,
  visibility=26.0, air_pol=-0.8, clouds_all=3.8
)

# ── Plotly base layout ─────────────────────────────────────
BL <- function(title = "") {
  l <- list(
    plot_bgcolor  = CARD,
    paper_bgcolor = "rgba(0,0,0,0)",
    font    = list(color=FG, family=B, size=12),
    margin  = list(l=44, r=16, t=36, b=40),
    legend  = list(bgcolor="rgba(0,0,0,0)", font=list(color=MUTED, size=11)),
    xaxis   = list(gridcolor=BORDER, zeroline=FALSE,
                   tickfont=list(color=MUTED,size=11,family=B), titlefont=list(color=MUTED,size=11,family=B)),
    yaxis   = list(gridcolor=BORDER, zeroline=FALSE,
                   tickfont=list(color=MUTED,size=11,family=B), titlefont=list(color=MUTED,size=11,family=B))
  )
  if (nchar(title) > 0)
    l$title <- list(text=title, font=list(size=13, color=MUTED, family=H), x=0, xanchor="left")
  l
}

plotly_layout <- function(p, title = "", ...) {
  args <- modifyList(BL(title), list(...))
  do.call(plotly::layout, c(list(p = p), args))
}

# ── Figuras estáticas ──────────────────────────────────────
fig_hour <- function() {
  plot_ly(x=0:23, y=HOUR_VOL, type="bar",
    marker=list(color=HOUR_VOL,
      colorscale=list(c(0,"#001a0e"),c(0.45,PRIMARY),c(1,CYAN)),
      line=list(color="rgba(0,0,0,0)")),
    hovertemplate="Hora %{x}h → %{y:,} veh<extra></extra>") |>
  plotly_layout("Volumen por Hora del Dia", bargap=0.18,
    xaxis=list(gridcolor=BORDER, zeroline=FALSE, title="Hora", dtick=3,
               tickfont=list(color=MUTED,size=11,family=B)))
}

fig_dow <- function() {
  plot_ly() |>
  add_bars(x=DOW_LABELS, y=DOW_VOL,
    marker=list(color=c(rep(PRIMARY,5),ACCENT,ACCENT), line=list(color="rgba(0,0,0,0)")),
    hovertemplate="%{x} → %{y:,}<extra></extra>") |>
  add_lines(x=DOW_LABELS, y=DOW_VOL,
    line=list(color=ACCENT,width=2,dash="dot"),
    marker=list(size=6,color=ACCENT), showlegend=FALSE) |>
  plotly_layout("Por Dia de Semana", bargap=0.28, showlegend=FALSE)
}

fig_month <- function() {
  plot_ly(x=MON_LABELS, y=MON_VOL, type="scatter", mode="lines+markers",
    fill="tozeroy", fillcolor=paste0(PRIMARY,"1a"),
    line=list(color=PRIMARY,width=2.5),
    marker=list(size=7,color=PRIMARY,line=list(color=BG,width=2)),
    hovertemplate="%{x} → %{y:,}<extra></extra>") |>
  plotly_layout("Por Mes")
}

fig_corr <- function() {
  ord <- order(CORR_V); v <- CORR_V[ord]; f <- CORR_F[ord]
  plot_ly(x=v, y=f, type="bar", orientation="h",
    marker=list(color=ifelse(v>=0,PRIMARY,DANGER), opacity=0.85,
                line=list(color="rgba(0,0,0,0)")),
    hovertemplate="%{y}: %{x:.3f}<extra></extra>") |>
  plotly_layout("Correlacion Spearman con traffic_volume", bargap=0.28,
    xaxis=list(gridcolor=BORDER, zeroline=FALSE, title="rho Spearman", range=c(-0.85,0.5),
               tickfont=list(color=MUTED,size=11,family=B)),
    shapes=list(list(type="line",x0=0,x1=0,y0=-0.5,y1=9.5,
                     line=list(color=BORDER,width=1))))
}

fig_weather <- function() {
  ord <- order(WEATHER_V, decreasing=TRUE); v <- WEATHER_V[ord]; w <- WEATHER_C[ord]
  n   <- length(w)
  pal <- sapply(seq_len(n), function(i) sprintf("rgba(0,229,153,%.2f)",0.25+0.75*(i-1)/(n-1)))
  plot_ly(x=w, y=v, type="bar",
    marker=list(color=pal, line=list(color="rgba(0,0,0,0)")),
    hovertemplate="%{x} → %{y:,}<extra></extra>") |>
  plotly_layout("Volumen por Tipo de Clima", bargap=0.30)
}

fig_vif <- function() {
  ord <- order(VIF_V, decreasing=TRUE); v <- VIF_V[ord]; f <- VIF_F[ord]
  plot_ly(x=f, y=v, type="bar",
    marker=list(color=ifelse(v>10,DANGER,ifelse(v>5,ACCENT,PRIMARY)),
                opacity=0.85, line=list(color="rgba(0,0,0,0)")),
    hovertemplate="%{x}: VIF=%{y:.2f}<extra></extra>") |>
  plotly_layout("VIF — Multicolinealidad", bargap=0.25,
    xaxis=list(gridcolor=BORDER, zeroline=FALSE, tickangle=-30, tickfont=list(color=MUTED,size=10,family=B)),
    yaxis=list(gridcolor=BORDER, zeroline=FALSE, title="VIF", tickfont=list(color=MUTED,size=11,family=B)),
    shapes=list(
      list(type="line",x0=-0.5,x1=11.5,y0=10,y1=10,line=list(color=DANGER,width=1.5,dash="dot")),
      list(type="line",x0=-0.5,x1=11.5,y0=5, y1=5, line=list(color=ACCENT,width=1.5,dash="dot"))
    ))
}

fig_outliers <- function() {
  cols <- ifelse(OUTLIER_P>8, DANGER, ifelse(OUTLIER_P>4, ACCENT, PRIMARY))
  plot_ly(x=OUTLIER_V, y=OUTLIER_P, type="bar",
    marker=list(color=cols, opacity=0.85, line=list(color="rgba(0,0,0,0)")),
    text=paste0(OUTLIER_P,"%"), textposition="outside", textfont=list(color=MUTED,size=10,family=B),
    hovertemplate="%{x}: %{y}%<extra></extra>") |>
  plotly_layout("Outliers por Variable (IQR)", bargap=0.30,
    yaxis=list(gridcolor=BORDER, zeroline=FALSE, title="%", range=c(0,16),
               tickfont=list(color=MUTED,size=11,family=B)))
}

fig_r2 <- function() {
  nm <- sapply(MODELS_DATA,`[[`,"name")
  plot_ly() |>
  add_bars(x=nm, y=sapply(MODELS_DATA,`[[`,"r2"), name="R² Test",
    marker=list(color=PRIMARY, opacity=0.85, line=list(color="rgba(0,0,0,0)")),
    hovertemplate="%{x}<br>R² Test: %{y:.4f}<extra></extra>") |>
  add_bars(x=nm, y=sapply(MODELS_DATA,`[[`,"cv"), name="CV R²",
    marker=list(color=CYAN, opacity=0.55, line=list(color="rgba(0,0,0,0)")),
    hovertemplate="%{x}<br>CV R²: %{y:.4f}<extra></extra>") |>
  plotly_layout("R² Test vs Validacion Cruzada",
    barmode="group", bargap=0.20, bargroupgap=0.1,
    xaxis=list(gridcolor=BORDER,zeroline=FALSE,tickangle=-20,tickfont=list(color=MUTED,size=11,family=B)),
    yaxis=list(gridcolor=BORDER,zeroline=FALSE,title="R²",range=c(0.55,1.0),
               tickfont=list(color=MUTED,size=11,family=B)),
    shapes=list(list(type="line",x0=-0.5,x1=6.5,y0=0.95,y1=0.95,
                     line=list(color=ACCENT,width=1.5,dash="dot"))))
}

fig_err <- function() {
  nm <- sapply(MODELS_DATA,`[[`,"name")
  plot_ly() |>
  add_bars(x=nm, y=sapply(MODELS_DATA,`[[`,"rmse"), name="RMSE",
    marker=list(color=DANGER, opacity=0.75, line=list(color="rgba(0,0,0,0)")),
    hovertemplate="%{x}<br>RMSE: %{y:.1f}<extra></extra>") |>
  add_bars(x=nm, y=sapply(MODELS_DATA,`[[`,"mae"), name="MAE",
    marker=list(color=ACCENT, opacity=0.75, line=list(color="rgba(0,0,0,0)")),
    hovertemplate="%{x}<br>MAE: %{y:.1f}<extra></extra>") |>
  plotly_layout("RMSE y MAE",
    barmode="group", bargap=0.20, bargroupgap=0.1,
    xaxis=list(gridcolor=BORDER,zeroline=FALSE,tickangle=-20,tickfont=list(color=MUTED,size=11,family=B)),
    yaxis=list(gridcolor=BORDER,zeroline=FALSE,title="Vehiculos",tickfont=list(color=MUTED,size=11,family=B)))
}

fig_overfit <- function() {
  nm  <- sapply(MODELS_DATA,`[[`,"name")
  tr  <- sapply(MODELS_DATA,`[[`,"tr")
  te  <- sapply(MODELS_DATA,`[[`,"r2")
  del <- tr - te
  plot_ly() |>
  add_trace(x=nm, y=tr, type="scatter", mode="lines+markers", name="R² Train",
    line=list(color=CYAN,width=2.5),
    marker=list(size=8,color=CYAN,line=list(color=BG,width=2)),
    hovertemplate="%{x}<br>Train: %{y:.4f}<extra></extra>") |>
  add_trace(x=nm, y=te, type="scatter", mode="lines+markers", name="R² Test",
    line=list(color=PRIMARY,width=2.5),
    marker=list(size=8,color=PRIMARY,line=list(color=BG,width=2)),
    hovertemplate="%{x}<br>Test: %{y:.4f}<extra></extra>") |>
  add_bars(x=nm, y=del, name="Delta", yaxis="y2",
    marker=list(color=ifelse(del>0.07,DANGER,ifelse(del>0.03,ACCENT,PRIMARY)),
                opacity=0.40, line=list(color="rgba(0,0,0,0)")),
    hovertemplate="%{x}<br>Delta: %{y:.4f}<extra></extra>") |>
  plotly_layout("Train vs Test — Diagnostico de Overfitting",
    barmode="overlay",
    xaxis =list(gridcolor=BORDER,zeroline=FALSE,tickangle=-20,tickfont=list(color=MUTED,size=11,family=B)),
    yaxis =list(gridcolor=BORDER,zeroline=FALSE,title="R²",range=c(0.55,1.02),
                tickfont=list(color=MUTED,size=11,family=B)),
    yaxis2=list(overlaying="y",side="right",title="Delta",range=c(0,0.16),
                tickfont=list(color=MUTED,size=10,family=B),gridcolor="rgba(0,0,0,0)",zeroline=FALSE))
}

fig_imp <- function() {
  ord <- order(FEAT_VALS); v <- FEAT_VALS[ord]; f <- FEAT_NAMES[ord]
  plot_ly(x=v, y=f, type="bar", orientation="h",
    marker=list(color=sapply(v,function(x) sprintf("rgba(0,229,153,%.2f)",max(0.20,x/0.38))),
                line=list(color="rgba(0,0,0,0)")),
    hovertemplate="%{y}: %{x:.3f}<extra></extra>") |>
  plotly_layout("Importancia de Features — XGBoost", bargap=0.22,
    xaxis=list(gridcolor=BORDER,zeroline=FALSE,title="Importancia",tickfont=list(color=MUTED,size=11,family=B)),
    yaxis=list(gridcolor=BORDER,zeroline=FALSE,tickfont=list(color=MUTED,size=10,family=B)))
}

fig_ridge <- function() {
  ord <- order(RIDGE_C); v <- RIDGE_C[ord]; f <- RIDGE_F[ord]
  plot_ly(x=v, y=f, type="bar", orientation="h",
    marker=list(color=ifelse(v>=0,PRIMARY,DANGER), opacity=0.85,
                line=list(color="rgba(0,0,0,0)")),
    hovertemplate="%{y}: %{x:.1f}<extra></extra>") |>
  plotly_layout("Coeficientes Ridge (estandarizados)", bargap=0.28,
    xaxis=list(gridcolor=BORDER,zeroline=FALSE,title="Coeficiente",tickfont=list(color=MUTED,size=11,family=B)),
    shapes=list(list(type="line",x0=0,x1=0,y0=-0.5,y1=9.5,line=list(color=BORDER,width=1))))
}

fig_gauge <- function(v) {
  plot_ly(type="indicator", mode="gauge+number", value=v,
    number=list(font=list(color=FG,size=34,family=H), suffix=" veh"),
    gauge=list(
      axis =list(range=list(0,7200),tickcolor=MUTED,tickfont=list(color=MUTED)),
      bar  =list(color=PRIMARY),
      bgcolor=CARD, borderwidth=1, bordercolor=BORDER,
      steps=list(
        list(range=c(0,2500),   color="rgba(0,229,153,0.16)"),
        list(range=c(2500,5000),color="rgba(249,115,22,0.18)"),
        list(range=c(5000,7200),color="rgba(239,68,68,0.20)")
      ),
      threshold=list(line=list(color=ACCENT,width=4),thickness=0.75,value=v)
    )) |>
  layout(paper_bgcolor="rgba(0,0,0,0)",
         margin=list(l=20,r=20,t=20,b=20), font=list(color=FG,family=B))
}

# ── EDA: figuras reactivas ─────────────────────────────────
set.seed(42)
nsamp <- function(var, n=400) {
  r <- NUM_RANGES[[var]]
  sort(pmin(pmax(rnorm(n, r[3], r[4]), r[1]), r[2]))
}

fig_num_dist <- function(var) {
  meta <- EDA_META[[var]]; x <- nsamp(var, 500)
  plot_ly(x=x, type="histogram", nbinsx=28,
    marker=list(color=PRIMARY, opacity=0.78, line=list(color="rgba(0,0,0,0)")),
    hovertemplate=paste0(meta$title,": %{x:.2f}<br>Frecuencia: %{y}<extra></extra>")) |>
  plotly_layout(paste0("Distribucion · ",meta$title), bargap=0.08,
    xaxis=list(gridcolor=BORDER,zeroline=FALSE,title=meta$title,tickfont=list(color=MUTED,size=11,family=B)),
    yaxis=list(gridcolor=BORDER,zeroline=FALSE,title="Frecuencia",tickfont=list(color=MUTED,size=11,family=B)))
}

fig_num_tgt <- function(var) {
  meta <- EDA_META[[var]]; x <- nsamp(var, 230)
  sl   <- TARGET_EFFECT[[var]] %||% 1
  y    <- pmin(pmax(3200+sl*(x-mean(x))+rnorm(length(x),0,1150), 250), 7000)
  tx   <- c(min(x),max(x)); ty <- 3200+sl*(tx-mean(x))
  plot_ly() |>
  add_trace(x=x, y=y, type="scatter", mode="markers",
    marker=list(size=7,color=CYAN,opacity=0.38,line=list(color="rgba(0,0,0,0)")),
    hovertemplate=paste0(meta$title,": %{x:.2f}<br>traffic_volume: %{y:.0f}<extra></extra>")) |>
  add_lines(x=tx, y=ty, line=list(color=ACCENT,width=3,dash="dot"), name="Tendencia") |>
  plotly_layout(paste0(meta$title," vs traffic_volume"), showlegend=FALSE,
    xaxis=list(gridcolor=BORDER,zeroline=FALSE,title=meta$title,tickfont=list(color=MUTED,size=11,family=B)),
    yaxis=list(gridcolor=BORDER,zeroline=FALSE,title="traffic_volume",tickfont=list(color=MUTED,size=11,family=B)))
}

fig_beh <- function(var) {
  if(var=="hour")        return(fig_hour())
  if(var=="day_of_week") return(fig_dow())
  if(var=="month")       return(fig_month())
  if(var=="weather") return(
    plot_ly(x=WEATHER_C, y=c(38,32,9,7,5,4,2,1.5,1.5), type="bar",
      marker=list(color=c(PRIMARY,PRIMARY,CYAN,CYAN,ACCENT,ACCENT,DANGER,DANGER,DANGER),
                  opacity=0.82, line=list(color="rgba(0,0,0,0)")),
      hovertemplate="%{x}: %{y:.1f}%<extra></extra>") |>
    plotly_layout("Distribucion · Tipo de clima", bargap=0.25,
      xaxis=list(gridcolor=BORDER,zeroline=FALSE,tickangle=-25,tickfont=list(color=MUTED,size=11,family=B)),
      yaxis=list(gridcolor=BORDER,zeroline=FALSE,title="% registros",tickfont=list(color=MUTED,size=11,family=B)))
  )
  if(var=="holiday") return(
    plot_ly(x=c("No festivo","Festivo"), y=c(95.2,4.8), type="bar",
      marker=list(color=c(PRIMARY,ACCENT), opacity=0.85, line=list(color="rgba(0,0,0,0)")),
      hovertemplate="%{x}: %{y:.1f}%<extra></extra>") |>
    plotly_layout("Distribucion · Festivos", bargap=0.35,
      yaxis=list(gridcolor=BORDER,zeroline=FALSE,title="% registros",tickfont=list(color=MUTED,size=11,family=B)))
  )
  fig_num_dist(var)
}

fig_tgt <- function(var) {
  if(var=="hour")        return(fig_hour())
  if(var=="day_of_week") return(fig_dow())
  if(var=="month")       return(fig_month())
  if(var=="weather")     return(fig_weather())
  if(var=="holiday") return(
    plot_ly(x=c("No festivo","Festivo"), y=c(3350,2600), type="bar",
      marker=list(color=c(PRIMARY,DANGER), opacity=0.85, line=list(color="rgba(0,0,0,0)")),
      hovertemplate="%{x}: %{y:,} veh<extra></extra>") |>
    plotly_layout("Festivo vs traffic_volume", bargap=0.35,
      yaxis=list(gridcolor=BORDER,zeroline=FALSE,title="Volumen promedio",tickfont=list(color=MUTED,size=11,family=B)))
  )
  fig_num_tgt(var)
}

# ── Estimador de tráfico ───────────────────────────────────
estimate <- function(hour,dow,month,holiday,temp,humidity,ws,vis,poll,rain,snow,clouds,weather,wind_dir=320) {
  base    <- 3100
  hprof   <- approx(0:23, HOUR_VOL, xout=hour)$y - 3200
  dow_eff <- c(120,220,260,300,260,-520,-850)[dow+1]
  mon_eff <- c(-180,-80,40,120,160,180,80,120,180,150,-40,-160)[month]
  wmap    <- c(Clear=180,Clouds=80,Mist=-90,Haze=-110,Rain=-240,
               Drizzle=-260,Snow=-650,Thunderstorm=-820,Fog=-720)
  weff    <- if(!is.na(wmap[weather])) wmap[weather] else 0
  clim    <- 4.5*(temp-288) - 5*rain - 9*snow - 250*holiday +
             8*(vis-5) + 0.8*(clouds-40) - 0.4*(humidity-70) +
             6*(ws-3) - 0.15*(poll-120)
  as.integer(max(150, min(7200, base+hprof+dow_eff+mon_eff+weff+clim)))
}

# ── UI helpers compartidos ─────────────────────────────────
sHdr <- function(num, title, sub="")
  div(
    div(class="snum", num),
    div(class="stitle", title),
    if(nchar(sub)>0) div(class="ssub", sub)
  )

pill <- function(txt, color=PRIMARY)
  span(class="pill",
       style=paste0("background:",color,"14;color:",color,";border:1px solid ",color,"2e;"),
       tags$span("●", style=paste0("color:",color,";margin-right:6px;")),
       txt)

signalChip <- function(text, color=PRIMARY)
  span(class="signal-chip",
       style=paste0("background:",color,"14;border:1px solid ",color,"32;color:",color,";"),
       text)

contextKpi <- function(icon, title, value, desc, color=PRIMARY)
  div(class="kc dc",
    div(class="ktop",
      div(class="kicon", style=paste0("background:",color,"18;border:1px solid ",color,"35;color:",color,";"), icon),
      div(class="kval", value)
    ),
    div(class="klbl", title),
    div(class="kdesc", desc)
  )

contextValue <- function(icon, title, desc, color=PRIMARY)
  div(style=paste0("padding:18px;border-radius:18px;border:1px solid ",BORDER,";background:rgba(8,13,22,0.72);"),
    div(style=paste0("font-size:23px;margin-bottom:12px;color:",color,";"), icon),
    tags$h4(title, style=paste0("font-family:",H,";font-size:16px;font-weight:800;color:",FG,";margin:0 0 8px 0;")),
    tags$p(desc, style=paste0("font-family:",B,";font-size:12px;color:",MUTED,";line-height:1.55;margin:0;"))
  )

metricChip <- function(title, desc, color=PRIMARY)
  div(style=paste0("padding:14px;border-radius:16px;border:1px solid ",color,"30;background:",color,"0d;"),
    div(style=paste0("font-family:",H,";font-size:15px;font-weight:900;color:",color,";margin-bottom:6px;"), title),
    div(style=paste0("font-family:",B,";font-size:12px;color:",MUTED,";line-height:1.5;"), desc)
  )

theoryTable <- function(rows)
  div(style="margin-top:16px;",
    div(style=paste0("display:flex;gap:14px;padding:0 0 12px 0;border-bottom:1px solid ",BORDER,";margin-bottom:8px;"),
      div("Modelo",      style=paste0("font-family:",B,";font-size:12px;font-weight:900;color:",MUTED,";text-transform:uppercase;letter-spacing:0.08em;width:170px;")),
      div("Descripción", style=paste0("font-family:",B,";font-size:12px;font-weight:900;color:",MUTED,";text-transform:uppercase;letter-spacing:0.08em;flex:1;"))
    ),
    tagList(lapply(rows, function(r)
      div(style=paste0("display:flex;gap:14px;padding:12px 0;border-bottom:1px solid ",BORDER,"33;"),
        div(r[[1]], style=paste0("font-family:",H,";font-size:14px;font-weight:800;color:",FG,";width:170px;")),
        div(r[[2]], style=paste0("font-family:",B,";font-size:13px;color:",MUTED,";line-height:1.55;flex:1;"))
      )
    ))
  )

modelNote <- function(title, body, color=PRIMARY)
  div(class="model-note",
      style=paste0("border:1px solid ",color,"30;background:",color,"0f;"),
      div(style=paste0("font-family:",H,";font-size:12px;font-weight:800;color:",color,";margin-bottom:6px;"), title),
      tags$p(body, style=paste0("font-family:",B,";font-size:12px;color:",MUTED,";line-height:1.55;margin:0;"))
  )

benefitRow <- function(icon, text, color)
  div(class="benefit-row",
    span(icon, style=paste0("color:",color,";font-size:16px;margin-right:12px;flex-shrink:0;")),
    span(text, style=paste0("font-size:15px;color:",MUTED,";line-height:1.5;"))
  )

gpsDiv <- function(x, y, color, sz="8px")
  div(class="gps-dot",
      style=paste0("width:",sz,";height:",sz,";background:",color,
                   ";box-shadow:0 0 10px ",color,", 0 0 20px ",color,"55;left:",x,";top:",y,";"))

contextVisual <- function()
  div(style=paste0(
    "min-height:360px;position:relative;overflow:hidden;border-radius:20px;",
    "background:linear-gradient(135deg,rgba(0,229,153,0.08),rgba(34,211,238,0.04)),",CARD,";",
    "border:1px solid ",BORDER,";"
  ),
    tags$video(src="carretera_noche.mp4", autoPlay=NA, muted=NA, loop=NA,
               style="position:absolute;inset:0;width:100%;height:100%;object-fit:cover;z-index:0;"),
    div(style="position:absolute;inset:0;background:rgba(8,18,28,0.58);z-index:1;"),
    div(style=paste0("position:absolute;inset:0;background:radial-gradient(circle at 30% 30%,",
                     PRIMARY,"18,transparent 42%),radial-gradient(circle at 82% 72%,",CYAN,"12,transparent 45%);z-index:1;")),
    div(style="position:relative;z-index:2;padding:26px;",
      div("Ciudad", style=paste0("font-family:",B,";font-size:11px;color:",MUTED,";font-weight:800;letter-spacing:0.12em;text-transform:uppercase;")),
      div("Demanda vial", style=paste0("font-family:",H,";font-size:28px;color:",FG,";font-weight:900;margin-top:4px;")),
      div("Clima + calendario + hora → traffic_volume", style=paste0("font-family:",B,";font-size:13px;color:",MUTED,";margin-top:8px;"))
    )
  )

kpiCard <- function(icon,title,val,desc,col=PRIMARY)
  div(class="kc dc", style=paste0("border-top:3px solid ",col,";"),
    div(class="ktop",
      div(class="kicon",style=paste0("background:",col,"18;border:1px solid ",col,"35;color:",col,";"),icon),
      div(class="kval",val)),
    div(class="klbl",title), div(class="kdesc",desc))

navCard <- function(bid,icon,title,desc,col=PRIMARY)
  actionButton(
    bid,
    label = tagList(
      div(class="nc-ic",style=paste0("background:",col,"18;border:1px solid ",col,"28;color:",col,";"),icon),
      div(div(class="nc-t",title), div(class="nc-d",desc))
    ),
    class="nc"
  )

CW <- function(...) div(class="cw", ...)

NAVBAR <- div(class="nvb",
  div(class="nvi",
    div(class="nvl",
      div(class="nvic", "∿"),
      span(class="nvbr", "Predicción Volumen Tráfico")
    ),
    tags$ul(class="nvlinks",
      tags$li(actionButton("n_home","Inicio")),
      tags$li(actionButton("n_ctx", "Contexto")),
      tags$li(actionButton("n_eda", "Exploratorio")),
      tags$li(actionButton("n_mod", "Evaluación")),
      tags$li(actionButton("n_sim", "Prueba de Modelo")),
      tags$li(actionButton("n_ins", "Aplicaciones"))
    ),
    actionButton("n_sim2","Probar modelo", class="nvcta")
  )
)

# ── Pages ──────────────────────────────────────────────────

pageHome <- function()
  div(class="hero",
    div(style=paste0(
      "position:absolute;inset:0;z-index:0;pointer-events:none;",
      "background-image:",
      "radial-gradient(circle at 18% 38%,rgba(255,255,255,0.22) 1px,transparent 1.5px),",
      "radial-gradient(circle at 72% 26%,rgba(255,255,255,0.18) 1px,transparent 1.5px),",
      "radial-gradient(circle at 84% 64%,rgba(255,255,255,0.16) 1px,transparent 1.5px),",
      "radial-gradient(circle at 31% 72%,rgba(255,255,255,0.16) 1px,transparent 1.5px),",
      "radial-gradient(circle at 58% 82%,rgba(255,255,255,0.14) 1px,transparent 1.5px);",
      "background-size:460px 360px,520px 430px,480px 420px,560px 460px,600px 500px;"
    )),
    div(style=paste0(
      "position:absolute;left:50%;top:20%;transform:translateX(-50%);",
      "width:760px;height:360px;border-radius:50%;",
      "background:radial-gradient(ellipse,",CYAN,"10,transparent 70%);",
      "filter:blur(12px);pointer-events:none;z-index:0;"
    )),
    div(class="hero-in",
      tags$h1("Predicción Volumen Tráfico", class="ht"),
      div(class="hs", "Machine Learning aplicado a movilidad urbana"),
      div(class="hd",
        "Estima",
        tags$span(" el Volumen del tráfico", style=paste0("color:",PRIMARY,";font-family:monospace;")),
        " a partir de hora, calendario, clima y variables ambientales. Una demo ejecutiva para entender datos, comparar modelos y probar predicciones."
      ),
      div(class="hbtns",
        actionButton("hb_ctx", "Contexto",              class="hbp"),
        actionButton("hb_eda", "Exploratorio",          class="hbg"),
        actionButton("hb_mod", "Evaluación de modelos", class="hbg"),
        actionButton("hb_sim", "Prueba de modelo",      class="hbg"),
        actionButton("hb_ins", "Aplicaciones",          class="hbg")
      ),
      div(class="htags",
        actionButton("sc_ctx", tagList(
          span("▣",style="font-size:14px;margin-right:9px;"),
          span("Contexto",style=paste0("font-weight:700;font-size:13px;color:",FG,";")),
          span("4",style="margin-left:10px;min-width:24px;height:24px;border-radius:999px;background:rgba(255,255,255,0.10);display:inline-flex;align-items:center;justify-content:center;font-size:11px;font-weight:800;")
        ), class="htag"),
        actionButton("sc_eda", tagList(
          span("彡",style="font-size:14px;margin-right:9px;"),
          span("Exploratorio",style=paste0("font-weight:700;font-size:13px;color:",FG,";")),
          span("9",style="margin-left:10px;min-width:24px;height:24px;border-radius:999px;background:rgba(255,255,255,0.10);display:inline-flex;align-items:center;justify-content:center;font-size:11px;font-weight:800;")
        ), class="htag"),
        actionButton("sc_sim", tagList(
          span("◎",style="font-size:14px;margin-right:9px;"),
          span("Prueba",style=paste0("font-weight:700;font-size:13px;color:",FG,";")),
          span("14",style="margin-left:10px;min-width:24px;height:24px;border-radius:999px;background:rgba(255,255,255,0.10);display:inline-flex;align-items:center;justify-content:center;font-size:11px;font-weight:800;")
        ), class="htag"),
        actionButton("sc_mod", tagList(
          span("◈",style="font-size:14px;margin-right:9px;"),
          span("Modelos",style=paste0("font-weight:700;font-size:13px;color:",FG,";")),
          span("7",style="margin-left:10px;min-width:24px;height:24px;border-radius:999px;background:rgba(255,255,255,0.10);display:inline-flex;align-items:center;justify-content:center;font-size:11px;font-weight:800;")
        ), class="htag"),
        actionButton("sc_ins", tagList(
          span("⍁",style="font-size:14px;margin-right:9px;"),
          span("Aplicaciones",style=paste0("font-weight:700;font-size:13px;color:",FG,";")),
          span("4",style="margin-left:10px;min-width:24px;height:24px;border-radius:999px;background:rgba(255,255,255,0.10);display:inline-flex;align-items:center;justify-content:center;font-size:11px;font-weight:800;")
        ), class="htag")
      ),
      div(style=paste0("font-family:",B,";font-size:13px;font-weight:700;color:#9aa6b8;margin-bottom:28px;"),
          "Selecciona una sección para continuar"),
      div(class="ngrid",
        navCard("nc_ctx","\U0001f3d9","Contexto","Problema, valor de negocio y uso del modelo.", CYAN),
        navCard("nc_eda","彡","Exploratorio de datos","Distribuciones, comportamiento temporal y relación con el target.", PRIMARY),
        navCard("nc_sim","⚡","Prueba de Modelo","Ingresa variables y estima el volumen de tráfico.", ACCENT),
        navCard("nc_mod","◎","Evaluación de modelos","RMSE, MAE, R², CV y overfitting.", PRIMARY),
        navCard("nc_ins","✦","Aplicaciones Reales","Casos de uso industriales: semáforos inteligentes y navegación predictiva.", CYAN)
      )
    )
  )

pageContexto <- function()
  div(class="pw",
    div(style=paste0("display:flex;gap:22px;align-items:center;flex-wrap:wrap;margin-bottom:22px;"),
      div(style=paste0(
        "flex:1.2;min-width:330px;background:rgba(3,8,18,0.58);backdrop-filter:blur(12px);",
        "border:1px solid ",BORDER,";border-radius:28px;padding:36px;box-shadow:0 12px 45px rgba(0,0,0,0.45);"
      ),
        pill("CONTEXTO PREVIO AL ANALISIS"),
        tags$h1(
          "¿Por qué predecir el ",
          span("volumen de tráfico", style=paste0("color:",PRIMARY,";")),
          "?",
          style=paste0("font-family:",H,";font-size:clamp(36px,5vw,64px);font-weight:900;",
                       "line-height:1.04;letter-spacing:-0.045em;color:",FG,";margin:18px 0 16px 0;")
        ),
        tags$p("El tráfico cambia por hora, clima, festivos y condiciones ambientales. Esta solución convierte esos datos en una predicción útil para anticipar demanda, planear recursos y reducir decisiones reactivas.",
               style=paste0("font-family:",B,";font-size:16px;line-height:1.7;color:",MUTED,";max-width:700px;margin:0 0 24px 0;")),
        div(
          signalChip("Regresión supervisada", PRIMARY),
          signalChip("Target: traffic_volume", CYAN),
          signalChip("Datos horarios", ACCENT),
          signalChip("Clima + tiempo + ambiente", PRIMARY)
        ),
        div(style="display:flex;gap:12px;flex-wrap:wrap;margin-top:18px;",
          actionButton("ctx_eda","Explorar datos →",
            style=paste0("display:inline-flex;padding:12px 22px;border-radius:999px;",
                         "background:",PRIMARY,";color:#061016;font-weight:900;",
                         "font-family:",B,";font-size:13px;border:none;cursor:pointer;")),
          actionButton("ctx_sim","Probar modelo",
            style=paste0("display:inline-flex;padding:12px 22px;border-radius:999px;",
                         "border:1px solid ",BORDER,";color:",FG,";font-weight:800;",
                         "font-family:",B,";font-size:13px;background:rgba(255,255,255,0.035);",
                         "cursor:pointer;box-shadow:none;"))
        )
      ),
      div(style="flex:0.8;min-width:320px;", contextVisual())
    ),
    div(class="kgrid",
      contextKpi("\U0001f697","Problema","Congestión","La demanda vehicular no se comporta igual a las 3 AM que a las 8 AM o en lluvia.", PRIMARY),
      contextKpi("\U0001f4e1","Datos disponibles","Históricos","Registros horarios con clima, calendario, visibilidad, contaminación y volumen vehicular.", CYAN),
      contextKpi("\U0001f3af","Objetivo","Predecir","Estimar traffic_volume antes de tomar decisiones operativas.", ACCENT),
      contextKpi("⚙️","Enfoque","ML","Comparar modelos de regresión para encontrar el mejor balance entre error e interpretación.", PRIMARY)
    ),
    div(style="display:flex;gap:16px;flex-wrap:wrap;margin-bottom:16px;",
      div(class="dc", style="flex:1.2;min-width:360px;",
        tags$h3("3. Planteamiento del Problema",
                style=paste0("font-family:",H,";font-size:24px;color:",FG,";margin:0 0 14px 0;")),
        tags$p("Las entidades de gestión de tráfico carecen frecuentemente de herramientas predictivas que integren variables meteorológicas y temporales para estimar el volumen vehicular en tiempo real o anticipado. Esto dificulta la toma de decisiones proactivas para mitigar la congestión, especialmente en días festivos, eventos climáticos extremos o en horas pico.",
               style=paste0("font-family:",B,";font-size:14px;line-height:1.7;color:",MUTED,";margin:0 0 14px 0;")),
        div(style=paste0("padding:16px 18px;border-radius:16px;border:1px solid ",ACCENT,"40;background:",ACCENT,"0d;"),
          div("Pregunta de investigación",
              style=paste0("font-family:",B,";font-size:10px;letter-spacing:0.12em;text-transform:uppercase;font-weight:900;color:",ACCENT,";margin-bottom:8px;")),
          div("¿Es posible predecir con precisión el volumen de tráfico vehicular a partir de variables meteorológicas, ambientales y temporales, utilizando modelos de aprendizaje automático supervisado?",
              style=paste0("font-family:",B,";font-size:14px;line-height:1.65;color:",FG,";font-style:italic;"))
        )
      ),
      div(class="dc", style="flex:0.8;min-width:320px;",
        tags$h3("4. Objetivo General",
                style=paste0("font-family:",H,";font-size:24px;color:",FG,";margin:0 0 14px 0;")),
        tags$p("Desarrollar y evaluar un modelo de regresión basado en aprendizaje automático capaz de predecir el volumen de tráfico vehicular con alta precisión, utilizando variables meteorológicas, ambientales y temporales derivadas del dataset.",
               style=paste0("font-family:",B,";font-size:14px;line-height:1.7;color:",MUTED,";margin:0;"))
      )
    ),
    div(class="dc", style="margin-bottom:16px;",
      tags$h3("5. Objetivos Específicos",
              style=paste0("font-family:",H,";font-size:24px;color:",FG,";margin:0 0 18px 0;")),
      div(style="display:grid;grid-template-columns:repeat(5,minmax(180px,1fr));gap:12px;",
        contextValue("01","Análisis exploratorio","Identificar distribuciones, valores atípicos, correlaciones y patrones frente a la variable objetivo.", PRIMARY),
        contextValue("02","Preprocesamiento","Extraer componentes temporales de date_time y codificar variables categóricas.", CYAN),
        contextValue("03","Pipeline ML","Estandarizar el flujo de preprocesamiento, entrenamiento y evaluación de modelos.", ACCENT),
        contextValue("04","Comparación","Entrenar y comparar XGBoost, Random Forest, Árbol de Decisión, KNN, SVM, Ridge y Lasso.", PRIMARY),
        contextValue("05","Validación final","Seleccionar el modelo de mayor poder predictivo mediante métricas, pruebas estadísticas y residuales.", CYAN)
      )
    ),
    div(style="display:flex;gap:16px;flex-wrap:wrap;margin-bottom:16px;",
      div(class="dc", style="flex:1;min-width:360px;",
        tags$h3("6. Justificación",
                style=paste0("font-family:",H,";font-size:24px;color:",FG,";margin:0 0 14px 0;")),
        tags$p("La predicción del volumen de tráfico tiene impacto directo en la planificación urbana, reducción de emisiones de CO₂, diseño de rutas de transporte público y respuesta de emergencias.",
               style=paste0("font-family:",B,";font-size:14px;line-height:1.7;color:",MUTED,";margin:0 0 16px 0;")),
        div(style="display:grid;grid-template-columns:repeat(3,1fr);gap:12px;",
          contextValue("\U0001f6a6","Reducir congestión","Semáforos adaptativos y redireccionamiento vehicular.", PRIMARY),
          contextValue("\U0001f527","Optimizar recursos","Mantenimiento vial e identificación de períodos de alta demanda.", CYAN),
          contextValue("\U0001f4ca","Apoyar inversión","Infraestructura basada en datos y comportamiento real del tráfico.", ACCENT)
        ),
        tags$p("El uso de múltiples modelos y su comparación sistemática garantiza que la solución adoptada sea la más adecuada para el comportamiento específico del dataset.",
               style=paste0("font-family:",B,";font-size:13px;line-height:1.65;color:",MUTED,";margin:16px 0 0 0;"))
      ),
      div(class="dc", style="flex:0.75;min-width:300px;",
        tags$h3("7.1 Regresión Supervisada",
                style=paste0("font-family:",H,";font-size:24px;color:",FG,";margin:0 0 14px 0;")),
        tags$p("La regresión es una técnica de aprendizaje supervisado que busca aprender una función f: X → y, donde y es una variable continua. El objetivo es minimizar el error de predicción sobre datos no vistos.",
               style=paste0("font-family:",B,";font-size:14px;line-height:1.7;color:",MUTED,";margin:0;"))
      )
    ),
    div(class="dc", style="margin-bottom:16px;",
      tags$h3("7.2 Modelos Implementados",
              style=paste0("font-family:",H,";font-size:24px;color:",FG,";margin:0 0 10px 0;")),
      theoryTable(list(
        list("XGBoost","Gradient boosting optimizado; combina múltiples árboles débiles con regularización L1/L2. Alta capacidad predictiva y resistencia al overfitting."),
        list("Random Forest","Ensemble de árboles de decisión con bagging. Robusto ante outliers y ruido. Proporciona importancia de variables."),
        list("Árbol de Decisión","Modelo base interpretable. Particiona el espacio de características de forma jerárquica. Propenso a overfitting sin poda."),
        list("KNN","Predicción basada en los k vecinos más cercanos. Sensible a la escala y dimensionalidad."),
        list("SVM (SVR)","Support Vector Regression; maximiza el margen en un espacio de alta dimensión. Efectivo con datos no lineales mediante kernels."),
        list("Ridge","Regresión lineal con regularización L2. Reduce la varianza penalizando coeficientes grandes."),
        list("Lasso","Regresión lineal con regularización L1. Realiza selección automática de variables al llevar coeficientes a cero.")
      ))
    ),
    div(class="dc",
      tags$h3("7.3 Métricas de Evaluación",
              style=paste0("font-family:",H,";font-size:24px;color:",FG,";margin:0 0 18px 0;")),
      div(style="display:grid;grid-template-columns:repeat(4,1fr);gap:12px;",
        metricChip("MAE","Mean Absolute Error: error promedio en unidades originales.", PRIMARY),
        metricChip("RMSE","Root Mean Squared Error: penaliza errores grandes.", ACCENT),
        metricChip("R²","Coeficiente de Determinación: proporción de varianza explicada.", CYAN),
        metricChip("MAPE","Mean Absolute Percentage Error: error porcentual promedio.", PRIMARY)
      )
    )
  )

pageEDA <- function() {
  ch <- c("Hora del dia"="hour","Dia de semana"="day_of_week","Mes"="month",
          "Temperatura"="temperature","Humedad"="humidity","Vel. viento"="wind_speed",
          "Visibilidad"="visibility","Contaminacion"="air_pol","Nubosidad"="clouds_all",
          "Tipo de clima"="weather","Festivo"="holiday")
  div(class="pw",
    sHdr("03 — EDA","Explorador interactivo de datos",
         "Selecciona una variable para inspeccionar su comportamiento y su relación con traffic_volume"),
    div(style="display:flex;gap:16px;flex-wrap:wrap;margin-bottom:16px;",
      div(class="dc", style="flex:1.8;min-width:320px;",
        div(style="display:flex;align-items:flex-start;margin-bottom:22px;",
          div(style=paste0("font-size:22px;color:",PRIMARY,";margin-right:10px;"), "\U0001f50d"),
          div(
            tags$h3("Explorador de variables",
                    style=paste0("font-family:",H,";font-size:22px;color:",FG,";margin:0 0 4px 0;")),
            tags$p(tagList("Elige una variable para revisar su distribución y compararla contra ",
                           tags$code("traffic_volume", style=paste0("color:",PRIMARY,";background:transparent;"))),
                   style=paste0("color:",MUTED,";font-size:14px;font-family:",B,";margin:0;"))
          )
        ),
        div(class="eda-opt",
          radioButtons("eda_var",NULL,choices=ch,selected="hour",inline=TRUE)),
        uiOutput("eda_desc_txt")
      ),
      div(class="dc", style=paste0("width:280px;min-width:240px;display:flex;flex-direction:column;justify-content:center;"),
        div("CORRELACION DE SPEARMAN",
            style=paste0("font-size:10px;color:",MUTED,";font-weight:700;letter-spacing:0.14em;font-family:",B,";text-transform:uppercase;")),
        uiOutput("eda_corr"),
        uiOutput("eda_dir")
      )
    ),
    div(style="display:grid;grid-template-columns:1fr 1fr;gap:16px;margin-bottom:16px;",
      div(class="dc",
        div(style="display:flex;justify-content:space-between;align-items:center;margin-bottom:8px;",
          tags$h4("Comportamiento de la variable", style=paste0("font-family:",H,";font-size:16px;color:",FG,";margin:0;")),
          span(class="badge",style=paste0("background:rgba(0,229,153,.13);color:",PRIMARY,";border:1px solid rgba(0,229,153,.3);"),"EDA")
        ),
        plotlyOutput("eda_beh",height="390px")
      ),
      div(class="dc",
        div(style="display:flex;justify-content:space-between;align-items:center;margin-bottom:8px;",
          tags$h4("Variable vs target", style=paste0("font-family:",H,";font-size:16px;color:",FG,";margin:0;")),
          span(class="badge",style=paste0("background:rgba(34,211,238,.13);color:",CYAN,";border:1px solid rgba(34,211,238,.3);"),"TARGET")
        ),
        plotlyOutput("eda_tgt",height="390px")
      )
    ),
    div(style="display:flex;gap:16px;flex-wrap:wrap;margin-bottom:16px;",
      div(class="dc", style="flex:1;",
        tags$h4("Resumen rápido", style=paste0("font-family:",H,";font-size:14px;color:",FG,";margin:0 0 16px 0;")),
        uiOutput("eda_insight")
      ),
      div(class="dc", style="flex:1.4;",
        tags$h4("Diagnósticos del dataset", style=paste0("font-family:",H,";font-size:14px;color:",FG,";margin:0 0 16px 0;")),
        div(style="display:flex;gap:28px;flex-wrap:wrap;",
          lapply(list(list("33,750",FG,"Registros"),list("16",PRIMARY,"Features"),
                      list("0",CYAN,"Nulos"),list("No normal",DANGER,"Shapiro p<0.05")),
            function(x) div(
              div(style=paste0("font-family:",H,";font-size:22px;font-weight:800;color:",x[[2]],";"),x[[1]]),
              div(style=paste0("font-size:11px;color:",MUTED,";"),x[[3]])
            )
          )
        )
      )
    ),
    tags$hr(class="dvd"),
    sHdr("-","Análisis complementario","VIF · Outliers · Correlaciones · Clima"),
    div(style="display:grid;grid-template-columns:1fr 1fr;gap:16px;margin-bottom:16px;",
      CW(plotlyOutput("s_vif",    height="310px")),
      CW(plotlyOutput("s_out",    height="310px"))
    ),
    div(style="display:grid;grid-template-columns:1fr 1fr;gap:16px;",
      CW(plotlyOutput("s_corr",   height="310px")),
      CW(plotlyOutput("s_weather",height="310px"))
    )
  )
}

pageModelos <- function() {
  rows <- lapply(seq_along(MODELS_DATA), function(i) {
    m <- MODELS_DATA[[i]]; b <- i==1
    div(class=paste0("mrow",if(b)" best"else""),
      div(class="mc",style=paste0("width:42px;text-align:center;font-size:18px;font-weight:",if(b)"800"else"500",";color:",if(b)PRIMARY else MUTED,";"),i),
      div(style="flex:1;display:flex;align-items:center;gap:12px;",
        span(style=paste0("font-size:18px;font-weight:",if(b)"800"else"600",
          ";color:",if(b)PRIMARY else FG,";font-family:",H,";"),m$name),
        if(b) span(class="badge",
          style=paste0("background:rgba(0,229,153,.13);color:",PRIMARY,";border:1px solid rgba(0,229,153,.3);"),
          "GANADOR") else NULL
      ),
      div(class="mc num",style=paste0("color:",if(b)PRIMARY else FG,";font-weight:",if(b)"800"else"600",";"),sprintf("%.4f",m$r2)),
      div(class="mc num",style=paste0("color:",MUTED,";"),sprintf("%.1f",m$rmse)),
      div(class="mc num",style=paste0("color:",MUTED,";"),sprintf("%.1f",m$mae)),
      div(class="mc num",style=paste0("color:",MUTED,";"),sprintf("%.4f",m$cv))
    )
  })
  div(class="pw",
    sHdr("04 — Modelos","Comparación de Algoritmos",
         "7 modelos · XGBoost · Random Forest · KNN · SVR · Ridge · Lasso"),
    div(class="dc", style="margin-bottom:16px;padding:32px;",
      tags$h4("Ranking Final", style=paste0("font-family:",H,";font-size:22px;color:",FG,";margin:0 0 24px 0;")),
      div(style=paste0("display:flex;gap:14px;padding:0 18px 18px;border-bottom:1px solid ",BORDER,";"),
        lapply(list(list("#","42px"),list("Modelo","flex:1"),
                    list("R²","110px"),list("RMSE","110px"),list("MAE","110px"),list("CV R²","110px")),
          function(h) div(h[[1]], style=paste0(
            "font-size:15px;font-weight:800;color:",MUTED,";letter-spacing:0.08em;",
            "text-transform:uppercase;font-family:",B,";",
            if(h[[2]]=="flex:1") "flex:1;" else paste0("width:",h[[2]],";"),
            "text-align:",if(h[[1]] %in% c("#","Modelo")) "left" else "right",";"
          ))
        )
      ),
      tagList(rows)
    ),
    div(style="display:grid;grid-template-columns:1fr 1fr;gap:16px;margin-bottom:16px;",
      div(class="dc", CW(plotlyOutput("m_r2",  height="340px")),
          modelNote("Lectura rápida","XGBoost y Random Forest mantienen los mejores R² y una validación cruzada muy cercana al test, lo que indica buena capacidad de generalización.")),
      div(class="dc", CW(plotlyOutput("m_err", height="340px")),
          modelNote("Lectura rápida","Los modelos ensemble reducen claramente el MAE y RMSE; Ridge y Lasso funcionan como baseline, pero dejan errores más altos."))
    ),
    div(class="dc", style="margin-bottom:16px;",
      CW(plotlyOutput("m_ov", height="340px")),
      modelNote("Diagnóstico","La distancia entre R² Train y R² Test permite detectar sobreajuste. Un delta pequeño en XGBoost sugiere un modelo fuerte sin sobreentrenamiento severo.", ACCENT)
    ),
    div(style="display:grid;grid-template-columns:1fr 1fr;gap:16px;",
      div(class="dc", CW(plotlyOutput("m_imp",   height="420px")),
          modelNote("Interpretación","La importancia de variables muestra qué señales usa más XGBoost. La hora del día domina el comportamiento del tráfico frente a variables climáticas aisladas.")),
      div(class="dc", CW(plotlyOutput("m_ridge", height="420px")),
          modelNote("Interpretación","Los coeficientes Ridge ayudan a explicar dirección e impacto lineal. Son útiles para interpretación, aunque su desempeño predictivo es menor que XGBoost."))
    )
  )
}

pageSimulador <- function() {
  ff <- function(lbl, ctrl, note="")
    div(style="margin-bottom:14px;",
        tags$label(class="flbl", lbl), ctrl,
        if(nchar(note)>0) div(class="fnote", note))
  div(class="pw",
    sHdr("04 — Prueba de Modelo","Simulador dinámico de traffic_volume",
         "Ingresa las features y obtén una predicción estimada con el modelo final."),
    div(style="display:flex;gap:16px;flex-wrap:wrap;",
      div(class="dc", style="flex:1.3;min-width:420px;",
        tags$h3("Variables de entrada",
                style=paste0("font-family:",H,";font-size:20px;color:",FG,";margin:0 0 18px 0;")),
        div(style="display:grid;grid-template-columns:1fr 1fr;gap:14px;",
          ff("Hora del día",
             sliderInput("p_h","",min=0,max=23,value=8,step=1,width="100%"),
             "0 – 23"),
          ff("Día de semana",
             selectInput("p_d","",choices=setNames(0:6,DOW_LABELS),selected=0,width="100%"),
             "Lunes=0, Domingo=6"),
          ff("Mes",
             selectInput("p_m","",choices=setNames(1:12,MON_LABELS),selected=5,width="100%")),
          ff("Festivo",
             selectInput("p_hol","",choices=c("No"=0,"Sí"=1),selected=0,width="100%"))
        ),
        tags$hr(style=paste0("border:none;border-top:1px solid ",BORDER,";margin:16px 0;")),
        tags$h4("Clima y ambiente",
                style=paste0("font-family:",H,";font-size:15px;color:",FG,";margin:0 0 14px 0;")),
        div(style="display:grid;grid-template-columns:1fr 1fr;gap:14px;",
          ff("Temperatura (K)",
             sliderInput("p_temp","",min=240,max=320,value=288,step=1,width="100%")),
          ff("Humedad (%)",
             sliderInput("p_hum","",min=0,max=100,value=70,step=1,width="100%")),
          ff("Velocidad viento",
             sliderInput("p_ws","",min=0,max=20,value=3,step=0.5,width="100%")),
          ff("Dirección viento (°)",
             sliderInput("p_wd","",min=0,max=360,value=320,step=5,width="100%")),
          ff("Visibilidad (millas)",
             sliderInput("p_vis","",min=0,max=10,value=5,step=0.5,width="100%")),
          ff("Índice contaminación",
             sliderInput("p_pol","",min=0,max=300,value=120,step=5,width="100%")),
          ff("Lluvia por hora",
             sliderInput("p_rai","",min=0,max=30,value=0,step=0.5,width="100%")),
          ff("Nieve por hora",
             sliderInput("p_sno","",min=0,max=30,value=0,step=0.5,width="100%")),
          ff("Nubosidad (%)",
             sliderInput("p_clo","",min=0,max=100,value=20,step=1,width="100%")),
          div(style="grid-column:1/-1;",
            ff("Tipo de clima",
               selectInput("p_wt","",choices=WEATHER_C,selected="Clear",width="100%")))
        )
      ),
      div(class="dc", style="flex:0.8;min-width:300px;position:sticky;top:78px;align-self:flex-start;",
        div(style=paste0("font-size:10px;font-weight:900;letter-spacing:0.14em;color:",PRIMARY,";text-transform:uppercase;margin-bottom:8px;font-family:",B,";"),
            "MODELO FINAL"),
        tags$h3("XGBoost Regressor",
                style=paste0("font-family:",H,";font-size:24px;color:",FG,";margin:0 0 18px 0;")),
        div(class="pval", textOutput("pred_v", inline=TRUE)),
        div(class="psub", "vehículos estimados"),
        uiOutput("pred_band"),
        CW(plotlyOutput("pred_gauge", height="270px")),
        div(style=paste0("font-size:13px;color:",MUTED,";line-height:1.6;margin-top:14px;font-family:",B,";"),
            textOutput("pred_txt"))
      )
    )
  )
}

pageInsights <- function()
  div(class="pw",
    sHdr("05 — Aplicaciones Reales","Del modelo al mundo real.",
         "Casos de uso industriales · Impacto urbano y tecnológico"),
    div(style="display:flex;gap:20px;flex-wrap:wrap;",
      # Card 1: Semáforos Inteligentes
      div(style=paste0(
        "background:radial-gradient(ellipse at top left,",PRIMARY,"14,transparent 55%),",
        "radial-gradient(ellipse at bottom right,",CYAN,"08,transparent 50%),",CARD,";",
        "border:1px solid ",BORDER,";border-radius:20px;padding:32px;",
        "border-top:3px solid ",PRIMARY,";position:relative;overflow:hidden;",
        "flex:1;min-width:340px;display:flex;gap:28px;align-items:flex-start;"
      ),
        div(style="flex:1;min-width:0;",
          tags$h2("Semáforos Inteligentes",
                  style=paste0("font-family:",H,";font-size:clamp(22px,2.8vw,32px);color:",FG,";",
                               "font-weight:900;margin:0 0 6px 0;text-shadow:0 0 40px ",PRIMARY,"55;")),
          tags$p("El modelo anticipa picos de congestión con minutos de anticipación, permitiendo que los semáforos reajusten sus ciclos automáticamente antes de que el colapso ocurra. Ciudades más fluidas, conductores menos estresados, aire más limpio.",
                 style=paste0("font-size:16px;color:",MUTED,";font-family:",B,";line-height:1.75;margin:0 0 24px 0;")),
          div(
            benefitRow("▲","Reducción significativa de la congestión vehicular", PRIMARY),
            benefitRow("⟳","Sincronización adaptativa en tiempo real entre intersecciones", PRIMARY),
            benefitRow("◗","Menos tiempo de espera para conductores y peatones", PRIMARY),
            benefitRow("◈","Menor emisión de CO₂ al reducir arranques y frenadas", PRIMARY)
          )
        ),
        div(style="width:190px;flex-shrink:0;padding-top:4px;",
          div(style="text-align:center;",
            tags$video(src="Semaforo.mp4", autoPlay=NA, muted=NA, loop=NA,
                       style=paste0("width:100%;border-radius:16px;display:block;",
                                    "box-shadow:0 0 32px ",PRIMARY,"44;border:2px solid ",PRIMARY,"33;")),
            div(style="display:flex;align-items:center;justify-content:center;margin-top:10px;",
              span(class="live-dot"),
              span("LIVE", style=paste0("font-size:10px;color:",PRIMARY,";font-family:",B,";font-weight:800;letter-spacing:0.14em;vertical-align:middle;"))
            )
          )
        )
      ),
      # Card 2: Navegación Predictiva
      div(style=paste0(
        "background:radial-gradient(ellipse at top right,",CYAN,"12,transparent 55%),",
        "radial-gradient(ellipse at bottom left,",PRIMARY,"06,transparent 50%),",CARD,";",
        "border:1px solid ",BORDER,";border-radius:20px;padding:32px;",
        "border-top:3px solid ",CYAN,";position:relative;overflow:hidden;",
        "flex:1;min-width:340px;display:flex;gap:28px;align-items:flex-start;"
      ),
        div(style="flex:1;min-width:0;",
          tags$h2("Google Maps · Waze · Uber",
                  style=paste0("font-family:",H,";font-size:clamp(22px,2.8vw,32px);color:",FG,";",
                               "font-weight:900;margin:0 0 6px 0;text-shadow:0 0 40px ",CYAN,"55;")),
          tags$p("Integrar el modelo con plataformas de navegación permite ofrecer rutas que se adaptan a condiciones futuras: lluvia, hora pico, festivos. El sistema no reacciona al tráfico, lo anticipa, reduciendo fricciones urbanas a escala masiva.",
                 style=paste0("font-size:16px;color:",MUTED,";font-family:",B,";line-height:1.75;margin:0 0 24px 0;")),
          div(
            benefitRow("⇢","Rutas sugeridas basadas en tráfico futuro predicho", CYAN),
            benefitRow("◎","Anticipación de picos antes de que se formen", CYAN),
            benefitRow("◗","ETAs más precisos con variables meteorológicas incluidas", CYAN),
            benefitRow("⚑","Alertas tempranas a conductores y operadores de flota", CYAN),
            benefitRow("⊕","Optimización de asignación de conductores en plataformas", CYAN)
          )
        ),
        div(style="width:190px;flex-shrink:0;padding-top:4px;",
          div(style=paste0(
            "position:relative;height:200px;background:rgba(6,12,20,0.55);",
            "border-radius:14px;border:1px solid ",BORDER,";overflow:hidden;margin-bottom:10px;"
          ),
            tagList(lapply(c(20,40,60,80), function(y)
              div(style=paste0("position:absolute;left:0;right:0;height:1px;background:",BORDER,";top:",y,"%;opacity:0.6;"))
            )),
            tagList(lapply(c(25,50,75), function(x)
              div(style=paste0("position:absolute;top:0;bottom:0;width:1px;background:",BORDER,";left:",x,"%;opacity:0.6;"))
            )),
            div(style=paste0("position:absolute;left:5%;top:20%;width:70%;height:2px;background:linear-gradient(90deg,",CYAN,",",CYAN,"11);border-radius:999px;box-shadow:0 0 10px ",CYAN,";animation:routeGlow 2s ease-in-out infinite;")),
            div(style=paste0("position:absolute;left:25%;top:20%;width:2px;height:40%;background:linear-gradient(180deg,",CYAN,",",PRIMARY,");border-radius:999px;box-shadow:0 0 10px ",CYAN,";animation:routeGlow 2.4s ease-in-out infinite;animation-delay:0.5s;")),
            div(style=paste0("position:absolute;left:5%;top:60%;width:95%;height:2px;background:linear-gradient(90deg,",PRIMARY,",",PRIMARY,"11);border-radius:999px;box-shadow:0 0 10px ",PRIMARY,";animation:routeGlow 2.8s ease-in-out infinite;animation-delay:1s;")),
            div(style=paste0("position:absolute;left:75%;top:20%;width:2px;height:65%;background:linear-gradient(180deg,",CYAN,"55,",ACCENT,");border-radius:999px;box-shadow:0 0 8px ",ACCENT,"88;animation:routeGlow 3s ease-in-out infinite;animation-delay:1.4s;")),
            gpsDiv("4%","18%", CYAN, "12px"),
            gpsDiv("73%","18%", PRIMARY, "10px"),
            gpsDiv("24%","58%", ACCENT, "11px"),
            gpsDiv("93%","83%", CYAN, "10px"),
            gpsDiv("25%","18%", PRIMARY, "9px"),
            gpsDiv("4%","58%", CYAN, "9px"),
            div(style=paste0(
              "position:absolute;right:6px;top:6px;background:rgba(6,12,20,0.92);",
              "border-radius:9px;padding:6px 10px;border:1px solid ",ACCENT,"55;text-align:center;"
            ),
              div("Demanda", style=paste0("font-size:8px;color:",MUTED,";font-family:",B,";font-weight:700;text-transform:uppercase;letter-spacing:0.08em;")),
              div("Alta", style=paste0("font-size:15px;color:",ACCENT,";font-family:",H,";font-weight:900;line-height:1;"))
            )
          ),
          div("Mapa predictivo urbano en tiempo real",
              style=paste0("font-size:10px;color:",MUTED,";font-family:",B,
                           ";text-align:center;letter-spacing:0.06em;text-transform:uppercase;"))
        )
      )
    )
  )
