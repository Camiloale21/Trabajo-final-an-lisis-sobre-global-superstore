# =============================================================================
#  PROYECTO FINAL - ANALÍTICA DE DATOS
#  ¿Cuánto erosiona el descuento la rentabilidad? Evidencia de Global Superstore
#  ---------------------------------------------------------------------------
#  Script único y reproducible. Ejecutar de principio a fin desde RStudio
#  abriendo el proyecto (global-superstore-descuentos.Rproj) y pulsando
#  "Source" (Ctrl/Cmd + Shift + S).
#
#  Entradas : data/raw/Global Superstore Retail Dataset.csv
#  Salidas  : output/tablas/*.csv   (tablas estadísticas y econométricas)
#             output/figuras/*.png  (gráficas del informe)
#             output/modelos.txt    (resúmenes completos de los modelos)
# =============================================================================

# -----------------------------------------------------------------------------
# 0. CONFIGURACIÓN
# -----------------------------------------------------------------------------
paquetes <- c("readr", "dplyr", "tidyr", "ggplot2", "scales",
              "sandwich", "lmtest", "car")
faltan <- paquetes[!paquetes %in% rownames(installed.packages())]
if (length(faltan) > 0) install.packages(faltan)
invisible(lapply(paquetes, library, character.only = TRUE))

options(scipen = 999, digits = 6)
set.seed(2026)                                   # reproducibilidad (muestreo de la fig. 8)

dir.create("output/tablas",  recursive = TRUE, showWarnings = FALSE)
dir.create("output/figuras", recursive = TRUE, showWarnings = FALSE)

FUENTE <- "Fuente: elaboración propia con datos de Global Superstore (Kaggle), 2011-2014."
AZUL <- "#2a78d6"; NARANJA <- "#eb6834"; GRIS <- "#52514e"
tema <- theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 11),
        plot.caption = element_text(colour = GRIS, size = 8, hjust = 0),
        panel.grid.minor = element_blank())
guardar <- function(g, archivo, w = 7, h = 4.2) {
  ggsave(file.path("output/figuras", archivo), g, width = w, height = h, dpi = 150, bg = "white")
}
guardar_tabla <- function(x, archivo) write.csv(x, file.path("output/tablas", archivo), row.names = TRUE)

# -----------------------------------------------------------------------------
# 1. IMPORTACIÓN Y DIAGNÓSTICO DE LA BASE
# -----------------------------------------------------------------------------
raw <- read_csv("data/raw/Global Superstore Retail Dataset.csv",
                show_col_types = FALSE, locale = locale(encoding = "UTF-8"))

cat("Dimensiones:", dim(raw), "\n")                       # 51.290 x 27
glimpse(raw)
cat("Valores faltantes totales:", sum(is.na(raw)), "\n")    # 0
cat("Filas duplicadas:", sum(duplicated(raw)), "\n")        # 0
cat("Row.ID duplicados:", sum(duplicated(raw$Row.ID)), "\n")
cat("Líneas con Ventas = 0:", sum(raw$Sales == 0), "\n")    # 1

# La columna '记录数' (\"número de registros\" en chino) vale 1 en todas las filas:
# es un contador de Tableau sin información. 'Market2' agrega US + Canadá en
# 'North America'; se usa 'Market' (7 mercados) por ser más desagregada.
print(table(raw[[8]]))
print(table(raw$Market, raw$Market2))

# -----------------------------------------------------------------------------
# 2. LIMPIEZA Y TRANSFORMACIÓN
# -----------------------------------------------------------------------------
# Se elimina UNA sola línea (Ventas = 0) porque el margen Profit/Sales no está
# definido. No se eliminan atípicos: son transacciones reales (pedidos grandes
# o con descuentos altos) y precisamente explican el fenómeno estudiado.
df <- raw %>%
  select(-8) %>%                                         # contador constante
  filter(Sales > 0) %>%
  mutate(
    Order.Date   = as.Date(substr(Order.Date, 1, 10)),
    Ship.Date    = as.Date(substr(Ship.Date, 1, 10)),
    dias_envio   = as.numeric(Ship.Date - Order.Date),
    margen       = Profit / Sales,                       # variable dependiente
    precio_lista = Sales / (Quantity * (1 - Discount)),  # precio unitario antes de descuento
    log_precio   = log(precio_lista),
    perdida      = as.integer(Profit < 0),
    tramo_desc   = cut(Discount, c(-0.001, 0, 0.1, 0.2, 0.3, 0.5, 0.85),
                       labels = c("0%", "0-10%", "10-20%", "20-30%", "30-50%", ">50%")),
    # Niveles de referencia explícitos (evita depender del orden alfabético/locale)
    Category       = relevel(factor(Category), ref = "Office Supplies"),
    Segment        = relevel(factor(Segment), ref = "Consumer"),
    Market         = relevel(factor(Market), ref = "US"),
    Order.Priority = relevel(factor(Order.Priority), ref = "Medium"),
    Ship.Mode      = relevel(factor(Ship.Mode), ref = "Standard Class"),
    Year           = relevel(factor(Year), ref = "2011")
  ) %>%
  arrange(Order.Date, Row.ID)                            # orden temporal (para Durbin-Watson)

cat("Observaciones para el análisis:", nrow(df), "\n")   # 51.289
cat("Inconsistencias de fechas (envío antes del pedido):", sum(df$dias_envio < 0), "\n")

# -----------------------------------------------------------------------------
# 3. ANÁLISIS EXPLORATORIO
# -----------------------------------------------------------------------------
# 3.1 Estadística descriptiva de variables numéricas
asim <- function(x) { m <- mean(x); mean((x - m)^3) / mean((x - m)^2)^1.5 }
curt <- function(x) { m <- mean(x); mean((x - m)^4) / mean((x - m)^2)^2 }  # curtosis (normal = 3)
vars_num <- c("Sales", "Profit", "Discount", "Quantity", "Shipping.Cost", "margen", "dias_envio")
descriptivos <- t(sapply(df[vars_num], function(x) {
  q <- quantile(x, c(.25, .75)); iqr <- q[2] - q[1]
  c(media = mean(x), mediana = median(x), desv_est = sd(x), min = min(x), max = max(x),
    cv = sd(x) / abs(mean(x)), asimetria = asim(x), curtosis = curt(x),
    q1 = unname(q[1]), q3 = unname(q[2]),
    atipicos_IQR = sum(x < q[1] - 1.5 * iqr | x > q[2] + 1.5 * iqr))
}))
print(round(descriptivos, 4)); guardar_tabla(round(descriptivos, 4), "t1_descriptivos.csv")

# 3.2 Tablas de rentabilidad por dimensión de negocio
resumen <- function(data, var) {
  data %>% group_by({{ var }}) %>%
    summarise(lineas = n(), ventas = sum(Sales), utilidad = sum(Profit),
              margen_agregado = sum(Profit) / sum(Sales), margen_medio = mean(margen),
              desc_medio = mean(Discount), pct_perdida = mean(perdida), .groups = "drop")
}
t_tramo <- resumen(df, tramo_desc);   print(t_tramo);   guardar_tabla(t_tramo, "t2_tramo_descuento.csv")
t_cat   <- resumen(df, Category);     print(t_cat);     guardar_tabla(t_cat, "t3_categoria.csv")
t_subc  <- resumen(df, Sub.Category); print(t_subc);    guardar_tabla(t_subc, "t4_subcategoria.csv")
t_mkt   <- resumen(df, Market);       print(t_mkt);     guardar_tabla(t_mkt, "t5_mercado.csv")
t_seg   <- resumen(df, Segment);      print(t_seg);     guardar_tabla(t_seg, "t6_segmento.csv")
t_year  <- resumen(df, Year);         print(t_year);    guardar_tabla(t_year, "t7_anio.csv")
cat("Margen agregado total:", sum(df$Profit) / sum(df$Sales), "\n")
cat("% de líneas con pérdida:", mean(df$perdida), "\n")
cat("% de líneas con descuento:", mean(df$Discount > 0), "\n")

# 3.3 Correlaciones
cor_p <- cor(df[vars_num]); cor_s <- cor(df[vars_num], method = "spearman")
print(round(cor_p, 3)); print(round(cor_s, 3))
guardar_tabla(round(cor_p, 3), "t8_corr_pearson.csv"); guardar_tabla(round(cor_s, 3), "t9_corr_spearman.csv")

# 3.4 Pruebas bivariadas
#  H0: el margen medio es igual en líneas con y sin descuento (t de Welch)
print(t.test(margen ~ I(Discount > 0), data = df, var.equal = FALSE))
#  H0: la distribución del margen es igual entre categorías (Kruskal-Wallis)
print(kruskal.test(margen ~ Category, data = df))

# 3.5 Gráficas exploratorias
g1 <- ggplot(df, aes(margen)) + geom_histogram(bins = 80, fill = AZUL, colour = "white", linewidth = .2) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = GRIS) +
  labs(title = "Distribución del margen de utilidad por línea de pedido",
       x = "Margen (Utilidad / Ventas)", y = "Número de líneas", caption = FUENTE) + tema
guardar(g1, "fig1_hist_margen.png")

g2 <- df %>% transmute(`Ventas (USD)` = Sales, `log(Ventas)` = log(Sales)) %>%
  pivot_longer(everything()) %>%
  mutate(name = factor(name, levels = c("Ventas (USD)", "log(Ventas)"))) %>%
  ggplot(aes(value)) + geom_histogram(bins = 70, fill = AZUL, colour = "white", linewidth = .2) +
  facet_wrap(~name, scales = "free") +
  labs(title = "Ventas en niveles y en logaritmos", x = NULL, y = "Número de líneas", caption = FUENTE) + tema
guardar(g2, "fig2_ventas_log.png", w = 9, h = 3.8)

g3 <- ggplot(t_tramo, aes(tramo_desc, margen_agregado, fill = margen_agregado >= 0)) +
  geom_col(width = .6) + geom_hline(yintercept = 0, colour = GRIS) +
  geom_text(aes(label = percent(margen_agregado, .1), vjust = ifelse(margen_agregado >= 0, -.4, 1.3)), size = 3.2) +
  scale_fill_manual(values = c(`TRUE` = AZUL, `FALSE` = NARANJA), guide = "none") +
  scale_y_continuous(labels = percent) +
  labs(title = "Margen agregado según tramo de descuento", x = "Tramo de descuento",
       y = "Margen agregado", caption = FUENTE) + tema
guardar(g3, "fig3_margen_tramo.png")

g5a <- ggplot(t_cat, aes(reorder(Category, utilidad), utilidad / 1e3)) + geom_col(fill = AZUL, width = .55) +
  coord_flip() + labs(title = "Utilidad total por categoría (miles USD)", x = NULL, y = NULL) + tema
g5b <- ggplot(t_mkt, aes(reorder(Market, margen_agregado), margen_agregado, fill = margen_agregado >= 0)) +
  geom_col(width = .55) + coord_flip() + scale_y_continuous(labels = percent) +
  scale_fill_manual(values = c(`TRUE` = AZUL, `FALSE` = NARANJA), guide = "none") +
  labs(title = "Margen agregado por mercado", x = NULL, y = NULL, caption = FUENTE) + tema
guardar(g5a, "fig5a_utilidad_categoria.png", w = 5, h = 3.5)
guardar(g5b, "fig5b_margen_mercado.png", w = 5, h = 3.5)

etq <- c("Ventas", "Utilidad", "Descuento", "Cantidad", "Costo envío", "Margen", "Días envío")
cm <- as.data.frame(as.table(cor_p)); levels(cm$Var1) <- etq; levels(cm$Var2) <- etq
g6 <- ggplot(cm, aes(Var2, Var1, fill = Freq)) + geom_tile(colour = "white") +
  geom_text(aes(label = sprintf("%.2f", Freq)), size = 3) +
  scale_fill_distiller(palette = "RdBu", limits = c(-1, 1), direction = 1, name = "r") +
  labs(title = "Matriz de correlaciones de Pearson", x = NULL, y = NULL, caption = FUENTE) +
  tema + theme(axis.text.x = element_text(angle = 45, hjust = 1))
guardar(g6, "fig6_correlaciones.png", w = 6.5, h = 5.2)

g7 <- t_year %>% transmute(Year = as.numeric(as.character(Year)),
                           `Ventas (millones USD)` = ventas / 1e6, `Margen agregado (%)` = margen_agregado * 100) %>%
  pivot_longer(-Year) %>%
  ggplot(aes(Year, value)) + geom_line(colour = AZUL, linewidth = 1) + geom_point(colour = AZUL, size = 2) +
  facet_wrap(~name, scales = "free_y") + expand_limits(y = 0) +
  labs(title = "Evolución anual de ventas y margen", x = NULL, y = NULL, caption = FUENTE) + tema
guardar(g7, "fig7_tendencia.png", w = 9, h = 3.6)

# -----------------------------------------------------------------------------
# 4. MODELOS ECONOMÉTRICOS (MCO)
# -----------------------------------------------------------------------------
#  Variable dependiente: margen = Profit / Sales
#  M1: regresión lineal simple          margen ~ Discount
#  M2: regresión múltiple lineal        + controles (precio, cantidad, categoría,
#                                        segmento, mercado, prioridad, envío, año)
#  M3: M2 + término cuadrático          (no linealidad del descuento)  <- MODELO PRINCIPAL
#  M4: M3 + interacción Discount x Category (heterogeneidad por categoría)
#  M5: especificación teórica            margen = a + b * 1/(1 - Discount) + controles
#  MB: modelo de volumen                 log(Quantity) ~ Discount + controles
controles <- "log_precio + Quantity + Category + Segment + Market + Order.Priority + Ship.Mode + Year"
f2 <- as.formula(paste("margen ~ Discount +", controles))
f3 <- as.formula(paste("margen ~ Discount + I(Discount^2) +", controles))
f4 <- as.formula(paste("margen ~ Discount + I(Discount^2) +", controles, "+ Discount:Category"))
f5 <- as.formula(paste("margen ~ I(1/(1 - Discount)) +", controles))
fB <- as.formula("log(Quantity) ~ Discount + log_precio + Category + Segment + Market + Order.Priority + Ship.Mode + Year")

m1 <- lm(margen ~ Discount, data = df)
m2 <- lm(f2, data = df)
m3 <- lm(f3, data = df)
m4 <- lm(f4, data = df)
m5 <- lm(f5, data = df)
mB <- lm(fB, data = df)
modelos <- list(M1 = m1, M2 = m2, M3 = m3, M4 = m4, M5 = m5, MB = mB)

# Errores estándar robustos a heterocedasticidad (HC1, White) para toda inferencia
robusto <- function(m) coeftest(m, vcov = vcovHC(m, type = "HC1"))
ic_robusto <- function(m) coefci(m, vcov. = vcovHC(m, type = "HC1"), level = 0.95)

sink("output/modelos.txt")
for (n in names(modelos)) {
  cat("\n\n==================== ", n, " ====================\n")
  print(summary(modelos[[n]]))                  # MCO clásico (R2, R2 ajustado, prueba F global)
  cat("\n--- Coeficientes con errores robustos HC1 ---\n"); print(robusto(modelos[[n]]))
  cat("\n--- Intervalos de confianza 95% (HC1) ---\n");  print(ic_robusto(modelos[[n]]))
}
sink()

tabla_coef <- function(m) {
  ct <- robusto(m); ic <- ic_robusto(m)
  data.frame(coef = ct[, 1], ee_HC1 = ct[, 2], t = ct[, 3], p = ct[, 4], ic_inf = ic[, 1], ic_sup = ic[, 2])
}
for (n in names(modelos)) guardar_tabla(round(tabla_coef(modelos[[n]]), 5), paste0("coef_", n, ".csv"))

# Bondad de ajuste comparada
ajuste <- t(sapply(modelos, function(m) {
  s <- summary(m)
  c(n = nobs(m), k = length(coef(m)), R2 = s$r.squared, R2_aj = s$adj.r.squared,
    F = unname(s$fstatistic[1]), sigma = s$sigma, AIC = AIC(m))
}))
print(round(ajuste, 4)); guardar_tabla(round(ajuste, 4), "t10_ajuste_modelos.csv")

# H1: significancia conjunta del descuento en M3 (beta1 = beta2 = 0), Wald robusto
print(linearHypothesis(m3, c("Discount = 0", "I(Discount^2) = 0"), vcov. = vcovHC(m3, type = "HC1")))
# H2: prueba F (robusta) de que el término cuadrático mejora el modelo: M2 vs M3
print(waldtest(m2, m3, vcov = vcovHC(m3, type = "HC1"), test = "F"))
# Significancia conjunta de cada grupo de variables categóricas en M3 (Wald robusto)
print(Anova(m3, type = "II", vcov. = vcovHC(m3, type = "HC1")))
# ¿Difiere el efecto del descuento entre categorías? (M4)
print(linearHypothesis(m4, c("Discount:CategoryFurniture = 0", "Discount:CategoryTechnology = 0"),
                       vcov. = vcovHC(m4, type = "HC1")))

# -----------------------------------------------------------------------------
# 5. PRUEBAS DE DIAGNÓSTICO Y VALIDACIÓN
# -----------------------------------------------------------------------------
jarque_bera <- function(e) {                          # H0: residuos normales
  n <- length(e); s <- asim(e); k <- curt(e)
  jb <- n / 6 * (s^2 + (k - 3)^2 / 4); c(JB = jb, p = pchisq(jb, 2, lower.tail = FALSE), asim = s, curt = k)
}
diagnostico <- t(sapply(modelos[c("M1", "M2", "M3", "M5", "MB")], function(m) {
  bp <- bptest(m)                                     # Breusch-Pagan (Koenker). H0: homocedasticidad
  rs <- resettest(m, power = 2:3, type = "fitted")    # Ramsey RESET. H0: forma funcional correcta
  dw <- dwtest(m)                                     # Durbin-Watson. H0: sin autocorrelación
  jb <- jarque_bera(residuals(m))
  c(BP = unname(bp$statistic), BP_p = bp$p.value, RESET = unname(rs$statistic), RESET_p = rs$p.value,
    jb, DW = unname(dw$statistic))
}))
print(round(diagnostico, 4)); guardar_tabla(round(diagnostico, 4), "t11_diagnostico.csv")

# Multicolinealidad: GVIF (car::vif). Regla: GVIF^(1/(2Df)) > sqrt(10) ~ 3.16 indica problema
vif_m2 <- vif(m2); print(vif_m2); guardar_tabla(round(vif_m2, 3), "t12_vif_M2.csv")

# Robustez de la inferencia: errores agrupados por pedido (líneas del mismo pedido)
print(coeftest(m3, vcov = vcovCL(m3, cluster = ~Order.ID, type = "HC1"))[1:3, ])

# Robustez de la especificación
lim <- quantile(df$margen, c(.01, .99))
rob <- list(`Base (M3)` = m3,
            `Ventas >= 5 USD` = lm(f3, data = filter(df, Sales >= 5)),
            `Margen winsorizado 1%-99%` = lm(update(f3, pmin(pmax(margen, lim[1]), lim[2]) ~ .), data = df))
t_rob <- t(sapply(rob, function(m) {
  ct <- robusto(m); c(n = nobs(m), b_desc = ct[2, 1], p_desc = ct[2, 4], b_desc2 = ct[3, 1], p_desc2 = ct[3, 4],
                      R2_aj = summary(m)$adj.r.squared)
}))
print(round(t_rob, 5)); guardar_tabla(round(t_rob, 5), "t13_robustez.csv")

# Gráfica de residuos del modelo principal
res <- data.frame(ajustado = fitted(m3), residuo = residuals(m3))
g8a <- ggplot(res[sample(nrow(res), 8000), ], aes(ajustado, residuo)) +
  geom_point(alpha = .3, size = .6, colour = AZUL) + geom_hline(yintercept = 0, colour = GRIS) +
  labs(title = "Residuos vs. ajustados (M3, muestra de 8.000)", x = "Valores ajustados", y = "Residuos") + tema
g8b <- ggplot(res, aes(sample = residuo)) + stat_qq(size = .5, colour = AZUL) + stat_qq_line(colour = NARANJA) +
  labs(title = "Gráfico Q-Q normal de los residuos (M3)", x = "Cuantiles teóricos",
       y = "Cuantiles de los residuos", caption = FUENTE) + tema
guardar(g8a, "fig8a_residuos.png", w = 5, h = 4); guardar(g8b, "fig8b_qq.png", w = 5, h = 4)

# -----------------------------------------------------------------------------
# 6. RESULTADOS DERIVADOS PARA LA DECISIÓN EMPRESARIAL
# -----------------------------------------------------------------------------
b  <- coef(m3); V <- vcovHC(m3, type = "HC1")
X3 <- model.matrix(m3)
otros <- setdiff(colnames(X3), c("Discount", "I(Discount^2)"))

# 6.1 Efecto marginal del descuento: d E[margen] / d Discount = b1 + 2*b2*d
efecto_marginal <- t(sapply(c(0, .1, .2, .3, .4, .5, .6, .7), function(d) {
  g <- c(1, 2 * d); idx <- c("Discount", "I(Discount^2)")
  c(descuento = d, efecto = sum(g * b[idx]), ee = sqrt(drop(t(g) %*% V[idx, idx] %*% g)))
}))
print(round(efecto_marginal, 4)); guardar_tabla(round(efecto_marginal, 4), "t14_efecto_marginal.csv")

# 6.2 Descuento de equilibrio (margen predicho = 0), resto de variables en sus medias.
#     Raíz de b2*d^2 + b1*d + a = 0; error estándar por método delta (gradiente numérico).
equilibrio <- function(beta, xbar) {
  a <- sum(xbar * beta[otros]); b1 <- beta["Discount"]; b2 <- beta["I(Discount^2)"]
  unname((-b1 - sqrt(b1^2 - 4 * b2 * a)) / (2 * b2))
}
equilibrio_delta <- function(xbar) {
  est <- equilibrio(b, xbar); h <- 1e-6
  grad <- sapply(seq_along(b), function(j) { bp <- b; bp[j] <- bp[j] + h; (equilibrio(bp, xbar) - est) / h })
  ee <- sqrt(drop(t(grad) %*% V %*% grad)); c(d_equilibrio = est, ee = ee, ic_inf = est - 1.96 * ee, ic_sup = est + 1.96 * ee)
}
xbar0 <- colMeans(X3[, otros])
perfil <- function(var, nivel, ref) {                       # fija un mercado/categoría, resto en medias
  x <- xbar0; cols <- grep(paste0("^", var), otros, value = TRUE); x[cols] <- 0
  if (nivel != ref) x[paste0(var, nivel)] <- 1; x
}
t_equil <- rbind(
  `Promedio global` = equilibrio_delta(xbar0),
  t(sapply(c("US", "APAC", "Africa", "Canada", "EMEA", "EU", "LATAM"),
           function(k) equilibrio_delta(perfil("Market", k, "US")))),
  t(sapply(c("Office Supplies", "Furniture", "Technology"),
           function(k) equilibrio_delta(perfil("Category", k, "Office Supplies")))))
print(round(t_equil, 4)); guardar_tabla(round(t_equil, 4), "t15_descuento_equilibrio.csv")

# 6.3 Gráfica: datos observados y curvas estimadas
obs <- df %>% group_by(Discount) %>% summarise(m = mean(margen), s = sd(margen), n = n()) %>% filter(n >= 30)
curva <- data.frame(d = seq(0, .8, length.out = 200)) %>%
  mutate(M1 = coef(m1)[1] + coef(m1)[2] * d,
         M3 = sum(xbar0 * b[otros]) + b["Discount"] * d + b["I(Discount^2)"] * d^2) %>%
  pivot_longer(-d, names_to = "modelo")
g4 <- ggplot() +
  geom_pointrange(data = obs, aes(Discount, m, ymin = m - 1.96 * s / sqrt(n), ymax = m + 1.96 * s / sqrt(n)),
                  colour = GRIS, size = .25) +
  geom_line(data = curva, aes(d, value, colour = modelo), linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = GRIS) +
  scale_colour_manual(values = c(M1 = NARANJA, M3 = AZUL),
                      labels = c(M1 = "M1: recta MCO simple", M3 = "M3: cuadrático (controles en medias)"), name = NULL) +
  labs(title = "Margen frente a descuento: datos y modelos estimados",
       x = "Descuento", y = "Margen", caption = FUENTE) + tema + theme(legend.position = "top")
guardar(g4, "fig4_margen_descuento.png")

eq_m <- as.data.frame(t_equil[c("US", "APAC", "Africa", "Canada", "EMEA", "EU", "LATAM"), ]); eq_m$mercado <- rownames(eq_m)
g9 <- ggplot(eq_m, aes(d_equilibrio, reorder(mercado, d_equilibrio))) +
  geom_errorbarh(aes(xmin = ic_inf, xmax = ic_sup), height = .2, colour = AZUL) + geom_point(colour = AZUL, size = 2.5) +
  geom_text(aes(label = percent(d_equilibrio, .1)), vjust = -1, size = 3) + scale_x_continuous(labels = percent) +
  labs(title = "Descuento máximo antes de pérdidas, por mercado (M3, IC 95%)",
       x = "Descuento de equilibrio (margen predicho = 0)", y = NULL, caption = FUENTE) + tema
guardar(g9, "fig9_equilibrio.png")

cat("\nAnálisis completado. Revise la carpeta output/.\n")
sessionInfo()
