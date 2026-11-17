library(tidyverse)
library(ggplot2)
library(gridExtra)

# ==========================================================
# DATOS
# ==========================================================

datos <- tibble(
  lab = c(7,7,5,5,9,9,3,3,2,2,6,6,1,1),
  
  envase = c(
    "120","132","138","145",
    "126","158","125","136",
    "130","151","118","137",
    "123","157"
  ),
  
  r1 = c(
    1.25,0.98,
    1.21,1.22,
    1.30,1.31,
    1.02,0.93,
    1.01,1.04,
    0.72,0.83,
    1.55,1.43
  ),
  
  r2 = c(
    1.26,1.00,
    1.22,1.30,
    1.31,1.26,
    0.95,1.15,
    1.06,1.09,
    0.72,0.87,
    1.55,1.48
  ),
  
  r3 = c(
    1.25,0.97,
    1.28,1.35,
    1.20,1.28,
    1.08,1.00,
    1.16,0.99,
    0.71,0.86,
    1.55,1.53
  )
)

# ==========================================================
# h OBSERVADO
# ==========================================================

datos$media <- rowMeans(
  datos[,c("r1","r2","r3")]
)

media_global <- mean(
  datos$media
)

sx <- sd(
  datos$media
)

datos$h <- (
  datos$media -
    media_global
)/sx

# ==========================================================
# k OBSERVADO
# ==========================================================

datos$sd_lab <- apply(
  datos[,c("r1","r2","r3")],
  1,
  sd
)

sr <- sqrt(
  mean(
    datos$sd_lab^2
  )
)

datos$k <- datos$sd_lab/sr

# ==========================================================
# BOOTSTRAP 
# ==========================================================

todos <- as.numeric(
  unlist(
    datos[,c("r1","r2","r3")]
  )
)

# ----------------------------------------------------------
# DETECCION DE EXTREMOS
# ----------------------------------------------------------

outliers <- boxplot.stats(
  todos
)$out

cat("\n")
cat("=====================================\n")
cat("OBSERVACIONES EXTREMAS DETECTADAS\n")
cat("=====================================\n")

print(outliers)

cat(
  "\nNumero de extremos:",
  length(outliers),
  "\n"
)

# ----------------------------------------------------------
# BOXPLOT
# ----------------------------------------------------------

boxplot(
  todos,
  col = "#A8DADC",
  main = "Detección de observaciones extremas"
)

# ----------------------------------------------------------
# ELIMINACION DE EXTREMOS
# ----------------------------------------------------------

todos_rob <- todos[
  !(todos %in% outliers)
]

cat(
  "\nObservaciones originales:",
  length(todos)
)

cat(
  "\nObservaciones robustas:",
  length(todos_rob),
  "\n"
)

# ==========================================================
# BOOTSTRAP
# ==========================================================

set.seed(123)

B <- 5000

L <- 14
n <- 3

h_boot <- c()
k_boot <- c()

for(b in 1:B){
  
  muestra_boot <- sample(
    todos_rob,
    size = L*n,
    replace = TRUE
  )
  
  datos_boot <- matrix(
    muestra_boot,
    nrow = L,
    byrow = TRUE
  )
  
  medias_boot <- rowMeans(
    datos_boot
  )
  
  h_aux <- (
    medias_boot -
      mean(medias_boot)
  ) /
    sd(medias_boot)
  
  sd_boot <- apply(
    datos_boot,
    1,
    sd
  )
  
  sr_boot <- sqrt(
    mean(
      sd_boot^2
    )
  )
  
  k_aux <- sd_boot/sr_boot
  
  h_boot <- c(
    h_boot,
    h_aux
  )
  
  k_boot <- c(
    k_boot,
    k_aux
  )
  
}

# ==========================================================
# VALORES CRITICOS
# ==========================================================

alpha <- 0.05

h_inf <- quantile(
  h_boot,
  probs = 0.025
)

h_sup <- quantile(
  h_boot,
  probs = 0.975
)

k_sup <- quantile(
  k_boot,
  probs = 0.95
)

# ==========================================================
# CLASIFICACION
# ==========================================================

datos <- datos %>%
  mutate(
    
    h_outlier =
      h < h_inf |
      h > h_sup,
    
    k_outlier =
      k > k_sup
    
  )

# ==========================================================
# RESULTADOS
# ==========================================================

cat("\n")
cat("=====================================\n")
cat("RESULTADOS BOOTSTRAP ROBUSTO A\n")
cat("=====================================\n\n")

cat(
  "Media global =",
  round(media_global,4),
  "\n"
)

cat(
  "Sx =",
  round(sx,4),
  "\n"
)

cat(
  "sr =",
  round(sr,4),
  "\n\n"
)

cat(
  "h inferior =",
  round(h_inf,4),
  "\n"
)

cat(
  "h superior =",
  round(h_sup,4),
  "\n"
)

cat(
  "k superior =",
  round(k_sup,4),
  "\n\n"
)

print(
  datos %>%
    select(
      lab,
      envase,
      media,
      h,
      k,
      h_outlier,
      k_outlier
    )
)


#==========================================================
# PALETA 
#==========================================================

col_fondo   <- "#FAFAE3"
col_claro   <- "#E7E5CE"
col_medio   <- "#D9D1BA"
col_cacao   <- "#C8B99A"
col_oscuro  <- "#A89372"
col_linea   <- "#D98E04"

#  "#6F614A"

#==========================================================
# DISTRIBUCION BOOTSTRAP h
#==========================================================

g1 <- ggplot(
  data.frame(h = h_boot),
  aes(h)
)+
  geom_histogram(
    bins = 60,
    fill = col_cacao,
    colour = "white"
  )+
  geom_vline(
    xintercept = c(h_inf,h_sup),
    colour = col_linea,
    linewidth = 1
  )+
  labs(
    title = expression("Distribución bootstrap de " * italic(h)),
    x = "h",
    y = "Frecuencia"
  )+
  theme_minimal(base_size = 13)+
  theme(
    plot.background  = element_rect(fill = col_fondo,color = NA),
    panel.background = element_rect(fill = col_fondo,color = NA),
    plot.title = element_text(face = "bold")
  )

#==========================================================
# DISTRIBUCION BOOTSTRAP k
#==========================================================

g2 <- ggplot(
  data.frame(k = k_boot),
  aes(k)
)+
  geom_histogram(
    bins = 60,
    fill = col_oscuro,
    colour = "white"
  )+
  geom_vline(
    xintercept = k_sup,
    colour = col_linea,
    linewidth = 1
  )+
  labs(
    title = expression("Distribución bootstrap de " * italic(k)),
    x = "k",
    y = "Frecuencia"
  )+
  theme_minimal(base_size = 13)+
  theme(
    plot.background  = element_rect(fill = col_fondo,color = NA),
    panel.background = element_rect(fill = col_fondo,color = NA),
    plot.title = element_text(face = "bold")
  )

#==========================================================
# MANDEL h
#==========================================================

g3 <- ggplot(
  datos,
  aes(
    x = paste(lab,envase),
    y = h,
    fill = h_outlier
  )
)+
  geom_col(width = 0.8)+
  geom_hline(
    yintercept = h_inf,
    colour = col_linea,
    linewidth = 1
  )+
  geom_hline(
    yintercept = h_sup,
    colour = col_linea,
    linewidth = 1
  )+
  scale_fill_manual(
    values = c(
      "FALSE" = col_cacao,
      "TRUE"  = col_linea
    )
  )+
  labs(
    title = expression("Estadístico de Mandel " * italic(h)),
    x = "Laboratorio - Envase",
    y = "h"
  )+
  theme_minimal(base_size = 13)+
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    legend.position = "none",
    plot.background  = element_rect(fill = col_fondo,color = NA),
    panel.background = element_rect(fill = col_fondo,color = NA),
    plot.title = element_text(face = "bold")
  )

#==========================================================
# MANDEL k
#==========================================================

g4 <- ggplot(
  datos,
  aes(
    x = paste(lab,envase),
    y = k,
    fill = k_outlier
  )
)+
  geom_col(width = 0.8)+
  geom_hline(
    yintercept = k_sup,
    colour = col_linea,
    linewidth = 1
  )+
  scale_fill_manual(
    values = c(
      "FALSE" = col_oscuro,
      "TRUE"  = col_linea
    )
  )+
  labs(
    title = expression("Estadístico de Mandel " * italic(k)),
    x = "Laboratorio - Envase",
    y = "k"
  )+
  theme_minimal(base_size = 13)+
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    legend.position = "none",
    plot.background  = element_rect(fill = col_fondo,color = NA),
    panel.background = element_rect(fill = col_fondo,color = NA),
    plot.title = element_text(face = "bold")
  )

#==========================================================
# PANEL FINAL
#==========================================================



grafico_mr06<- grid.arrange(
  g1,
  g2,
  g3,
  g4,
  ncol = 2
)


ggsave(
  filename = "mr0.6png",
  plot = grafico_mr06,
  width = 12,
  height = 10,
  dpi = 300
)
