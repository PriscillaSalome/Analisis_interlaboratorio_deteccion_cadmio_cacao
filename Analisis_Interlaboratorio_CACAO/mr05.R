# ==========================================================
# MANDEL h Y k BOOTSTRAP 
#
# MR-05a
#
# Unidad experimental:
# Laboratorio - Envase
# ==========================================================

library(tidyverse)
library(ggplot2)
library(patchwork)
library(gridExtra)

#==========================================================
# DATOS
#==========================================================

datos <- tibble(
  lab = c(7,7,5,5,9,9,3,3,2,2,6,6,1,1),
  
  envase = c(
    "015","031","011","071",
    "032","041","022","049",
    "004","037","046","062",
    "024","078"
  ),
  
  r1 = c(
    0.34,0.24,0.23,0.30,
    0.16,0.23,0.21,0.18,
    0.30,0.29,0.16,0.18,
    0.32,0.32
  ),
  
  r2 = c(
    0.35,0.23,0.23,0.23,
    0.21,0.21,0.21,0.18,
    0.25,0.29,0.16,0.17,
    0.33,0.28
  ),
  
  r3 = c(
    0.34,0.23,0.24,0.28,
    0.19,0.18,0.16,0.18,
    0.31,0.31,0.14,0.16,
    0.33,0.30
  )
)

#==========================================================
# h OBSERVADO
#==========================================================

datos$media <- rowMeans(
  datos[,c("r1","r2","r3")]
)

media_global <- mean(datos$media)

sx <- sd(datos$media)

datos$h <- (
  datos$media -
    media_global
)/sx

#==========================================================
# k OBSERVADO
#==========================================================

datos$sd_lab <- apply(
  datos[,c("r1","r2","r3")],
  1,
  sd
)

sr <- sqrt(
  mean(datos$sd_lab^2)
)

datos$k <- datos$sd_lab/sr

#==========================================================
# BOOTSTRAP 
#==========================================================

todos <- as.numeric(
  unlist(
    datos[,c("r1","r2","r3")]
  )
)

#----------------------------------------------------------
# DETECCION DE EXTREMOS
#----------------------------------------------------------

outliers <- boxplot.stats(
  todos
)$out

cat("\nObservaciones extremas detectadas:\n")
print(outliers)

#----------------------------------------------------------
# ELIMINACION DE EXTREMOS
#----------------------------------------------------------

todos_rob <- todos[
  !(todos %in% outliers)
]

cat("\nObservaciones originales :",length(todos))
cat("\nObservaciones robustas   :",length(todos_rob))
cat("\n")

#==========================================================
# BOOTSTRAP
#==========================================================

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
  ) / sd(medias_boot)
  
  sd_boot <- apply(
    datos_boot,
    1,
    sd
  )
  
  sr_boot <- sqrt(
    mean(sd_boot^2)
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

#==========================================================
# VALORES CRITICOS
#==========================================================

alpha <- 0.05

h_inf <- quantile(
  h_boot,
  0.025
)

h_sup <- quantile(
  h_boot,
  0.975
)

k_sup <- quantile(
  k_boot,
  0.95
)

#==========================================================
# CLASIFICACION
#==========================================================

datos <- datos %>%
  mutate(
    
    h_outlier =
      h < h_inf |
      h > h_sup,
    
    k_outlier =
      k > k_sup
    
  )

#==========================================================
# RESULTADOS
#==========================================================

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

cat("\n")
cat("h inferior =",round(h_inf,3),"\n")
cat("h superior =",round(h_sup,3),"\n")
cat("k superior =",round(k_sup,3),"\n")

#==========================================================
# PALETA 
#==========================================================

col_fondo   <- "#FAFAE3"
col_claro   <- "#E7E5CE"
col_medio   <- "#D9D1BA"
col_cacao   <- "#C8B99A"
col_oscuro  <- "#A89372"
col_linea   <- "#D98E04"


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
# PANEL 
#==========================================================

grafico_mr05 <- grid.arrange(
  g1,
  g2,
  g3,
  g4,
  ncol = 2
)

ggsave(
  filename = "mr05.png",
  plot = grafico_mr05,
  width = 12,
  height = 10,
  dpi = 300
)
