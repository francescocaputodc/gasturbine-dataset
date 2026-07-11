library(knitr)
library(kableExtra)
library(tidyverse)
library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(mclust)
library(FactoMineR)
library(factoextra)
library(reshape2)
library(commonmark)
library(GGally)
library(moments)
library(patchwork)
library(skmeans)
library(caret)
library(ppclust)



gt_2015 <- read_csv("gt_2015.csv")


oss10 <- gt_2015[1:10, ] |>
  mutate(Indice = paste0(row_number(), ".")) |>
  select(Indice, everything()) |>
  kable(
    caption = "La tabella riporta le prime 10 osservazioni del dataset",
    col.names = c("", colnames(gt_2015))
  ) |>
  kable_styling(full_width = FALSE) |>
  column_spec(
    1,
    border_right = TRUE,
    bold = FALSE,
    width = "2em"
  )


tab_testo <- data.frame(
  Variabile = c("AT", "AP", "AH", "AFDP", "GTEP", "TIT", "TAT", "CDP", "TEY", "CO", "NOx"),
  Significato = c("Ambient Temperature", "Ambient Preassure", "Ambient Humidity",
                  "Air Filter Difference Pressure", "Gas Turbine Exhaust Pressure", "Turbine Inlet Temperature",
                  "Turbine After Temperature", "Compressor Discharge Pressure", "Turbine Energy Yield",
                  "Carbon Monoxide", "Nitrogen Oxides"),
  Descrizione = c(
    "È la <b>temperatura dell’aria</b> aspirata dalla turbina dall’ambiente circostante, prima di entrare nel compressore e dare inizio al ciclo di produzione dell'energia.",
    
    "È la <b>pressione atmosferica dell’aria</b> aspirata dalla turbina a gas.",
    
    "Rappresenta l’<b>umidità dell’aria</b> aspirata dalla turbina.",
    
    "Indica la <b>differenza di pressione dell’aria</b> attraverso il filtro di aspirazione. Essa misura la resistenza opposta dal filtro al flusso d’aria ed è un indicatore dello stato del filtro e della portata d’aria richiesta dalla turbina.",
    
    "È la <b>pressione dei gas di scarico</b> all’uscita della turbina, dopo che i gas combusti hanno ceduto energia meccanica alle turbine.",
    
    "Rappresenta la <b>temperatura dei gas di combustione</b> all’ingresso della turbina, immediatamente dopo la camera di combustione.",
    
    "È la <b>temperatura dei gas di scarico</b> dopo il passaggio attraverso la turbina.",
    
    "Indica la <b>pressione dell’aria in uscita dal compressore</b>, immediatamente prima dell’ingresso nella camera di combustione.",
    
    "Rappresenta la <b>quantità di energia elettrica prodotta dalla turbina</b> in un determinato intervallo di tempo.",
    
    "Indica la <b>concentrazione di monossido di carbonio</b> presente nei gas di scarico.",
    
    "Rappresenta la <b>concentrazione degli ossidi di azoto</b> nei gas di scarico, principalmente NO e NO₂. Questi composti si formano a elevate temperature di combustione e costituiscono uno dei principali inquinanti associati alle turbine a gas."
  ),
  `Unità di misura` = c("°C (gradi Celsius)", "mbar (millibar)", "% (percentuale)",
                        "mbar (millibar)", "mbar (millibar)", "°C (gradi Celsius)", "°C (gradi Celsius)",
                        "mbar (millibar)", "MWh (megawattora)", "mg/m³ (milligrammi per metro cubo)",
                        "mg/m³ (milligrammi per metro cubo)"),
  check.names = FALSE,
  stringsAsFactors = FALSE
)



desc_stats <- gt_2015 |>
  summarise(across(
    everything(),
    list(
      Media = ~mean(. , na.rm = TRUE),
      Mediana = ~median(. , na.rm = TRUE),
      Varianza = ~var(. , na.rm = TRUE),
      DevStd = ~sd(. , na.rm = TRUE),
      Q1 = ~quantile(. , 0.25, na.rm = TRUE),
      Q3 = ~quantile(. , 0.75, na.rm = TRUE),
      Asimmetria = ~skewness(. , na.rm = TRUE),
      Curtosi = ~kurtosis(. , na.rm = TRUE)
    ),
    .names = "{.col}_{.fn}"
  ))

desc_table <- desc_stats |>
  pivot_longer(
    cols = everything(),
    names_to = c("Variabile", "Statistica"),
    names_sep = "_",
    values_to = "Valore"
  ) |>
  pivot_wider(
    names_from = Statistica,
    values_from = Valore
  )

desc_table |>
  mutate(across(where(is.numeric), ~round(., 3))) |>
  kable(
    col.names = c(
      "",
      "Media", "Mediana", "Varianza", "DevStd",
      "Q1", "Q3", "Asimmetria", "Curtosi"
    ),
    booktabs = TRUE,
    align = "lcccccccc",
  ) |>
  kable_styling(
    full_width = FALSE,
    position = "center"
  ) |>
  column_spec(
    1,
    border_right = TRUE
  )




vars_order <- names(gt_2015 |> select(where(is.numeric)))

gt_long <- gt_2015 |>
  select(where(is.numeric)) |>
  pivot_longer(everything(), names_to = "var", values_to = "x") |>
  mutate(var = factor(var, levels = vars_order))

densitaGraf <- ggplot(gt_long, aes(x = x)) +
  geom_density() +
  facet_wrap(~ var, ncol = 3, nrow = 4, scales = "free") +
  theme_minimal(base_size = 25) +
  theme(
    panel.spacing = unit(1.5, "lines")
  )

ggsave(
  filename = "densitaGraf.png",
  plot = densitaGraf,
  width = 18,
  height = 16,
  dpi = 300
)


boxplotGraf <- ggplot(gt_long, aes(x = "", y = x)) +
  geom_boxplot(outlier.alpha = 0.5, width = 0.7) +
  facet_wrap(~ var, ncol = 3, nrow = 4, scales = "free_x") +
  coord_flip() +
  theme_minimal(base_size = 25) +
  theme(
    panel.spacing = unit(1.5, "lines"),
    axis.title = element_blank(),
    axis.text.x = element_text(size = 18),  
    axis.text.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank()
  )


ggsave(
  filename = "boxplotGraf.png",
  plot = boxplotGraf,
  width = 18,
  height = 16,
  dpi = 300
)



corr <- cor(gt_2015, use = "complete.obs")

# Ordine variabili come nel dataset
vars_order <- colnames(gt_2015)

# Long format
corr_melt <- melt(corr)
corr_melt$Var1 <- factor(corr_melt$Var1, levels = vars_order)
corr_melt$Var2 <- factor(corr_melt$Var2, levels = rev(vars_order))

# Heatmap a quadrati
matricecorrGraf <- ggplot(corr_melt, aes(Var1, Var2, fill = value)) +
  geom_point(
    shape = 21,      # cerchio pieno
    size = 15,       # DIMENSIONE FISSA
    color = "white", # bordo
    stroke = 0.5
  ) +
  geom_text(aes(label = round(value, 2)), size = 3)+
  coord_equal() +
  scale_x_discrete(position = "top") +
  scale_fill_gradient2(
    low = "#2166AC",
    mid = "#FFFFFF",
    high = "#B2182B",
    midpoint = 0,
    limits = c(-1, 1)
  ) +
  guides(
    fill = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barheight = unit(7, "cm"),
      barwidth  = unit(0.6, "cm")
    )
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 0,
      hjust = 0.5,
      vjust = 0.5,
      size = 11        # ⬅ dimensione testo asse X
    ),
    axis.text.y = element_text(
      size = 11        # ⬅ dimensione testo asse Y
    ),
    axis.title = element_blank()
  ) +
  labs(fill = "Correlazione")


ggsave(
  filename = "matricecorrGraf.png",
  plot = matricecorrGraf,
  width = 8,
  height = 6,
  dpi = 300
)



matricesp <- ggpairs(
  gt_2015,
  switch = "y",
  upper = list(continuous = wrap("points", alpha = 0.4, size = 0.6)),
  lower = list(continuous = wrap("points", alpha = 0.4, size = 0.6)),
  diag  = list(continuous = "densityDiag")
) +
  theme_minimal(base_size = 16) +
  theme(
    # 🔽 meno spazio tra i pannelli
    panel.spacing = unit(0.05, "lines"),
    
    # 🔽 margini esterni più stretti
    plot.margin = margin(5, 5, 5, 5),
    
    # assi nascosti
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    
    # strip più grandi
    strip.text = element_text(size = 16)
  )

ggsave(
  filename = "matricesp.png",
  plot = matricesp,
  width = 18,
  height = 18,
  dpi = 300
)

#K means
max_k <- 10

wss <- numeric(max_k)

for (k in 1:max_k) {
  km <- kmeans(gt_2015, centers = k, nstart = 25)
  wss[k] <- km$tot.withinss
}

elbow_df <- data.frame(
  k = 1:max_k,
  wss = wss
)

kmeansPlot <- ggplot(elbow_df, aes(x = k, y = wss)) +
  geom_line(color = "black", linewidth = 1) +
  geom_point(color = "red", size = 3) +
  scale_x_continuous(breaks = 1:10) +
  labs(
    title = "Metodo del gomito",
    x = "Numero di cluster k",
    y = "Devianza within"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(
      size = 18,
      face = "bold",
      hjust = 0.5,
      margin = margin(b = 10)
    ),
    axis.title = element_text(size = 15),
    axis.text  = element_text(size = 14),
    plot.margin = margin(10, 10, 10, 10)
  )

gt_2015_scaled <- scale(gt_2015)
k3 <- kmeans(gt_2015_scaled, centers = 3, nstart = 25)

mu <- attr(gt_2015_scaled, "scaled:center")  # medie
sd <- attr(gt_2015_scaled, "scaled:scale")   # deviazioni standard
centroidi_destd <- sweep(k3$centers, 2, sd, "*")
centroidi_destd <- sweep(centroidi_destd, 2, mu, "+")



centroidi_df <- as.data.frame(centroidi_destd)

centroidi_df2 <- centroidi_df %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

# 2) Aggiungi la colonna cluster in LaTeX e mettila a sinistra
centroidi_df2 <- centroidi_df2 %>%
  mutate(
    cluster = if (knitr::is_html_output()) {
      paste0("\\(c_", 1:3, "\\)")
    } else {
      paste0("$c_", 1:3, "$")
    }
  ) %>%
  relocate(cluster, .before = 1)



# ACP ---------------------------------------------------------------------

dati_scaled <- scale(gt_2015)
pca <- prcomp(dati_scaled, center = TRUE, scale. = TRUE)


varianza <- pca$sdev^2
perc_varianza <- varianza / sum(varianza) * 100
df_varianza <- data.frame(
  PC = factor(paste0("PC", seq_along(perc_varianza)),
              levels = paste0("PC", seq_along(perc_varianza))),
  Percentuale = perc_varianza
)

graficoPCA <- ggplot(df_varianza, aes(x = PC, y = Percentuale)) +
  geom_col(fill = "steelblue") +
  geom_point() +
  geom_line(aes(group = 1)) +
  labs(
    title = "Percentuale di varianza spiegata dalle componenti principali",
    x = "Componenti principali",
    y = "Varianza spiegata (%)"
  ) +
  theme_minimal()


ind <- as.data.frame(pca$x)
PC1PC2 <- ggplot(ind, aes(x = PC1, y = PC2)) +
  geom_point(size = 1, alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +  # retta orizzontale
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +  # retta verticale
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )+
  labs(
    title = "ACP - Lo spazio degli individui",
    x = paste0("PC1 (", round(summary(pca)$importance[2,1] * 100, 1), "%)"),
    y = paste0("PC2 (", round(summary(pca)$importance[2,2] * 100, 1), "%)"))


ind <- as.data.frame(pca$x)
var_exp <- summary(pca)$importance[2, ]

pc_lab <- function(pc) paste0(pc, " (", round(var_exp[pc] * 100, 1), "%)")

make_plot <- function(xpc, ypc, titolo = NULL) {
  ggplot(ind, aes(x = .data[[xpc]], y = .data[[ypc]])) +
    geom_point(size = 0.7, alpha = 0.6) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
    theme_minimal() +
    theme(
      plot.margin = margin(15, 15, 15, 15)
    ) +
    labs(
      title = titolo,
      x = pc_lab(xpc),
      y = pc_lab(ypc)
    )
}


#k means spazio ridotto
coordinate <- as.data.frame(pca$x[, 1:2])

set.seed(123)
kmPCA <- kmeans(coordinate, centers = 3, nstart = 25)

# crea cluster PRIMA
coordinate$cluster <- factor(kmPCA$cluster, levels = c(2, 3, 1))

# ora puoi calcolare i centroidi
centroidi <- coordinate %>%
  group_by(cluster) %>%
  summarise(
    PC1 = mean(PC1),
    PC2 = mean(PC2),
    .groups = "drop"
  )

# palette coerenti con i livelli (2,3,1)
col_punti <- c(
  "2" = "#377eb8",  # rosso
  "3" = "#e41a1c",  # blu
  "1" = "#2ecc71"   # verde
)

col_x <- c(
  "2" = "#08306b",  # rosso scuro
  "3" = "#99000d",  # blu scuro
  "1" = "#145a32"   # verde scuro
)

graficoKMEANS <- ggplot() +
  geom_point(data = coordinate, aes(PC1, PC2, color = cluster),
             size = 1, alpha = 0.6) +
  scale_color_manual(values = col_punti) +
  ggnewscale::new_scale_color() +
  geom_point(data = centroidi, aes(PC1, PC2, color = cluster),
             shape = 4, size = 5, stroke = 2.2) +
  scale_color_manual(values = col_x) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  ) +
  labs(
    title = "K-means (k = 3) sullo spazio PCA",
    x = "Intensità operativa della turbina",
    y = "Condizioni ambientali"
  )

#median k-means
set.seed(123)
kmedians <- function(X, k, max_iter = 100) {
  X <- as.matrix(X)
  centers <- X[sample(nrow(X), k), , drop = FALSE]
  
  for (i in 1:max_iter) {
    dists <- sapply(1:k, function(j)
      rowSums(abs(X - matrix(centers[j,], nrow(X), ncol(X), byrow=TRUE)))
    )
    cluster <- max.col(-dists)
    
    new_centers <- centers
    for (j in 1:k) {
      new_centers[j,] <- apply(X[cluster == j, , drop=FALSE], 2, median)
    }
    
    if (sum(abs(new_centers - centers)) < 1e-6) break
    centers <- new_centers
  }
  
  list(cluster = cluster, centers = centers)
}

coordinateKMEDIAM <- as.data.frame(pca$x[, 1:2])

#
fit <- kmedians(coordinateKMEDIAM, 3)
centoridiKmedian <- fit$centers
centoridiKmedian <- as.data.frame(fit$centers)
centoridiKmedian$cluster <- factor(seq_len(nrow(centoridiKmedian)))
df_clusterKMEDIAN <- as.data.frame(coordinateKMEDIAM)
df_clusterKMEDIAN$cluster <- fit$cluster

centers_df <- as.data.frame(fit$centers)
centers_df$cluster <- factor(1:nrow(centers_df))

df_clusterKMEDIAN$cluster <- factor(df_clusterKMEDIAN$cluster)
centers_df$cluster <- factor(centers_df$cluster)

ggplot(df_clusterKMEDIAN, aes(x = PC1, y = PC2, color = factor(cluster))) +
  geom_point(size = 2, alpha = 0.8) +
  labs(
    title = "Clustering k-medians sulle prime due componenti principali",
    color = "Cluster"
  ) +
  theme_minimal()


col_puntiMM <- c(
  "2" = "#377eb8",  # rosso vivo3
  "1" = "#2ecc71",  # 1
  "3" = "#e41a1c"   # blu intenso2
)

col_xMM <- c(
  "2" = "#08306b",  # rosso scuro
  "1" = "#145a32",  # verde scuro
  "3" = "#99000d"   # blu scuro
)

graficoKmedian <- ggplot() +
  geom_point(data = df_clusterKMEDIAN, aes(PC1, PC2, color = cluster),
             size = 1, alpha = 0.6) +
  scale_color_manual(values = col_puntiMM) +
  
  ggnewscale::new_scale_color() +
  
  # centroidi (X più scure)
  geom_point(data = centoridiKmedian, aes(PC1, PC2, color = cluster),
             shape = 4, size = 5, stroke = 2.2) +
  scale_color_manual(values = col_xMM) +
  
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  ) +
  labs(
    title = "K-median (k = 3) sullo spazio PCA",
    x = "Intensità operativa della turbina",
    y = "Condizioni ambientali"
  )

#GMM
gt_2015_scaled <- scale(gt_2015)
mod <- Mclust(gt_2015_scaled)
summary(mod) 

tab_modello <- tibble(
  `log-likelihood` = 17180.1,
  n   = 7384,
  df  = 701,
  BIC = 28116.35,
  ICL = 27639.65
)

tab_cluster_oriz <- tibble(
  `1` = 826,
  `2` = 1152,
  `3` = 395,
  `4` = 963,
  `5` = 1344,
  `6` = 730,
  `7` = 670,
  `8` = 865,
  `9` = 439
)






#McLust su dati trasformati
gt_trans <- gt_2015

# trasformazioni solo dove serve
gt_trans$CO  <- log1p(gt_2015$CO)
gt_trans$NOX <- log1p(gt_2015$NOX)


gt_trans_scaled <- scale(gt_trans)
mclustTR <- Mclust(gt_trans_scaled)
summary(mclustTR)


tab_modello2 <- tibble(
  `log-likelihood` = 13787.74,
  n   = 7384,
  df  = 701,
  BIC = 21331.62,
  ICL = 20811.56
)

tab_cluster_oriz2 <- tibble(
  `1` = 711,
  `2` = 892,
  `3` = 838,
  `4` = 1039,
  `5` = 342,
  `6` = 785,
  `7` = 630,
  `8` = 1393,
  `9` = 754
)


#McLust su PCA
coordinate2 <- as.data.frame(pca$x[, 1:2])
mclustPCA <- Mclust(coordinate2)
summary(mclustPCA)
plot(mclustPCA)

tab_modello3 <- tibble(
  `log-likelihood` = -26005.33,
  n   = 7384,
  df  = 47,
  BIC = -52429.28,
  ICL = -54906.04
)

tab_cluster_oriz3 <- tibble(
  `1` = 122,
  `2` = 1709,
  `3` = 1792,
  `4` = 1023,
  `5` = 916,
  `6` = 530,
  `7` = 715,
  `8` = 577
)





#Fuzzy

coordinate <- as.data.frame(pca$x[, 1:2])

set.seed(1)
res <- fcm(coordinate, centers = 3, m = 2)

res$centers       # centroidi (cluster centers)
res$u             # matrice membership (c x n) (quanto ogni punto appartiene a ogni cluster)
res$cluster 

dfFuzzy <- data.frame(
  x = coordinate[, 1],
  y = coordinate[, 2],
  cluster = factor(res$cluster)  # factor IMPORTANTISSIMO
)


graficoFCMHARD <- ggplot(dfFuzzy, aes(x = x, y = y, color = cluster)) +
  geom_point(size = 1, alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  scale_color_manual(
    values = c(
      "1" = "#e41a1c",
      "2" = "#2ecc71",
      "3" = "#377eb8"
    )) +
  
  
  ggnewscale::new_scale_color() +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  ) +
  labs(
    title = "Fuzzy C-Means: assegnazione Hard",
    x = "Intensità operativa della turbina",
    y = "Condizioni ambientali"
  )

U <- t(res$u)  # ora n x c


n <- nrow(coordinate)

# res$u può essere c×n (ppclust) oppure n×c (altre funzioni / casi)
Uraw <- res$u

if (ncol(Uraw) == n) {
  # caso c×n -> trasponi
  U <- t(Uraw)      # n×c
} else if (nrow(Uraw) == n) {
  # caso n×c -> ok
  U <- Uraw
} else {
  stop("Dimensioni non compatibili: n=", n,
       " nrow(res$u)=", nrow(Uraw),
       " ncol(res$u)=", ncol(Uraw))
}

dfFuzzy <- data.frame(
  x = coordinate[, 1],
  y = coordinate[, 2],
  cluster = factor(res$cluster),
  max_u = apply(U, 1, max)
)





graficoFCMCrisp <- ggplot(dfFuzzy, aes(x, y, color = cluster, alpha = max_u)) +
  geom_point(size = 1) +
  scale_alpha(range = c(0.2, 1)) +
  scale_color_manual(
    values = c(
      "1" = "#e41a1c",
      "2" = "#2ecc71",
      "3" = "#377eb8"
    )) +
  
  
  ggnewscale::new_scale_color() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14)
  )+
  labs(alpha = "Membership", 
       color = "Cluster",
       title = "Fuzzy C-Means: assegnazione Crisp",
       x = "Intensità operativa della turbina",
       y = "Condizioni ambientali")

#grafico 2

dfFuzzy$borderline <- dfFuzzy$max_u < 0.6

fuzzyborder <- ggplot(dfFuzzy, aes(x, y, color = cluster)) +
  geom_point(size = 1, alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  scale_color_manual(
    values = c(
      "1" = "#e41a1c",
      "2" = "#2ecc71",
      "3" = "#377eb8"
    )) +
  
  
  ggnewscale::new_scale_color() +
  geom_point(data = subset(dfFuzzy, borderline),
             aes(x, y),
             shape = 21, stroke = 0.7, fill = NA, color = "black", size = 1.1) +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14)
  )+
  labs(title = "Punti per cui membership < 0.6",
       x = "Intensità operativa della turbina",
       y = "Condizioni ambientali",
       legend.position = "none",)



#grafico 3


df_u <- data.frame(
  x = coordinate[,1],
  y = coordinate[,2],
  U
)
colnames(df_u)[3:ncol(df_u)] <- paste0("Cluster ", 1:ncol(U))

df_long <- pivot_longer(df_u,
                        cols = starts_with("Cluster"),
                        names_to = "cluster",
                        values_to = "membership")

df_long$cluster <- factor(df_long$cluster,
                          levels = c("Cluster 1", "Cluster 3", "Cluster 2"))

fuzzygraf3 <- ggplot(df_long, aes(x, y, color = membership)) +
  geom_point(size = 1.5) +
  facet_wrap(~ cluster) +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14)
  )+
  labs(color = "Membership",
       x = "Intensità operativa della turbina",
       y = "Condizioni ambientali")

library(clustrd)#Factorial k means
XFKM <- scale(gt_2015)   # standardizzazione fondamentale

set.seed(1234)
risultato <- cluspca(XFKM, nclus = 3, ndim = 2, method = "FKM")
# Sintesi dei risultati
print(risultato)
plot(risultato)


df_plot <- data.frame(
  Dim1 = risultato$obscoord[,1],
  Dim2 = risultato$obscoord[,2],
  Cluster = as.factor(risultato$cluster)
)


# 2. Creo il grafico con ggplot2

df_plot$Cluster <- factor(
  df_plot$Cluster,
  levels = c("1", "2", "3")
)

col_puntiF <- c(
  "2" = "#2ecc71",  # rosso vivo3
  "1" = "#e41a1c",  # 1
  "3" = "#377eb8"   # blu intenso2
)


graficoFKM <- ggplot(df_plot, aes(x = Dim1, y = Dim2, color = Cluster)) +
  geom_point(alpha = 0.6, size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  theme_minimal() + 
  theme(
    legend.position = "none",
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  ) +
  labs(title = "Factorial K-means",
       x = "Dimensione 1",
       y = "Dimensione 2") +
  scale_color_manual(
    values = c(
      "1" = "#377eb8",
      "2" = "#2ecc71",
      "3" = "#e41a1c"
    )) 




#Inverto dimensione 2
df_plot <- data.frame(
  Dim1 = risultato$obscoord[,1],
  Dim2 = -risultato$obscoord[,2],  # inversione qui
  Cluster = as.factor(risultato$cluster)
)

centroidi <- aggregate(
  cbind(Dim1, Dim2) ~ Cluster,
  data = df_plot,
  FUN = mean
)


# 2. Creo il grafico con ggplot2

df_plot$Cluster <- factor(
  df_plot$Cluster,
  levels = c("1", "2", "3")
)

col_puntiF <- c(
  "2" = "#2ecc71",  # rosso vivo3
  "1" = "#e41a1c",  # 1
  "3" = "#377eb8"   # blu intenso2
)


graficoFKM <- ggplot(df_plot, aes(x = Dim1, y = Dim2, color = Cluster)) +
  geom_point(alpha = 0.6, size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  theme_minimal() + 
  theme(
    legend.position = "none",
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  ) +
  labs(title = "Factorial K-means",
       x = "Dimensione 1",
       y = "Dimensione 2") +
  scale_color_manual(
    values = c(
      "1" = "#377eb8",
      "2" = "#2ecc71",
      "3" = "#e41a1c"
    )) 


graficoFKM +
  geom_point(
    data = centroidi,
    aes(x = Dim1, y = Dim2, color = Cluster),
    shape = 4,
    size = 5,
    stroke = 1.5
  )



#Reduced k means
set.seed(123)
risultatoRKM <- cluspca(XFKM, nclus = 3, ndim = 2, method = "RKM")
plot(risultatoRKM)


df_plot2 <- data.frame(
  Dim1 = risultatoRKM$obscoord[,1],
  Dim2 = risultatoRKM$obscoord[,2],
  Cluster = as.factor(risultatoRKM$cluster)
)

graficoRKM <- ggplot(df_plot2, aes(x = Dim1, y = Dim2, color = Cluster)) +
  geom_point(alpha = 0.6, size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  theme_minimal() + 
  theme(
    legend.position = "none",
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  ) +
  labs(title = "Reduced K-means",
       x = "Dimensione 1",
       y = "Dimensione 2") +
  scale_color_manual(
    values = c(
      "1" = "#377eb8",
      "2" = "#e41a1c",
      "3" = "#2ecc71"
    )) 
