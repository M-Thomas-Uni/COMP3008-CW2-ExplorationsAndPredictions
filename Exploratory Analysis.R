
library(ggplotify)
library(patchwork)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggplot2)

as.ggplot(fviz_mca_var(
  res.mca.1921,
  repel = TRUE,
  col.var = "grey60",         # active categories
  col.quanti.sup = "darkgreen",
  col.quali.sup = "red",
  title="Variable Categories - MCA 2019-22")) /
  as.ggplot(fviz_mca_var(
    res.mca.2224,
    repel = TRUE,
    col.var = "grey60",         # active categories
    col.quanti.sup = "darkgreen",
    col.quali.sup = "red",
    title="Variable Categories - MCA 2022-24"))

eig_1921 <- as.data.frame(res.mca.1921$eig)
eig_1921$Dim <- seq_len(nrow(eig_1921))

head(res.mca.1921$eig[,2])

as.ggplot(fviz_mca_biplot(res.mca.1921)) / as.ggplot(fviz_mca_biplot(res.mca.2224))


library(ggplot2)
library(ggrepel)

### 1. Extract ACTIVE categories
active <- as.data.frame(res.mca.1921$var$coord) %>% rename(Dim.1 = `Dim 1`, Dim.2 = `Dim 2`)
active$label <- rownames(active)


### 2. Extract SUPPLEMENTARY categories
# If you used qualitative supplementary variables:
sup <- as.data.frame(res.mca.1921$quali.sup$coord)%>% rename(Dim.1 = `Dim 1`, Dim.2 = `Dim 2`)
sup$label <- rownames(sup)

# If instead you used quantitative supplementary variables, swap:
# sup <- as.data.frame(res.mca.1921$quanti.sup$coord)
# sup$label <- rownames(sup)

### 3. Plot
ggplot() +
  # Active categories (grey)
  geom_point(data = active, aes(Dim.1, Dim.2), color = "grey70") +
  geom_text_repel(
    data = active,
    aes(Dim.1, Dim.2, label = label),
    color = "grey60",
    size = 3
  ) +
  
  # Supplementary categories (bold + coloured)
  geom_point(data = sup, aes(Dim.1, Dim.2), color = "red") +
  geom_text_repel(
    data = sup,
    aes(Dim.1, Dim.2, label = label),
    color = "red",
    fontface = "bold",
    size = 3.5
  ) +
  
  theme_minimal() +
  labs(
    title = "MCA Variable Map (Active vs Supplementary)",
    x = "Dimension 1",
    y = "Dimension 2"
  )


###########
library(ggplot2)
library(ggrepel)
library(dplyr)

vars_to_highlight <- c("INECAC05", "LNGLST", "LIMACT", "SC20MMJ")

pattern <- paste0("^(", paste(vars_to_highlight, collapse="|"), ")")

highlight_labels <- active$label[grepl(pattern, active$label)]
highlight_labels


library(ggplot2)
library(ggrepel)
library(dplyr)

### 1. Extract ACTIVE categories
active <- as.data.frame(res.mca.1921$var$coord)%>% rename(Dim.1 = `Dim 1`, Dim.2 = `Dim 2`)
active$label <- rownames(active)

### 2. Identify categories belonging to the variables of interest
vars_to_highlight <- c("FTPTWK", "SC20MMJ")

pattern <- paste0("^(", paste(vars_to_highlight, collapse="|"), ")")

highlight_labels <- active$label[grepl(pattern, active$label)]

active$highlight <- ifelse(active$label %in% highlight_labels, "highlight", "other")

### 3. Extract SUPPLEMENTARY categories
sup <- as.data.frame(res.mca.1921$quali.sup$coord)%>% rename(Dim.1 = `Dim 1`, Dim.2 = `Dim 2`)
sup$label <- rownames(sup)

### 4. Plot
ggplot() +
  # Other active categories (grey)
  geom_point(
    data = active %>% filter(highlight == "other"),
    aes(Dim.1, Dim.2),
    color = "grey70"
  ) +
  geom_text_repel(
    data = active %>% filter(highlight == "other"),
    aes(Dim.1, Dim.2, label = label),
    color = "grey60",
    size = 3
  ) +
  
  # Highlighted active categories (bold + blue)
  geom_point(
    data = active %>% filter(highlight == "highlight"),
    aes(Dim.1, Dim.2),
    color = "blue"
  ) +
  geom_text_repel(
    data = active %>% filter(highlight == "highlight"),
    aes(Dim.1, Dim.2, label = label),
    color = "blue",
    fontface = "bold",
    size = 3.5
  ) +
  
  # Supplementary categories (bold + red)
  geom_point(data = sup, aes(Dim.1, Dim.2), color = "red") +
  geom_text_repel(
    data = sup,
    aes(Dim.1, Dim.2, label = label),
    color = "red",
    fontface = "bold",
    size = 3.5
  ) +
  
  theme_minimal() +
  labs(
    title = "MCA Variable Map for 2019-21, with Highlights for Dim.1 Analysis",
    x = "Dimension 1",
    y = "Dimension 2"
  )

###########



# Kaiser plot for 2022–24
eig_2224 <- as.data.frame(res.mca.2224$eig)
eig_2224$Dim <- seq_len(nrow(eig_2224))

ggplot(eig_1921, aes(x = Dim, y = eigenvalue)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "red") +
  labs(
    title = "MCA Eigenvalues with Kaiser Line (2019–21)",
    x = "Dimension",
    y = "Eigenvalue"
  ) +
  theme_minimal() +
  
  
  ggplot(eig_2224, aes(x = Dim, y = eigenvalue)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "red") +
  labs(
    title = "MCA Eigenvalues with Kaiser Line (2022–24)",
    x = "Dimension",
    y = "Eigenvalue"
  ) +
  theme_minimal()


# --- SCREE PLOTS ---

p_scree_1921 <- as.ggplot(fviz_screeplot(res.mca.1921) +
                            ggtitle("Scree Plot: 2019–21"))

p_scree_2224 <- as.ggplot(fviz_screeplot(res.mca.2224) +
                            ggtitle("Scree Plot: 2022–24"))

p_scree_1921 / p_scree_2224


# --- REGIONAL CENTROID PLOTS ---

# --- 2019–21 ---
coords_1921 <- as.data.frame(res.mca.1921$ind$coord[,1:2]) %>% rename(Dim.1 = `Dim 1`,
                                                                      Dim.2 = `Dim 2`
)

coords_1921$Region <- cleaned.1921.mca$GOVTOF


set.seed(42)
k_clusters_1921 <- kmeans(coords_1921[,1:2], centers = 3)  # choose k
coords_1921$Cluster <- factor(k_clusters_1921$cluster)

centroids_1921 <- aggregate(coords_1921[,1:2],
                            by = list(coords_1921$Region),
                            FUN = mean)
colnames(centroids_1921) <- c("Region", "Dim.1", "Dim.2")

p_clusters_1921 <- ggplot(coords_1921, aes(Dim.1, Dim.2, colour = Cluster)) +
  geom_point(alpha = 0.2, size = 0.8) +
  stat_ellipse(type = "norm", size = 1) +
  geom_point(data = centroids_1921,
             aes(Dim.1, Dim.2),
             colour = "black",
             size = 4,
             inherit.aes = FALSE) +
  geom_text(data = centroids_1921,
            aes(Dim.1, Dim.2, label = Region),
            vjust = -0.5,
            colour = "black",
            inherit.aes = FALSE) +
  theme_minimal() +
  ggtitle("MCA k-means Clusters with Ellipses (2019–21)")



# --- 2022–24 ---
coords_2224 <- as.data.frame(res.mca.2224$ind$coord[,1:2]) %>% rename(Dim.1 = `Dim 1`,
                                                                      Dim.2 = `Dim 2`
)
coords_2224$Region <- cleaned.2224.mca$GOVTOF

vars_1921 <- res.mca.1921$var$coord[, 1:2]
vars_2224 <- res.mca.2224$var$coord[, 1:2]

# Align Dim1
if (cor(vars_1921[,1], vars_2224[,1]) < 0) {
  coords_2224$Dim.1 <- -coords_2224$Dim.1
}

# Align Dim2
if (cor(vars_1921[,2], vars_2224[,2]) < 0) {
  coords_2224$Dim.2 <- -coords_2224$Dim.2
}


set.seed(42)
k_clusters_2224 <- kmeans(coords_2224[,1:2], centers = 3)  # choose k
coords_2224$Cluster <- factor(k_clusters_2224$cluster)

centroids_2224 <- aggregate(coords_2224[,1:2],
                            by = list(coords_2224$Region),
                            FUN = mean)
colnames(centroids_2224) <- c("Region", "Dim.1", "Dim.2")

p_clusters_2224 <- ggplot(coords_2224, aes(Dim.1, Dim.2, colour = Cluster)) +
  geom_point(alpha = 0.2, size = 0.8) +
  stat_ellipse(type = "norm", size = 1) +
  geom_point(data = centroids_2224,
             aes(Dim.1, Dim.2),
             colour = "black",
             size = 4,
             inherit.aes = FALSE) +
  geom_text(data = centroids_2224,
            aes(Dim.1, Dim.2, label = Region),
            vjust = -0.5,
            colour = "black",
            inherit.aes = FALSE) +
  theme_minimal() +
  ggtitle("MCA k-means Clusters with Ellipses (2022-24)")

# Side-by-side

p_clusters_1921 / p_clusters_2224

p_centroids_1921 + p_centroids_2224


### --- MISSINGNESS BY REGION ---

df <- cleaned.1921.mca   # or cleaned.2224.mca

vars <- c(active_vars, supp_quali, supp_quanti)
vars_no_region <- setdiff(c(active_vars, supp_quali, supp_quanti), "GOVTOF")


missing_by_region <- cleaned.1921.mca %>%
  select(GOVTOF, all_of(vars_no_region)) %>%
  mutate(across(all_of(vars_no_region), ~ is.na(.))) %>%   # TRUE = missing
  group_by(GOVTOF) %>%
  summarise(across(all_of(vars_no_region), ~ mean(.x))) %>%
  ungroup()


missing_summary <- missing_by_region %>%
  pivot_longer(-GOVTOF, names_to = "variable", values_to = "missing_rate") %>%
  group_by(GOVTOF) %>%
  summarise(avg_missing = mean(missing_rate)) %>%
  arrange(desc(avg_missing))


ggplot(missing_summary,
       aes(x = reorder(GOVTOF, avg_missing),
           y = avg_missing)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Average Missingness Across MCA Variables by Region",
    x = "Region",
    y = "Average Missing Rate"
  )




### --- REGIONAL CENTROID MOVEMENTS ---

temp.res.mca.1921 <- res.mca.1921
temp.res.mca.2224 <- res.mca.2224


temp.res.mca.2224$ind$coord[,2] <- -res.mca.2224$ind$coord[,2]
temp.res.mca.2224$var$coord[,2] <- -res.mca.2224$var$coord[,2]

scale_factor_dim1 <- sd(temp.res.mca.1921$ind$coord[,1]) / 
  sd(temp.res.mca.2224$ind$coord[,1])

scale_factor_dim2 <- sd(temp.res.mca.1921$ind$coord[,2]) / 
  sd(temp.res.mca.2224$ind$coord[,2])

temp.res.mca.2224$ind$coord[,1] <- temp.res.mca.2224$ind$coord[,1] * scale_factor_dim1
temp.res.mca.2224$ind$coord[,2] <- temp.res.mca.2224$ind$coord[,2] * scale_factor_dim2

temp.res.mca.2224$var$coord[,1] <- temp.res.mca.2224$var$coord[,1] * scale_factor_dim1
temp.res.mca.2224$var$coord[,2] <- temp.res.mca.2224$var$coord[,2] * scale_factor_dim2


centroids_1921 <- aggregate(temp.res.mca.1921$ind$coord[,1:2],
                            by = list(cleaned.1921.mca$GOVTOF),
                            FUN = mean)

centroids_2224 <- aggregate(temp.res.mca.2224$ind$coord[,1:2],
                            by = list(cleaned.2224.mca$GOVTOF),
                            FUN = mean)

colnames(centroids_1921) <- c("Region", "Dim1_1921", "Dim2_1921")
colnames(centroids_2224) <- c("Region", "Dim1_2224", "Dim2_2224")

movement <- merge(centroids_1921, centroids_2224, by = "Region")

movement_long <- movement %>%
  tidyr::pivot_longer(
    cols = c(Dim1_1921, Dim2_1921, Dim1_2224, Dim2_2224),
    names_to = c(".value", "period"),
    names_pattern = "(Dim[12])_(.*)"
  ) %>%
  mutate(period = dplyr::recode(period,
                                "1921" = "2019–21",
                                "2224" = "2022–24"))


ggplot() +
  # arrows
  geom_segment(data = movement,
               aes(x = Dim1_1921, y = Dim2_1921,
                   xend = Dim1_2224, yend = Dim2_2224),
               arrow = arrow(length = unit(0.2, "cm")),
               colour = "grey40") +
  
  # points with legend
  geom_point(data = movement_long,
             aes(x = Dim1, y = Dim2, colour = period),
             size = 3) +
  
  # region labels (on 2022–24 points)
  geom_text(data = movement_long %>% filter(period == "2022–24"),
            aes(x = Dim1, y = Dim2, label = Region),
            vjust = -0.5, size = 3) +
  
  scale_colour_manual(values = c("2019–21" = "blue",
                                 "2022–24" = "red")) +
  
  theme_minimal() +
  labs(title = "Regional Movement in MCA Space (2019–21 → 2022–24)",
       x = "Dimension 1",
       y = "Dimension 2",
       colour = "Period")

##############################


library(dplyr)
library(ggplot2)
library(ggrepel)
library(patchwork)

### -----------------------------
### 2019–21 VARIABLES
### -----------------------------
vars_1921 <- as.data.frame(res.mca.1921$var$coord[,1:2]) %>%
  rename(Dim.1 = `Dim 1`, Dim.2 = `Dim 2`)
vars_1921$label <- rownames(vars_1921)

sup_1921 <- as.data.frame(res.mca.1921$quali.sup$coord[,1:2]) %>%
  rename(Dim.1 = `Dim 1`, Dim.2 = `Dim 2`)
sup_1921$label <- rownames(sup_1921)

### -----------------------------
### 2019–21 INDIVIDUALS
### -----------------------------
coords_1921 <- as.data.frame(res.mca.1921$ind$coord[,1:2]) %>%
  rename(Dim.1 = `Dim 1`, Dim.2 = `Dim 2`)
coords_1921$Region <- cleaned.1921.mca$GOVTOF

set.seed(42)
k_clusters_1921 <- kmeans(coords_1921[,1:2], centers = 3)
coords_1921$Cluster <- factor(k_clusters_1921$cluster)

### -----------------------------
### 2019–21 PLOT
### -----------------------------
p_mca_ellipses_1921 <- ggplot() +
  geom_point(data = coords_1921, aes(Dim.1, Dim.2, colour = Cluster),
             alpha = 0.15, size = 0.8) +
  stat_ellipse(data = coords_1921, aes(Dim.1, Dim.2, colour = Cluster),
               type = "norm", size = 1) +
  geom_point(data = vars_1921, aes(Dim.1, Dim.2), colour = "grey40") +
  geom_text_repel(data = vars_1921, aes(Dim.1, Dim.2, label = label),
                  colour = "grey40", size = 3) +
  geom_point(data = sup_1921, aes(Dim.1, Dim.2), colour = "red") +
  geom_text_repel(data = sup_1921, aes(Dim.1, Dim.2, label = label),
                  colour = "red", fontface = "bold", size = 3.5) +
  theme_minimal() +
  labs(title = "MCA + K-Means Ellipses (2019–21)",
       x = "Dimension 1", y = "Dimension 2")

### -----------------------------
### 2022–24 VARIABLES
### -----------------------------
vars_2224 <- as.data.frame(res.mca.2224$var$coord[,1:2]) %>%
  rename(Dim.1 = `Dim 1`, Dim.2 = `Dim 2`)
vars_2224$label <- rownames(vars_2224)

sup_2224 <- as.data.frame(res.mca.2224$quali.sup$coord[,1:2]) %>%
  rename(Dim.1 = `Dim 1`, Dim.2 = `Dim 2`)
sup_2224$label <- rownames(sup_2224)

### -----------------------------
### 2022–24 INDIVIDUALS
### -----------------------------
coords_2224 <- as.data.frame(res.mca.2224$ind$coord[,1:2]) %>%
  rename(Dim.1 = `Dim 1`, Dim.2 = `Dim 2`)
coords_2224$Region <- cleaned.2224.mca$GOVTOF

### -----------------------------
### AXIS ALIGNMENT (SAFE)
### -----------------------------
flip_dim1 <- cor(vars_1921$Dim.1, vars_2224$Dim.1) < 0
flip_dim2 <- cor(vars_1921$Dim.2, vars_2224$Dim.2) < 0

if (flip_dim1) {
  vars_2224$Dim.1   <- -vars_2224$Dim.1
  sup_2224$Dim.1    <- -sup_2224$Dim.1
  coords_2224$Dim.1 <- -coords_2224$Dim.1
}

if (flip_dim2) {
  vars_2224$Dim.2   <- -vars_2224$Dim.2
  sup_2224$Dim.2    <- -sup_2224$Dim.2
  coords_2224$Dim.2 <- -coords_2224$Dim.2
}

### -----------------------------
### 2022–24 CLUSTERS
### -----------------------------
set.seed(42)
k_clusters_2224 <- kmeans(coords_2224[,1:2], centers = 3)
coords_2224$Cluster <- factor(k_clusters_2224$cluster)

### -----------------------------
### 2022–24 PLOT
### -----------------------------
p_mca_ellipses_2224 <- ggplot() +
  geom_point(data = coords_2224, aes(Dim.1, Dim.2, colour = Cluster),
             alpha = 0.15, size = 0.8) +
  stat_ellipse(data = coords_2224, aes(Dim.1, Dim.2, colour = Cluster),
               type = "norm", size = 1) +
  geom_point(data = vars_2224, aes(Dim.1, Dim.2), colour = "grey40") +
  geom_text_repel(data = vars_2224, aes(Dim.1, Dim.2, label = label),
                  colour = "grey40", size = 3) +
  geom_point(data = sup_2224, aes(Dim.1, Dim.2), colour = "red") +
  geom_text_repel(data = sup_2224, aes(Dim.1, Dim.2, label = label),
                  colour = "red", fontface = "bold", size = 3.5) +
  theme_minimal() +
  labs(title = "MCA + K-Means Ellipses (2022–24)",
       x = "Dimension 1", y = "Dimension 2")

p_mca_ellipses_1921 / p_mca_ellipses_2224



################

ggplot(coords_1921, aes(Dim.1, Dim.2)) +
  stat_bin_hex(aes(weight = weight)) +
  scale_fill_viridis_c() +
  theme_minimal()

ggplot(coords_1921, aes(Dim.1, Dim.2)) +
  geom_bin2d(aes(weight = weight)) +
  scale_fill_viridis_c() +
  theme_minimal()

ggplot(coords_1921, aes(Dim.1, Dim.2)) +
  stat_density_2d_filled(alpha = 0.5)

ggplot() +
  stat_density_2d(
    data = coords_1921,
    aes(Dim.1, Dim.2, fill = ..level..),
    geom = "polygon",
    alpha = 0.4
  ) +
  stat_density_2d(
    data = coords_2224,
    aes(Dim.1, Dim.2, colour = ..level..),
    geom = "contour",
    size = 0.7
  ) +
  theme_minimal() +
  labs(title = "Density Contours in MCA Space (2019–21 vs 2022–24)")




