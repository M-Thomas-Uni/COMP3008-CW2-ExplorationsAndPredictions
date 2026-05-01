
library(ggplotify)
library(patchwork)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggplot2)


# --- SCREE PLOTS ---

p_scree_1921 <- as.ggplot(fviz_screeplot(res.mca.1921) +
  ggtitle("Scree Plot: 2019–21"))

p_scree_2224 <- as.ggplot(fviz_screeplot(res.mca.2224) +
  ggtitle("Scree Plot: 2022–24"))

p_scree_1921 + p_scree_2224


# --- REGIONAL CENTROID PLOTS ---

# --- 2019–21 ---
coords_1921 <- as.data.frame(res.mca.1921$ind$coord[,1:2])
coords_1921$Region <- cleaned.1921.mca$GOVTOF

centroids_1921 <- aggregate(coords_1921[,1:2],
                            by = list(coords_1921$Region),
                            FUN = mean)
colnames(centroids_1921) <- c("Region", "Dim1", "Dim2")

p_centroids_1921 <- ggplot(centroids_1921,
                           aes(Dim1, Dim2, label = Region)) +
  geom_point(size = 4) +
  geom_text(vjust = -0.5) +
  theme_minimal() +
  ggtitle("Regional MCA Centroids (2019–21)")

# --- 2022–24 ---
coords_2224 <- as.data.frame(res.mca.2224$ind$coord[,1:2])
coords_2224$Region <- cleaned.2224.mca$GOVTOF

centroids_2224 <- aggregate(coords_2224[,1:2],
                            by = list(coords_2224$Region),
                            FUN = mean)
colnames(centroids_2224) <- c("Region", "Dim1", "Dim2")

p_centroids_2224 <- ggplot(centroids_2224,
                           aes(Dim1, Dim2, label = Region)) +
  geom_point(size = 4) +
  geom_text(vjust = -0.5) +
  theme_minimal() +
  ggtitle("Regional MCA Centroids (2022–24)")

# Side-by-side
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
