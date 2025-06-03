# Downloadet data 6/2-2025 
DNK <- read_excel("/Users/philipstrommen/Desktop/Bachelor/DNK.xlsx")

#Filtrering af data
DNK_filtered <- DNK %>%
  filter(Season %in% c("2022/2023", "2023/2024", "2024/2025"))

#Middelværdi af antal scorede hjemmemål og udemål
mean_values <- DNK_filtered %>%
  summarise(
    mean_HG = mean(HG, na.rm = TRUE),
    mean_AG = mean(AG, na.rm = TRUE)
  )
mean_HG <- mean_values$mean_HG
mean_AG <- mean_values$mean_AG

print(mean_values)

#Lav data til fordeling
#For hjemmemål
poisson_data_HG <- data.frame(
  x = 0:max(DNK_filtered$HG, na.rm = TRUE),
  prob = dpois(0:max(DNK_filtered$HG, na.rm = TRUE), lambda = mean_HG) * nrow(DNK_filtered)
)

#For udemål
poisson_data_AG <- data.frame(
  x = 0:max(DNK_filtered$AG, na.rm = TRUE),
  prob = dpois(0:max(DNK_filtered$AG, na.rm = TRUE), lambda = mean_AG) * nrow(DNK_filtered)
)

#Plot data imod Poisson fordeling
# Plot for HG (hjemmemål) med  Poisson-fordeling
plot_HG <- ggplot(DNK_filtered, aes(x = HG)) +
  geom_histogram(aes(y = ..count..), bins = 15, fill = "blue", color = "black", alpha = 1) +
  geom_line(data = poisson_data_HG, aes(x = x, y = prob), color = "red", size = 1.2) +
  geom_point(data = poisson_data_HG, aes(x = x, y = prob), color = "red", size = 2) +
  scale_x_continuous(breaks = 0:7) +  
  labs(title = "Fordeling af antal scorede hjemmemål",
       x = "Antal hjemmemål",
       y = "Antal kampe") +
  theme_minimal()


# Plot for AG (udemål) med  Poisson-fordeling
plot_AG <- ggplot(DNK_filtered, aes(x = AG)) +
  geom_histogram(aes(y = ..count..), bins = 15, fill = "blue", color = "black", alpha = 1) +
  geom_line(data = poisson_data_AG, aes(x = x, y = prob), color = "red", size = 1.2) +
  geom_point(data = poisson_data_AG, aes(x = x, y = prob), color = "red", size = 2) +
  labs(title = "Fordeling af antal scorede udemål",
       x = "Antal udemål",
       y = "Antal kampe") +
  theme_minimal()

plot_HG_AG <- plot_HG + plot_AG 
plot_HG_AG

#Tjek om det forventede fra modellen stemmer overens med det faktiske data
actual_freq_HG <- DNK_filtered %>%
  count(HG) %>%
  rename(actual_count = n)

actual_freq_AG <- DNK_filtered %>%
  count(AG) %>%
  rename(actual_count = n)

#Tilføjer forventede værdier op til 20 mål og samler dem
# For HG
poisson_data_HG_20 <- data.frame(
  x = 0:20,
  prob = dpois((0:20), lambda = mean_HG) * nrow(DNK_filtered)
)

# For AG
poisson_data_AG_20 <- data.frame(
  x = 0:20,
  prob = dpois((0:20), lambda = mean_AG) * nrow(DNK_filtered)
)

#Samler antal mål 5+ sammen for HG 
#Samler for observerede
actual_freq_HG_samlet <- actual_freq_HG %>%
  mutate(
    HG_lumped = ifelse(HG >= 5, "5+", as.character(HG))
  ) %>%
  group_by(HG_lumped) %>%
  summarise(
    actual_count = sum(actual_count)
  ) %>%
  ungroup()

#Samler for forventede
poisson_data_HG_samlet <- poisson_data_HG_20 %>%
  mutate(
    x_lumped = ifelse(x >= 5, "5+", as.character(x))
  ) %>%
  group_by(x_lumped) %>%
  summarise(
    prob = sum(prob)
  ) %>%
  ungroup()

#Samler antal mål 5+ sammen for AG 
#Samler for observerede
actual_freq_AG_samlet <- actual_freq_AG %>%
  mutate(
    AG_lumped = ifelse(AG >= 5, "5+", as.character(AG))
  ) %>%
  group_by(AG_lumped) %>%
  summarise(
    actual_count = sum(actual_count)
  ) %>%
  ungroup()

#Samler for forventede
poisson_data_AG_samlet <- poisson_data_AG_20 %>%
  mutate(
    x_lumped = ifelse(x >= 5, "5+", as.character(x))
  ) %>%
  group_by(x_lumped) %>%
  summarise(
    prob = sum(prob)
  ) %>%
  ungroup()

#Standardiserede residualer for HG OG AG
HG_resid <- actual_freq_HG_samlet %>%
  rename(kategori = HG_lumped) %>%
  inner_join(poisson_data_HG_samlet %>% rename(kategori = x_lumped), by = "kategori") %>%
  mutate(
    std_resid = (actual_count - prob) / sqrt(prob),
    type = "Hjemmemål"
  )

AG_resid <- actual_freq_AG_samlet %>%
  rename(kategori = AG_lumped) %>%
  inner_join(poisson_data_AG_samlet %>% rename(kategori = x_lumped), by = "kategori") %>%
  mutate(
    std_resid = (actual_count - prob) / sqrt(prob),
    type = "Udemål"
  )

# Kombiner
residual_data <- bind_rows(HG_resid, AG_resid)

# Plot
ggplot(residual_data, aes(x = kategori, y = std_resid, fill = type)) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = c(-2, 2), color = "red", linetype = "dashed") +
  labs(
    title = "Standardiserede residualer for hjemme- og udemål",
    x = "Antal mål",
    y = "Standardiseret residual",
    fill = "Type"
  ) +
  theme_minimal()

#p-test for AG
# Vektorer
obs <- actual_freq_AG_samlet$actual_count
exp <- poisson_data_AG_samlet$prob

# Chi^2
chi_sq <- sum((obs - exp)^2 / exp)

# Frihedsgrader
k  <- length(obs)
df <- k - 1

# p-værdi
p_val <- 1 - pchisq(chi_sq, df)

cat("Chi-squared =", chi_sq, ", df =", df, ", p =", p_val, "\n")

#P-test for HG
# Vektorer
obs <- actual_freq_HG_samlet$actual_count
exp <- poisson_data_HG_samlet$prob

# Chi^2
chi_sq <- sum((obs - exp)^2 / exp)

# Frihedsgrader
k  <- length(obs)
df <- k - 1

# p-værdi
p_val <- 1 - pchisq(chi_sq, df)

cat("Chi-squared =", chi_sq, ", df =", df, ", p =", p_val, "\n")

