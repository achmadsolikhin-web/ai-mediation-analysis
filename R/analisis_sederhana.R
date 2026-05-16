# ANALISIS  SEDERHANA
# AI Dependency -> Burnout -> Career Readiness
# Metode: Baron & Kenny

library(tidyverse)

# Setup
dir.create("output", showWarnings = FALSE)

# Fungsi untuk menampilkan tabel dengan rapi
coef_table <- function(model) {
  as_tibble(summary(model)$coefficients, rownames = "term") |>
    mutate(across(where(is.numeric), ~ round(.x, 4)))
}

# Impor data
df <- read_csv("C:/AI-Dependency/data/ai_dependency_career_anxiety_students.csv", show_col_types = FALSE)

cat("Baris awal:", nrow(df), "\n")
cat("Kolom awal:", ncol(df), "\n")

# Select Variabel yang akan digunakan dan Data cleaning
data_med <- df |>
  select(ai_dependency_score,
         burnout_score,
         overall_career_readiness_score,
         stream) |>
  drop_na()

cat("Baris setelah drop NA:", nrow(data_med), "\n\n")

# Deskripsi data
summary_stats <- data_med |>
  summarise(
    n = n(),
    mean_ai = mean(ai_dependency_score),
    sd_ai = sd(ai_dependency_score),
    mean_burnout = mean(burnout_score),
    sd_burnout = sd(burnout_score),
    mean_career = mean(overall_career_readiness_score),
    sd_career = sd(overall_career_readiness_score)
  ) |>
  mutate(across(where(is.numeric), ~ round(.x, 3)))

print(summary_stats)

# Visuaslisasi
# Distribusi tiga variabel utama
plot_dist <- data_med |>
  pivot_longer(
    cols = c(ai_dependency_score, burnout_score, overall_career_readiness_score),
    names_to = "variable",
    values_to = "value"
  ) |>
  ggplot(aes(x = value)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white") +
  facet_wrap(~ variable, scales = "free_x", ncol = 1) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Distribusi Variabel Utama",
    x = NULL,
    y = "Frekuensi"
  )

ggsave("C:/AI-Dependency/output/01_distribusi_variabel.png", plot_dist, width = 8, height = 9, dpi = 150)

# B. Path a: AI dependency -> burnout
plot_a <- ggplot(data_med, aes(x = ai_dependency_score, y = burnout_score)) +
  geom_point(alpha = 0.15) +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Path a: AI Dependency -> Burnout",
    x = "AI Dependency Score",
    y = "Burnout Score"
  )

ggsave("C:/AI-Dependency/output/02_path_a.png", plot_a, width = 7, height = 5, dpi = 150)

# C. Path b: burnout -> career readiness
plot_b <- ggplot(data_med, aes(x = burnout_score, y = overall_career_readiness_score)) +
  geom_point(alpha = 0.15) +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Path b: Burnout -> Career Readiness",
    x = "Burnout Score",
    y = "Career Readiness Score"
  )

ggsave("C:/AI-Dependency/output/03_path_b.png", plot_b, width = 7, height = 5, dpi = 150)

# Regresi Mediasi
# Model 1: total effect (c)
model_c <- lm(overall_career_readiness_score ~ ai_dependency_score, data = data_med)

# Model 2: path a
model_a <- lm(burnout_score ~ ai_dependency_score, data = data_med)

# Model 3: path b dan direct effect (c')
model_cprime <- lm(overall_career_readiness_score ~ ai_dependency_score + burnout_score,
                   data = data_med)

cat("\n=== MODEL c: total effect ===\n")
print(coef_table(model_c))

cat("\n=== MODEL a: AI dependency -> burnout ===\n")
print(coef_table(model_a))

cat("\n=== MODEL c': AI dependency + burnout -> career readiness ===\n")
print(coef_table(model_cprime))

# ─Hitung Inderect Effect
a <- coef(model_a)["ai_dependency_score"]
b <- coef(model_cprime)["burnout_score"]
c_total <- coef(model_c)["ai_dependency_score"]
c_prime <- coef(model_cprime)["ai_dependency_score"]

indirect <- a * b
prop_mediated <- (indirect / c_total) * 100
reduction <- ((c_total - c_prime) / c_total) * 100

result_mediasi <- tibble(
  path = c("c total", "a", "b", "c prime", "indirect a*b", "prop mediated (%)", "reduction (%)"),
  value = c(c_total, a, b, c_prime, indirect, prop_mediated, reduction)
) |>
  mutate(across(where(is.numeric), ~ round(.x, 4)))

cat("\n=== RINGKASAN MEDIASI ===\n")
print(result_mediasi)

# Kesimpulan akhir
cat("\n============================================================\n")
cat("KESIMPULAN AKHIR\n")
cat("AI dependency berhubungan negatif dengan career readiness.\n")
cat("Sebagian efek itu bekerja melalui burnout.\n")
cat("Jika indirect effect bernilai negatif dan c' masih berbeda dari nol,\n")
cat("maka mediasi bersifat parsial, bukan penuh.\n")
cat("============================================================\n")
