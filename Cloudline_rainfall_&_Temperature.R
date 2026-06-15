# ============================================================
# Figure: Cloudline with rainfall and temperature
# Panel A = Rainfall
# Panel B = Temperature
# Journal-quality export: 600 dpi TIFF + vector PDF
# ============================================================

library(readxl)
library(ggplot2)
library(dplyr)
library(patchwork)

# -----------------------------
# 1. Set working directory
# -----------------------------
setwd("C:/Users/Ayeshma/OneDrive - UBC (1)/Documents/Other/Biotropica/Final Figures/plotting/Climate")

# -----------------------------
# 2. Read Excel sheet 5
# -----------------------------
raw_data <- read_excel(
  "Cloudline_temperature_rainfall_data.xlsx",
  sheet = "Sheet5"
)

# -----------------------------
# 3. Clean column types
# -----------------------------
raw_data$Sampling_Session <- as.numeric(as.character(raw_data$Sampling_Session))
raw_data$Cloudline <- as.numeric(as.character(raw_data$Cloudline))
raw_data$rain <- as.numeric(as.character(raw_data$rain))
raw_data$Max <- round(as.numeric(as.character(raw_data$Max)), 2)
raw_data$Min <- round(as.numeric(as.character(raw_data$Min)), 2)
raw_data$Mean <- round(as.numeric(as.character(raw_data$Mean)), 2)

# -----------------------------
# 4. Count repeated cloudline values
# -----------------------------
cloud_data <- raw_data %>%
  count(Sampling_Session, Cloudline, name = "Freq")

# OPTIONAL:
# If you want to remove all cloudline = 2000 points, keep this line.
# If you want to show 2000 points, put # before this line.
cloud_data <- cloud_data %>%
  filter(Cloudline != 2000)

# -----------------------------
# 5. Separate monthly climate data
# This avoids repeated rainfall/temperature labels
# -----------------------------
climate_data <- raw_data %>%
  distinct(Sampling_Session, rain, Max, Min, Mean)

# -----------------------------
# 6. Common theme
# -----------------------------
theme_pub <- theme_bw(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.major = element_line(linewidth = 0.3),
    panel.grid.minor = element_line(linewidth = 0.2),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    plot.tag = element_text(size = 18, face = "bold"),
    plot.tag.position = c(0.02, 0.98),
    plot.margin = margin(8, 8, 8, 8)
  )

month_labels <- c("Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr")

# ============================================================
# Panel A: Rainfall plot
# ============================================================

p_rain <- ggplot() +
  geom_point(
    data = cloud_data,
    aes(x = Sampling_Session, y = Cloudline, size = Freq),
    color = "black"
  ) +
  geom_text(
    data = cloud_data,
    aes(x = Sampling_Session, y = Cloudline,
        label = paste0(Cloudline, " (", Freq, ")")),
    size = 3.3,
    vjust = 2.2
  ) +
  scale_size(range = c(5, 12)) +
  geom_line(
    data = climate_data,
    aes(x = Sampling_Session, y = rain / 0.035, group = 1),
    color = "black",
    linewidth = 0.3,
    linetype = "longdash"
  ) +
  geom_text(
    data = climate_data,
    aes(x = Sampling_Session, y = rain / 0.035, label = rain),
    hjust = 0.5,
    vjust = 2,
    size = 3,
    color = "black"
  ) +
  scale_x_continuous(
    breaks = 1:8,
    labels = month_labels
  ) +
  scale_y_continuous(
    name = "Cloudline Elevation (m)",
    limits = c(0, 2000),
    breaks = seq(0, 2000, by = 500),
    sec.axis = sec_axis(~ . * 0.035, name = "Rainfall (mm)")
  ) +
  labs(
    tag = "A",
    x = "Month",
    y = "Cloudline Elevation"
  ) +
  theme_pub

# ============================================================
# Panel B: Temperature plot
# ============================================================

p_temp <- ggplot() +
  geom_point(
    data = cloud_data,
    aes(x = Sampling_Session, y = Cloudline, size = Freq),
    color = "black"
  ) +
  geom_text(
    data = cloud_data,
    aes(x = Sampling_Session, y = Cloudline,
        label = paste0(Cloudline, " (", Freq, ")")),
    size = 3.3,
    vjust = 2.2
  ) +
  scale_size(range = c(5, 12)) +
  geom_line(
    data = climate_data,
    aes(x = Sampling_Session, y = (Max - 20) / 0.01, group = 1),
    color = "black",
    linewidth = 0.3,
    linetype = "solid"
  ) +
  geom_text(
    data = climate_data,
    aes(x = Sampling_Session, y = (Max - 20) / 0.01, label = Max),
    hjust = 0.5,
    vjust = 1.5,
    size = 3,
    color = "black"
  ) +
  geom_line(
    data = climate_data,
    aes(x = Sampling_Session, y = (Min - 20) / 0.01, group = 1),
    color = "black",
    linewidth = 0.3,
    linetype = "longdash"
  ) +
  geom_text(
    data = climate_data,
    aes(x = Sampling_Session, y = (Min - 20) / 0.01, label = Min),
    hjust = 0.5,
    vjust = -0.5,
    size = 3,
    color = "black"
  ) +
  geom_line(
    data = climate_data,
    aes(x = Sampling_Session, y = (Mean - 20) / 0.01, group = 1),
    color = "black",
    linewidth = 0.3,
    linetype = "dotted"
  ) +
  geom_text(
    data = climate_data,
    aes(x = Sampling_Session, y = (Mean - 20) / 0.01, label = Mean),
    hjust = 1,
    vjust = -0.5,
    size = 3,
    color = "black"
  ) +
  scale_x_continuous(
    breaks = 1:8,
    labels = month_labels
  ) +
  scale_y_continuous(
    name = "Cloudline Elevation (m)",
    limits = c(0, 2000),
    breaks = seq(0, 2000, by = 500),
    sec.axis = sec_axis(~ . * 0.01 + 20, name = "Temperature (°C)")
  ) +
  labs(
    tag = "B",
    x = "Month",
    y = "Cloudline Elevation"
  ) +
  theme_pub

# ============================================================
# 7. Combine A and B side by side
# ============================================================

figure_AB <- p_rain + p_temp +
  plot_layout(ncol = 2)

print(figure_AB)

# ============================================================
# 8. Save journal-quality files
# ============================================================

ggsave(
  filename = "Figure_AB_Rainfall_Temperature_600dpi.tiff",
  plot = figure_AB,
  width = 14,
  height = 5,
  units = "in",
  dpi = 600,
  compression = "lzw"
)

ggsave(
  filename = "Figure_AB_Rainfall_Temperature_300dpi.tiff",
  plot = figure_AB,
  width = 14,
  height = 5,
  units = "in",
  dpi = 300,
  compression = "lzw"
)

ggsave(
  filename = "Figure_AB_Rainfall_Temperature_vector.pdf",
  plot = figure_AB,
  width = 14,
  height = 5,
  units = "in"
)