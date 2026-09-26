# DO data exploratory plots

library(readr)
library(ggplot2)
library(dplyr)

# read csv
do <- read_csv("data/processed/merged DO data.csv")

summary(do)

# no tengo info de HOS o PON, remove for now
do <- do %>%
  filter(!site == "PON" & !site == "HOS")

# add mainstem vs HW to each site

# LLA (Llavina) = HW
# SFE (Santa Fe) = HW
# CAS (Castanyet) = intermittent HW
# REG (Font del Regas) = pristine + perennial HW
# FUI (Fuirosos) = pristine + intermittent HW

# SMP (Santa Maria de Palautordera)  mainstem, 800m downstream of WWTP
# TFU (Fuirosos Tributary) = mainstem, 100m downstream of Fuirosos-Tordera confluence
# CEL (Sant Celoni) = mainstem, downstream of SMP, ACA gauging station
# TAR (Hostalric) = mainstem
# FOG (Fogars) = mainstem, ACA gauging station

do <- do %>%
  mutate(
    HW_MS = case_when(
      site == "LLA" ~ "HW",
      site == "SFE" ~ "HW",
      site == "CAS" ~ "HW",
      site == "REG" ~ "HW",
      site == "FUI" ~ "HW",
      site == "SMP" ~ "MS",
      site == "TFU" ~ "MS",
      site == "CEL" ~ "MS",
      site == "TAR" ~ "MS",
      site == "FOG" ~ "MS",
      TRUE ~ NA_character_
    )
  )

# make plots 

# plot DO data
ggplot(do, aes(x=local_datetime, y=do_mgL, color = site)) +
  geom_point() +
  facet_wrap(~HW_MS, ncol = 2)

# want to loop through sites, make a plot, and save that plot

sites <- unique(do$site)

date_lims <- range(do$local_datetime, na.rm = TRUE)

for (s in sites) {
  
  # find the scaling factor so temp_c maps onto a similar range as do_mgL
  scale_factor <- max(do$do_mgL, na.rm = TRUE) / max(do$temp_c, na.rm = TRUE)
  
  p <- do %>%
    filter(site == s) %>%
    ggplot(aes(x = local_datetime)) +
    geom_point(aes(y = do_mgL), size = 0.5) +
    geom_point(aes(y = temp_c * scale_factor), size = 0.5, color = "blue") +
    geom_hline(yintercept = 2, linetype = "dashed") +
    scale_x_datetime(limits = date_lims) +
    scale_y_continuous(
      name = "DO (mg/L)",
      sec.axis = sec_axis(~ . / scale_factor, name = "Temperature (°C)")
    ) +
    labs(
      title = paste0("DO time-series in ", s),
      x = "Date"
    ) +
    theme(
      axis.title.y.right = element_text(color = "blue"),
      axis.text.y.right = element_text(color = "blue")   # optional: color the tick labels too
    )
  
  ggsave(
    filename = file.path("graphs", paste0("DO_timeseries_", s, ".png")),
    plot = p,
    width = 8, height = 5, dpi = 300
  )
}
