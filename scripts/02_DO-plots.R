# DO data exploratory plots

library(readr)
library(ggplot2)

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
