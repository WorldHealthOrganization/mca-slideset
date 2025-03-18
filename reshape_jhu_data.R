
PointEstimates5to9_Regional <- read_csv("C:/Users/kpeve/OneDrive - London School of Hygiene and Tropical Medicine/My OneDrive Documents/WHO/data/raw/JHU_MCEE/PointEstimates5to9-Regional.csv")

PointEstimates5to9_Regional_long <- PointEstimates5to9_Regional %>% pivot_longer(c(8:23), names_to="cause", values_to="cause_fraction")

PointEstimates10to14_Regional <- read_csv("C:/Users/kpeve/OneDrive - London School of Hygiene and Tropical Medicine/My OneDrive Documents/WHO/data/raw/JHU_MCEE/PointEstimates10to14-Regional.csv")

PointEstimates10to14_Regional_long <- PointEstimates10to14_Regional %>% pivot_longer(c(8:21), names_to="cause", values_to="cause_fraction")

PointEstimates15to19_Regional <- read_csv("C:/Users/kpeve/OneDrive - London School of Hygiene and Tropical Medicine/My OneDrive Documents/WHO/data/raw/JHU_MCEE/PointEstimates15to19-Regional.csv")

PointEstimates15to19_Regional_long <- PointEstimates15to19_Regional %>% pivot_longer(c(8:22), names_to="cause", values_to="cause_fraction")

PointEstimates5to19_Regional_long <- rbind(PointEstimates5to9_Regional_long, PointEstimates10to14_Regional_long, PointEstimates15to19_Regional_long)

write_csv(PointEstimates5to19_Regional_long, "C:/Users/kpeve/OneDrive - London School of Hygiene and Tropical Medicine/My OneDrive Documents/WHO/data/raw/JHU_MCEE/PointEstimates5to19_Regional_long.csv")
