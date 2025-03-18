library(data.table)
library(arrow)

Cause_specific_ranking_AllYears <- fread("data/raw/Cause_specific_ranking_AllYears.csv")
write_parquet(Cause_specific_ranking_AllYears,"data/raw/Cause_specific_ranking_AllYears.parquet", compression = "lz4")

# Cause_specific_ranking_AllYears <- fread("C:/Users/lopezg/OneDrive - World Health Organization/MCA Data Portal/2025 Updates/2025-02 GHE and Slides/regina/2021/small.csv")
# write_parquet(Cause_specific_ranking_AllYears,"C:/Users/lopezg/OneDrive - World Health Organization/MCA Data Portal/2025 Updates/2025-02 GHE and Slides/regina/2021/Cause_specific_ranking_AllYears.parquet", compression = "lz4")