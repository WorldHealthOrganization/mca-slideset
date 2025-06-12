library(arcgisbinding)
arc.check_product()

x <- arc.open("C:/Users/kpeve/OneDrive - London School of Hygiene and Tropical Medicine/My OneDrive Documents/WHO/Slideset/WHO_Map_Template.aptx")

x <- arc.open("C:/Users/kpeve/OneDrive - London School of Hygiene and Tropical Medicine/My OneDrive Documents/WHO/data/WHOmap_generalisedBoundaries.gdb")

x@.info$gdb_version
