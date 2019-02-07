
######################################################################################

# Libraries

library(NVEDATA)
library(lubridate)

# Select stations from Bruksomrade

metadata <- get_metadata()

isel <- which(metadata$br23_HBV == "Y" |
              metadata$br6_Klimastudier == "Y" |
              metadata$br33_Flomkart_aktive_ureg == "Y")

metadata <- metadata[isel, ]

# Select stations without reservoirs and hydropower

isel <- which(metadata$regulation_part_area == 0 &
                metadata$regulation_part_reservoirs == 0 &
                is.na(metadata$first_year_regulation))

metadata <- metadata[isel, ]

# Select stations without glacier

isel <- which(metadata$perc_glacier < 2)

metadata <- metadata[isel, ]

regine_main <- metadata$regine_main

write.table(metadata, file = "metadata.txt", quote = TRUE, sep = ";", row.names = FALSE)

# Select time period

time_vec <- seq(ymd("2000-01-01"), ymd("2013-12-31"), by = "days")


######################################################################################

# # Process senorge v1.0 data
#
# # Download data
#
# senorge <- 'v1.0'
#
# data_daily <- load_data(senorge, regine_main, time_vec)
#
# # Average data
#
# data_yearly <- day_to_year(data_daily)
#
# # Save data
#
# write_to_text(data_daily, "C:/Work/Avrenningskart/senorge_analysis/senorge_daily_v10")
#
# write_to_text(data_yearly, "C:/Work/Avrenningskart/senorge_analysis/senorge_yearly_v10")
#
# save(data_daily, file = "senorge_daily_v10.RData")
#
# save(data_yearly, file = "senorge_yearly_v10.RData")


######################################################################################

# # Process senorge v2.0 data
#
# # Download data
#
# senorge <- 'v2.0'
#
# data_daily <- load_data(senorge, regine_main, time_vec)
#
# # Average data
#
# data_yearly <- day_to_year(data_daily)
#
# # Save data
#
# write_to_text(data_daily, "C:/Work/Avrenningskart/senorge_analysis/senorge_daily_v20")
#
# write_to_text(data_yearly, "C:/Work/Avrenningskart/senorge_analysis/senorge_yearly_v20")
#
# save(data_daily, file = "senorge_daily_v20.RData")
#
# save(data_yearly, file = "senorge_yearly_v20.RData")


######################################################################################

# # Process senorge v2.2 data
#
# # Download data
#
# senorge <- 'v2.2'
#
# data_daily <- load_data(senorge, regine_main, time_vec)
#
# # Average data
#
# data_yearly <- day_to_year(data_daily)
#
# # Save data
#
# write_to_text(data_daily, "C:/Work/Avrenningskart/senorge_analysis/senorge_daily_v22")
#
# write_to_text(data_yearly, "C:/Work/Avrenningskart/senorge_analysis/senorge_yearly_v22")
#
# save(data_daily, file = "senorge_daily_v22.RData")
#
# save(data_yearly, file = "senorge_yearly_v22.RData")



######################################################################################

# Process senorge 2018 data

# Download data

senorge <- 'seNorge2018'

data_daily <- load_data(senorge, regine_main, time_vec)

# Average data

data_yearly <- day_to_year(data_daily)

# Save data

write_to_text(data_daily, "C:/Work/Avrenningskart/senorge_analysis/senorge_daily_seNorge2018")

write_to_text(data_yearly, "C:/Work/Avrenningskart/senorge_analysis/senorge_yearly_seNorge2018")

save(data_daily, file = "senorge_daily_seNorge2018.RData")

save(data_yearly, file = "senorge_yearly_seNorge2018.RData")



