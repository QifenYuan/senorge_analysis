

# Required libraries ------------------------------------------------------

rm(list = ls())

library(ggplot2)
library(lubridate)
library(gridExtra)
library(rgdal)
require(plyr)
library(gpclib)
library(tidyr)


# Function for preparing data

prepare_data <- function(data_daily, data_yearly, stats_keep)  {

  time_vec <- data_daily[[1]]$time_vec

  regine_main <- sapply(data_daily, function(x) x$regine_main)

  qobs <- sapply(data_daily, function(x) x$Runoff)

  prec <- sapply(data_daily, function(x) x$Prec)

  aet <- sapply(data_daily, function(x) x$aet_mean)

  r_prec_qobs <- sapply(data_yearly, function(x) cor(x$Runoff, x$Prec))

  colnames(qobs) <- regine_main

  colnames(prec) <- regine_main

  names(aet) <- regine_main

  names(r_prec_qobs) <- regine_main

  keep <- regine_main[regine_main %in% stats_keep$V1]

  qobs <- qobs[ , keep]

  prec <- prec[ , keep]

  aet <- aet[keep]

  r_prec_qobs <- r_prec_qobs[keep]

  nyear <- length(unique(year(time_vec)))

  df_all <- data.frame(prec = colSums(prec)/nyear, qobs = colSums(qobs)/nyear, aet = aet, r_prec_qobs = r_prec_qobs)

  return(df_all)

}


# Function for collecting metadata

get_metadata <- function(data_daily, stats_keep)  {

  regine_main <- sapply(data_daily, function(x) x$regine_main)

  area_total <- sapply(data_daily, function(x) x$metadata$area_total)

  utm_east <- sapply(data_daily, function(x) x$metadata$utm_east_z33)

  utm_north <- sapply(data_daily, function(x) x$metadata$utm_north_z33)

  names(regine_main) <- regine_main

  names(area_total) <- regine_main

  names(utm_east) <- regine_main

  names(utm_north) <- regine_main

  keep <- regine_main[regine_main %in% stats_keep$V1]

  regine_main <- regine_main[keep]

  area_total <- area_total[keep]

  utm_east <- utm_east[keep]

  utm_north <- utm_north[keep]

  df_meta <- data.frame(area_total = area_total, utm_east = utm_east, utm_north = utm_north)

  return(df_meta)

}


# Function for regression statistics

lm_eqn = function(m) {

  l <- list(a = format(coef(m)[1], digits = 2),
            b = format(abs(coef(m)[2]), digits = 2),
            r2 = format(summary(m)$r.squared, digits = 2));

  if (coef(m)[2] >= 0)  {
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
  } else {
    eq <- substitute(italic(y) == a - b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
  }

  as.character(as.expression(eq));
}


# Prepare data ------------------------------------------------------------


# Prepare daily data for seNorge v1.0

load("senorge_daily_v10.RData")

load("senorge_yearly_v10.RData")

stats_keep <- read.table("stats_keep.txt", colClasses = "character")

df_v10 <- prepare_data(data_daily, data_yearly, stats_keep)

colnames(df_v10) <- c("prec_v10", "qobs_v10", "aet_v10", "r_prec_qobs_v10")


# Prepare daily data for seNorge v2.0

load("senorge_daily_v20.RData")

load("senorge_yearly_v20.RData")

stats_keep <- read.table("stats_keep.txt", colClasses = "character")

df_v20 <- prepare_data(data_daily, data_yearly, stats_keep)

colnames(df_v20) <- c("prec_v20", "qobs_v20", "aet_v20", "r_prec_qobs_v20")


# Prepare daily data for seNorge v2.2

load("senorge_daily_v22.RData")

load("senorge_yearly_v22.RData")

stats_keep <- read.table("stats_keep.txt", colClasses = "character")

df_v22 <- prepare_data(data_daily, data_yearly, stats_keep)

colnames(df_v22) <- c("prec_v22", "qobs_v22", "aet_v22", "r_prec_qobs_v22")


# Prepare daily data for seNorge 2018

load("senorge_daily_seNorge2018.RData")

load("senorge_yearly_seNorge2018.RData")

stats_keep <- read.table("stats_keep.txt", colClasses = "character")

df_v2018 <- prepare_data(data_daily, data_yearly, stats_keep)

colnames(df_v2018) <- c("prec_v2018", "qobs_v2018", "aet_v2018", "r_prec_qobs_v2018")


# Get metadata

df_meta <- get_metadata(data_daily, stats_keep)


# Combine all data frames

df_all <- cbind(df_meta, df_v10, df_v20, df_v22, df_v2018)

df_all <- na.omit(df_all)

# Save data for Cristian

save(df_all, file = "data_for_cristian.RData")


# Scatter plots with discharge, precipitation -----------------------------

limit <- max(df_all[c("prec_v10", "prec_v20", "prec_v22", "prec_v2018",
                      "qobs_v10", "qobs_v20", "qobs_v22", "qobs_v2018")]) + 50

p_v10 <- ggplot(data = df_all, aes(x = qobs_v10, y = prec_v10)) +
  geom_point() +
  scale_y_continuous(limits = c(0, limit)) +
  xlim(c(0, limit)) +
  xlab("Runoff (mm/year)") +
  ylab("Precipitaton (mm/year)") +
  ggtitle("seNorge v1.0") +
  geom_smooth(method = 'lm', formula = y ~ x, se = FALSE) +
  annotate("text", x = 3200, y = 300, label = lm_eqn(lm(prec_v10 ~ qobs_v10, df_all)), parse = TRUE, size=4) +
  theme_bw(base_size = 10)

p_v20 <- ggplot(data = df_all, aes(x = qobs_v20, y = prec_v20)) +
  geom_point() +
  scale_y_continuous(limits = c(0,limit)) +
  xlim(c(0, limit)) +
  xlab("Runoff (mm/year)") +
  ylab("Precipitaton (mm/year)") +
  ggtitle("seNorge v2.0") +
  geom_smooth(method = 'lm', formula = y ~ x, se = FALSE) +
  annotate("text", x = 3200, y = 300, label = lm_eqn(lm(prec_v20 ~ qobs_v10, df_all)), parse = TRUE, size=4) +
  theme_bw(base_size = 10)

p_v22 <- ggplot(data = df_all, aes(x = qobs_v22, y = prec_v22)) +
  geom_point() +
  scale_y_continuous(limits = c(0,limit)) +
  xlim(c(0, limit)) +
  xlab("Runoff (mm/year)") +
  ylab("Precipitaton (mm/year)") +
  ggtitle("seNorge v2.2") +
  geom_smooth(method = 'lm', formula = y ~ x, se = FALSE) +
  annotate("text", x = 3200, y = 300, label = lm_eqn(lm(prec_v22 ~ qobs_v10, df_all)), parse = TRUE, size=4) +
  theme_bw(base_size = 10)

p_v2018 <- ggplot(data = df_all, aes(x = qobs_v2018, y = prec_v2018)) +
  geom_point() +
  scale_y_continuous(limits = c(0,limit)) +
  xlim(c(0, limit)) +
  xlab("Runoff (mm/year)") +
  ylab("Precipitaton (mm/year)") +
  ggtitle("seNorge v2018") +
  geom_smooth(method = 'lm', formula = y ~ x, se = FALSE) +
  annotate("text", x = 3200, y = 300, label = lm_eqn(lm(prec_v2018 ~ qobs_v10, df_all)), parse = TRUE, size=4) +
  theme_bw(base_size = 10)

grid.arrange(p_v10, p_v20, p_v22, p_v2018, ncol = 2, nrow = 2)

g <- arrangeGrob(p_v10, p_v20, p_v22, p_v2018, ncol = 2, nrow = 2)

ggsave(file = "figures/scatter_prec_runoff.png", g, units = "cm", width = 20, height = 20)


# Scatter plots with discharge, precipitation, actual evapotranspi --------

df_wb <- data.frame(in_v10 = df_all$prec_v10, out_v10 = (df_all$qobs_v10 + df_all$aet_v10),
                    in_v20 = df_all$prec_v20, out_v20 = (df_all$qobs_v20 + df_all$aet_v20),
                    in_v22 = df_all$prec_v22, out_v22 = (df_all$qobs_v22 + df_all$aet_v22),
                    in_v2018 = df_all$prec_v2018, out_v2018 = (df_all$qobs_v2018 + df_all$aet_v2018))

limit <- max(df_wb) + 50

p_v10 <- ggplot(data = df_wb, aes(x = out_v10, y = in_v10)) +
  geom_point() +
  scale_y_continuous(limits = c(0, limit)) +
  xlim(c(0, limit)) +
  xlab("Runoff + evapotranspiration (mm/year)") +
  ylab("Precipitaton (mm/year)") +
  ggtitle("seNorge v1.0") +
  geom_smooth(method='lm', formula = y ~ x, se = FALSE) +
  annotate("text", x = 3200, y = 100, label = lm_eqn(lm(in_v10 ~ out_v10, df_wb)), parse = TRUE, size=4) +
  theme_bw(base_size = 10)

p_v20 <- ggplot(data = df_wb, aes(x = out_v20, y = in_v20)) +
  geom_point() +
  scale_y_continuous(limits = c(0, limit)) +
  xlim(c(0, limit)) +
  xlab("Runoff + evapotranspiration (mm/year)") +
  ylab("Precipitaton (mm/year)") +
  ggtitle("seNorge v2.0") +
  geom_smooth(method='lm', formula = y ~ x, se = FALSE) +
  annotate("text", x = 3200, y = 100, label = lm_eqn(lm(in_v20 ~ out_v20, df_wb)), parse = TRUE, size=4) +
  theme_bw(base_size = 10)

p_v22 <- ggplot(data = df_wb, aes(x = out_v22, y = in_v22)) +
  geom_point() +
  scale_y_continuous(limits = c(0, limit)) +
  xlim(c(0, limit)) +
  xlab("Runoff + evapotranspiration (mm/year)") +
  ylab("Precipitaton (mm/year)") +
  ggtitle("seNorge v2.2") +
  geom_smooth(method='lm', formula = y ~ x, se = FALSE) +
  annotate("text", x = 3200, y = 100, label = lm_eqn(lm(in_v22 ~ out_v20, df_wb)), parse = TRUE, size=4) +
  theme_bw(base_size = 10)

p_v2018 <- ggplot(data = df_wb, aes(x = out_v2018, y = in_v2018)) +
  geom_point() +
  scale_y_continuous(limits = c(0, limit)) +
  xlim(c(0, limit)) +
  xlab("Runoff + evapotranspiration (mm/year)") +
  ylab("Precipitaton (mm/year)") +
  ggtitle("seNorge v2018") +
  geom_smooth(method='lm', formula = y ~ x, se = FALSE) +
  annotate("text", x = 3200, y = 100, label = lm_eqn(lm(in_v2018 ~ out_v2018, df_wb)), parse = TRUE, size=4) +
  theme_bw(base_size = 10)

grid.arrange(p_v10, p_v20, p_v22, p_v2018, ncol = 2, nrow = 2)

g <- arrangeGrob(p_v10, p_v20, p_v22, p_v2018, ncol = 2, nrow = 2)

ggsave(file = "figures/scatter_water_balance.png", g, units = "cm", width = 20, height = 20)



# Plot map with ratio between precipitation and runoff --------------------

# Prepare shape file

no_df <- readOGR(dsn = "norway_shapefiles" , layer = "norge")

# Categorise data

pbreaks <- c(0.0, 0.8, 1.2, 1.6, 2.0, 3.0)

df_map <- data.frame(utm_east = df_all$utm_east,
                     utm_north = df_all$utm_north,
                     ratio_v10 = cut(df_all$prec_v10/df_all$qobs_v10,pbreaks),
                     ratio_v20 = cut(df_all$prec_v20/df_all$qobs_v20,pbreaks),
                     ratio_v22 = cut(df_all$prec_v22/df_all$qobs_v22,pbreaks),
                     ratio_v2018 = cut(df_all$prec_v2018/df_all$qobs_v2018,pbreaks))

# Plot data

cbPalette <- c('#5060E8', '#91bfdb','#fee090','#fc8d59','#d73027')

labels = c("< 0.8", "0.8 - 1.2", "1.2 - 1.6", "1.6 - 2.0", "> 2.0")

map1 <- ggplot(df_map, aes(x = utm_east, y = utm_north)) +
  geom_polygon(data = no_df, aes(long, lat , group=group), fill = "white", color = "gray") +
  geom_point(aes(colour = ratio_v10), size = 2) +   # Color according to senorge version
  geom_point(shape = 1, size = 2, colour = "black") +
  theme_classic(base_size = 10) +
  scale_colour_manual(values = cbPalette, drop = FALSE, labels = labels) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  coord_fixed(ratio = 1) +
  theme(legend.position = c(0.7,0.4), legend.title=element_blank(), legend.text = element_text(size = 12)) +
  ggtitle("seNorge v1.0")

map2 <- ggplot(df_map, aes(x = utm_east, y = utm_north)) +
  geom_polygon(data = no_df, aes(long, lat , group=group), fill = "white", color = "gray") +
  geom_point(aes(colour = ratio_v20), size = 2) +   # Color according to senorge version
  geom_point(shape = 1, size = 2, colour = "black") +
  theme_classic(base_size = 10) +
  scale_colour_manual(values = cbPalette, drop = FALSE, labels = labels) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  coord_fixed(ratio = 1) +
  theme(legend.position = c(0.7,0.4), legend.title=element_blank(), legend.text = element_text(size = 12)) +
  ggtitle("seNorge v2.0")

map3 <- ggplot(df_map, aes(x = utm_east, y = utm_north)) +
  geom_polygon(data = no_df, aes(long, lat , group=group), fill = "white", color = "gray") +
  geom_point(aes(colour = ratio_v22), size = 2) +   # Color according to senorge version
  geom_point(shape = 1, size = 2, colour = "black") +
  theme_classic(base_size = 10) +
  scale_colour_manual(values = cbPalette, drop = FALSE, labels = labels) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  coord_fixed(ratio = 1) +
  theme(legend.position = c(0.7,0.4), legend.title=element_blank(), legend.text = element_text(size = 12)) +
  ggtitle("seNorge v2.2")

map4 <- ggplot(df_map, aes(x = utm_east, y = utm_north)) +
  geom_polygon(data = no_df, aes(long, lat , group=group), fill = "white", color = "gray") +
  geom_point(aes(colour = ratio_v2018), size = 2) +   # Color according to senorge version
  geom_point(shape = 1, size = 2, colour = "black") +
  theme_classic(base_size = 10) +
  scale_colour_manual(values = cbPalette, drop = FALSE, labels = labels) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  coord_fixed(ratio = 1) +
  theme(legend.position = c(0.7,0.4), legend.title=element_blank(), legend.text = element_text(size = 12)) +
  ggtitle("seNorge v2018")

grid.arrange(map1, map2, map3, map4, ncol = 2, nrow = 2)

g <- arrangeGrob(map1, map2, map3, map4, ncol = 2, nrow = 2)

ggsave(file = "figures/map_prec_div_runoff.png", g, units = "cm", width = 20, height = 20)


# Plot map with ratio between precipitation and runoff --------------------

# Prepare shape file

no_df <- readOGR(dsn = "norway_shapefiles" , layer = "norge")

# Categorise data

pbreaks <- c(0.0, 0.8, 1.2, 1.6, 2.0, 3.0)

df_map <- data.frame(utm_east = df_all$utm_east,
                     utm_north = df_all$utm_north,
                     ratio_v10 = cut(df_all$prec_v10/(df_all$qobs_v10 + df_all$aet_v10),pbreaks),
                     ratio_v20 = cut(df_all$prec_v20/(df_all$qobs_v20 + df_all$aet_v20),pbreaks),
                     ratio_v22 = cut(df_all$prec_v22/(df_all$qobs_v22 + df_all$aet_v22),pbreaks),
                     ratio_v2018 = cut(df_all$prec_v2018/(df_all$qobs_v2018 + df_all$aet_v2018),pbreaks))

# Plot data

cbPalette <- c('#5060E8', '#91bfdb','#fee090','#fc8d59','#d73027')

labels = c("< 0.8", "0.8 - 1.2", "1.2 - 1.6", "1.6 - 2.0", "> 2.0")

map1 <- ggplot(df_map, aes(x = utm_east, y = utm_north)) +
  geom_polygon(data = no_df, aes(long, lat , group=group), fill = "white", color = "gray") +
  geom_point(aes(colour = ratio_v10), size = 2) +   # Color according to senorge version
  geom_point(shape = 1, size = 2, colour = "black") +
  theme_classic(base_size = 10) +
  scale_colour_manual(values = cbPalette, drop = FALSE, labels = labels) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  coord_fixed(ratio = 1) +
  theme(legend.position = c(0.7,0.4), legend.title=element_blank(), legend.text = element_text(size = 12)) +
  ggtitle("seNorge v1.0")

map2 <- ggplot(df_map, aes(x = utm_east, y = utm_north)) +
  geom_polygon(data = no_df, aes(long, lat , group=group), fill = "white", color = "gray") +
  geom_point(aes(colour = ratio_v20), size = 2) +   # Color according to senorge version
  geom_point(shape = 1, size = 2, colour = "black") +
  theme_classic(base_size = 10) +
  scale_colour_manual(values = cbPalette, drop = FALSE, labels = labels) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  coord_fixed(ratio = 1) +
  theme(legend.position = c(0.7,0.4), legend.title=element_blank(), legend.text = element_text(size = 12)) +
  ggtitle("seNorge v2.0")

map3 <- ggplot(df_map, aes(x = utm_east, y = utm_north)) +
  geom_polygon(data = no_df, aes(long, lat , group=group), fill = "white", color = "gray") +
  geom_point(aes(colour = ratio_v22), size = 2) +   # Color according to senorge version
  geom_point(shape = 1, size = 2, colour = "black") +
  theme_classic(base_size = 10) +
  scale_colour_manual(values = cbPalette, drop = FALSE, labels = labels) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  coord_fixed(ratio = 1) +
  theme(legend.position = c(0.7,0.4), legend.title=element_blank(), legend.text = element_text(size = 12)) +
  ggtitle("seNorge v2.2")

map4 <- ggplot(df_map, aes(x = utm_east, y = utm_north)) +
  geom_polygon(data = no_df, aes(long, lat , group=group), fill = "white", color = "gray") +
  geom_point(aes(colour = ratio_v2018), size = 2) +   # Color according to senorge version
  geom_point(shape = 1, size = 2, colour = "black") +
  theme_classic(base_size = 10) +
  scale_colour_manual(values = cbPalette, drop = FALSE, labels = labels) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  coord_fixed(ratio = 1) +
  theme(legend.position = c(0.7,0.4), legend.title=element_blank(), legend.text = element_text(size = 12)) +
  ggtitle("seNorge v2018")

grid.arrange(map1, map2, map3, map4, ncol = 2, nrow = 2)

g <- arrangeGrob(map1, map2, map3, map4, ncol = 2, nrow = 2)

ggsave(file = "figures/map_prec_div_runoff_plus_aet.png", g, units = "cm", width = 20, height = 20)


# Plot map with correlation between precipitation and runoff --------------

# Prepare shape file

no_df <- readOGR(dsn = "norway_shapefiles" , layer = "norge")

# Categorise data

pbreaks <- c(-Inf, 0.0, 0.25, 0.5, 0.75, 1.0)

df_map <- data.frame(utm_east = df_all$utm_east,
                     utm_north = df_all$utm_north,
                     corr_v10 = cut(df_all$r_prec_qobs_v10, pbreaks),
                     corr_v20 = cut(df_all$r_prec_qobs_v20, pbreaks),
                     corr_v22 = cut(df_all$r_prec_qobs_v22, pbreaks),
                     corr_v2018 = cut(df_all$r_prec_qobs_v2018, pbreaks))

# Plot data

cbPalette <- c( '#5060E8', '#91bfdb','#fee090','#fc8d59','#d73027')

labels = c("< 0.0", "0.0 - 0.25", "0.25 - 0.50", "0.50 - 0.75", "0.75 - 1.00")

map1 <- ggplot(df_map, aes(x = utm_east, y = utm_north)) +
  geom_polygon(data = no_df, aes(long, lat , group=group), fill = "white", color = "gray") +
  geom_point(aes(colour = corr_v10), size = 2) +   # Color according to senorge version
  geom_point(shape = 1, size = 2, colour = "black") +
  theme_classic(base_size = 10) +
  scale_colour_manual(values = cbPalette, drop = FALSE, labels = labels) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  coord_fixed(ratio = 1) +
  theme(legend.position = c(0.7,0.4), legend.title=element_blank(), legend.text = element_text(size = 12)) +
  ggtitle("seNorge v1.0")

map2 <- ggplot(df_map, aes(x = utm_east, y = utm_north)) +
  geom_polygon(data = no_df, aes(long, lat , group=group), fill = "white", color = "gray") +
  geom_point(aes(colour = corr_v20), size = 2) +   # Color according to senorge version
  geom_point(shape = 1, size = 2, colour = "black") +
  theme_classic(base_size = 10) +
  scale_colour_manual(values = cbPalette, drop = FALSE, labels = labels) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  coord_fixed(ratio = 1) +
  theme(legend.position = c(0.7,0.4), legend.title=element_blank(), legend.text = element_text(size = 12)) +
  ggtitle("seNorge v2.0")

map3 <- ggplot(df_map, aes(x = utm_east, y = utm_north)) +
  geom_polygon(data = no_df, aes(long, lat , group=group), fill = "white", color = "gray") +
  geom_point(aes(colour = corr_v22), size = 2) +   # Color according to senorge version
  geom_point(shape = 1, size = 2, colour = "black") +
  theme_classic(base_size = 10) +
  scale_colour_manual(values = cbPalette, drop = FALSE, labels = labels) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  coord_fixed(ratio = 1) +
  theme(legend.position = c(0.7,0.4), legend.title=element_blank(), legend.text = element_text(size = 12)) +
  ggtitle("seNorge v2.2")

map4 <- ggplot(df_map, aes(x = utm_east, y = utm_north)) +
  geom_polygon(data = no_df, aes(long, lat , group=group), fill = "white", color = "gray") +
  geom_point(aes(colour = corr_v2018), size = 2) +   # Color according to senorge version
  geom_point(shape = 1, size = 2, colour = "black") +
  theme_classic(base_size = 10) +
  scale_colour_manual(values = cbPalette, drop = FALSE, labels = labels) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  coord_fixed(ratio = 1) +
  theme(legend.position = c(0.7,0.4), legend.title=element_blank(), legend.text = element_text(size = 12)) +
  ggtitle("seNorge v2018")


grid.arrange(map1, map2, map3, map4, ncol = 2, nrow = 2)

g <- arrangeGrob(map1, map2, map3, map4, ncol = 2, nrow = 2)

ggsave(file = "figures/map_corr_prec_runoff.png", g, units = "cm", width = 20, height = 20)


# Plot cummulative precipitation and runoff -------------------------------

# Load data

load("senorge_daily_v10.RData")

data_v10 <- data_daily

load("senorge_daily_v20.RData")

data_v20 <- data_daily

load("senorge_daily_v22.RData")

data_v22 <- data_daily

load("senorge_daily_seNorge2018.RData")

data_v2018 <- data_daily

pdf("figures/plots_cummulative.pdf", width = 10, height = 7, onefile = TRUE)

for (istat in 1:length(data_daily)) {

  station_name <- data_daily[[istat]]$metadata$station_name
  regine_main <- data_daily[[istat]]$regine_main

  if (regine_main %in% stats_keep$V1) {

    df_cum1 <- data.frame(prec_v10 = cumsum(data_v10[[istat]]$Prec),
                          prec_v20 = cumsum(data_v20[[istat]]$Prec),
                          prec_v22 = cumsum(data_v22[[istat]]$Prec),
                          prec_v2018 = cumsum(data_v2018[[istat]]$Prec),
                          qobs = cumsum(data_daily[[istat]]$Runoff))

    df_cum2 <- gather(df_cum1, senorge_version, prec, prec_v10, prec_v20, prec_v22, prec_v2018)

    if (!any(is.na(df_cum2))) {

      xmax <- max(df_cum2$prec)
      ymax <- max(df_cum2$qobs)

      eq_v10 <- lm_eqn(lm(prec_v10 ~ qobs, df_cum1))
      eq_v20 <- lm_eqn(lm(prec_v20 ~ qobs, df_cum1))
      eq_v22 <- lm_eqn(lm(prec_v22 ~ qobs, df_cum1))
      eq_v2018 <- lm_eqn(lm(prec_v2018 ~ qobs, df_cum1))

      pobj <- ggplot(df_cum2, aes(x = qobs, y = prec)) +
        geom_line(mapping=aes(colour=senorge_version)) +
        geom_smooth(mapping=aes(colour=senorge_version), method = 'lm', se = FALSE, size = 0.1, linetype = 2) +
        xlab("Runoff (mm)") +
        ylab("Precipitation (mm)") +
        ggtitle(paste(station_name, regine_main)) +
        annotate("text", x=0.3*xmax, y=4000, label = "seNorge 1.0, 2.0, 2.2, 2018:", size=4) +
        annotate("text", x=0.3*xmax, y=3000, label = eq_v10, parse = TRUE, size=4) +
        annotate("text", x=0.3*xmax, y=2000, label = eq_v20, parse = TRUE, size=4) +
        annotate("text", x=0.3*xmax, y=1000, label = eq_v22, parse = TRUE, size=4) +
        annotate("text", x=0.3*xmax, y=1000, label = eq_v2018, parse = TRUE, size=4)

      print(pobj)

    }

  }

}

dev.off()







