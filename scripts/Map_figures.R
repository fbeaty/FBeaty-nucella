#This script will make a figure with my study sites identified

#Load packages----
pkgs <- c("sf", "ggplot2", "bcmaps", "dplyr", "tidyverse", "ggspatial", "ggpubr", "cowplot")
lapply(pkgs, library, character.only = TRUE)
rm(pkgs)

#Load in data----
#I store the two coastline shapefiles outside of my project director as they are very large and mess with github version control
#First read in the BC dataset (data from https://catalogue.data.gov.bc.ca/dataset/b9bd93e1-0226-4351-b943-05c6f80bd5da?fbclid=IwAR2VBTULWJmiWlH0II9rZ_uoMTySHLUa_5jM8Ae3cKCrsFMLa-_bVJw0i-g)
df <- st_read("/Users/fionabeaty/Dropbox/Fiona/School/Chapter 2 - Local Adaptation/Ch2 Data Analysis/maps data/DBM_BC_7H_MIL_POLITICAL_POLY")
df <- df %>% 
  filter(STATUS != "Canadian Waters")

#Load this other dataset for the insets (data from https://catalogue.data.gov.bc.ca/dataset/87b1d6a7-d4d1-4c25-a879-233becdbffed)
df_2 <- st_read("/Users/fionabeaty/Dropbox/Fiona/School/Chapter 2 - Local Adaptation/Ch2 Data Analysis/maps data/BC_Boundary_Terrestrial.gdb", layer = "BC_Boundary_Terrestrial_Singlepart")

#Next read in the csv with the sites data
sites <- read_csv("data/maps/sites.csv")

#Convert sites to spatial points dataframe, with the 4326 coordinates because that's what the GPS data were collected with
sites <- st_as_sf(sites, coords = c("Long", "Lat")) %>% 
  st_set_crs(4326)

#Specify zoom in & projection, BC Albers projection is NAD83, EPSG 3005
#This document provided the below code: https://datascience.blog.wzb.eu/2019/04/30/zooming-in-on-maps-with-sf-and-ggplot2/
target_crs <- '+init=epsg:3005'
df_transformed <- st_transform(df, 3005)
df_2_transformed <- st_transform(df_2, 3005)
sites_transformed <- st_transform(sites, 3005)

#Set your regional, Calvert, and Nanaimo windows & label positions
disp_win_wgs84_reg <- st_sfc(st_point(c(-132.2, 46.5)), st_point(c(-121, 55)),
                             crs = 4326)
disp_win_trans_reg <- st_transform(disp_win_wgs84_reg, crs = target_crs)
disp_win_coord_reg <- st_coordinates(disp_win_trans_reg)

disp_win_wgs84_cal <- st_sfc(st_point(c(-128.4, 51.05)), st_point(c(-127.6, 51.85)),
                             crs = 4326)
disp_win_trans_cal <- st_transform(disp_win_wgs84_cal, crs = target_crs)
disp_win_coord_cal <- st_coordinates(disp_win_trans_cal)

disp_win_wgs84_nan <- st_sfc(st_point(c(-124.3, 48.8)), st_point(c(-123.5, 49.5)),
                             crs = 4326)
disp_win_trans_nan <- st_transform(disp_win_wgs84_nan, crs = target_crs)
disp_win_coord_nan <- st_coordinates(disp_win_trans_nan)

box_cal_coords <- st_sfc(st_point(c(-128.4, 51.05)), st_point(c(-127.6, 51.85)),
                  crs = 4326)
box_cal_win <- st_transform(box_cal_coords, crs = target_crs)
box_cal <- st_as_sf(box_cal_win, coords = "Value") %>% 
  rename(geometry = x)

box_nan_coords <- st_sfc(st_point(c(-124.3, 48.8)), st_point(c(-123.5, 49.5)),
                         crs = 4326)
box_nan_win <- st_transform(box_nan_coords, crs = target_crs)
box_nan <- st_as_sf(box_nan_win, coords = "Value") %>% 
  rename(geometry = x)

label_bc <- st_sfc(st_point(c(-124, 53)), crs = 4326)
label_bc_trans <-st_transform(label_bc, crs = target_crs)
label_bc_trans_coord <- st_coordinates(label_bc_trans)

#Visualize the data ----
full <- ggplot() +
  geom_sf(data = df_transformed, color = "grey34", fill = "wheat") +
  geom_sf(data = sites_transformed, color = "black") +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = disp_win_coord_reg[,'X'], ylim = disp_win_coord_reg[,'Y'],
           expand = FALSE) +
  annotate(geom = "text", x = label_bc_trans_coord[,'X'], y = label_bc_trans_coord[,'Y'], 
           label = "British Columbia", fontface = "bold", color = "grey34", size = 4) +
  geom_rect(aes(xmin = st_bbox(box_cal)[[1]], ymin = st_bbox(box_cal)[[2]], 
                xmax = st_bbox(box_cal)[[3]], ymax = st_bbox(box_cal)[[4]]), fill = NA, 
            colour = "red", size = 0.6) +
  geom_rect(aes(xmin = st_bbox(box_nan)[[1]], ymin = st_bbox(box_nan)[[2]], 
                xmax = st_bbox(box_nan)[[3]], ymax = st_bbox(box_nan)[[4]]), fill = NA,
            colour = "red", size = 0.6) +
  labs(y = "", x = "") +
  theme(panel.background = element_rect(fill = "skyblue4",
                                        color = "skyblue4"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey87"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "grey87"),
        legend.position = "none") 

calvert <- ggplot() +
  geom_sf(data = df_2_transformed, color = "grey34", fill = "wheat") +
  geom_sf(data = sites_transformed, aes(color = Region, size = 1)) +
  scale_color_manual(values = c("skyblue", "black", "coral")) +
  annotation_scale(location = "bl", width_hint = 0.3) +
  coord_sf(xlim = disp_win_coord_cal[,'X'], ylim = disp_win_coord_cal[,'Y'],
           expand = FALSE) +
  ggtitle("Calvert Island") +
  theme(panel.background = element_rect(fill = "skyblue2",
                                        color = "skyblue2"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_blank(), axis.ticks = element_blank(),
        plot.title = element_text(face = "bold", size = 12),
        legend.position = "none")

#it takes about 15-20 seconds to load the nanaimo & calvert figures b/c the spatial files are larger

nanaimo <- ggplot() +
  geom_sf(data = df_2_transformed, color = "black", fill = "grey94") +
  geom_sf(data = sites_transformed, aes(color = Region, size = 1)) +
  scale_color_manual(values = c("skyblue", "black", "coral")) +
  annotation_scale(location = "bl", width_hint = 0.3) +
  coord_sf(xlim = disp_win_coord_nan[,'X'], ylim = disp_win_coord_nan[,'Y'],
           expand = FALSE) +
  ggtitle("Nanaimo") +
  theme(panel.background = element_rect(fill = "skyblue2",
                                        color = "skyblue2"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_blank(), axis.ticks = element_blank(),
        plot.title = element_text(face = "bold", size = 12),
        legend.position = "none")

#Combine plots into one figure & export ----
#First create the insets
#Note that I removed the axis ticks & grid lines for the insets, since those data are visualized in the big map
insets <- plot_grid(calvert, nanaimo, nrow = 2, align = "hv", 
                    rel_widths = c(1,1))
#it takes about 30 seconds to load this figure

#Then combine with full
combined <- plot_grid(full, insets, nrow = 1, rel_widths = c(2,1),
                        align = "hv")
#It takes about 35 seconds to draw this figure

ggsave(combined, file = "plots/maps/combined_map.pdf", width = 9, height = 8, dpi = 300)
ggsave(full, file = "plots/maps/fullmap.pdf", width = 8, height = 8, dpi = 300)
ggsave(insets, file = "plots/maps/inset_maps.pdf", width = 4, height = 8, dpi = 300)

#There is still some awkward white space in this figure, but I have spent too long on figuring it out so Im moving on!
#I also think it could be worth changing either the colour of the sites or the boxes around them to match
#whatever colour scheme I pick elsewhere
#Also, perhaps worth adjusting the font size & labelling the lat & longs with N & W


#Remove old variables----
rm(full, insets, combined, calvert, df, df_2, df_2_transformed, df_transformed,
   disp_win_coord_cal, disp_win_coord_nan, disp_win_coord_reg, disp_win_trans_cal,
   disp_win_trans_nan, disp_win_trans_reg, disp_win_wgs84_cal, disp_win_wgs84_nan,
   disp_win_wgs84_reg, nanaimo, sites, sites_transformed,
   box_cal, box_cal_coords, box_cal_win, box_nan, box_nan_coords, box_nan_win, 
   label_bc, label_bc_trans, label_bc_trans_coord, target_crs)

