#########################
##### prerequisites #####
#########################

library(ggplot2)
library(dplyr)
library(RColorBrewer)


###############################
## create grid & assign zone ##
###############################
# with exemplary coordinates 

x_start <- 4011
y_start <- 4231
grid <- expand.grid(x = x_start:(x_start + 10), y = y_start:(y_start + 10)) %>%
  mutate(
    dist = pmax(abs(x - (x_start + 5)), abs(y - (y_start + 5))),
    zone = case_when(
      dist <= 1 ~ "Treatment-Zone",
      dist == 2 ~ "Buffer-Zone",
      dist %in% 3:4 ~ "Control-Zone",
      TRUE ~ "Not considered"
    ),
    zone = factor(zone, levels = c("Treatment-Zone", "Buffer-Zone", 
                                   "Control-Zone", "Not considered"))
  )

accent_colors <- brewer.pal(8, "Accent")
zone_colors <- c("Treatment-Zone" = accent_colors[1],
                 "Buffer-Zone" = accent_colors[2],
                 "Control-Zone" = accent_colors[3],
                 "Not considered" = "#808080")


# design without buffer zone
grid_no_buffer <- grid %>%
  mutate(
    zone_no_buffer = case_when(
      dist <= 1 ~ "Treatment-Zone",
      dist >= 2 & dist <= 4 ~ "Control-Zone",
      TRUE ~ "Not considered"
    ),
    zone_no_buffer = factor(zone_no_buffer, levels = c("Treatment-Zone", 
                                                       "Control-Zone", 
                                                       "Not considered"))
  )

zone_colors_no_buffer <- c("Treatment-Zone" = accent_colors[1],
                           "Control-Zone" = accent_colors[3],
                           "Not considered" = "#808080")

plot_no_buffer <- ggplot(grid_no_buffer, aes(x, y, fill = zone_no_buffer)) +
  geom_tile(color = "black", linewidth = 0.5) +
  annotate("text", x = x_start + 5, y = y_start + 5, label = "X", size = 8, 
           fontface = "bold") +
  scale_fill_manual(values = zone_colors_no_buffer, name = "Legend:") +
  coord_fixed(expand = FALSE) +
  scale_x_continuous(breaks = seq(x_start, x_start + 10, by = 2), 
                     expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(y_start, y_start + 10, by = 2), 
                     expand = c(0, 0)) +
  labs(x = "X-Coordinate", y = "Y-Coordinate", 
       title = "School Treatment Zones (Queen Contiguity)",
       subtitle = "Each cell represents 1 km²") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.position = "right",
        plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("plot_zones_no_buffer.pdf", plot_no_buffer, width = 8, height = 6)


# design with buffer zone
plot_with_buffer <- ggplot(grid, aes(x, y, fill = zone)) +
  geom_tile(color = "black", linewidth = 0.5) +
  annotate("text", x = x_start + 5, y = y_start + 5, label = "X", size = 8, 
           fontface = "bold") +
  scale_fill_manual(values = zone_colors, name = "Legend:") +
  coord_fixed(expand = FALSE) +
  scale_x_continuous(breaks = seq(x_start, x_start + 10, by = 2), 
                     expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(y_start, y_start + 10, by = 2), 
                     expand = c(0, 0)) +
  labs(x = "X-Coordinate", y = "Y-Coordinate", 
       title = "School Treatment Zones (Queen Contiguity)",
       subtitle = "Each cell represents 1 km²") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.position = "right",
        plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("plot_zones_with_buffer.pdf", plot_with_buffer, width = 8, height = 6)