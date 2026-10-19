

# for trophic level colours
cols <- c(0.99, 1.99, 2.99, 3.99, 4.99, 5.99, 6.95, 1000)
tl_cols <- c("#DDDDDD", "#1E7548", "#32c479", 
             "#47799d", "#69B3E7", "#A9431E", 
             "#f05e29", "#5F249F")

accent_col <- "#5e5e5e"

plot_col <- "#DB8655"

figure_theme <- 
  theme_classic() +
  theme(panel.border = element_rect(colour = '#262406',
                                    fill = "#ffffff00"), # Transparent fill
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_line(colour = colorspace::darken("#5e5e5e", 0.1),
                                  linewidth = 0.3),
        plot.background = element_rect(fill = "white", colour = NA),
        panel.background = element_rect(fill = "white", colour = NA),
        legend.background = element_rect(fill = "white", colour = NA),
        legend.key = element_blank(),
        text = element_text(color = accent_col), # Soft gray text for better readability
        plot.margin = margin(10, 5, 5, 10),
        legend.margin = margin(1, 2, 1, 2)
  )

set_theme(figure_theme)
