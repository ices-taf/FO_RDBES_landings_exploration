library(dplyr)
library(htmlwidgets)
library(ggplot2)
library(plotly)


source("utilities_GES_pies.R")
source("utilities.R")


plot_GES_pies(clean_status, catch_current_adj)
p4 <- plot_GES_pies(clean_status, catch_current_adj)

### save p4 as png
file_name_png <- "GES_pies_catches_adjusted_RDBES_BI"
ggsave(
        filename = file.path("./output", paste0(file_name_png, ".png")),
        plot = p4,
        device = "png"
      )

# test <- catch_current_adj %>% filter(StockKeyLabel == "mac.27.nea")


p <- plot_GES_pies_plotly(clean_status, catch_current_adj)
file_name <- "GES_pies_catches_guilds_adjusted_with_plotly_RDBES_BI"
htmlwidgets::saveWidget(
        widget = p,
        file = file.path("./output", paste0(file_name, ".html")),
        selfcontained = TRUE
      )


# head(clean_status)
# head(clean_status)
# test_pil <- catch_current %>% filter(StockKeyLabel == "pil.27.8c9a")

# test_pil2 <- catch_current_adj %>% filter(StockKeyLabel == "pil.27.8c9a")
# test_pli3 <- clean_status %>% filter(StockKeyLabel == "pil.27.8c9a")
# print(test_pil, width = Inf)
# print(test_pil2, width = Inf)
# print(test_pli3, width = Inf)

# head(catch_current)

# p <- plot_GES_pies(clean_status, catch_current_adj)
# p2 <- plot_GES_pies_plotly2(clean_status, catch_current_adj)
library(icesTAF)
mkdir("./output/CLD_bar_static/")
### run plot_CLD_bar_app for all guuilds and save png plots
p2 <- plot_CLD_bar_app(catch_current_adj, "benthic")
file_name <- "CLD_bar_app_benthic_RDBES_BI"
ggsave(
        filename = file.path("./output/CLD_bar_static/", paste0(file_name, ".png")),
        plot = p2,
        device = "png"
      )

p2 <- plot_CLD_bar_app(catch_current_adj, "demersal")
file_name <- "CLD_bar_app_demersal_RDBES_BI"
ggsave(
        filename = file.path("./output/CLD_bar_static/", paste0(file_name, ".png")),
        plot = p2,
        device = "png"
      )


p2 <- plot_CLD_bar_app(catch_current_adj, "elasmobranch")
file_name <- "CLD_bar_app_elasmobranch_RDBES_BI"
ggsave(
        filename = file.path("./output/CLD_bar_static/", paste0(file_name, ".png")),
        plot = p2,
        device = "png"
      )

p2 <- plot_CLD_bar_app(catch_current_adj, "pelagic")
file_name <- "CLD_bar_app_pelagic_RDBES_BI"
ggsave(
        filename = file.path("./output/CLD_bar_static/", paste0(file_name, ".png")),
        plot = p2,
        device = "png"
      )

p2 <- plot_CLD_bar_app(catch_current_adj, "shellfish")
file_name <- "CLD_bar_app_shellfish_RDBES_BI"
ggsave(
        filename = file.path("./output/CLD_bar_static/", paste0(file_name, ".png")),
        plot = p2,
        device = "png"
      )

p2 <- plot_CLD_bar_app(catch_current_adj, "All")
file_name <- "CLD_bar_app_all_RDBES_BI"
ggsave(
        filename = file.path("./output/CLD_bar_static/", paste0(file_name, ".png")),
        plot = p2,
        device = "png"
      )


# test_mac <- catch_current_adj %>% filter(StockKeyLabel == "mac.27.nea")
# print(test_mac, width = Inf)
# head(catch_current_adj)

mkdir("./output/CLD_bar_interactive/")
p2 <- plot_CLD_bar_app_plotly(catch_current_adj, "benthic")
file_name <- "CLD_bar_catches_benthic_adjusted_with_RDBES_plotly_BI"
htmlwidgets::saveWidget(
        widget = p2,
        file = file.path("./output/CLD_bar_interactive/", paste0(file_name, ".html")),
        selfcontained = TRUE
      )

p2 <- plot_CLD_bar_app_plotly(catch_current_adj, "demersal")
file_name <- "CLD_bar_catches_demersal_adjusted_with_RDBES_plotly_BI"
htmlwidgets::saveWidget(
        widget = p2,
        file = file.path("./output/CLD_bar_interactive/", paste0(file_name, ".html")),
        selfcontained = TRUE
      )

p2 <- plot_CLD_bar_app_plotly(catch_current_adj, "elasmobranch")
file_name <- "CLD_bar_catches_elasmobranch_adjusted_with_RDBES_plotly_BI"
htmlwidgets::saveWidget(
        widget = p2,
        file = file.path("./output/CLD_bar_interactive/", paste0(file_name, ".html")),
        selfcontained = TRUE
      )

p2 <- plot_CLD_bar_app_plotly(catch_current_adj, "pelagic")
file_name <- "CLD_bar_catches_pelagic_adjusted_with_RDBES_plotly_BI"
htmlwidgets::saveWidget(
        widget = p2,
        file = file.path("./output/CLD_bar_interactive/", paste0(file_name, ".html")),
        selfcontained = TRUE
      )

p2 <- plot_CLD_bar_app_plotly(catch_current_adj, "shellfish")
file_name <- "CLD_bar_catches_shellfish_adjusted_with_RDBES_plotly_BI"
htmlwidgets::saveWidget(
        widget = p2,
        file = file.path("./output/CLD_bar_interactive/", paste0(file_name, ".html")),
        selfcontained = TRUE
      )

p2 <- plot_CLD_bar_app_plotly(catch_current_adj, "All")
file_name <- "CLD_bar_catches_all_adjusted_with_RDBES_plotly_BI"
htmlwidgets::saveWidget(
        widget = p2,
        file = file.path("./output/CLD_bar_interactive/", paste0(file_name, ".html")),
        selfcontained = TRUE
      )


mkdir("./output/CLD_dumbbell_interactive/")
p3 <- plot_CLD_bar_app_dumbbell_plotly(catch_current_adj, "benthic",log_scale = FALSE)
file_name <- "CLD_dumbbell_catches_benthic_adjusted_with_RDBES_plotly_BI"
htmlwidgets::saveWidget(
        widget = p3,
        file = file.path("./output/CLD_dumbbell_interactive/", paste0(file_name, ".html")),
        selfcontained = TRUE
      )

p3 <- plot_CLD_bar_app_dumbbell_plotly(catch_current_adj, "demersal",log_scale = FALSE)
file_name <- "CLD_dumbbell_catches_demersal_adjusted_with_RDBES_plotly_BI"
htmlwidgets::saveWidget(
        widget = p3,
        file = file.path("./output/CLD_dumbbell_interactive/", paste0(file_name, ".html")),
        selfcontained = TRUE
      )

p3 <- plot_CLD_bar_app_dumbbell_plotly(catch_current_adj, "elasmobranch",log_scale = FALSE)
file_name <- "CLD_dumbbell_catches_elasmobranch_adjusted_with_RDBES_plotly_BI"
htmlwidgets::saveWidget(
        widget = p3,
        file = file.path("./output/CLD_dumbbell_interactive/", paste0(file_name, ".html")),
        selfcontained = TRUE
      )
p3 <- plot_CLD_bar_app_dumbbell_plotly(catch_current_adj, "pelagic",log_scale = FALSE)
file_name <- "CLD_dumbbell_catches_pelagic_adjusted_with_RDBES_plotly_BI"
htmlwidgets::saveWidget(
        widget = p3,
        file = file.path("./output/CLD_dumbbell_interactive/", paste0(file_name, ".html")),
        selfcontained = TRUE
      )

p3 <- plot_CLD_bar_app_dumbbell_plotly(catch_current_adj, "shellfish",log_scale = FALSE)
file_name <- "CLD_dumbbell_catches_shellfish_adjusted_with_RDBES_plotly_BI"
htmlwidgets::saveWidget(
        widget = p3,
        file = file.path("./output/CLD_dumbbell_interactive/", paste0(file_name, ".html")),
        selfcontained = TRUE
      )

p3 <- plot_CLD_bar_app_dumbbell_plotly(catch_current_adj, "All",log_scale = FALSE)
file_name <- "CLD_dumbbell_catches_all_adjusted_with_RDBES_plotly_BI"
htmlwidgets::saveWidget(
        widget = p3,
        file = file.path("./output/CLD_dumbbell_interactive/", paste0(file_name, ".html")),
        selfcontained = TRUE
      )

 ### log scale true


p3 <- plot_CLD_bar_app_dumbbell_plotly(catch_current_adj, "benthic",log_scale = TRUE)
file_name <- "CLD_dumbbell_catches_benthic_adjusted_with_RDBES_plotly_BI_log" 
htmlwidgets::saveWidget(
        widget = p3,
        file = file.path("./output/CLD_dumbbell_interactive/", paste0(file_name, ".html")),
        selfcontained = TRUE
      )

p3 <- plot_CLD_bar_app_dumbbell_plotly(catch_current_adj, "demersal",log_scale = TRUE)
file_name <- "CLD_dumbbell_catches_demersal_adjusted_with_RDBES_plotly_BI_log"
htmlwidgets::saveWidget(
        widget = p3,
        file = file.path("./output/CLD_dumbbell_interactive/", paste0(file_name, ".html")),
        selfcontained = TRUE
      )

p3 <- plot_CLD_bar_app_dumbbell_plotly(catch_current_adj, "elasmobranch",log_scale = TRUE)
file_name <- "CLD_dumbbell_catches_elasmobranch_adjusted_with_RDBES_plotly_BI_log"
htmlwidgets::saveWidget(
        widget = p3,
        file = file.path("./output/CLD_dumbbell_interactive/", paste0(file_name, ".html")),
        selfcontained = TRUE
      )
p3 <- plot_CLD_bar_app_dumbbell_plotly(catch_current_adj, "pelagic",log_scale = TRUE)
file_name <- "CLD_dumbbell_catches_pelagic_adjusted_with_RDBES_plotly_BI_log"
htmlwidgets::saveWidget(
        widget = p3,
        file = file.path("./output/CLD_dumbbell_interactive/", paste0(file_name, ".html")),
        selfcontained = TRUE
      )

p3 <- plot_CLD_bar_app_dumbbell_plotly(catch_current_adj, "shellfish",log_scale = TRUE)
file_name <- "CLD_dumbbell_catches_shellfish_adjusted_with_RDBES_plotly_BI_log"
htmlwidgets::saveWidget(
        widget = p3,
        file = file.path("./output/CLD_dumbbell_interactive/", paste0(file_name, ".html")),
        selfcontained = TRUE
      )

p3 <- plot_CLD_bar_app_dumbbell_plotly(catch_current_adj, "All",log_scale = TRUE)
file_name <- "CLD_dumbbell_catches_all_adjusted_with_RDBES_plotly_BI_log"
htmlwidgets::saveWidget(
        widget = p3,
        file = file.path("./output/CLD_dumbbell_interactive/", paste0(file_name, ".html")),
        selfcontained = TRUE
      )


unique(catch_current_adj$Status)

mkdir("./output/kobe_static/")
p4 <- plot_kobe_app(catch_current_adj, "benthic")
file_name <- "kobe_benthic_adjusted_with_RDBES_static_BI"
ggsave(
        filename = file.path("./output/kobe_static/", paste0(file_name, ".png")),
        plot = p4,
        device = "png"
      )

p4 <- plot_kobe_app(catch_current_adj, "demersal")
file_name <- "kobe_demersal_adjusted_with_RDBES_static_BI"
ggsave(
        filename = file.path("./output/kobe_static/", paste0(file_name, ".png")),
        plot = p4,
        device = "png"
      )

p4 <- plot_kobe_app(catch_current_adj, "elasmobranch")
file_name <- "kobe_elasmobranch_adjusted_with_RDBES_static_BI"
ggsave(
        filename = file.path("./output/kobe_static/", paste0(file_name, ".png")),
        plot = p4,
        device = "png"
      )

p4 <- plot_kobe_app(catch_current_adj, "pelagic")
file_name <- "kobe_pelagic_adjusted_with_RDBES_static_BI"
ggsave(
        filename = file.path("./output/kobe_static/", paste0(file_name, ".png")),
        plot = p4,
        device = "png"
      )
p4 <- plot_kobe_app(catch_current_adj, "shellfish")
file_name <- "kobe_shellfish_adjusted_with_RDBES_static_BI"
ggsave(
        filename = file.path("./output/kobe_static/", paste0(file_name, ".png")),
        plot = p4,
        device = "png"
      )

p4 <- plot_kobe_app(catch_current_adj, "All")
file_name <- "kobe_all_adjusted_with_RDBES_static_BI"
ggsave(
        filename = file.path("./output/kobe_static/", paste0(file_name, ".png")),
        plot = p4,
        device = "png"
      )

mkdir("./output/kobe_interactive/")
p5 <- plot_kobe_app_plotly(catch_current_adj, "benthic")
file_name <- "kobe_benthic_adjusted_with_RDBES_plotly_BI"
htmlwidgets::saveWidget(
        widget = p5,
        file = file.path("./output/kobe_interactive/", paste0(file_name, ".html")),
        selfcontained = TRUE
      )

p5 <- plot_kobe_app_plotly(catch_current_adj, "demersal")
file_name <- "kobe_demersal_adjusted_with_RDBES_plotly_BI"
htmlwidgets::saveWidget(
        widget = p5,
        file = file.path("./output/kobe_interactive/", paste0(file_name, ".html")),
        selfcontained = TRUE
      )

p5 <- plot_kobe_app_plotly(catch_current_adj, "elasmobranch")
file_name <- "kobe_elasmobranch_adjusted_with_RDBES_plotly_BI"
htmlwidgets::saveWidget(
        widget = p5,
        file = file.path("./output/kobe_interactive/", paste0(file_name, ".html")),
        selfcontained = TRUE
      )

p5 <- plot_kobe_app_plotly(catch_current_adj, "pelagic")
file_name <- "kobe_pelagic_adjusted_with_RDBES_plotly_BI"
htmlwidgets::saveWidget(
        widget = p5,
        file = file.path("./output/kobe_interactive/", paste0(file_name, ".html")),
        selfcontained = TRUE
      )

p5 <- plot_kobe_app_plotly(catch_current_adj, "shellfish")
file_name <- "kobe_shellfish_adjusted_with_RDBES_plotly_BI"
htmlwidgets::saveWidget(
        widget = p5,
        file = file.path("./output/kobe_interactive/", paste0(file_name, ".html")),
        selfcontained = TRUE
      )

p5 <- plot_kobe_app_plotly(catch_current_adj, "All")
file_name <- "kobe_all_adjusted_with_RDBES_plotly_BI"
htmlwidgets::saveWidget(
        widget = p5,
        file = file.path("./output/kobe_interactive/", paste0(file_name, ".html")),
        selfcontained = TRUE
      )