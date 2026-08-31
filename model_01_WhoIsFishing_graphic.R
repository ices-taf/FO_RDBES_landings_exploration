library(plotly)
library(dplyr)



### total number of vessels by year and flag country
plot_ly(
  nVessel,
  x = ~CLyear,
  y = ~N_Vessel_Total,
  color = ~CLvesselFlagCountry,
  type = "scatter",
  mode = "lines+markers",
  text = ~paste0(
    "<b>", CLvesselFlagCountry, "</b>",
    "<br>Year: ", CLyear,
    "<br>Vessels: ", N_Vessel_Total
  ),
  hoverinfo = "text"
)



# nVessel_long <- nVessel %>%
#   tidyr::pivot_longer(
#     c(N_Vessel_Total, N_Vessel_80),
#     names_to = "type",
#     values_to = "n"
#   )

# plot_ly(
#   nVessel_long,
#   x = ~CLyear,
#   y = ~n,
#   color = ~type,
#   split = ~interaction(CLvesselFlagCountry, type),
#   type = "scatter",
#   mode = "lines+markers"
# )


### num and percentage of vessels by size category, year and flag country
### could add selector for year
vessel_size_plot <- nbreVesselInAreaVS_all %>%
  group_by(CLyear, CLvesselFlagCountry) %>%
  mutate(
    share = nbreVessels / sum(nbreVessels, na.rm = TRUE) * 100
  ) %>%
  ungroup()

  d <- vessel_size_plot %>%
  filter(CLyear == 2025)

plot_ly(
  d,
  x = ~share,
  y = ~CLvesselFlagCountry,
  color = ~CLvesselLengthCategory,
  type = "bar",
  orientation = "h",
  text = ~paste0(
    "<b>", CLvesselFlagCountry, "</b>",
    "<br>Length: ", CLvesselLengthCategory,
    "<br>Vessels: ", nbreVessels,
    "<br>Fleet: ", round(share, 1), "%"
  ),
  hoverinfo = "text"
) %>%
  layout(
    barmode = "stack",
    xaxis = list(title = "% of fleet", range = c(0,100)),
    yaxis = list(title = "")
  )

### heatmap of fishing technique by flag country and year
# One warning from your own script is important here: 
# you already identified that fishing technique is not
# always unique per vessel. So I would be careful about 
# labelling these percentages as literally “% of vessels” 
# until that issue is resolved. The heatmap itself is still appropriate.
#appropriate.
gear_plot <- nbreVesselFT_all %>%
    group_by(CLyear, CLvesselFlagCountry) %>%
    mutate(
        share = nbreVessels / sum(nbreVessels, na.rm = TRUE) * 100
    ) %>%
    ungroup()

d <- gear_plot %>%
    filter(CLyear == 2024)

library(ggplot2)

p <- ggplot(
    d,
    aes(
        x = CLfishingTechnique,
        y = CLvesselFlagCountry,
        fill = share,
        text = paste0(
            "Country: ", CLvesselFlagCountry,
            "<br>Technique: ", CLfishingTechnique,
            "<br>Vessels: ", nbreVessels,
            "<br>Share: ", round(share, 1), "%"
        )
    )
) +
    geom_tile() +
    scale_fill_gradient(
    low = "white",
    high = "darkblue"
  ) +
    labs(
        x = "Fishing technique",
        y = NULL,
        fill = "% fleet"
    )

ggplotly(p, tooltip = "text")


### main species by weight for a given year and flag country
d <- MainSppKG %>%
  filter(
    CLyear == 2023,
    CLvesselFlagCountry == "ES"
  ) %>%
  arrange(weight)

plot_ly(
  d,
  x = ~weight,
  y = ~reorder(CLspeciesFaoCode, weight),
  type = "bar",
  orientation = "h",
  text = ~paste0(
    "<b>", CLspeciesFaoCode, "</b>",
    "<br>Weight: ", scales::comma(weight),
    "<br>Value: €", scales::comma(value)
  ),
  hoverinfo = "text"
)

### main species by value for a given year and flag country
### give option to select either landed weight or landed value
d <- MainSppEURO %>%
  filter(
    CLyear == 2024,
    CLvesselFlagCountry == "PT"
  ) %>%
  arrange(value)

plot_ly(
  d,
  x = ~value,
  y = ~reorder(CLspeciesFaoCode, value),
  type = "bar",
  orientation = "h"
)


### new object, combination of MainSppKG + MainSppEURO
species_weight <- MainSppKG %>%
  group_by(CLyear, CLvesselFlagCountry) %>%
  arrange(desc(weight), .by_group = TRUE) %>%
  mutate(rank = row_number()) %>%
  select(
    CLyear,
    CLvesselFlagCountry,
    CLspeciesFaoCode,
    rank
  ) %>%
  mutate(measure = "Tonnage")

species_value <- MainSppEURO %>%
  group_by(CLyear, CLvesselFlagCountry) %>%
  arrange(desc(value), .by_group = TRUE) %>%
  mutate(rank = row_number()) %>%
  select(
    CLyear,
    CLvesselFlagCountry,
    CLspeciesFaoCode,
    rank
  ) %>%
  mutate(measure = "Value")

species_rank <- bind_rows(
  species_weight,
  species_value
)

d <- species_rank %>%
  filter(
    CLyear == 2024,
    CLvesselFlagCountry == "ES"
  )

plot_ly(
  d,
  x = ~measure,
  y = ~rank,
  split = ~CLspeciesFaoCode,
  type = "scatter",
  mode = "lines+markers",
  text = ~paste0(
    CLspeciesFaoCode,
    "<br>Rank: ", rank
  ),
  hoverinfo = "text"
) %>%
  layout(
    yaxis = list(
      autorange = "reversed",
      dtick = 1
    )
  )



  ### heatmap of main species by weight for a given year and flag country
  p <- ggplot(
  MainSppVS %>%
    filter(
      CLyear == 2024,
      CLvesselFlagCountry == "ES"
    ),
  aes(
    x = CLvesselLengthCategory,
    y = CLspeciesFaoCode,
    fill = weight
  )
) +
  geom_tile() +
  scale_fill_gradient(
    low = "white",
    high = "darkblue"
  ) 

ggplotly(p)


### weight of landings by year, flag country and comparison Iberian vs Bay of biscay
plot_ly(
  Landings_sub_area,
  x = ~CLyear,
  y = ~weight,
  color = ~IntraCS,
  split = ~interaction(CLvesselFlagCountry, IntraCS),
  type = "scatter",
  mode = "lines+markers"
)

### weight of landing by species and area
### a species selector would be necessary
d <- Landings_sub_area_spp %>%
  filter(CLspeciesFaoCode == "HKE")

plot_ly(
  d,
  x = ~CLyear,
  y = ~weight,
  color = ~IntraCS,
  type = "scatter",
  mode = "lines+markers"
)
