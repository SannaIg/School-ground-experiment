# Wear and Tear Analysis

## Overview
This project analyzes wear and tear data for plants grown in different square sizes over time. It showcases data wrangling, visualization, and statistical modeling techniques to explore how plant species, spatial layout, and environmental factors influence wear and tear. The analysis includes:

- Time-series visualizations to track wear and tear over multiple dates.
- Spatial heatmaps for cumulative wear and tear.
- Statistical modeling to identify significant factors.

## Features

### Data Wrangling
- Filtered and cleaned raw data using `dplyr`.
- Handled missing values effectively, ensuring accurate computations.
- Calculated cumulative metrics like mean wear and tear and spatially aligned data for heatmaps.

### Data Visualization
- Created bar plots with error bars to visualize trends in mean wear and tear across species and square sizes.
- Generated spatial heatmaps to illustrate cumulative wear and tear for different layouts (e.g., 6x6, 4x4, 2x2 squares).

### Statistical Modeling
- Used Generalized Linear Mixed Models (GLMMs) with the `glmmTMB` package for beta regression.
- Included zero-inflation to account for plants with no recorded wear and tear.
- Performed post-hoc analysis with `emmeans` for estimated marginal means and contrasts.

## Key Outputs

### Visualizations
- **Bar Plots**: Show trends in mean wear and tear over time, grouped by species and square size.
- **Heatmaps**: Visualize spatial distribution of cumulative wear and tear for different square layouts.

Example:
![Bar Plot](figures/bar_plot.png)
![Heatmap](figures/heatmap.png)

### Statistical Insights
- Identified significant interactions between species, date, and square size.
- Highlighted spatial and temporal patterns influencing wear and tear.

## Learning Opportunities
- Demonstrates advanced R programming for data analysis.
- Explores time-series, spatial data, and mixed-effects modeling.
- Highlights data visualization techniques for effective communication.
