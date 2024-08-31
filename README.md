# Trait Distribution Simulator

## Overview

The Trait Distribution Simulator is an interactive Shiny application designed to help researchers, students, and educators explore the complex relationships between genetic architecture and phenotypic trait distributions. By simulating various genetic scenarios, users can gain insights into how different factors influence trait expression in populations.

## Features

- **Individual Simulation**: Explore trait distributions based on customizable genetic parameters.
- **Combined Simulation**: Compare trait distributions across different genetic architectures simultaneously.
- **Interactive Visualizations**: Dynamic plots that update in real-time as you adjust parameters.
- **Customizable Parameters**: Adjust key genetic factors such as:
  - Number of loci
  - Sample size
  - Effect size distribution
  - Genetic architecture

## Getting Started

### Prerequisites

Make sure you have R installed on your system. You'll also need the following R packages:

- shiny
- shinyBS
- bs4Dash
- ggplot2
- dplyr

You can install these packages using the following R command:

```R
install.packages(c("shiny", "shinyBS", "bs4Dash", "ggplot2", "dplyr"))
```

### Running the App

1. Clone this repository or download the source code.
2. Open R or RStudio.
3. Set your working directory to the folder containing the app files.
4. Run the following command:

```R
shiny::runApp()
```

The app should open in your default web browser.

## How to Use

### Individual Simulation

1. Navigate to the "Individual Simulation" tab.
2. Adjust the input parameters:
   - Number of Loci
   - Number of Individuals
   - Effect Size Distribution (Fisher or Kimura)
   - Genetic Architecture (Additive, Dominant, or Epistatic)
3. Click "Run Simulation" to generate the plots.
4. Analyze the resulting Trait Distribution and Effect Size Distribution plots.

### Combined Simulation

1. Go to the "Combined Simulation" tab.
2. Set the parameters:
   - Number of Loci
   - Number of Individuals
   - Effect Size Distribution
3. Click "Run Combined Simulation" to generate a comparison plot.
4. Examine how trait distributions differ across Additive, Dominant, and Epistatic architectures.

## Understanding the Results

- **Trait Distribution Plot**: Shows the frequency of trait values in the simulated population.
- **Effect Size Distribution**: Displays the distribution of effect sizes for each locus.
- **Combined Trait Distribution Plot**: Compares trait distributions across different genetic architectures.

## Contributing

We welcome contributions to improve the Trait Distribution Simulator. Please feel free to submit issues or pull requests on our GitHub repository.

## License

This project is licensed under the MIT License - see the LICENSE.md file for details.

## Acknowledgments

- This simulator was inspired by classical quantitative genetics models and modern genomic research.
- Special thanks to the R and Shiny communities for their excellent tools and documentation.

