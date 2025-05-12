# Nitrogen Analysis in BZea Population Using Hyperspectral Data

## Overview
This repository provides a comprehensive analysis of Nitrogen content in the BZea population through hyperspectral measurement. The project aims to evaluate nitrogen concentrations in leaves, using hyperspectral reflectance and predictive modeling through the Partial Least Squares Regression (PLSR) approach.

### Repository Structure

- **`data/`**: Contains the raw hyperspectral data files used for the nitrogen analysis.
- **`src/`**: Includes code for the preprocessing and modeling of hyperspectral data using the PLSR model.

## Methods

### Hyperspectral Analysis
The hyperspectral data was gathered using the ASD FieldSpec 4 Hi-Res apectroradiometer, allowing for high-resolution spectral data across different leaf samples in the BZea population.

- **Measurements**: 
  - For each genotype, we took two measurements on three leaves of 2 plants per plot. The leaves measured were chosen relative to the primary ear, where the first was 2 below the ear (e1), then one above the ear (e2), and finally four above the ear (e3). We took measurements at timepoints that roughly corresponded to VT, R1, and R4. 

### Nitrogen Analysis
For nitrogen analysis, we weighed 2 mg of each dry leaf sample and placed it in the Thermo Scientific Flash 2000 CHNS/O Analyzer. Each sample was then combusted in a high-temperature reactor with added oxygen. Using the Dumas method, the analyzer measured the nitrogen gas produced, providing a nitrogen content reading for each sample. This analysis was conducted with the assistance of Rashid (from The Sustainable Soil Lab at NCSU).

---

### Running the Analysis

To reproduce the analysis, follow these steps:

1. **Data Preparation**: Place raw nitrogen data files in the `data/` directory.
2. **Model Execution**: Execute the scripts in `src/` to preprocess data and apply the PLSR model.

### Dependencies

Ensure the following packages are installed before running the analysis:

- R packages: `dplyr`, `ggplot2`, `caret`, `pls`

---

### License

This project is licensed under the MIT License. See `LICENSE` for more details.

