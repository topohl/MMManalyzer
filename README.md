# MMManalyzer

MMManalyzer is a data processing and analysis tool for RFID data collected from rodents, such as mice, using the MultiMouseMonitor (MMM) system by Phenosys. This toolset assists researchers in formatting data from MMM and conducting insightful analysis.

## Table of Contents

- [Introduction](#introduction)
- [Data Formatting and Analysis](#data-formatting-and-analysis)
  - [Features](#features)
  - [Analysis Features](#analysis-features)
- [Getting Started](#getting-started)
  - [Prerequisites](#prerequisites)
  - [Installation](#installation)
- [Usage](#usage)
- [Folder Structure](#folder-structure)
- [Contributing](#contributing)
- [License](#license)

## Introduction

The MultiMouseMonitor (MMM) is a state-of-the-art system designed for tracking rodents through RFID technology. MMManalyzer serves as a valuable complement by providing tools for data processing and analysis.

## Data Formatting and Analysis

MMManalyzer is equipped with features for batch processing data. This includes formatting data based on phases (active and inactive) according to the time of day, grouping and filtering of animals, and cropping of data based on time.

### Features

- **Data Formatting**: Format data based on active and inactive phases and other criteria.
- **Grouping and Filtering**: Group and filter animals for targeted analysis.
- **Time Cropping**: Crop data based on specific time intervals.

## Analysis Features

The Analysis component of MMManalyzer offers a range of features for insightful data analysis. These features include:

- **Activity Plotting**: Visualize the activity of groups of mice.
- **Sleep Analysis**: Perform in-depth sleep analysis.
- **Activity Comparison**: Compare activity between different groups using linear mixed-effects models (lme).

## Getting Started

### Prerequisites

Before using MMManalyzer, ensure that you have the following software and tools installed:

- [R](https://cran.r-project.org/) and [RStudio](https://posit.co/download/rstudio-desktop/)
- A spreadsheet software of your choice (e.g., Microsoft Excel, Google Sheets)

### Installation

1. **Clone the Repository:**

   ```bash
   git clone https://github.com/topohl/MMManalyzer.git

## Usage

To utilize MMManalyzer:

- **Data Formatting**: Use the tools in the `Formatting` folder to format raw data from MMM.
- **Data Analysis**: Employ the provided scripts and sheets in the `Analysis` folder for detailed data analysis.

## Folder Structure

- `Analysis`: Contains analysis scripts and sheets for interpreting MMM data.
- `Formatting`: Tools for formatting data collected from the MMM system.
- `Testing` (Experimental): Includes experimental code.
## Contributing

We encourage contributions to MMManalyzer. If you wish to contribute, please follow our Contribution Guidelines.

## License

This project is licensed under the MIT License. You are free to use, modify, and distribute this software in compliance with the terms of the license.
