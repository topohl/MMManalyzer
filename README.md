# MMManalyzer

MMManalyzer is an analysis and data processing tool for RFID-based tracking data of rodents, specifically designed for the MultiMouseMonitor (MMM) system by Phenosys. It enables efficient formatting and analysis of MMM data, particularly regarding behavioral patterns such as locomotion, sleep, and general activity.

## Table of Contents

- [Introduction](#introduction)
- [Key Features](#key-features)
  - [Data Formatting](#data-formatting)
  - [Analysis Functions](#analysis-functions)
- [Getting Started](#getting-started)
  - [Prerequisites](#prerequisites)
  - [Installation](#installation)
- [Usage](#usage)
- [Folder Structure](#folder-structure)
- [Contributing](#contributing)
- [License](#license)

## Introduction

The MultiMouseMonitor (MMM) system utilizes RFID technology to continuously track rodents housed in groups, allowing for long-term behavioral studies. MMManalyzer simplifies data processing and provides analytical tools to extract relevant information such as locomotion patterns and sleep behavior under different experimental conditions.

## Key Features

### Data Formatting

MMManalyzer provides automated tools for cleaning and structuring MMM data in preparation for analysis. Key features include:

- **Phase-Based Segmentation:** Automatic division of data into active and inactive phases based on circadian rhythm.
- **Grouping and Filtering:** Sorting data by experimental groups (e.g., control vs. stress group, sex differences).
- **Time-Based Selection:** Extraction of relevant time windows for targeted analyses.

### Analysis Functions

MMManalyzer offers analytical tools for interpreting RFID tracking data:

- **Activity Visualization:** Generate plots to illustrate movement patterns and group activity.
- **Sleep Analysis:** Estimate sleep-wake cycles based on movement and RFID detection data.
- **Comparative Behavioral Analysis:** Perform statistical comparisons of locomotion and sleep parameters between experimental groups using linear mixed-effects models (*lme*), accounting for repeated measurements and individual variability.

## Getting Started

### Prerequisites

Before using MMManalyzer, ensure that you have the following installed:

- [R](https://cran.r-project.org/) and [RStudio](https://posit.co/download/rstudio-desktop/)
- Spreadsheet software of your choice (e.g., Microsoft Excel, Google Sheets)

### Installation

1. **Clone the repository:**

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
