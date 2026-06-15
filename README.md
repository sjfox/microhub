# MicroHub Forecasting Tool

![MicroHub home page](images/home-page.png)

## Table of Contents

-   [Purpose](#purpose)
-   [Currently Available Models](#currently-available-models)
-   [Installation](#installation)
    -   [1. Install R and RStudio](#1-install-r-and-rstudio)
    -   [2. Download the repository](#2-download-the-repository)
    -   [3. Open the project](#3-open-the-project)
    -   [4. Install required R packages](#4-install-required-r-packages)
    -   [5. Development model setup (optional)](#5-development-model-setup-optional)
    -   [6. Launch the application](#6-launch-the-application)
-   [Using the Tool](#using-the-tool)
    -   [Data Format](#data-format)
    -   [Settings](#settings)
    -   [Running Models](#running-models)
    -   [Ensemble](#ensemble)
    -   [Downloading Results](#downloading-results)
    -   [Retrospective Forecasts](#retrospective-forecasts)
-   [Model Descriptions](#model-descriptions)
-   [Acknowledgements](#acknowledgements)
-   [Contact](#contact)

---

## Purpose

The **MicroHub Forecasting Tool** is a general-purpose infectious disease forecasting dashboard designed to support real-time epidemic prediction within collaborative forecasting hub infrastructure. The tool allows users to upload epidemiological data and produce forecasts from established forecasting approaches that have been used across a number of seasonal infectious diseases and geographies.

The tool was originally developed to generate forecasts for Severe Acute Respiratory Illness (SARI) within the Paraguay sentinel surveillance system as part of the Paraguay Forecast Hub. Since its initial deployment, the software has been generalized to support infectious disease forecasting workflows across a wide range of pathogens, locations, and hub infrastructures. Ultimately, the MicroHub is designed to empower public health practitioners and forecasting teams with accessible, transparent, and operational forecasting tools that can be deployed locally while remaining fully compatible with modern hub-based forecasting infrastructure.

---

## Currently Available Models

The dashboard currently includes:

-   Three baseline models: **Regular Baseline**, **Optimal Baseline**, and **Seasonal Baseline**
-   Three main individual models: **Copycat**, **INFLAenza**, and **newGBQR**
-   Development models in the **Development** tab: **CalCopycat** and **FourCAT**
-   Ensemble functionality to aggregate forecasts from any combination of individual models
-   Retrospective forecast runs for evaluating selected models across past reference weeks

The tool facilitates three primary tasks:

### 1. Forecast Generation

Producing real-time forecasts using statistical, machine learning, and deep learning models on user-uploaded surveillance data, without requiring direct interaction with model source code.

### 2. Model Exploration, Visualization, and Ensembling

Visualizing epidemiological data and forecast outputs to support model diagnostics, parameter adjustment, and transparent interpretation. Users can also create ensemble predictions that aggregate individually generated forecasts and have often performed strongly in collaborative forecast hub settings.

### 3. Hub-Ready Output Formatting

The tool facilitates the export of forecasts in standardized formats compatible with the [**hubverse**](https://hubverse.io/) ecosystem, enabling seamless submission to forecasting hubs and integration into evaluation pipelines and reporting systems. By producing forecasts directly in hubverse-compliant formats, the MicroHub tool supports reproducible, interoperable forecasting workflows and lowers technical barriers for public health agencies wishing to produce forecasts for themselves or as part of collaborative forecast hubs.

---

## Installation

The MicroHub Forecasting Tool is implemented as a **Shiny dashboard in R**. Due to computational and software constraints, the tool is not hosted online and must be run locally.

### Docker quick start

If Docker Desktop is installed, you can run MicroHub without installing R, RStudio, INLA, Python, or the app's package dependencies directly on your computer.

From the repository root, build and start the app:

```bash
docker compose up --build
```

Then open:

```text
http://localhost:3838
```

The first build can take a while because it installs R packages, INLA, Python packages, and the FourCAT PyTorch runtime. Later starts are much faster:

```bash
docker compose up
```

To stop the app, press `Ctrl+C` in the terminal running Docker Compose.

---

The steps below assume introductory familiarity with R.

### 1. Install R and RStudio

Download and install:

-   R and RStudio Desktop: <https://posit.co/download/rstudio-desktop/>

More detailed setup instructions can be found in *Hands-On Programming with R*.

### 2. Download the repository

Clone or download the code from:

<https://github.com/sjfox/microhub>

You may download a `.zip` file by clicking the green **Code** button and selecting `Download ZIP`. Extract the folder to a location on your machine.

### 3. Open the project

Open `microhub.Rproj` in RStudio.

### 4. Install required R packages

**Windows users:** Install Rtools before installing the required packages:
<https://cran.r-project.org/bin/windows/Rtools/>

Rtools provides the build tools needed to install R packages from source on Windows. This is especially important because some MicroHub dependencies, including `epiprocess`, are installed from GitHub rather than CRAN binaries. Choose the Rtools version that matches your installed R version.

After installing Rtools, restart RStudio and confirm R can find it:

```r
Sys.which("make")
```

This should return a path rather than an empty string.

Run the `install-packages.R` script located in the `R/` folder. This script installs CRAN packages, R-INLA, and GitHub-only packages such as `epiprocess` and `simplets`.

> **Note:**
> The INFLAenza model depends on **R-INLA**, which is not available on CRAN.
> Installation instructions and compatibility guidance are available at:
> <https://www.r-inla.org/download-install>
> If the test example on that site runs successfully, INLA is properly installed.

### 5. Development model setup (optional)

The **Development** tab includes experimental models. **CalCopycat** uses the standard R package setup above and does not require additional installation. **FourCAT** is a development deep learning model that requires Python 3.11 and a one-time environment setup. If you do not intend to run FourCAT, skip this section — all main workflow models will work without it.

#### 5a. Install Python 3.11 for FourCAT

FourCAT requires **Python 3.11 specifically** (not 3.12 or later) due to package compatibility with the model checkpoint files.

**macOS:**
```bash
brew install python@3.11
```
If you do not have Homebrew, install it first from <https://brew.sh>.

**Windows:**

Download the Python 3.11 installer from:
<https://www.python.org/downloads/release/python-3110/>

Run the installer and make sure to check **"Add Python to PATH"** during installation.

**Linux (Ubuntu/Debian):**
```bash
sudo apt install python3.11 python3.11-venv
```

#### 5b. Run the FourCAT setup script

Once Python 3.11 is installed, open RStudio and run:

```r
install.packages("reticulate")
source("R/setup_fourcat_env.R")
```

This script will:

1. Locate your Python 3.11 installation automatically
2. Create a Python virtual environment called `fourcat_env`
3. Install the required Python packages (`torch 2.2.2`, `pandas 2.2.3`, `numpy 1.26.4`)
4. Verify that everything installed correctly

The setup takes a few minutes on first run due to the size of the PyTorch package. You only need to run it once.

> **Note:** If you need to rebuild the environment from scratch, delete it first and then rerun the setup script:
>
> macOS/Linux: `rm -rf ~/.virtualenvs/fourcat_env`
>
> Windows: Delete the folder `%USERPROFILE%\.virtualenvs\fourcat_env`

#### 5c. FourCAT checkpoint files

FourCAT uses pre-trained model checkpoint files located in `data/fourcat_checkpoints/`. These files are included in the repository. The setup script does not need to download them.

### 6. Launch the application

Open `app.R` and click **Run App** in RStudio. The MicroHub dashboard will open in your browser.

---

## Using the Tool

### Data Format

Before running any models, upload your surveillance data using the **Data Upload & Settings** tab. Your CSV file must contain the following columns:

| Column | Type | Description |
|---|---|---|
| `date` | Date (MM/DD/YYYY) | Epidemiological week date |
| `target_group` | Character | Population subgroup (e.g. Overall, Adult, Pediatric) |
| `value` | Numeric | Case count or hospitalizations for that week |

A downloadable data template is available in the **Data Upload & Settings** tab.

### Settings

The following settings apply to all models and should be configured before running any forecasts:

| Setting | Description |
|---|---|
| **Forecast Date** | The date the forecast is being run. Defaults to the current date when the app is opened or a dataset is uploaded. |
| **Data to Drop** | Number of recent weeks to exclude from model input to account for data backfill lag (0–4 weeks). |
| **Forecast Output** | Whether to keep all formatted horizons or only horizons greater than or equal to 0. |
| **Local Seasonality** | Flu seasonality zone for your location (Zones A–E). The app defaults this from the uploaded filename when it contains a recognized country name; otherwise it defaults to Paraguay. |
| **Forecast Horizon** | Number of weeks ahead to forecast (1–6 weeks). |

#### Data to Drop

The **Data to Drop** setting is particularly important for real-time forecasting. Recent surveillance data is often incomplete due to reporting delays — selecting 1 or more weeks removes the most recent observations from the model input and extends the forecast horizon accordingly, so the output still covers the full requested window ahead.

### Running Models

Each model has its own tab in the navigation bar. Navigate to a model tab, adjust any model-specific settings if needed, and click the **Run** button. Results are displayed as a plot and can be downloaded as a PNG.

Models can be run in any order and in any combination. Each model runs independently.

### Ensemble

Once at least two models have been run, navigate to the **Ensemble** tab. Select which models to include and click **Run Ensemble**. The ensemble combines forecasts by taking the median value across the selected models at each quantile, horizon, and target group. You can also upload an outside model forecast using the template provided in the Ensemble tab and include it as an ensemble member.

### Downloading Results

Navigate to the **Download** tab to preview and download all forecast outputs as a single CSV file. The file is formatted for direct submission to hubverse-compatible forecasting hubs. The Download tab can also export all generated app plots as a multi-page PDF, with one model plot per page.

### Retrospective Forecasts

The **Retrospective** tab lets you upload a historical dataset, choose a range of past reference weeks, select models, and run forecasts as if they had been generated at each historical reference week. Results are packaged as a downloadable ZIP file.

---

## Model Descriptions

### Regular Baseline

A flat baseline model that projects the most recent observed value forward across all forecast horizons. Provides a simple reference point for evaluating more complex models.

### Optimal Baseline

A variant of the regular baseline that uses an 8-week trailing window to estimate the baseline level, which can better reflect recent trends compared to the single most recent value.

### Seasonal Baseline

A seasonally-adjusted baseline that incorporates historical seasonal patterns to project forward. Useful when strong seasonal structure is present in the data.

### Copycat

A trajectory-matching model that identifies historical epidemic seasons with similar recent dynamics and uses them to project forward. Performs well when current season dynamics resemble historical patterns.

### INFLAenza

A Bayesian hierarchical model implemented using R-INLA. Uses a random walk seasonal component and an autoregressive temporal component to produce calibrated probabilistic forecasts. Well-suited for multi-group surveillance data.

### newGBQR

newGBQR is a year-round gradient-boosted quantile regression model. It removes fixed in-season windows, learns peak timing empirically from the uploaded data, and uses cyclic week-of-year and recent-trend features to produce quantile forecasts across multiple horizons.

### CalCopycat

CalCopycat is a development model that extends Copycat with leave-one-out historical calibration. It is available in the **Development** tab and does not require additional setup beyond the standard R package installation.

### FourCAT

FourCAT is a development deep learning model based on a Transformer encoder architecture with Fourier-augmented epiweek embeddings. Trained on multi-season surveillance data across multiple locations, FourCAT captures complex seasonal and temporal patterns to produce calibrated probabilistic forecasts. Forecasts are generated from an ensemble of three model seeds and averaged to improve robustness.

FourCAT requires a one-time Python environment setup only if you want to run it — see [Development model setup](#5-development-model-setup-optional) above.

---

## Acknowledgements

The MicroHub tool originated through close collaboration with public health partners in Paraguay, including the Ministerio de Salud Pública y Bienestar Social (MSPBS), the Pan American Health Organization (PAHO), the Council for State and Territorial Epidemiologists (CSTE), and the Centers for Disease Control and Prevention (CDC). The authors gratefully acknowledge the foundational work supporting the Paraguay Forecast Hub and the development of the original SARI forecasting workflows that informed this generalized tool through CSTE/CDC grant NU38OT000297.

We especially thank Jadey Ryan for the initial development of the Shiny application and integration of forecasting model software. Additional feedback from members of the CSTE, CDC, and MIDAS forecasting working groups also greatly strengthened the tool's development.

The content is solely the responsibility of the authors and does not necessarily represent the official views of MSPBS, PAHO, CSTE, CDC, or MIDAS.

---

## Contact

Spencer J. Fox\
School of Informatics, Computing, and Cyber Systems\
Northern Arizona University\
[spencer.fox@nau.edu](mailto:spencer.fox@nau.edu)
