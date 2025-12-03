# R Environment for Spatial Analysis & Machine Learning

A reproducible conda environment for R with support for:
- **Geospatial analysis** (sf package with GDAL, GEOS, PROJ)
- **Econometric panel methods** (MCPanel)

## Prerequisites

- **Conda/Mamba** - Package manager (Mamba recommended for faster installs)
- **Linux x86_64** - Tested on RHEL 8.10

## Installation (1-7 from cluster terminal)

### 1. Request an interactive node and move to the Github Repository

```bash
salloc
cd REDD_specification_curve
```

### 2. Choose Your Environment (5-10 minutes)

**For CPU-only:**
```bash
module load miniconda
mamba env create -f code/cluster_init/environment-cpu.yml
```

### 3. Activate the Environment

```bash
conda activate r-geo
```

### 4. Rebuild stringi (3-5 minutes) 
Fixes ICU version mismatch between conda's pre-built stringi and environment ICU library, required for MCPanel. 

```bash
R_LIBS_USER="" R --quiet --no-save -e "install.packages('stringi', type='source', repos='https://cloud.r-project.org')"
```

### 5. Install MCPanel (2-3 minutes)

```bash
R_LIBS_USER="" R --quiet --no-save -e "library(devtools); install_github('susanathey/MCPanel', force=TRUE)"
```

### 6. Catalog conda environment for OOD Apps

```bash
ycrc_conda_env.sh
```

### 7. Verify that all packages load correctly:

```bash
R_LIBS_USER="" R --quiet --no-save -e "library(sf); library(MCPanel); cat('✓ All packages loaded successfully\n')"
```

**Expected output:**
```
Linking to GEOS 3.12.1, GDAL 3.9.1, PROJ 9.4.1; sf_use_s2() is TRUE
✓ All packages loaded successfully
```

### 8. Start a RStudio Server interactive session on Bouchet.

Set `R Version` to `conda: r-geo` when starting the session. In the session, run `code/setup.R` interactively to install remaining packages. If asked if you want to update package dependencies, say NO - do not update any at any point! 


## Environment Contents

### Core Packages

| Package | Version | Purpose |
|---------|---------|---------|
| R | 4.2.3 | Base R environment |
| r-sf | 1.0.16 | Spatial data handling |
| MCPanel | ? | Matrix completion for panels |

### System Libraries

| Library | Version | Purpose |
|---------|---------|---------|
| GCC | 12.3.0 | Compiler (MCPanel compatibility) |
| GDAL | 3.9.1 | Geospatial data abstraction |
| GEOS | 3.12.1 | Geometry engine |
| PROJ | 9.4.1 | Cartographic projections |
| UDUNITS2 | 2.2.28 | Unit conversions |