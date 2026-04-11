#!/bin/bash
#SBATCH --job-name=malaria_pipeline
#SBATCH --account=co_carleton
#SBATCH --qos=carleton_htc4_normal
#SBATCH --partition=savio4_htc
#SBATCH --nodes=1
#SBATCH --exclusive
# SBATCH --ntasks=1
#SBATCH --time=12:00:00
#SBATCH --mail-type=ALL
#SBATCH --mail-user=cmolitor@berkeley.edu
#SBATCH --output=./logs/pipeline_%j.out
#SBATCH --error=./logs/pipeline_%j.err

################################################################################
# SLURM Pipeline Orchestration Script for Malaria Attribution Analysis
# This script runs the complete pipeline from data extraction through figure
# generation in sequential order.
################################################################################

set -e  # Exit on error
set -u  # Exit on undefined variable
set -o pipefail  # Exit on pipe failure

################################################################################
# Setup
################################################################################

echo "========================================="
echo "Starting Malaria Attribution Pipeline"
echo "Job ID: ${SLURM_JOB_ID}"
echo "Start time: $(date)"
echo "========================================="

# Set working directory
REPO_DIR="/global/home/users/cmolitor/falciparum"
PIPELINE_DIR="${REPO_DIR}/Pipeline"
LOG_DIR="${REPO_DIR}/logs"

# Create log directory if it doesn't exist
mkdir -p "${LOG_DIR}"

cd "${REPO_DIR}"

# Load R module (adjust version as needed for your cluster)
module purge
# module load r/4.2.2

# Default apptainer image for most scripts
APPTAINER_DIR="/global/scratch/projects/co_carleton/carleton_colab/software/apptainers"
DEFAULT_APPTAINER="${APPTAINER_DIR}/rocker-geospatial.sif"

# Special apptainer for B01, F03, F04 (requires old R packages)
OLD_R_APPTAINER="${APPTAINER_DIR}/r-malaria-cru_4.2.3.sif"

################################################################################
# Helper function to run R scripts with error handling
################################################################################

run_r_script() {
    local script_path="$1"
    local script_name=$(basename "$script_path")
    local log_file="${LOG_DIR}/${script_name%.R}.log"

    echo ""
    echo "========================================="
    echo "Running: ${script_name}"
    echo "Via rocker-geospatial environment"
    echo "Time: $(date)"
    echo "========================================="

    # Run the script using default apptainer container
    if apptainer exec \
        "${DEFAULT_APPTAINER}" Rscript "${script_path}" 2>&1 \
        | tee "${log_file}"; then
        echo "✓ SUCCESS: ${script_name}"
    else
        echo "✗ FAILED: ${script_name}"
        exit 1
    fi
}

run_r_script_old() {
    local script_path="$1"
    local script_name=$(basename "$script_path")
    local log_file="${LOG_DIR}/${script_name%.R}.log"

    echo ""
    echo "========================================="
    echo "Running (via r-malaria-cru): ${script_name}"
    echo "Via r-malaria-cru environment"
    echo "Time: $(date)"
    echo "========================================="

    # Run the script using old R apptainer container
    if apptainer exec --cleanenv --contain \
        --bind /global/scratch/projects/co_carleton:/global/scratch/projects/co_carleton \
        --bind /global/home/users/cmolitor/falciparum:/global/home/users/cmolitor/falciparum \
        --pwd /global/home/users/cmolitor/falciparum \
        "${OLD_R_APPTAINER}" Rscript "${script_path}" 2>&1 \
        | tee "${log_file}"; then
        echo "✓ SUCCESS: ${script_name}"
    else
        echo "✗ FAILED: ${script_name}"
        exit 1
    fi
}

# ################################################################################
# # Section B: Extract climate and prevalence data
# ################################################################################

# echo ""
# echo "################################################################"
# echo "# SECTION B: Extract climate and prevalence data"
# echo "################################################################"

# PIPELINE_B_DIR="${PIPELINE_DIR}/B - Extract climate and prevalence data"
# run_r_script_old "${PIPELINE_B_DIR}/B01 - Extract CRU tmp and prc data ADM1.R"
# run_r_script "${PIPELINE_B_DIR}/B02 - Extract GCM tmp and prc data ADM1.R"
# run_r_script "${PIPELINE_B_DIR}/B03 - Extract CRU tmp and prc data grid.R"
# run_r_script "${PIPELINE_B_DIR}/B04 - Join prev and CRU data.R"
# run_r_script "${PIPELINE_B_DIR}/B05 - Extract ERA5 tmp and prc data ADM1.R"
# run_r_script "${PIPELINE_B_DIR}/B06 - Join prev and ERA5 data.R"

# ################################################################################
# # Section C: Model estimation
# ################################################################################

# echo ""
# echo "################################################################"
# echo "# SECTION C: Model estimation"
# echo "################################################################"

# PIPELINE_C_DIR="${PIPELINE_DIR}/C - Model estimation"
# run_r_script "${PIPELINE_C_DIR}/C01 - Main specification.R"
# run_r_script "${PIPELINE_C_DIR}/C02 - Bootstrap estimation.R"
# run_r_script "${PIPELINE_C_DIR}/C03 - VCOV sampling.R"

# ################################################################################
# # Section D: Model sensitivity analyses and checks
# ################################################################################

echo ""
echo "################################################################"
echo "# SECTION D: Model sensitivity analyses and checks"
echo "################################################################"

PIPELINE_D_DIR="${PIPELINE_DIR}/D - Model sensitivity analyses and checks"
run_r_script "${PIPELINE_D_DIR}/D01 - Model sensitivity.R"
run_r_script "${PIPELINE_D_DIR}/D02 - Randomization tests.R"
run_r_script "${PIPELINE_D_DIR}/D03 - Additional robustness.R"
run_r_script "${PIPELINE_D_DIR}/D04 - Uncertainty analysis.R"
run_r_script "${PIPELINE_D_DIR}/D05 - High resolution model.R"
run_r_script "${PIPELINE_D_DIR}/D06 - Urbanization.R"
run_r_script "${PIPELINE_D_DIR}/D07 - ERA5 analysis.R"

# ################################################################################
# # Section E: Estimate historical and future prevalence
# ################################################################################

# echo ""
# echo "################################################################"
# echo "# SECTION E: Estimate historical and future prevalence"
# echo "################################################################"

# PIPELINE_E_DIR="${PIPELINE_DIR}/E - Estimate historical and future prevalence"
# run_r_script "${PIPELINE_E_DIR}/E01 - Predict prevalence.R"

################################################################################
# Section F: Figure generation for main text
################################################################################

# echo ""
# echo "################################################################"
# echo "# SECTION F: Figure generation for main text"
# echo "################################################################"

# PIPELINE_F_DIR="${PIPELINE_DIR}/F - Figure generation for main text"
# run_r_script "${PIPELINE_F_DIR}/F01 - Prev maps and TS.R"
# run_r_script "${PIPELINE_F_DIR}/F02 - Coeff and TS.R"
# run_r_script_old "${PIPELINE_F_DIR}/F03 - Hist map, temp, elev, and TS.R"
# run_r_script_old "${PIPELINE_F_DIR}/F04 - Future map, temp, elev, and TS.R"

################################################################################
# Section G: Figure generation for supplement
################################################################################

echo ""
echo "################################################################"
echo "# SECTION G: Figure generation for supplement"
echo "################################################################"

PIPELINE_G_DIR="${PIPELINE_DIR}/G - Figure generation for supplement"
run_r_script "${PIPELINE_G_DIR}/G01 - Visualize thermal curve expectations and data.R"
run_r_script "${PIPELINE_G_DIR}/G02 - Historical partials.R"
run_r_script "${PIPELINE_G_DIR}/G03 - Future partials.R"
run_r_script "${PIPELINE_G_DIR}/G04 - Attributable map.R"
run_r_script "${PIPELINE_G_DIR}/G05 - Projection maps.R"
run_r_script "${PIPELINE_G_DIR}/G06 - Monthly time series.R"
run_r_script "${PIPELINE_G_DIR}/G07 - vcov sample.R"

# ################################################################################
# # Section H: Summary statistics for main text
# ################################################################################

# echo ""
# echo "################################################################"
# echo "# SECTION H: Summary statistics for main text"
# echo "################################################################"

# PIPELINE_H_DIR="${PIPELINE_DIR}/H - Summary statistics for main text"
# run_r_script "${PIPELINE_H_DIR}/H01 - Thermal responses.R"
# run_r_script "${PIPELINE_H_DIR}/H02 - Historical summary.R"
# run_r_script "${PIPELINE_H_DIR}/H03 - Future summary.R"
# run_r_script "${PIPELINE_H_DIR}/H04 - Global warming levels.R"

################################################################################
# Pipeline Complete
################################################################################

echo ""
echo "========================================="
echo "Pipeline completed successfully!"
echo "End time: $(date)"
echo "Total elapsed time: ${SECONDS} seconds"
echo "========================================="
