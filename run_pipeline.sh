#!/bin/bash
################################################################################
# Local Pipeline Orchestration Script for Malaria Attribution Analysis
# Runs the complete pipeline from data extraction through figure generation
# in sequential order, calling Rscript directly on the local machine.
################################################################################

set -e  # Exit on error
set -u  # Exit on undefined variable
set -o pipefail  # Exit on pipe failure

export OBJC_DISABLE_INITIALIZE_FORK_SAFETY=YES

################################################################################
# Setup 
################################################################################

# Set working directory - adjust this to your local repo location
REPO_DIR="${REPO_DIR:-$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)}"
PIPELINE_DIR="${REPO_DIR}/Pipeline"
LOG_DIR="${REPO_DIR}/logs"

# Create log directory if it doesn't exist
mkdir -p "${LOG_DIR}"

cd "${REPO_DIR}"

# Send ALL output (banners, echoes, and R output) to a master log while
# still printing to the terminal. Each script also gets its own per-script log.
MASTER_LOG="${LOG_DIR}/pipeline_$(date +%Y%m%d_%H%M%S).log"
exec > >(tee "${MASTER_LOG}") 2>&1

echo "========================================="
echo "Starting Malaria Attribution Pipeline"
echo "Start time: $(date)"
echo "Master log: ${MASTER_LOG}"
echo "========================================="

################################################################################
# Helper function to run R scripts with error handling 
################################################################################

run_r_script() {
    local script_path="$1"
    local script_name
    script_name=$(basename "$script_path")
    local log_file="${LOG_DIR}/${script_name%.R}.log"

    echo ""
    echo "========================================="
    echo "Running: ${script_name}"
    echo "Time: $(date)"
    echo "========================================="

    if Rscript "${script_path}" 2>&1 | tee "${log_file}"; then
        echo "✓ SUCCESS: ${script_name}"
    else
        echo "✗ FAILED: ${script_name}"
        exit 1
    fi
}

################################################################################
# Section B: Extract climate and prevalence data 
################################################################################

echo ""
echo "################################################################"
echo "# SECTION B: Extract climate and prevalence data"
echo "################################################################"

PIPELINE_B_DIR="${PIPELINE_DIR}/B - Extract climate and prevalence data"
run_r_script "${PIPELINE_B_DIR}/B01 - Extract CRU tmp and prc data ADM1.R"
run_r_script "${PIPELINE_B_DIR}/B02 - Extract GCM tmp and prc data ADM1.R"
run_r_script "${PIPELINE_B_DIR}/B03 - Extract CRU tmp and prc data grid.R"
run_r_script "${PIPELINE_B_DIR}/B04 - Join prev and CRU data.R"
run_r_script "${PIPELINE_B_DIR}/B05 - Extract ERA5 tmp and prc data ADM1.R"
run_r_script "${PIPELINE_B_DIR}/B06 - Join prev and ERA5 data.R"

################################################################################
# Section C: Model estimation
################################################################################

echo ""
echo "################################################################"
echo "# SECTION C: Model estimation"
echo "################################################################"

PIPELINE_C_DIR="${PIPELINE_DIR}/C - Model estimation"
run_r_script "${PIPELINE_C_DIR}/C01 - Main specification.R"
run_r_script "${PIPELINE_C_DIR}/C02 - Bootstrap estimation.R"
run_r_script "${PIPELINE_C_DIR}/C03 - VCOV sampling.R"

################################################################################
# Section D: Model sensitivity analyses and checks
################################################################################

echo ""
echo "################################################################"
echo "# SECTION D: Model sensitivity analyses and checks"
echo "################################################################"

PIPELINE_D_DIR="${PIPELINE_DIR}/D - Model sensitivity analyses and checks"
run_r_script "${PIPELINE_D_DIR}/D01 - Model sensitivity.R"
run_r_script "${PIPELINE_D_DIR}/D02 - Randomization tests.R"
run_r_script "${PIPELINE_D_DIR}/D03 - Additional robustness.R"
run_r_script "${PIPELINE_D_DIR}/D04 - Uncertainty analysis.R"
run_r_script "${PIPELINE_D_DIR}/D04b - newrob.R"
run_r_script "${PIPELINE_D_DIR}/D05 - High resolution model.R"
run_r_script "${PIPELINE_D_DIR}/D06 - Urbanization.R"
run_r_script "${PIPELINE_D_DIR}/D07 - ERA5 analysis.R"

################################################################################
# Section E: Estimate historical and future prevalence
################################################################################

echo ""
echo "################################################################"
echo "# SECTION E: Estimate historical and future prevalence"
echo "################################################################"

PIPELINE_E_DIR="${PIPELINE_DIR}/E - Estimate historical and future prevalence"
run_r_script "${PIPELINE_E_DIR}/E01 - Predict prevalence.R"

################################################################################
# Section F: Figure generation for main text
################################################################################

echo ""
echo "################################################################"
echo "# SECTION F: Figure generation for main text"
echo "################################################################"

PIPELINE_F_DIR="${PIPELINE_DIR}/F - Figure generation for main text"
run_r_script "${PIPELINE_F_DIR}/F01 - Prev maps and TS.R"
run_r_script "${PIPELINE_F_DIR}/F02 - Coeff and TS.R"
run_r_script "${PIPELINE_F_DIR}/F03 - Hist map, temp, elev, and TS.R"
run_r_script "${PIPELINE_F_DIR}/F04 - Future map, temp, elev, and TS.R"

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

################################################################################
# Section H: Summary statistics for main text
################################################################################

echo ""
echo "################################################################"
echo "# SECTION H: Summary statistics for main text"
echo "################################################################"

PIPELINE_H_DIR="${PIPELINE_DIR}/H - Summary statistics for main text"
run_r_script "${PIPELINE_H_DIR}/H01 - Thermal responses.R"
run_r_script "${PIPELINE_H_DIR}/H02 - Historical summary.R"
run_r_script "${PIPELINE_H_DIR}/H03 - Future summary.R"
run_r_script "${PIPELINE_H_DIR}/H04 - Global warming levels.R"

################################################################################
# Pipeline Complete
################################################################################

echo ""
echo "========================================="
echo "Pipeline completed successfully!"
echo "End time: $(date)"
echo "Total elapsed time: ${SECONDS} seconds"
echo "========================================="