#!/usr/bin/env bash

### Yes, I used Google Gemini 😢🤖


# Exit immediately if a command exits with a non-zero status
set -e

# --- 1. Validation & Arguments ---
if [ "$#" -ne 2 ]; then
    echo "❌ Error: Missing arguments."
    echo "Usage: $0 <start_date> <end_date>"
    echo "Example: $0 2024-08-13 2024-10-30"
    exit 1
fi

START_DATE="$1"
END_DATE="$2"

# Extract the year (first 4 characters) from the start_date
YEAR=$(echo "$START_DATE" | cut -d'-' -f1)

# Quick validation to ensure year looks like a 4-digit number
if [[ ! "$YEAR" =~ ^[0-8][0-9]{3}$ ]]; then
    echo "❌ Error: Invalid date format. Please use YYYY-MM-DD."
    exit 1
fi

echo "📅 Processing data for Year: $YEAR ($START_DATE to $END_DATE)"
echo "--------------------------------------------------"

# Define paths dynamically using the inferred year
TARGET_DIR="census_model/Stan/$YEAR/censusUUNR"

# Ensure the local target directory exists before running R or copying
mkdir -p "$TARGET_DIR"

# --- 2. Create Data via R ---
echo "⚙️ Step 1: Generating Stan data..."
Rscript -e "source('census_model/Stan/create_stan_data.R'); generate_stan_data(
  start_date = '${START_DATE}',
  end_date = '${END_DATE}',
  out_file = '${TARGET_DIR}/UUNR_stan_data.json'
)"

# --- 3. Copy Local Model File ---
echo "📂 Step 2: Copying .stan model file to target folder..."
cp ./census_model/Stan/models/census_model_inclUUNR.stan "./$TARGET_DIR"

# --- 4. Sync with Remote Server ---
echo "🚀 Step 3: Transferring files to Delphinus server..."
# Using the inferred year for the remote path as well
scp -r "./$TARGET_DIR" "delphinus:/home/mike/censusUUNR/$YEAR"

echo "--------------------------------------------------"
echo "✅ Finished successfully!"