#!/usr/bin/env bash

### Yes, I used Google Gemini 😢🤖


# --- 1. Arguments ---
TARGET_DIR="$1"
STAN_FILE="$2"

# Require both arguments explicitly
if [ -z "$TARGET_DIR" ] || [ -z "$STAN_FILE" ]; then
    echo "❌ Error: Missing arguments."
    echo "Usage: compile_model <target_directory> <stan_file_name>"
    echo "Example: compile_model censusUUNR census_model_inclUUNR.stan"
    exit 1
fi

# Derive a safe container name from the directory and file name (removing dots)
SAFE_FILE_NAME="${STAN_FILE//./_}"
CONTAINER_NAME="${TARGET_DIR}_compile_${SAFE_FILE_NAME}"

echo "🛠️ Launching Stan Compilation Container on Delphinus..."
echo "📂 Target Directory: ~/$TARGET_DIR"
echo "📄 Stan File:        $STAN_FILE"
echo "🐳 Container Name:   $CONTAINER_NAME"
echo "----------------------------------------------------------------------"

# --- 2. Remote Execution via SSH ---
ssh delphinus "
    echo '🐳 Starting Docker compilation container...'
    
    nohup docker run --rm \
      --name \"$CONTAINER_NAME\" \
      -v /home/mike/$TARGET_DIR:/STAN \
      -w /STAN \
      census_tag \
      Rscript -e 'cmdstanr::cmdstan_model(\"$STAN_FILE\", compile = TRUE)' \
      > /home/mike/$TARGET_DIR/stan_compile.log 2>&1 &
      
    echo '✅ Compilation process kicked off in the background!'
    echo '📝 Log routing to: ~/$TARGET_DIR/stan_compile.log'
"