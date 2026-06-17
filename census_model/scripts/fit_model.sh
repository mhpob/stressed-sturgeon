#!/usr/bin/env bash

### Yes, I used Google Gemini 😢🤖


# --- 1. Arguments & Defaults ---
# Use the first argument as the year, or default to the current calendar year
YEAR="$1"

if [ -z "$YEAR" ]; then
    echo "❌ Error: Missing year argument."
    echo "Usage: fit_model <year>"
    echo "Example: fit_model 2024"
    exit 1
fi

echo "🚀 Launching Stan Model Container on Delphinus for Year: $YEAR..."
echo "------------------------------------------------------------------"

# --- 2. Remote Execution via SSH ---
# We use a multi-line SSH string. Variables like $YEAR will be evaluated 
# locally before being sent over to Delphinus.
ssh delphinus "
    echo '🐳 Starting Docker container (Name: census_$YEAR)...'
    
    nohup docker run --rm \
      --name census_$YEAR \
      -v /home/mike/censusUUNR/$YEAR:/STAN \
      -w /STAN \
      census_tag \
      Rscript -e 'source(\"${YEAR}_run_stan_UUNR.R\")' \
      > /home/mike/censusUUNR/out${YEAR}.log 2>&1 &
      
    echo '✅ Container process kicked off in the background!'
    echo '📝 Check logs using: check_in model $YEAR'
"