#!/usr/bin/env bash

### Yes, I used Google Gemini 😢🤖


# --- 1. Arguments & Defaults ---
COMMAND="$1"
YEAR="${2:-$(date +%Y)}"

# --- 2. Help/Usage Messaging ---
usage() {
    echo "📋 Usage: check_in <command> [year]"
    echo "Available commands:"
    echo "  list     - List the contents of the remote census folder for the year"
    echo "  compile  - View the compilation logs for Stan for the year"
    echo "  model    - View the docker status and the model's output log"
    echo ""
    echo "Examples:"
    echo "  check_in list 2024"
    echo "  check_in model       (Defaults to year $YEAR)"
    exit 1
}

if [ -z "$COMMAND" ]; then
    usage
fi

# --- 3. Command Execution Routing ---
case "$COMMAND" in
    "list")
        echo "🔍 Listing remote directory for year: $YEAR..."
        ssh delphinus "ls ~/censusUUNR/$YEAR"
        ;;

    "compile")
        echo "🛠️ Fetching compilation logs for year: $YEAR..."
        ssh delphinus "
            echo '=================================================='
            echo '🐳 RUNNING DOCKER CONTAINERS'
            echo '=================================================='
            docker ps
            echo -e '\n'
            echo '=================================================='
            echo '📄 STAN COMPILATION LOG ($YEAR) - TAIL'
            echo '=================================================='
            cat ~/censusUUNR/$YEAR/stan_compile.log | tail
        "
        ;;

    "model")
        echo "📊 Fetching model status for year: $YEAR..."
        ssh delphinus "
            echo '=================================================='
            echo '🐳 RUNNING DOCKER CONTAINERS'
            echo '=================================================='
            docker ps
            echo -e '\n'
            echo '=================================================='
            echo '📄 MODEL OUTPUT LOG (out${YEAR}.log) - TAIL'
            echo '=================================================='
            cat ~/censusUUNR/out${YEAR}.log | tail
        "
        ;;

    *)
        echo "❌ Error: Unknown command '$COMMAND'"
        usage
        ;;
esac