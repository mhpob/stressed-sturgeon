#!/usr/bin/env bash

### Yes, I used Google Gemini 😢🤖



# 1. Load global bash settings
if [ -f ~/.bashrc ]; then source ~/.bashrc; fi

echo "🚀 Initializing dynamic workspace commands..."

# 2. Get project root path
PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

# 3. Dynamic Alias Generator
for script_path in "$PROJECT_ROOT/scripts"/*; do
    # Only process files that are actually executable (and skip this loader)
    if [ -f "$script_path" ] && [ -x "$script_path" ] && [ "$(basename "$script_path")" != "init.sh" ]; then
        
        # Get the filename without the path (e.g., "run_census_model.sh")
        filename=$(basename "$script_path")
        
        # Strip the extension to create a clean command name (e.g., "run_census_model")
        cmd_name="${filename%.*}"
        
        # Create a dynamic alias pointing to the absolute path of the script
        alias "$cmd_name"="$script_path"
        
        echo "   └─ Command available: $cmd_name"
    fi
done

echo "✅ All commands registered!"