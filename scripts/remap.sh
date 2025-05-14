#!/bin/bash

# Path to your CSV file
csv_file="rename_table.csv"

# Loop through each line, skipping the header
tail -n +2 "$csv_file" | while IFS=',' read -r old new; do
  if [[ -f "$old" ]]; then
    mv "$old" "$new"
    echo "Renamed $old → $new"
  else
    echo "Warning: $old not found"
  fi
done
