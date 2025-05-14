#!/usr/bin/env python3

import pandas as pd
import sys

def extend_file_ids(input_file, output_file):
    # Load the original data
    df = pd.read_csv(input_file)

    # Ensure 'file_id' is treated as integer
    df['file_id'] = df['file_id'].astype(int)

    # List to store expanded rows
    extended_rows = []

    # Loop through each row to expand file_id ranges
    for index, row in df.iterrows():
        current_file_id = row['file_id']
        if index < len(df) - 1:
            next_file_id = df.iloc[index + 1]['file_id']
        else:
            next_file_id = current_file_id + 1

        for file_id in range(current_file_id, next_file_id):
            new_row = row.copy()
            new_row['file_id'] = file_id
            extended_rows.append(new_row)

    # Create a new DataFrame
    extended_df = pd.DataFrame(extended_rows)

    # Add measurement counter per group
    extended_df['measurement'] = extended_df.groupby(['date', 'stage', 'pot', 'plant', 'leaf']).cumcount() + 1

    # Reorder columns
    extended_df = extended_df[['date', 'stage', 'pot', 'plant', 'leaf', 'measurement', 'file_id']]

    # Write to output
    extended_df.to_csv(output_file, index=False)
    print(f"✅ Extended file_ids saved to: {output_file}")

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python extend_file_ids.py <input_file>")
        sys.exit(1)

    input_file = sys.argv[1]
    output_file = input_file.replace(".csv", "-extended.csv")
    extend_file_ids(input_file, output_file)
