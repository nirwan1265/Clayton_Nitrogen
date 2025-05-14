import pandas as pd
import os
import sys

def extend_file_ids(input_file, output_file):
    # Load the original data
    df = pd.read_csv(input_file, encoding='ISO-8859-1')  # Change encoding as needed

    # Ensure the 'file_id' column is treated as integer
    df['file_id'] = df['file_id'].astype(int)

    # Create an empty list to collect the new rows
    extended_rows = []

    # Iterate through the DataFrame
    for index, row in df.iterrows():
        current_file_id = row['file_id']
        if index < len(df) - 1:
            next_file_id = df.iloc[index + 1]['file_id']
        else:
            next_file_id = current_file_id + 1  # Handle last row case

        # Create a range of file_ids
        for file_id in range(current_file_id, next_file_id):
            new_row = row.copy()
            new_row['file_id'] = file_id
            extended_rows.append(new_row)

    # Convert the list of rows into a DataFrame
    extended_df = pd.DataFrame(extended_rows)

    # Add a new column for counting occurrences
    extended_df['measurement'] = extended_df.groupby(['CLY24-C8BC-', 'stage', 'leaf', 'plant']).cumcount() + 1

    # Reorder the columns so that "measurement" appears before "file_id"
    extended_df = extended_df[['CLY24-C8BC-', 'stage', 'leaf', 'plant', 'measurement', 'file_id']]

    # Save the extended DataFrame to a new CSV file
    extended_df.to_csv(output_file, index=False)

    print("Extended file_ids saved to:", output_file)

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python extend.py <input_file>")
        sys.exit(1)

    input_file = sys.argv[1]
    output_file = input_file.replace(".csv", "-extended.csv")

    # Call the function with provided paths
    extend_file_ids(input_file, output_file)
