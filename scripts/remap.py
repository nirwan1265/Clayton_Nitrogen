import os
import csv
import sys

def rename_files(csv_file_path, directory):
    with open(csv_file_path, encoding='utf-8-sig', newline='') as csvfile:
        reader = csv.DictReader(csvfile)
        print("Detected headers:", reader.fieldnames)  # Debug line

        for row in reader:
            try:
                old_file = row['old_name.asd'].strip()
                new_file = row['spectra_name.asd'].strip()
                old_file_path = os.path.join(directory, old_file)
                new_file_path = os.path.join(directory, new_file)

                os.rename(old_file_path, new_file_path)
                print(f'Renamed: {old_file_path} to {new_file_path}')
            except FileNotFoundError:
                print(f'File not found: {old_file_path}')
            except Exception as e:
                print(f'Error renaming {old_file_path} to {new_file_path}: {e}')

if __name__ == "__main__":
    if len(sys.argv) != 3:
        print("Usage: python3 remap.py <directory> <csv_file>")
        sys.exit(1)

    directory = sys.argv[1]
    csv_file_path = sys.argv[2]

    rename_files(csv_file_path, directory)
