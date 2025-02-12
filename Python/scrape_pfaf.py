import requests
from bs4 import BeautifulSoup
import pandas as pd
import time
import argparse

# Function to scrape edibility and medicinal use scores from PFAF
def get_pfaf_scores(genus, species):
  url = f"https://pfaf.org/user/Plant.aspx?LatinName={genus}+{species}"
  response = requests.get(url)
  soup = BeautifulSoup(response.text, 'html.parser')

  # Extract edibility score
  try:
    edibility = soup.find("span", id="ContentPlaceHolder1_txtEdrating").text.strip()
  except AttributeError:
    edibility = "N/A"

  # Extract medicinal use score
  try:
    medicinal = soup.find("span", id="ContentPlaceHolder1_txtMedRating").text.strip()
  except AttributeError:
    medicinal = "N/A"

  return edibility, medicinal

# Main function
def main(input_file, output_file):
  # Read the CSV file
  df = pd.read_csv(input_file)

  # Add new columns for edibility and medicinal use scores
  df["edibility_score"] = ""
  df["medicinal_score"] = ""

  # Iterate through each row and scrape data
  for index, row in df.iterrows():
    genus = row["genus"]
    species = row["species"]

    print(f"Scraping {genus} {species}...")

    # Get scores from PFAF
    edibility, medicinal = get_pfaf_scores(genus, species)

    # Update the DataFrame
    df.at[index, "edibility_score"] = edibility
    df.at[index, "medicinal_score"] = medicinal

    # Add a delay to avoid overloading the server
    time.sleep(2)  # Adjust the delay as needed

  # Save the updated DataFrame to a new CSV file
  df.to_csv(output_file, index=False)
  print(f"Data saved to {output_file}")

# Parse command-line arguments
if __name__ == "__main__":
  parser = argparse.ArgumentParser(description="Scrape edibility and medicinal use scores from PFAF.")
  parser.add_argument("input_file", help="Path to the input CSV file (e.g., species.csv)")
  parser.add_argument("output_file", help="Path to the output CSV file (e.g., species_with_scores.csv)")
  args = parser.parse_args()

  # Run the main function with user-provided file paths
  main(args.input_file, args.output_file)
