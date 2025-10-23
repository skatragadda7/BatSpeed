#!/usr/bin/env python3
"""
Script to remove duplicate rows from allData.csv
"""

import pandas as pd

def main():
    print("Loading allData.csv...")
    
    # Load the data
    df = pd.read_csv("allData.csv")
    
    print(f"Original dataset shape: {df.shape}")
    print(f"Original number of rows: {len(df)}")
    
    # Check for duplicates
    print(f"\nChecking for duplicates...")
    print(f"Number of duplicate rows: {df.duplicated().sum()}")
    
    # Show some examples of duplicates
    if df.duplicated().sum() > 0:
        print(f"\nFirst 5 duplicate rows:")
        duplicate_rows = df[df.duplicated()]
        print(duplicate_rows.head())
        
        # Show which rows are duplicates
        print(f"\nDuplicate row indices:")
        duplicate_indices = df[df.duplicated()].index.tolist()
        print(f"First 10 duplicate indices: {duplicate_indices[:10]}")
    
    # Remove duplicates
    print(f"\nRemoving duplicates...")
    df_clean = df.drop_duplicates()
    
    print(f"Cleaned dataset shape: {df_clean.shape}")
    print(f"Cleaned number of rows: {len(df_clean)}")
    print(f"Rows removed: {len(df) - len(df_clean)}")
    
    # Check for duplicates in cleaned data
    print(f"\nChecking cleaned data for duplicates...")
    print(f"Number of duplicate rows in cleaned data: {df_clean.duplicated().sum()}")
    
    # Show unique players
    if 'name' in df_clean.columns:
        unique_players = df_clean['name'].nunique()
        print(f"\nUnique players in cleaned data: {unique_players}")
        
        # Show some sample players
        print(f"\nSample players in cleaned data:")
        sample_players = df_clean['name'].dropna().unique()[:10]
        for i, player in enumerate(sample_players, 1):
            print(f"  {i}. {player}")
    
    # Save cleaned data
    output_file = "allData_cleaned.csv"
    df_clean.to_csv(output_file, index=False)
    print(f"\nCleaned data saved to: {output_file}")
    
    # Show data types
    print(f"\nData types in cleaned data:")
    dtype_counts = df_clean.dtypes.value_counts()
    for dtype, count in dtype_counts.items():
        print(f"  - {dtype}: {count} columns")
    
    # Show missing data summary
    print(f"\nMissing data summary (top 10 columns with most missing data):")
    missing_data = df_clean.isnull().sum()
    missing_data = missing_data[missing_data > 0].sort_values(ascending=False)
    if len(missing_data) > 0:
        for col, missing_count in missing_data.head(10).items():
            missing_pct = (missing_count / len(df_clean)) * 100
            print(f"  - {col}: {missing_count} ({missing_pct:.1f}%)")
    else:
        print("  No missing data!")

if __name__ == "__main__":
    main()
