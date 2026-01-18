import pandas as pd

df = pd.read_csv('../data/processed/master_dataset.csv')
export_vars = df[df['TITLE'].str.contains('export', case=False, na=False)].CODE
import_vars = df[df['TITLE'].str.contains('import', case=False, na=False)].CODE
debt_vars = df[df['TITLE'].str.contains('debt', case=False, na=False)].CODE

print(export_vars.unique())
print(import_vars.unique())
print(debt_vars.unique())