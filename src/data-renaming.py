import os

rename_map = {
    "../data/original/AMECO1.CSV": "../data/original/AMECO1_Population_Employment.csv",
    "../data/original/AMECO2.CSV": "../data/original/AMECO2_Consumption.csv",
    "../data/original/AMECO3.CSV": "../data/original/AMECO3_CapitalFormation_Saving.csv",
    "../data/original/AMECO4.CSV": "../data/original/AMECO4_Domestic_Final_Demand.csv",
    "../data/original/AMECO5.CSV": "../data/original/AMECO5_National_Income.csv",
    "../data/original/AMECO6.CSV": "../data/original/AMECO6_Domestic_Product.csv",
    "../data/original/AMECO7.CSV": "../data/original/AMECO7_GDP_Labour_Costs.csv",
    "../data/original/AMECO8.CSV": "../data/original/AMECO8_Capital_Stock.csv",
    "../data/original/AMECO9.CSV": "../data/original/AMECO9_Exports_Imports.csv",
    "../data/original/AMECO10.CSV": "../data/original/AMECO10_External_Balances.csv",
    "../data/original/AMECO11.CSV": "../data/original/AMECO11_Foreign_Trade.csv",
    "../data/original/AMECO12.CSV": "../data/original/AMECO12_National_Accounts_by_Activity.csv",
    "../data/original/AMECO13.CSV": "../data/original/AMECO13_Monetary_Variables.csv",
    "../data/original/AMECO14.CSV": "../data/original/AMECO14_Corporations.csv",
    "../data/original/AMECO15.CSV": "../data/original/AMECO15_Households_NPISH.csv",
    "../data/original/AMECO16.CSV": "../data/original/AMECO16_General_Government.csv",
    "../data/original/AMECO17.CSV": "../data/original/AMECO17_Cyclical_Adjustment.csv",
    "../data/original/AMECO18.CSV": "../data/original/AMECO18_Public_Debt.csv"
}

for old_name, new_name in rename_map.items():
    if os.path.exists(old_name):
        os.rename(old_name, new_name)
        print(f"Renamed: {old_name} -> {new_name}")
    else:
        print(f"File not found: {old_name}")
