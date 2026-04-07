import pandas as pd
import os

def create_master_dataset():
    data = {}

    folder = "../data/original/"
    for file_name in os.listdir(folder):
        file_path = os.path.join(folder, file_name)
        if file_name.endswith('.csv'):
            try:
                data[file_name] = pd.read_csv(file_path, encoding='utf-8')
            except UnicodeDecodeError:
                data[file_name] = pd.read_csv(file_path, encoding='ISO-8859-1')

    return data


def clean_unnamed(data):
    for df in data.values():
        columns = df.columns
        unnamed = [column for column in columns if "Unnamed" in column]
        df.drop(columns=unnamed, axis=1, inplace=True)


def join_datasets(data):
    return pd.concat(data.values(), ignore_index=True)


if __name__ == '__main__':
    data = create_master_dataset()
    clean_unnamed(data)
    data = join_datasets(data)
    data.to_csv("../data/processed/master_dataset.csv", index=False)