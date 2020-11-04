from chembl_webresource_client.new_client import new_client
import multiprocessing as mp
import numpy as np
import pandas as pd
import swifter
from collections import defaultdict

# Get a list of available tables to query
available_resources = [resource for resource in dir(new_client) if not
    resource.startswith('_')]

# Initiate connections to the tables we want to query
activity = new_client.activity
target = new_client.target
molecule = new_client.molecule

# Read in some sample genes
drug_info = pd.read_csv('/home/ceeles/Development/DataIngestion/PharmacoDI/data/rawdata/GDSC_v1_PSet/GDSC_v1@drug.csv.gz')
drug_ids = drug_info.inchikey.dropna()

# Setup a pool
pool = mp.Pool(mp.cpu_count())


# Retrieve all targets available in Chembl
target_result = target.filter(organism__in=[
    'Homo sapiens', 'Rattus norvegicus'])

results = list(target_result)
df = pd.DataFrame(results)

object_columns = df.dtypes[df.dtypes == 'object'].index.values

for column in object_columns:
    df = df.explode(column)

cross_references = pd.DataFrame(df.cross_references).dropna()

def group_dict_by_key(dictionary):
    new_dict = defaultdict(list)
    for key, value in dictionary:
        new_dict[key] = value
    return new_dict

# TODO:: Parse the object columns into their own DataFrames

## TODO:: Query the API to map Chembl ID to an identifier in drug_info
## - ideally it would map to IUPAC name, since we have that for every drug
## - if not we can map to smiles and inchikey then merge the non-redundant
## results

