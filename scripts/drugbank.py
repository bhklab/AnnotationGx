#!/usr/bin/env python
# -*- coding: utf-8 -*-
​
import sys
import argparse
import codecs
from collections import Counter
from lxml import etree
# from xml import etree #untested
​
#-i /scratch1/tmp/myourshaw/drugbank/drugbank.xml
#-i /Users/myourshaw/Downloads/drugbank.xml
​
​
def run(input):
    """Writes relational database text files for drugs, drug_target, drug_target_action, and targets tables.
    Input: the path of a DrugBank xml file.
    Output: text files that can be used as inputs to SQL tables.
    Method:
        1. Read and parse xml file.
        2. Extract data and save records as key, value pairs.
        3. Write output files."""
    #output file names
    drugs_out = input + '_drugs.txt'
​
    #counter for number of records in each file
    record_counts = Counter()
​
    #open input file and parse xml
    #get drugbank namspace
    tree = etree.ElementTree(file=input)
    #a few namespace tricks to make the code more readable
    ns = tree.getroot().nsmap
    ns['db'] = ns[None]
    del ns[None]
​
    drugs = tree.xpath('db:drug', namespaces=ns)
​
    #process drug records
    for drug in drugs:
        #initialize dict to save unique records to print at end of run
        #in the form {(<key tuple>): {<value dict>}]}
        drug_id = [i for i in drug.xpath('db:drugbank-id', namespaces=ns) if i.attrib.get('primary') == 'true'][0].text
        drug_name = drug.xpath('db:name', namespaces=ns)[0].text
​
        #process the targets of each drug
        for target in drug.xpath('db:targets/db:target', namespaces=ns):
            target_id = target.xpath('db:id', namespaces=ns)[0].text
​
            #process the actions of the drug on the target
            for action in drug.xpath('db:targets/db:target/db:polypeptide/db:gene-name', namespaces=ns):
                if type(action.text) != type(None):
                    gene_name = action.text
                else:
                    gene_name = "NULL"
​
                lines = drug_id + "\t" + drug_name + "\t" + target_id + "\t" + gene_name
                #lines = u'\t'.join((drug_id, drug_name, target_id, gene_name)).encode('utf-8').strip()
                print(lines)