# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

import pandas as pd
import seaborn as sns

df = pd.read_table("kuitti.dat")
df.columns = ["Date","Expense","Description","Category","Remarks"]
sns.barplot(x=df.)