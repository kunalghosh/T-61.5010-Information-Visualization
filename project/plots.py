from __future__ import print_function
import seaborn as sns
import pandas as pd

import matplotlib.pyplot as plt
import matplotlib
matplotlib.style.use('ggplot')


df = pd.read_table("kuitti2.dat")
df.columns = ["Date","Expense","Description","Category","Remarks"]

newdf = df.groupby(df.Category).sum()
newdf["Expense"] = newdf["Expense"]/100
newdf.plot.barh()
plt.show()
