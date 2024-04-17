from scipy.io import arff
import pandas

arff_file = arff.loadarff('speeddating.arff')
df = pandas.DataFrame(arff_file[0])

print(df)

# MAKE DATA USABLE

# LOOKING FOR MISSING DATA AND TREAT IT

# LOOK FOR 0 VARIANCE VARIABLES

# NORMALIZE DATASET??