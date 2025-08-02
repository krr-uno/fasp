# fasp

clingo 6 API: https://potassco.org/clingo-preview/python-api/clingo.html

conda create -n clingo6 python 3=13
conda activate clingo6
conda install -c potassco/label/dev-20 -c conda-forge clingo
git clone ..
cd fasp
python -m pip install -e .