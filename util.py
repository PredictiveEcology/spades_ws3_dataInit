def load_pickled_hdt(path):
  import pickle
  return pickle.load(open(path, 'rb'))
