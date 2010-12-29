import rlcompleter
pdb.Pdb.complete = rlcompleter.Completer(locals()).complete
