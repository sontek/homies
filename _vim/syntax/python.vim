syn match pythonError "^\s*def\s\+\w\+(.*)\s*$" display       
syn match pythonError "^\s*class\s\+\w\+(.*)\s*$" display       
syn match pythonError "^\s*for\s.*[^:]$" display       
syn match pythonError "^\s*except\s*$" display       
syn match pythonError "^\s*finally\s*$" display       
syn match pythonError "^\s*try\s*$" display       
syn match pythonError "^\s*else\s*$" display       
syn match pythonError "^\s*else\s*[^:].*" display       
syn match pythonError "^\s*if\s.*[^\:]$" display       
syn match pythonError "^\s*except\s.*[^\:]$" display       
syn match pythonError "[;]$" display       
syn keyword pythonError do       

