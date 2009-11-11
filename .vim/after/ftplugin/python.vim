if getline(1) =~ 'from django.db import models'
    runtime! after/ftplugin/django_model_snippets.vim 
endif
set expandtab
set tabstop=4
set shiftwidth=4
