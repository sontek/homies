func! GetFilePlus()
    try
        "Standard Find
        silent find <cfile>
    catch
        try
            "Useful for css hardcodes beginning with a slash 
            silent exe "find ." . expand("<cfile>")
        catch
            try 
                "Python imports based on <cfile>
                silent exe "python import " . expand ('<cfile>')
                silent exe 'python import re; r=re.compile(".pyc$")'
                silent exe "python cfile = r.sub('.py',". expand('<cfile>'). ".__file__)"
                silent exe "python vim.command('edit %s' % cfile )"
            catch
                let path = split(expand('<cfile>'), '\.')
                let funcname = remove(path,-1)

                silent exe "python import " . join(path, '.') 
                silent exe 'python import re; r=re.compile(".pyc$")'
                silent exe "python cfile = r.sub('.py',". join(path,'.') . ".__file__)"
                silent exe "python vim.command('edit %s' % cfile )"
                "echo search ('^\s*\(def\|class\)\s+' . funcname )
                if searchdecl (funcname,0,1) == 0
                    echo "matched"
                end if
            endtry
        endtry
    endtry
endfun

nnoremap gf :call GetFilePlus()<cr>


fun! GetPythonFile(path)
    silent exe "python import " . a:path
    silent exe 'python import re; r=re.compile(".pyc$")'
    silent exe "python cfile = r.sub('.py',". a:path . ".__file__)"
    silent exe "python  import vim;vim.command('edit ' + cfile )"
endfun

command -nargs=+ Ppath call GetPythonFile(<q-args>)
