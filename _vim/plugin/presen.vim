" presen.vim - presentation for vim

if !exists('g:presen_vim_using')
    let g:presen_vim_using = 0
endif

command! StartPresentation call s:Start()

function! s:Start()

    if g:presen_vim_using == 1
        echo "presen.vim is running. please quit either presentation."
        return
    endif

    let s:page_number = 0
    let s:max_page_number = 0
    let s:pages = []

    setl readonly
    call s:ParseMarkdown()
    setl noreadonly

    if empty(s:pages)
        echo "No page detected!"
        return
    endif
    let g:presen_vim_using = 1

    tabe
    setl readonly
    call s:ShowPage(0)

    setf markdown

    command! -buffer PageNext call s:NextPage()
    command! -buffer PagePrev call s:PrevPage()
    command! -buffer ExitPresentation call s:Exit()

    nnoremap <buffer> <silent> <Space>n :PageNext<CR>
    nnoremap <buffer> <silent> <Space>p :PagePrev<CR>
    nnoremap <buffer> <silent> <Space>q :ExitPresentation<CR>
    
    autocmd BufWinLeave <buffer> call s:Exit()
endfunction

function! s:ShowPage(page_no)
    if a:page_no < 0
        return
    endif
    if len(s:pages) < a:page_no+1
        return
    endif
    let s:page_number = a:page_no
    setl noreadonly
    execute ":normal G$vggd"
    call append(0, s:pages[s:page_number])
    setl readonly
endfunction

function! s:NextPage()
    if s:page_number+1 <= s:max_page_number
        let s:page_number += 1
        call s:ShowPage(s:page_number)
    endif
endfunction

function! s:PrevPage()
    if s:page_number-1 >= 0
        let s:page_number -= 1
        call s:ShowPage(s:page_number)
    endif
endfunction

function! s:Exit()
    let g:presen_vim_using = 0
    bdelete!
endfunction


function! s:ParseMarkdown()
    let s:pages =  map(split(join(getline(1, '$'), "\n"), '\v(^|\n)\ze#+'), 'split(v:val, "\n")')
    let s:max_page_number = len(s:pages) - 1
endfunction
