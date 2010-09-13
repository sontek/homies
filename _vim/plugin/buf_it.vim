"Author:  Fvw (vimtexhappy@gmail.com)
"         Buffer list in statusline
"         2010-01-02 23:57:48 v2.0
"License: Copyright (c) 2001-2009, Fvw
"         GNU General Public License version 2 for more details.

noremap  <c-p>      :call BufPrevPart()<cr>
noremap  <c-n>      :call BufNextPart()<cr>
noremap  <leader>bo :call BufOnly()<cr>

hi NowBuf term=bold ctermfg=Cyan guifg=green guibg=blue gui=bold
set statusline=%m\{%{&ff}:%{&fenc}:%Y:%#NowBuf#%t%#StatusLine#}\ %{g:bufPartStr}%<%=%l,%c,%P,%L%<

let s:bufNowPartIdx = 0
let s:bufList = []
let s:bufPartStrList = []
let s:last_maxidx = -1

autocmd VimEnter,BufNew,BufEnter,BufWritePost * call UpdateStatus()
"if version >= 700
"autocmd InsertLeave,VimResized * call UpdateStatus()
"end

function! BufMap()
    let now_maxidx = len(s:bufList) - 1
    if now_maxidx > s:last_maxidx
        for i in range(s:last_maxidx+1, now_maxidx, 1)
            if i < 10
                exec "silent! noremap <M-".i."> :call BufChange(".i.")<CR>"
            else
                exec "silent! noremap <M-".i/10."><M-".i%10."> :call BufChange(".i.")<CR>"
            endif
            exec "silent! noremap <leader>".i." :call BufSplit(".i.")<CR>"
        endfor
    elseif now_maxidx < s:last_maxidx
        for i in range(now_maxidx+1, s:last_maxidx, 1)
            if i < 10
                exec "silent! unmap <M-".i.">"
            else
                exec "silent! unmap <M-".i/10."><M-".i%10.">"
            endif
            exec "silent! unmap <leader>".i
        endfor
    endif
    let s:last_maxidx = now_maxidx
endfunction

function! BufOnly()
    let i = 1
    while(i <= bufnr('$'))
        if buflisted(i) && getbufvar(i, "&modifiable")
                    \   && (bufwinnr(i) != winnr())
            exec 'bw'.i
        endif
        let i = i + 1
    endwhile
    call UpdateStatus()
endfun

function! BufChange(idx)
    if !empty(get(s:bufList, a:idx, []))
        exec 'b! '.s:bufList[a:idx][0]
    endif
endfunction

function! BufSplit(idx)
    if !empty(get(s:bufList, a:idx, []))
        exec 'sb! '.s:bufList[a:idx][0]
    endif
endfunction

function! BufNextPart()
    let s:bufNowPartIdx += 1
    if s:bufNowPartIdx >= len(s:bufPartStrList)
        let s:bufNowPartIdx = 0
    endif
    call UpdateBufPartStr()
endfunction

function! BufPrevPart()
    let s:bufNowPartIdx -= 1
    if s:bufNowPartIdx < 0
        let s:bufNowPartIdx = len(s:bufPartStrList)-1
    endif
    call UpdateBufPartStr()
endfunction

function! UpdateBufPartStr()
    let g:bufPartStr = s:bufPartStrList[s:bufNowPartIdx]
    if s:bufNowPartIdx > 0
        let g:bufPartStr = '<<'.g:bufPartStr
    endif
    if s:bufNowPartIdx < len(s:bufPartStrList)-1
        let g:bufPartStr = g:bufPartStr.'>>'
    endif
endfunction

function! UpdateStatus()
    let s:bufList = []
    let [i,idx] = [1, 0]
    while(i <= bufnr('$'))
        if buflisted(i) && getbufvar(i, "&modifiable")
            let buf  =  idx."-"
            "let buf .= fnamemodify(bufname(i), ":t")."(".i.")"
            let buf .= fnamemodify(bufname(i), ":t")
            let buf .= getbufvar(i, "&modified")? "+":''
            let buf .= " "
            call add(s:bufList, [i, buf])
            let idx += 1
        endif
        let i += 1
    endwhile

    if empty(s:bufList)
        return
    endif

    let s:bufPartStrList = []
    let str = ''
    let widthForBufStr = winwidth(0) - 20
    for [i, bufStr] in s:bufList
        if len(str.bufStr) > widthForBufStr
            call add(s:bufPartStrList, str)
            let str = ''
        endif
        let str .= bufStr
    endfor
    if str != ''
        call add(s:bufPartStrList, str)
    endif

    let s:bufNowPartIdx = 0
    call UpdateBufPartStr()
    call BufMap()
endfunction
