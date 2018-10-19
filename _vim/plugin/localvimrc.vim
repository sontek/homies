" Name:    localvimrc.vim
" Version: 3.0.1
" Author:  Markus Braun <markus.braun@krawel.de>
" Summary: Vim plugin to search local vimrc files and load them.
" Licence: This program is free software: you can redistribute it and/or modify
"          it under the terms of the GNU General Public License as published by
"          the Free Software Foundation, either version 3 of the License, or
"          (at your option) any later version.
"
"          This program is distributed in the hope that it will be useful,
"          but WITHOUT ANY WARRANTY; without even the implied warranty of
"          MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
"          GNU General Public License for more details.
"
"          You should have received a copy of the GNU General Public License
"          along with this program.  If not, see <http://www.gnu.org/licenses/>.
"
" Section: Plugin header {{{1

" guard against multiple loads {{{2
if (exists("g:loaded_localvimrc") || &cp)
  finish
endif
let g:loaded_localvimrc = 1

" check for correct vim version {{{2
if version < 702
  finish
endif

" Section: Default settings {{{1

" define default "localvimrc_enable" {{{2
let s:localvimrc_enable = 1
if (exists("g:localvimrc_enable") && type(g:localvimrc_enable) == type(0))
  let s:localvimrc_enable = g:localvimrc_enable
endif

" define default "localvimrc_name" {{{2
" copy to script local variable to prevent .lvimrc modifying the name option.
let s:localvimrc_name = [ ".lvimrc" ]
if (exists("g:localvimrc_name"))
  if type(g:localvimrc_name) == type("")
    let s:localvimrc_name = [ g:localvimrc_name ]
  elseif type(g:localvimrc_name) == type([])
    let s:localvimrc_name = g:localvimrc_name
  endif
endif

" define default "localvimrc_event" {{{2
" copy to script local variable to prevent .lvimrc modifying the event option.
let s:localvimrc_event = [ "BufWinEnter" ]
if (exists("g:localvimrc_event") && type(g:localvimrc_event) == type([]))
  let s:localvimrc_event = g:localvimrc_event
endif

" define default "localvimrc_event_pattern" {{{2
" copy to script local variable to prevent .lvimrc modifying the event pattern
" option.
let s:localvimrc_event_pattern = "*"
if (exists("g:localvimrc_event_pattern") && type(g:localvimrc_event_pattern) == type(""))
  let s:localvimrc_event_pattern = g:localvimrc_event_pattern
endif

" define default "localvimrc_reverse" {{{2
" copy to script local variable to prevent .lvimrc modifying the reverse option.
let s:localvimrc_reverse = 0
if (exists("g:localvimrc_reverse") && type(g:localvimrc_reverse) == type(0))
  let s:localvimrc_reverse = g:localvimrc_reverse
endif

" define default "localvimrc_count" {{{2
" copy to script local variable to prevent .lvimrc modifying the count option.
let s:localvimrc_count = -1
if (exists("g:localvimrc_count") && type(g:localvimrc_count) == type(0))
  let s:localvimrc_count = g:localvimrc_count
endif

" define default "localvimrc_file_directory_only" {{{2
" copy to script local variable to prevent .lvimrc modifying the file
" directory only option.
let s:localvimrc_file_directory_only = 0
if (exists("g:localvimrc_file_directory_only") && type(g:localvimrc_file_directory_only) == type(0))
  let s:localvimrc_file_directory_only = g:localvimrc_file_directory_only
endif

" define default "localvimrc_sandbox" {{{2
" copy to script local variable to prevent .lvimrc disabling the sandbox again.
let s:localvimrc_sandbox = 1
if (exists("g:localvimrc_sandbox") && type(g:localvimrc_sandbox) == type(0))
  let s:localvimrc_sandbox = g:localvimrc_sandbox
endif

" define default "localvimrc_ask" {{{2
" copy to script local variable to prevent .lvimrc modifying the ask option.
let s:localvimrc_ask = 1
if (exists("g:localvimrc_ask") && type(g:localvimrc_ask) == type(0))
  let s:localvimrc_ask = g:localvimrc_ask
endif

" define default "localvimrc_whitelist" {{{2
" copy to script local variable to prevent .lvimrc modifying the whitelist.
let s:localvimrc_whitelist = [ "^$" ] " this never matches a file
if (exists("g:localvimrc_whitelist"))
  if type(g:localvimrc_whitelist) == type("")
    let s:localvimrc_whitelist = [ g:localvimrc_whitelist ]
  elseif type(g:localvimrc_whitelist) == type([])
    let s:localvimrc_whitelist = g:localvimrc_whitelist
  endif
endif

" define default "localvimrc_blacklist" {{{2
" copy to script local variable to prevent .lvimrc modifying the blacklist.
let s:localvimrc_blacklist = [ "^$" ] " this never matches a file
if (exists("g:localvimrc_blacklist"))
  if type(g:localvimrc_blacklist) == type("")
    let s:localvimrc_blacklist = [ g:localvimrc_blacklist ]
  elseif type(g:localvimrc_blacklist) == type([])
    let s:localvimrc_blacklist = g:localvimrc_blacklist
  endif
endif

" define default "localvimrc_persistent" {{{2
" copy to script local variable to prevent .lvimrc modifying the persistent
" option.
let s:localvimrc_persistent = 0
if (exists("g:localvimrc_persistent") && type(g:localvimrc_persistent) == type(0))
  let s:localvimrc_persistent = g:localvimrc_persistent
endif

" define default "localvimrc_persistence_file" {{{2
" copy to script local variable to prevent .lvimrc modifying the persistence
" file.
if has("win16") || has("win32") || has("win64") || has("win95")
  let s:localvimrc_persistence_file = expand('$HOME') . "/_localvimrc_persistent"
else
  let s:localvimrc_persistence_file = expand('$HOME') . "/.localvimrc_persistent"
endif
if (exists("g:localvimrc_persistence_file") && type(g:localvimrc_persistence_file) == type(""))
  let s:localvimrc_persistence_file = g:localvimrc_persistence_file
endif

" define default "localvimrc_autocmd" {{{2
" copy to script local variable to prevent .lvimrc modifying the autocommand
" option.
let s:localvimrc_autocmd = 1
if (exists("g:localvimrc_autocmd") && type(g:localvimrc_autocmd) == type(0))
  let s:localvimrc_autocmd = g:localvimrc_autocmd
endif

" define default "localvimrc_debug" {{{2
if (!exists("g:localvimrc_debug"))
  let g:localvimrc_debug = 0
endif

" define default "localvimrc_debug_lines" {{{2
" this defines the number of debug messages kept in a buffer
let s:localvimrc_debug_lines = 100
if (exists("g:localvimrc_debug_lines"))
  let s:localvimrc_debug_lines = g:localvimrc_debug_lines
endif

" Section: Autocmd setup {{{1

if has("autocmd")
  augroup localvimrc
    autocmd!

    for event in s:localvimrc_event
      " call s:LocalVimRC() when creating or reading any file
      exec "autocmd ".event." ".s:localvimrc_event_pattern." call s:LocalVimRC()"
    endfor
  augroup END
endif

" Section: Functions {{{1

" Function: s:LocalVimRC() {{{2
"
" search all local vimrc files from current directory up to root directory and
" source them in reverse order.
"
function! s:LocalVimRC()
  " begin marker
  call s:LocalVimRCDebug(1, "== START LocalVimRC() ============================")

  " print version
  call s:LocalVimRCDebug(1, "localvimrc.vim " . g:loaded_localvimrc)

  " finish immediately if localvimrc is disabled
  if s:localvimrc_enable == 0
    return
  endif

  " read persistent information
  call s:LocalVimRCReadPersistent()

  " only consider normal buffers (skip especially CommandT's GoToFile buffer)
  " NOTE: in general the buftype is not set for new buffers (BufWinEnter),
  "       e.g. for help files via plugins (pydoc)
  if !empty(&buftype)
    call s:LocalVimRCDebug(1, "not a normal buffer, exiting")
    return
  endif

  " directory of current file (correctly escaped)
  let l:directory = fnameescape(expand("%:p:h"))
  if empty(l:directory)
    let l:directory = fnameescape(getcwd())
  endif
  call s:LocalVimRCDebug(2, "searching directory \"" . l:directory . "\"")

  " check if the local vimrc file shall be searched just in the files
  " directory or in the whole tree
  if s:localvimrc_file_directory_only == 1
    let l:search_option = ""
  else
    let l:search_option = ";"
  endif

  " generate a list of all local vimrc files with absolute file names along path to root
  let l:rcfiles = []
  for l:rcname in s:localvimrc_name
    for l:rcfile in findfile(l:rcname, l:directory . l:search_option, -1)
      let l:rcfile_unresolved = fnamemodify(l:rcfile, ":p")
      let l:rcfile_resolved = resolve(l:rcfile_unresolved)
      call insert(l:rcfiles, { "resolved": l:rcfile_resolved, "unresolved": l:rcfile_unresolved } )
    endfor
  endfor
  call s:LocalVimRCDebug(1, "found files: " . string(l:rcfiles))

  " shrink list of found files
  if (s:localvimrc_count >= 0 && s:localvimrc_count < len(l:rcfiles))
    call remove(l:rcfiles, 0, len(l:rcfiles) - s:localvimrc_count - 1)
  endif

  " reverse order of found files if reverse loading is requested
  if (s:localvimrc_reverse != 0)
    call reverse(l:rcfiles)
  endif

  call s:LocalVimRCDebug(1, "candidate files: " . string(l:rcfiles))

  " source all found local vimrc files in l:rcfiles variable
  let s:localvimrc_finish = 0
  let l:answer = ""
  let l:sandbox_answer = ""
  let l:sourced_files = []
  for l:rcfile_dict in l:rcfiles
    " get values from dictionary
    let l:rcfile = l:rcfile_dict["resolved"]
    let l:rcfile_unresolved = l:rcfile_dict["unresolved"]
    call s:LocalVimRCDebug(2, "processing \"" . l:rcfile . "\"")

    let l:rcfile_load = "unknown"

    if filereadable(l:rcfile)
      " extract information
      if has_key(s:localvimrc_data, l:rcfile)
        if len(s:localvimrc_data[l:rcfile]) == 2
          let [ l:stored_answer, l:stored_checksum ] = s:localvimrc_data[l:rcfile]
          let l:stored_sandbox_answer = ""
        elseif len(s:localvimrc_data[l:rcfile]) == 3
          let [ l:stored_answer, l:stored_sandbox_answer, l:stored_checksum ] = s:localvimrc_data[l:rcfile]
        else
          let l:stored_answer = ""
          let l:stored_sandbox_answer = ""
          let l:stored_checksum = ""
        endif
      else
        let l:stored_answer = ""
        let l:stored_sandbox_answer = ""
        let l:stored_checksum = ""
      endif
      call s:LocalVimRCDebug(3, "stored information: answer = '" . l:stored_answer . "' sandbox answer = '" . l:stored_sandbox_answer . "' checksum = '" . l:stored_checksum . "'")

      " check if checksum is the same
      let l:checksum_is_same = s:LocalVimRCCheckChecksum(l:rcfile, l:stored_checksum)

      " reset answers if checksum changed
      if (!l:checksum_is_same)
        call s:LocalVimRCDebug(2, "checksum mismatch, not reusing answer")
        let l:stored_answer = ""
        let l:stored_sandbox_answer = ""
      else
        call s:LocalVimRCDebug(2, "reuse previous answer = '" . l:stored_answer . "' sandbox answer = '" . l:stored_sandbox_answer . "'")
      endif

      " check if whitelisted
      if (l:rcfile_load == "unknown")
        if s:LocalVimRCMatchAny(l:rcfile, s:localvimrc_whitelist)
          call s:LocalVimRCDebug(2, l:rcfile . " is whitelisted")
          let l:rcfile_load = "yes"
        endif
      endif

      " check if blacklisted
      if (l:rcfile_load == "unknown")
        if s:LocalVimRCMatchAny(l:rcfile, s:localvimrc_blacklist)
          call s:LocalVimRCDebug(2, l:rcfile . " is blacklisted")
          let l:rcfile_load = "no"
        endif
      endif

      " check if an answer has been given for the same file
      if !empty(l:stored_answer)
        " check the answer
        if (l:stored_answer =~? '^y$')
          let l:rcfile_load = "yes"
        elseif (l:stored_answer =~? '^n$')
          let l:rcfile_load = "no"
        endif
      endif

      " ask if in interactive mode
      if (l:rcfile_load == "unknown")
        if (s:localvimrc_ask == 1)
          if (l:answer !~? "^a$")
            call s:LocalVimRCDebug(2, "need to ask")
            let l:answer = ""
            let l:message = ""
            while (l:answer !~? '^[ynaq]$')
              if (s:localvimrc_persistent == 0)
                let l:message .= "localvimrc: source " . l:rcfile . "? ([y]es/[n]o/[a]ll/[s]how/[q]uit) "
              elseif (s:localvimrc_persistent == 1)
                let l:message .= "localvimrc: source " . l:rcfile . "? ([y]es/[n]o/[a]ll/[s]how/[q]uit ; persistent [Y]es/[N]o/[A]ll) "
              else
                let l:message .= "localvimrc: source " . l:rcfile . "? ([y]es/[n]o/[a]ll/[s]how/[q]uit) "
              endif

              " turn off possible previous :silent command to force this
              " message to be printed
              unsilent let l:answer = inputdialog(l:message)
              call s:LocalVimRCDebug(2, "answer is \"" . l:answer . "\"")

              if empty(l:answer)
                call s:LocalVimRCDebug(2, "aborting on empty answer")
                let l:answer = "q"
              endif

              " add the content of the file for repeating the question
              let l:message = ""
              if (l:answer =~? "^s$")
                let l:message .= "localvimrc: >>>>>>>> start content of " . l:rcfile . "\n"
                let l:content_max = 10
                let l:content = readfile(l:rcfile, "", l:content_max + 1)
                for l:line in l:content
                  let l:message .= "localvimrc: " . l:line . "\n"
                endfor
                if len(l:content) > l:content_max
                  let l:message .= "localvimrc: ======== TRUNCATED AFTER " . l:content_max . " LINES!\n"
                endif
                let l:message .= "localvimrc: <<<<<<<< end  content of " . l:rcfile . "\n"
              endif
            endwhile
          endif

          " make answer upper case if persistence is 2 ("force")
          if (s:localvimrc_persistent == 2)
            let l:answer = toupper(l:answer)
          endif

          " store y/n answers
          if (l:answer =~? "^y$")
            let l:stored_answer = l:answer
          elseif (l:answer =~? "^n$")
            let l:stored_answer = l:answer
          elseif (l:answer =~# "^a$")
            let l:stored_answer = "y"
          elseif (l:answer =~# "^A$")
            let l:stored_answer = "Y"
          endif

          " check the answer
          if (l:answer =~? '^[ya]$')
            let l:rcfile_load = "yes"
          elseif (l:answer =~? "^q$")
            call s:LocalVimRCDebug(1, "ended processing files")
            break
          endif
        endif
      endif

      " load unconditionally if in non-interactive mode
      if (l:rcfile_load == "unknown")
        if (s:localvimrc_ask == 0)
          let l:rcfile_load = "yes"
        endif
      endif

      " should this rc file be loaded?
      if (l:rcfile_load == "yes")
        " store name and directory of file
        let g:localvimrc_file = resolve(expand("<afile>:p"))
        let g:localvimrc_file_dir = fnamemodify(g:localvimrc_file, ":h")
        call s:LocalVimRCDebug(3, "g:localvimrc_file = " . g:localvimrc_file . ", g:localvimrc_file_dir = " . g:localvimrc_file_dir)

        " store name and directory of script
        let g:localvimrc_script = l:rcfile
        let g:localvimrc_script_dir = fnamemodify(g:localvimrc_script, ":h")
        call s:LocalVimRCDebug(3, "g:localvimrc_script = " . g:localvimrc_script . ", g:localvimrc_script_dir = " . g:localvimrc_script_dir)

        " store name and directory of unresolved script
        let g:localvimrc_script_unresolved = l:rcfile_unresolved
        let g:localvimrc_script_dir_unresolved = fnamemodify(g:localvimrc_script_unresolved, ":h")
        call s:LocalVimRCDebug(3, "g:localvimrc_script_unresolved = " . g:localvimrc_script_unresolved . ", g:localvimrc_script_dir_unresolved = " . g:localvimrc_script_dir_unresolved)

        " reset if checksum changed
        if (!l:checksum_is_same)
          if has_key(s:localvimrc_sourced, l:rcfile)
            unlet s:localvimrc_sourced[l:rcfile]
            call s:LocalVimRCDebug(2, "resetting 'sourced' information")
          endif
        endif

        " detect if this local vimrc file had been loaded
        let g:localvimrc_sourced_once = 0
        let g:localvimrc_sourced_once_for_file = 0
        if has_key(s:localvimrc_sourced, l:rcfile)
          let g:localvimrc_sourced_once = 1
          if index(s:localvimrc_sourced[l:rcfile], g:localvimrc_file) >= 0
            let g:localvimrc_sourced_once_for_file = 1
          else
            call add(s:localvimrc_sourced[l:rcfile], g:localvimrc_file)
          endif
        else
          let s:localvimrc_sourced[l:rcfile] = [ g:localvimrc_file ]
        endif
        call s:LocalVimRCDebug(3, "g:localvimrc_sourced_once = " . g:localvimrc_sourced_once . ", g:localvimrc_sourced_once_for_file = " . g:localvimrc_sourced_once_for_file)

        " generate command
        let l:command = "source " . fnameescape(l:rcfile)

        " emit an autocommand before sourcing
        if (s:localvimrc_autocmd == 1)
          call s:LocalVimRCUserAutocommand('LocalVimRCPre')
        endif

        " add 'sandbox' if requested
        if (s:localvimrc_sandbox != 0)
          call s:LocalVimRCDebug(2, "using sandbox")
          try
            " execute the command
            exec "sandbox " . l:command
            call s:LocalVimRCDebug(1, "sourced " . l:rcfile)
          catch ^Vim\%((\a\+)\)\=:E48
            call s:LocalVimRCDebug(1, "unable to use sandbox on '" . l:rcfile . "'")

            if (s:localvimrc_ask == 1)
              if (l:sandbox_answer !~? "^a$")
                if l:stored_sandbox_answer != ""
                  let l:sandbox_answer = l:stored_sandbox_answer
                  call s:LocalVimRCDebug(2, "reuse previous sandbox answer \"" . l:stored_sandbox_answer . "\"")
                else
                  call s:LocalVimRCDebug(2, "need to ask")
                  let l:sandbox_answer = ""
                  while (l:sandbox_answer !~? '^[ynaq]$')
                    if (s:localvimrc_persistent == 0)
                      let l:message = "localvimrc: unable to use 'sandbox' for " . l:rcfile . ".\nlocalvimrc: Source it anyway? ([y]es/[n]o/[a]ll/[q]uit) "
                    elseif (s:localvimrc_persistent == 1)
                      let l:message = "localvimrc: unable to use 'sandbox' for " . l:rcfile . ".\nlocalvimrc: Source it anyway? ([y]es/[n]o/[a]ll/[q]uit ; persistent [Y]es/[N]o/[A]ll) "
                    else
                      let l:message = "localvimrc: unable to use 'sandbox' for " . l:rcfile . ".\nlocalvimrc: Source it anyway? ([y]es/[n]o/[a]ll/[q]uit) "
                    endif

                    " turn off possible previous :silent command to force this
                    " message to be printed
                    unsilent let l:sandbox_answer = inputdialog(l:message)
                    call s:LocalVimRCDebug(2, "sandbox answer is \"" . l:sandbox_answer . "\"")

                    if empty(l:sandbox_answer)
                      call s:LocalVimRCDebug(2, "aborting on empty sandbox answer")
                      let l:sandbox_answer = "q"
                    endif
                  endwhile
                endif
              endif

              " make sandbox_answer upper case if persistence is 2 ("force")
              if (s:localvimrc_persistent == 2)
                let l:sandbox_answer = toupper(l:sandbox_answer)
              endif

              " store y/n answers
              if (l:sandbox_answer =~? "^y$")
                let l:stored_sandbox_answer = l:sandbox_answer
              elseif (l:sandbox_answer =~? "^n$")
                let l:stored_sandbox_answer = l:sandbox_answer
              elseif (l:sandbox_answer =~# "^a$")
                let l:stored_sandbox_answer = "y"
              elseif (l:sandbox_answer =~# "^A$")
                let l:stored_sandbox_answer = "Y"
              endif

              " check the sandbox_answer
              if (l:sandbox_answer =~? '^[ya]$')
                " execute the command
                exec l:command
                call s:LocalVimRCDebug(1, "sourced " . l:rcfile)
              elseif (l:sandbox_answer =~? "^q$")
                call s:LocalVimRCDebug(1, "ended processing files")
                break
              endif
            endif
          endtry
        else
          " execute the command
          exec l:command
          call s:LocalVimRCDebug(1, "sourced " . l:rcfile)
        endif

        " emit an autocommands after sourcing
        if (s:localvimrc_autocmd == 1)
          call s:LocalVimRCUserAutocommand('LocalVimRCPost')
        endif

        call add(l:sourced_files, l:rcfile)

        " remove global variables again
        unlet g:localvimrc_file
        unlet g:localvimrc_file_dir
        unlet g:localvimrc_script
        unlet g:localvimrc_script_dir
        unlet g:localvimrc_script_unresolved
        unlet g:localvimrc_script_dir_unresolved
        unlet g:localvimrc_sourced_once
        unlet g:localvimrc_sourced_once_for_file
      else
        call s:LocalVimRCDebug(1, "skipping " . l:rcfile)
      endif

      " calculate checksum for each processed file
      let l:stored_checksum = s:LocalVimRCCalcChecksum(l:rcfile)

      " store information again
      let s:localvimrc_data[l:rcfile] = [ l:stored_answer, l:stored_sandbox_answer, l:stored_checksum ]

      " check if sourcing of files should be ended by variable set by
      " local vimrc file
      if (s:localvimrc_finish != 0)
        break
      endif
    endif
  endfor

  " store information about source local vimrc files in buffer local variable
  if exists("b:localvimrc_sourced_files")
    call extend(l:sourced_files, b:localvimrc_sourced_files)
  endif
  if exists("*uniq")
    call uniq(sort(l:sourced_files))
  else
    let l:sourced_files_uniq = {}
    for l:file in l:sourced_files
      let l:sourced_files_uniq[l:file] = 1
    endfor
    let l:sourced_files = sort(keys(l:sourced_files_uniq))
  endif
  let b:localvimrc_sourced_files = l:sourced_files

  " make information persistent
  call s:LocalVimRCWritePersistent()

  " end marker
  call s:LocalVimRCDebug(1, "== END LocalVimRC() ==============================")
endfunction

" Function: s:LocalVimRCUserAutocommand(event) {{{2
"
function! s:LocalVimRCUserAutocommand(event)
  if exists('#User#'.a:event)
    call s:LocalVimRCDebug(1, 'executing User autocommand '.a:event)
    if v:version >= 704 || (v:version == 703 && has('patch442'))
      exec 'doautocmd <nomodeline> User ' . a:event
    else
      exec 'doautocmd User ' . a:event
    endif
  endif
endfunction

" Function: s:LocalVimRCMatchAny(str, patterns) {{{2
"
" check if any of the regular expressions given in the list patterns matches the
" string. If there is a match, return value is "1". If there is no match,
" return value is "0".
"
function! s:LocalVimRCMatchAny(str, patterns)
  for l:pattern in a:patterns
    if (match(a:str, l:pattern) != -1)
      return 1
    endif
  endfor
  return 0
endfunction

" Function: s:LocalVimRCCalcFNV() {{{2
"
" implementation of Fowler–Noll–Vo (FNV-1) hash function calculated on given
" string (https://en.wikipedia.org/wiki/Fowler-Noll-Vo_hash_function)
"
function! s:LocalVimRCCalcFNV(text)
  " initialize the hash with defined offset value
  let l:prime = 0x01000193
  let l:checksum = 0x811c9dc5

  " loop over all characters
  for i in range(0, len(a:text)-1)
    let l:checksum = and((l:checksum * prime), 0xFFFFFFFF)
    let l:checksum = xor(l:checksum, char2nr(a:text[i]))
  endfor

  return l:checksum
endfunction

" Function: s:LocalVimRCCalcSHA256() {{{2
"
" calculate sha256 checksum using python hashlib
"
function! s:LocalVimRcCalcSHA256(text)
  exec s:localvimrc_python_command . " text = vim.eval('a:text')"
  exec s:localvimrc_python_command . " checksum = hashlib.sha256(text.encode('utf-8'))"
  exec s:localvimrc_python_command . " vim.command('return \"%s\"' % checksum.hexdigest())"
endfunction

" Function: s:LocalVimRCCalcChecksum(filename) {{{2
"
" calculate checksum. depending on Vim version this is done with sha256 or
" with FNV-1
"
function! s:LocalVimRCCalcChecksum(file)
  let l:content = join(readfile(a:file))
  let l:checksum = s:localvimrc_checksum_func(l:content)

  call s:LocalVimRCDebug(3, "checksum calc -> " . fnameescape(a:file) . " : " . l:checksum)

  return l:checksum
endfunction

" Function: s:LocalVimRCCheckChecksum(filename, checksum) {{{2
"
" Check checksum. Return "0" if it does not exist, "1" otherwise
"
function! s:LocalVimRCCheckChecksum(file, checksum)
  let l:return = 0
  let l:checksum = s:LocalVimRCCalcChecksum(a:file)

  if (a:checksum == l:checksum)
    let l:return = 1
  endif

  return l:return
endfunction

" Function: s:LocalVimRCReadPersistent() {{{2
"
" read decision variables from persistence file
"
function! s:LocalVimRCReadPersistent()
  if (s:localvimrc_persistent >= 1)
    " check if persistence file is readable
    if filereadable(s:localvimrc_persistence_file)

      " check if reading is needed
      let l:checksum = s:LocalVimRCCalcChecksum(s:localvimrc_persistence_file)
      if l:checksum != s:localvimrc_persistence_file_checksum

        " read persistence file
        let l:serialized = readfile(s:localvimrc_persistence_file)
        call s:LocalVimRCDebug(3, "read persistent data: " . string(l:serialized))

        " deserialize stored persistence information
        for l:line in l:serialized
          let l:columns = split(l:line, '[^\\]\zs|\|^|', 1)
          if len(l:columns) != 3 && len(l:columns) != 4
            call s:LocalVimRCDebug(1, "error in persistence file")
            call s:LocalVimRCError("error in persistence file")
          else
            if len(l:columns) == 3
              let [ l:key, l:answer, l:checksum ] = l:columns
              let l:sandbox = ""
            elseif len(l:columns) == 4
              let [ l:key, l:answer, l:sandbox, l:checksum ] = l:columns
            endif
            let l:key = substitute(l:key, '\\|', '|', "g")
            let l:answer = substitute(l:answer, '\\|', '|', "g")
            let l:sandbox = substitute(l:sandbox, '\\|', '|', "g")
            let l:checksum = substitute(l:checksum, '\\|', '|', "g")
            let s:localvimrc_data[l:key] = [ l:answer, l:sandbox, l:checksum ]
          endif
        endfor
      else
        call s:LocalVimRCDebug(3, "persistence file did not change")
      endif
    else
      call s:LocalVimRCDebug(1, "unable to read persistence file '" . s:localvimrc_persistence_file . "'")
    endif
  endif
endfunction

" Function: s:LocalVimRCWritePersistent() {{{2
"
" write decision variables to persistence file
"
function! s:LocalVimRCWritePersistent()
  if (s:localvimrc_persistent >= 1)
    " select only data relevant for persistence
    let l:persistent_data = filter(copy(s:localvimrc_data), 'v:val[0] =~# "^[YN]$" || v:val[1] =~# "^[YN]$"')

    " if there are answers to store and global variables are enabled for viminfo
    if (len(l:persistent_data) > 0)
      if l:persistent_data != s:localvimrc_persistent_data
        " check if persistence file is writable
        if filereadable(s:localvimrc_persistence_file) && filewritable(s:localvimrc_persistence_file) ||
              \ !filereadable(s:localvimrc_persistence_file) && filewritable(fnamemodify(s:localvimrc_persistence_file, ":h"))
          let l:serialized = [ ]
          for [ l:key, l:value ] in items(l:persistent_data)
            if len(l:value) == 2
              let [ l:answer, l:checksum ] = l:value
              let l:sandbox = ""
            elseif len(l:value) == 3
              let [ l:answer, l:sandbox, l:checksum ] = l:value
            else
              let l:answer = ""
              let l:sandbox = ""
              let l:checksum = ""
            endif

            " delete none persisten answers
            if l:answer !~# "^[YN]$"
              let l:answer = ""
            endif
            if l:sandbox !~# "^[YN]$"
              let l:sandbox = ""
            endif

            call add(l:serialized, escape(l:key, '|') . "|" . escape(l:answer, '|') . "|" . escape(l:sandbox, '|') . "|" . escape(l:checksum, '|'))
          endfor

          call s:LocalVimRCDebug(3, "write persistent data: " . string(l:serialized))

          " check if there is a exising persistence file
          if filereadable(s:localvimrc_persistence_file)
            " first write backup file to avoid lost persistence information
            " on write errors if partition is full. Done this way because
            " write/rename approach causes permission problems with sudo.
            let l:backup_name = s:localvimrc_persistence_file . "~"
            let l:backup_content = readfile(s:localvimrc_persistence_file, "b")
            if writefile(l:backup_content, l:backup_name, "b") == 0
              if writefile(l:serialized, s:localvimrc_persistence_file) == 0
                call delete(l:backup_name)
              else
                call s:LocalVimRCError("error while writing persistence file, backup stored in '" . l:backup_name . "'")
              endif
            else
              call s:LocalVimRCError("unable to write persistence file backup '" . l:backup_name . "'")
            endif
          else
            " there is no persistence file to backup, just write new one
            if writefile(l:serialized, s:localvimrc_persistence_file) != 0
              call s:LocalVimRCError("unable to write persistence file '" . s:localvimrc_persistence_file . "'")
            endif
          endif
        else
          call s:LocalVimRCError("unable to write persistence file '" . s:localvimrc_persistence_file . "'")
        endif

        " store persistence file checksum
        let s:localvimrc_persistence_file_checksum = s:LocalVimRCCalcChecksum(s:localvimrc_persistence_file)
      endif
      let s:localvimrc_persistent_data = l:persistent_data
    endif
  else
    " delete persistence file
    if filewritable(s:localvimrc_persistence_file)
      call delete(s:localvimrc_persistence_file)
    endif
  endif

  " remove old persistence data
  if exists("g:LOCALVIMRC_ANSWERS")
    unlet g:LOCALVIMRC_ANSWERS
  endif
  if exists("g:LOCALVIMRC_CHECKSUMS")
    unlet g:LOCALVIMRC_CHECKSUMS
  endif

endfunction

" Function: s:LocalVimRCClear() {{{2
"
" clear all stored persistence data
"
function! s:LocalVimRCClear()
  let s:localvimrc_data = {}
  call s:LocalVimRCDebug(3, "cleared local data")

  let s:localvimrc_persistence_file_checksum = ""
  call s:LocalVimRCDebug(3, "cleared persistence file checksum")

  let s:localvimrc_persistent_data = {}
  call s:LocalVimRCDebug(3, "cleared persistent data")

  if filewritable(s:localvimrc_persistence_file)
    call delete(s:localvimrc_persistence_file)
    call s:LocalVimRCDebug(3, "deleted persistence file")
  endif
endfunction

" Function: s:LocalVimRCCleanup() {{{2
"
" cleanup stored persistence data
"
function! s:LocalVimRCCleanup()
  " read persistent information
  call s:LocalVimRCReadPersistent()
  call s:LocalVimRCDebug(3, "read persistent data")

  " loop over all persistent data entries
  for l:file in keys(s:localvimrc_data)
    if !filereadable(l:file)
      unlet s:localvimrc_data[l:file]
      call s:LocalVimRCDebug(3, "removed file '".l:file."' from persistence file")
    else
      call s:LocalVimRCDebug(3, "keeping file '".l:file."' in persistence file")
    endif
  endfor

  " make information persistent
  call s:LocalVimRCWritePersistent()
  call s:LocalVimRCDebug(3, "write persistent data")
endfunction

" Function: s:LocalVimRCForget(...) {{{2
"
" forget stored persistence data for given files
"
function! s:LocalVimRCForget(...)
  " read persistent information
  call s:LocalVimRCReadPersistent()
  call s:LocalVimRCDebug(3, "read persistent data")

  " loop over all persistent data entries
  for l:file in a:000
    if !filereadable(l:file)
      let l:file = resolve(fnamemodify(l:file, ":p"))
      if has_key(s:localvimrc_data, l:file)
        unlet s:localvimrc_data[l:file]
        call s:LocalVimRCDebug(3, "removed file '".l:file."' from persistence file")
      else
        call s:LocalVimRCDebug(3, "file '".l:file."' does not exist in persistence file")
      endif
    endif
  endfor

  " make information persistent
  call s:LocalVimRCWritePersistent()
  call s:LocalVimRCDebug(3, "write persistent data")
endfunction

" Function: LocalVimRCEnable() {{{2
"
" enable processing of local vimrc files
"
function! s:LocalVimRCEnable()
  " if this call really enables the plugin load the local vimrc files for the
  " current buffer
  if s:localvimrc_enable == 0
    call s:LocalVimRCDebug(1, "enable processing of local vimrc files")
    let s:localvimrc_enable = 1
    call s:LocalVimRC()
  endif
endfunction

" Function: LocalVimRCDisable() {{{2
"
" disable processing of local vimrc files
"
function! s:LocalVimRCDisable()
  call s:LocalVimRCDebug(1, "disable processing of local vimrc files")
  let s:localvimrc_enable = 0
endfunction

" Function: LocalVimRCFinish() {{{2
"
" finish processing of local vimrc files
"
function! LocalVimRCFinish()
  call s:LocalVimRCDebug(1, "will finish sourcing files after this file")
  let s:localvimrc_finish = 1
endfunction

" Function: s:LocalVimRCEdit() {{{2
"
" open the local vimrc file for the current buffer in an split window for
" editing. If more than one local vimrc file has been sourced, the user
" can decide which file to edit.
"
function! s:LocalVimRCEdit()
  if exists("b:localvimrc_sourced_files")
    let l:items = len(b:localvimrc_sourced_files)
    if l:items == 0
      call s:LocalVimRCError("No local vimrc file has been sourced")
    elseif l:items == 1
      " edit the only sourced file
      let l:file = b:localvimrc_sourced_files[0]
    elseif l:items > 1
      " build message for asking the user
      let l:message = [ "Select local vimrc file to edit:" ]
      call extend(l:message, map(copy(b:localvimrc_sourced_files), 'v:key+1 . " " . v:val'))

      " ask the user which one should be edited
      let l:answer = inputlist(l:message)
      if l:answer =~ '^\d\+$' && l:answer > 0 && l:answer <= l:items
        let l:file = b:localvimrc_sourced_files[l:answer-1]
      endif
    endif

    if exists("l:file")
      execute 'silent! split ' . fnameescape(l:file)
    endif
  endif
endfunction

" Function: s:LocalVimRCError(text) {{{2
"
" output error message
"
function! s:LocalVimRCError(text)
  echohl ErrorMsg | echom "localvimrc: " . a:text | echohl None
endfunction

" Function: s:LocalVimRCDebug(level, text) {{{2
"
" store debug message, if this message has high enough importance
"
function! s:LocalVimRCDebug(level, text)
  if (g:localvimrc_debug >= a:level)
    call add(s:localvimrc_debug_message, a:text)

    " if the list is too long remove the first element
    if len(s:localvimrc_debug_message) > s:localvimrc_debug_lines
      call remove(s:localvimrc_debug_message, 0)
    endif
  endif
endfunction

" Function: s:LocalVimRCDebugShow() {{{2
"
" output stored debug message
"
function! s:LocalVimRCDebugShow()
  for l:message in s:localvimrc_debug_message
    echo l:message
  endfor
endfunction

" Section: Initialize internal variables {{{1

" initialize data dictionary {{{2
" key: localvimrc file
" value: [ answer, sandbox_answer, checksum ]
let s:localvimrc_data = {}

" initialize sourced dictionary {{{2
" key: localvimrc file
" value: [ list of files triggered sourcing ]
let s:localvimrc_sourced = {}

" initialize persistence file checksum {{{2
let s:localvimrc_persistence_file_checksum = ""

" initialize persistent data {{{2
let s:localvimrc_persistent_data = {}

" initialize processing finish flag {{{2
let s:localvimrc_finish = 0

" initialize debug message buffer {{{2
let s:localvimrc_debug_message = []

" determine python version {{{2
" for each available python version try to load the required modules and use
" this version only if loading worked
let s:localvimrc_python_available = 0
let s:localvimrc_python_command = "no working python available"
if s:localvimrc_python_available == 0 && has("pythonx")
  try
    pythonx import hashlib, vim
    let s:localvimrc_python_available = 1
    let s:localvimrc_python_command = "pythonx"
  catch
    call s:LocalVimRCDebug(1, "pythonx is available but not working")
  endtry
endif

if s:localvimrc_python_available == 0 && has("python")
  try
    python import hashlib, vim
    let s:localvimrc_python_available = 1
    let s:localvimrc_python_command = "python"
  catch
    call s:LocalVimRCDebug(1, "python is available but not working")
  endtry
endif

if s:localvimrc_python_available == 0 && has("python3")
  try
    python3 import hashlib, vim
    let s:localvimrc_python_available = 1
    let s:localvimrc_python_command = "python3"
  catch
    call s:LocalVimRCDebug(1, "python3 is available but not working")
  endtry
endif

" determine which function shall be used to calculate checksums {{{2
if exists("*sha256")
  let s:localvimrc_checksum_func = function("sha256")
elseif s:localvimrc_python_available == 1
  let s:localvimrc_checksum_func = function("s:LocalVimRcCalcSHA256")
else
  let s:localvimrc_checksum_func = function("s:LocalVimRCCalcFNV")
endif

" Section: Report settings {{{1

call s:LocalVimRCDebug(1, "== START settings ================================")
call s:LocalVimRCDebug(1, "localvimrc_enable = \"" . string(s:localvimrc_enable) . "\"")
call s:LocalVimRCDebug(1, "localvimrc_name = \"" . string(s:localvimrc_name) . "\"")
call s:LocalVimRCDebug(1, "localvimrc_event = \"" . string(s:localvimrc_event) . "\"")
call s:LocalVimRCDebug(1, "localvimrc_event_pattern = \"" . string(s:localvimrc_event_pattern) . "\"")
call s:LocalVimRCDebug(1, "localvimrc_reverse = \"" . string(s:localvimrc_reverse) . "\"")
call s:LocalVimRCDebug(1, "localvimrc_count = \"" . string(s:localvimrc_count) . "\"")
call s:LocalVimRCDebug(1, "localvimrc_file_directory_only = \"" . string(s:localvimrc_file_directory_only) . "\"")
call s:LocalVimRCDebug(1, "localvimrc_sandbox = \"" . string(s:localvimrc_sandbox) . "\"")
call s:LocalVimRCDebug(1, "localvimrc_ask = \"" . string(s:localvimrc_ask) . "\"")
call s:LocalVimRCDebug(1, "localvimrc_whitelist = \"" . string(s:localvimrc_whitelist) . "\"")
call s:LocalVimRCDebug(1, "localvimrc_blacklist = \"" . string(s:localvimrc_blacklist) . "\"")
call s:LocalVimRCDebug(1, "localvimrc_persistent = \"" . string(s:localvimrc_persistent) . "\"")
call s:LocalVimRCDebug(1, "localvimrc_persistence_file = \"" . string(s:localvimrc_persistence_file) . "\"")
call s:LocalVimRCDebug(1, "localvimrc_autocmd = \"" . string(s:localvimrc_autocmd) . "\"")
call s:LocalVimRCDebug(1, "localvimrc_debug = \"" . string(g:localvimrc_debug) . "\"")
call s:LocalVimRCDebug(1, "localvimrc_debug_lines = \"" . string(s:localvimrc_debug_lines) . "\"")
call s:LocalVimRCDebug(1, "localvimrc_checksum_func = \"" . string(s:localvimrc_checksum_func) . "\"")
call s:LocalVimRCDebug(1, "localvimrc_python_command = \"" . string(s:localvimrc_python_command) . "\"")
call s:LocalVimRCDebug(1, "== END settings ==================================")

" Section: Commands {{{1

command! LocalVimRC        call s:LocalVimRC()
command! LocalVimRCClear   call s:LocalVimRCClear()
command! LocalVimRCCleanup call s:LocalVimRCCleanup()
command! -nargs=+ -complete=file LocalVimRCForget  call s:LocalVimRCForget(<f-args>)
command! LocalVimRCEdit    call s:LocalVimRCEdit()
command! LocalVimRCEnable  call s:LocalVimRCEnable()
command! LocalVimRCDisable call s:LocalVimRCDisable()
command! LocalVimRCDebugShow call s:LocalVimRCDebugShow()

" vim600: foldmethod=marker foldlevel=0 :
