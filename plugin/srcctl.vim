" Source Control commands
" Author: Michael Geddes <michaelrgeddes@optushome.com.au>
" Version: 2.3
"
" This script is used at your own risk.
"
" Please feel free to use and modify all or part of this script.
" I would appreciate being acknowledged in any derived scripts, and would 
" appreciate any updates/modifications.
"

" This is based on my sourcesafe integration scripts, which has been expanded
" to provide a more generic solution.
"
" This script provides simultaneous support for various Source Control
" modules.
"
" Each Source Control module requires a unique identifier - please contact me
" to register your identifier here:
let g:srcctl_name_vss='Visual Source Safe'
let g:srcctl_name_st ='Star Team (StarBase/Borland)'
let g:srcctl_name_cvs='CVS'
let g:srcctl_name_p4='Perforce'
"
" The script is based on the contributions of (at least) these people:
"   David Eggum (borrowed code)
"   Vikas Agnihotri (fixes & suggestions)
"   Zak Beck (fixes & suggestions)
"
" I have borrowed a number of ideas from the sourcesafe script of David Eggum, with
" the idea of merging the scripts.  These are:
"   * Using a central function to provide all functionality.
"   * Adding ':' commands for all the functions.
"   * The basis for the 'checked out files' browser.
"   * Various option names
"   * The file status stuff
"
" I have added some compatability with David's SourceSafe scripts.
"
" The script works by scanning for a file  '.project' which should be in the root
" directory of the project.  This file should contain a single line, which
" starts with the identifier for the integration, followed by ':' and the
" module-specific specifier for the folder.
" eg: for Visual Source Safe
"   vss:@\\central\srcctl@$/MyProject
" or for Star Team
"   st:username:password@central:49200/Project/View
"
" The appropriate module will automaticly be loaded on demand.. loading from
" the runtime path in the following order (where {id} is the identifier of the
" source-control)
"     srcctl_{id}.vim
"     srcctl/{id}.vim
"     srcctl/srcctl_{id}.vim
"

" Note that in the following, you can specify a count for Get, Diff,  (checkout ??)
" This does a get of/diff against the specified version.
" 
" Here are the commands availble: (replace \ with <localleader>)
" 
" Command       Map       Count      Description
" --------------------------------------------------
" SDiff         \sf       Version    Diff (Vim Diff)
" SDiffClose    \sF                  Close Diff
" STDiff        \sd       Version    Diff (text diff)
" SCheckout     \sk       Version    Checkout
" SLock         \s<c-k>   Version    Checkout (No Get)
" SUnlock       \s<c-U>              Unlock the file
" SGet          \sg       Version    Get
" SMerge        \sG                  Merge get
" SUpdate       \su                  Update (checkin)
" SStatus       \ss                  Status
"               \sh       Count      History (last n / 3)
" SHistory      \sH                  History window (vertical split - syntax highlighted)
" SAdd          \sA                  Add file to sourcesafe
" SRepeatUpdate \\u                  Update (same comment)
" SUpdateLocked \sU                  Update and keep locked
" SRepeatUpdateLocked \\U            Update (same comment) and keep locked.
" SDeploy                            Deploy the specified directory (VSS only)
"
" SPLocked[!]  [project [username]]
"   View locked files for the given project (or the cwd) - ! specifies recursive.
"   Username can be left blank (for current user) or specified as '*' for all
"   users.
" Commands are similar to the browser
"    <enter> : open file or directory
"    O : open new window for file/directory
"    o : open file in previously visited window
"    p : preview the file
"    f : diff mode
"    u : update file
"    U : repeat update
"    K : checkout
"    L : lock
" Options: Define these in your own .vimrc - defaults in brackets.
" g:scMenuPath       (&VSS)      : The full menu path prefix for VSS
" g:scMenuPlace      (40)        : The order specifier prefix for menus
" g:scDiffVertical   (1)         : Whether diff is vertical or horizontal
" g:scHistVertical   (1)         : Whether history is vertical or horizontal
" g:scSetRuler       (1)         : Whether to set the ruler option
" g:scMaintainStatus (1)         : Maintain the buffers status as possible.
" g:scShowAllLocks   (1)         : Show all locks on the status line
" g:scShowExtra      (1)         : Show extra status information
" g:scUserName       ($USERNAME) : Username for Current user of Sourcesafe
"
" NOTES:
" * If the directory $TEMP/SS  (which is required by the script) does not
"   exist, then create it / warn the user if $TEMP doesn't exist.
" * Allow closing of the diff buffer, returning (as best as I can get) the
"   screen to how it was before the diff.
"  - Also closes the last srcsafe diff before opening the new one.
"  - Clears 'diff' on all other windows, and tries (as best as it can) to
"    restore the 'diff' when the srcsafe diff closes.
" * If you close the temporary diff file -then this also causes the file view to be
"    restored
" * Closing the original file when doing a srcsafe diff causes the diff file
"   to be closed as well.
"
" TODO:
"
let g:srcctl_version='2.4'
" History:
" 2.2: Various fixes
"   - Fix up reusing of comments
"   - Make merge checkout the default for Get
" 2.3:
"   - Handle missing directories.
"   - Handle no .project better
" 2.4:
"   - Update file status when doing \ss
"   - Make sure status is updated (For starteam)


" --------------------------- Generic ----------------------------------

" Set up defaults (don't override if they already exist)
fun! SSDlet(lhs,rhs)
  if a:lhs !~ '^[gs]:' | let lhs='g:'.a:lhs | else | let lhs=a:lhs | endif
  if !exists(lhs) | exe 'let '.lhs.' = "'.escape(a:rhs,"\\\"").'"' | endif
endfun

call SSDlet('scMenuPath', '&Version')   " The full menu path prefix for VSS
call SSDlet('scMenuPlace', '40')   " The order specifier prefix for menus
call SSDlet('scDiffVertical', 1)   " Whether diff is vertical or horizontal
call SSDlet('scHistVertical', 1)   " Whether history is vertical or horizontal
call SSDlet('scSetRuler', 1)       " Whether to set the ruler option
call SSDlet('scMaintainStatus', 1) " Maintain the buffers status as possible.
call SSDlet('scShowAllLocks', 1)   " Show all locks on the status line
call SSDlet('scShowExtra', 1)      " Show extra status information
call SSDlet('scUserName', $USERNAME) " Username for Current user of Source Control
call SSDlet('scCommandPrefix', 'S')  "Prefix for commands


delfun SSDlet

fun! s:CheckTemp()
  if !isdirectory($TEMP)
    call confirm("Your $TEMP directory doesn't exist!\n'".$TEMP."'")
  else
    if !isdirectory($TEMP.'\ss')
      call system( 'mkdir "'.$TEMP.'\SS"')
    endif
  endif
endfun

if !exists('g:srcctl_loaded_modules') | let g:srcctl_loaded_modules=':' | endif

" Return command name and attributes based on command
"
"    :self:                Command was specified without a name
"    :recurse:             Command should be recursive
"    :text:                Text output (Diff)
"    :lock:                Lock the file
"    :exlock:              Exclusive lock the file
"    :overwrite:           Overwrite the file
"    :single:              Command takes single file only
"    :count:               Command takes count
"    :?write:              Ask to write first
"    :?overwrite:          Ask to overwrite a file
"    :?lock:               Ask to lock the file
"    :?recurse:            Optional recurse
"    :status:              Changes the status
"    :?comment:            Ask for comment
"    :?reusecomment:       Ask to reuse the comment
"    :directory:           Default is directory
"    :locks:               Show locks on status
"    :extras:              Show extras on status
"    :noupdate:            Don't update Checkout status.
"    :forcedir:            Force the directory overwrite
"
"
" Unlock -> UnCheckout
" Get -> Checkout :overwrite: [:recurse:]
" Checkout ->Checkout :lock:
" Merge -> Checkout [:recurse:]
" Update -> Checkin
" UpdateLocked -> Checkin :lock:
" Summary -> History :text:
" RawStatus -> Status :text:
"
fun! s:SCCommandAttrib( cmd)
  if a:cmd=~?'\<D\%[iff]\>'               " D^iff
    return 'Diff:single:'
  elseif a:cmd=~?'\<TD\%[iff]\>'         " TD^iff
    return 'Diff:text:write:'
  elseif a:cmd=~?'\<Unc\%[heckout]\>' || a:cmd=~?'\<Unlock\>'     " Unc^heckout / Unlock
    return 'Unlock:?overwrite:status:'
  elseif a:cmd=~?'\<L\%[ock]\>'           " L^ock
    return 'Lock:?lock:status:'
  elseif a:cmd=~?'\<C\%[heckout]\>'       " C^heckout
    return 'Checkout:?write:?lock:?recurse:status:'
  elseif a:cmd=~?'\<GetF\%[orced]\>'            " GetF^orced
    return 'Checkout:overwrite:?recurse:?overwrite:status:'
  elseif a:cmd=~?'\<M\%[erge]\>' || a:cmd=~? '\<G\%[et]\>'   " M^erge
    return 'Checkout:merge:?recurse:?overwrite:status:'
  elseif a:cmd=~?'\<U\%[pdate]\>'         " U^pdate
    return 'Checkin:?write:?comment:status:'
  elseif a:cmd=~?'\<UpdateL\%[ocked]\>'   " UpdateL^ocked
    return 'Checkin:?write:?comment:lock:status:'
  elseif a:cmd=~?'\<Re\%[peatUpdate]\>'   " Re^peatUpdate
    return 'Checkin:?write:?reusecomment:status:'
  elseif a:cmd=~?'\<RepeatUpdateL\%[ocked]\>' " RepeatUpdateL^ocked
    return 'Checkin:?write:?reusecomment:lock:status:'
  elseif a:cmd=~?'\<RawS\%[tatus]\>'          " RawS^tatus
    return 'Status:text:'
  elseif a:cmd=~?'\<S\%[tatus]\>'         " S^tatus
    return 'Status:returnstatus:return:force:'
  elseif a:cmd=~?'\<Sum\%[mary]\>'        " Sum^mary
    return 'History:text'
  elseif a:cmd=~?'\<H\%[istory]\>'        " H^istory
    return 'History:'
  elseif a:cmd=~?'\<A\%[dd]\>'            " A^dd
    return 'Add:?write:update:'
  elseif a:cmd =~?'\<Dep\%[loy]\>'      " Dep^loy
    return 'Deploy:?write:?recurse:'
  elseif a:cmd =~?'\<Dir\%[ectory]\>'   " Dir&etory
    return 'Dir:?recurse:directory'
  endif
  return ''
endfun

fun! s:SCDoCommand(bang, count, cmd, ... )

  " Find the command and attributes for a given Source Control command.
  let attrib=s:SCCommandAttrib( a:cmd )
  if attrib==''
    echoerr "Unknown command: '".a:cmd."'"
    return
  endif

  if a:bang.'' == '!' 
    " Mark ! for recurse/bang if available
      let attrib=substitute(attrib, ':?\(recurse\|bang\):', ':\1:', '')
  endif

  " Get all the arguments into f1 to f..  translate no parameters as 'this file' or cwd
  if a:0==0
  " Indicate no command-line given
    let attrib=attrib.'self:'
    let c=1
    let f1= (attrib=~ ':directory:')? getcwd():expand('%')
  else
    let c=1
    while c <= a:0
      let f{c}=a:{c}
      let c=c+1
    endwhile
    let c=a:0
  endif

  " Ignore any extra arguments for commands marked as single 
  if attrib =~ ':single:' 
    let c = 1 
  endif
  let command=matchstr(attrib,'^[^:]\+')

  " Go through all the arguments and find the projects,
  " and find the modified options for all the different project types.
  " (mostly, there will only be one).
  let i=1
  while i <= c
    let prjid=s:GetProjectIdent(f{i})
    let ptype=matchstr(prjid,'^[^:]\+\ze:')
    let typ{i}=ptype
    let prj{i}=matchstr(prjid,'^[^:]\+:\zs.*$')

    " Find type specefic options
    if !exists( 'opts_'.ptype)
      if s:has_function( 'SourceControlAttrib_', ptype)
        let opts_{ptype}=SourceControlAttrib_{ptype}(attrib)
        if opts_{ptype}=~':cancel:'
          echo 'Cancelled'
          return
        endif
      else
        let opts_{ptype}=attrib
      endif
    endif
    let i=i+1
  endwhile


  " Now act on all files.
  let i=1
  while i <= c
    let ptype=typ{i}
    " Check to see if we should write files before acting or overwrite files.
    if ptype== ''
      echo 'No .project for '.f{i}
    elseif (attrib !~ ':?write:' || s:CheckWrite(f{i})) && (attrib !~ ':?overwrite:' || s:CheckOverWrite(f{i}))
      " Get a comment as required.
      if attrib =~ ':?\(reuse\)\=comment:'
        " After the first, prompt to reuse the comment.
        let comment=s:SCGetComment(prj{i}, (i>1) || (attrib=~ ':?reusecomment:') )
        if comment== ':cancel:'
          echo 'Command cancelled'
          return
        endif
      else
        let comment=''
      endif

      if command == 'Diff' && (opts_{ptype} !~ ':text:')
        call s:SCDiff( ptype, f{i}, prj{i}, 0, a:count )
      elseif s:has_function('SourceControlDo_',ptype)
        " Execute the action
        let ret=SourceControlDo_{ptype}(command, opts_{ptype}, a:count, f{i}, prj{i}, comment)
        if attrib =~ ':returnstatus:'
          call confirm( s:UpdateStatusVars( ret, bufnr(f{i})) , "&Ok" )
        endif
      else
        echoerr "File '".f{i}."': unable to find handler for type '".ptype."'"
        return
      endif

      " Update the status
      if attrib =~ ':status:'
        let bid=bufnr(f{i})
        if bid != -1
          call s:SetStatus( bid, f{i}, ptype, prj{i}, (g:scShowExtra?':extras:':'').(g:scShowAllLocks?':locks:':''))
        endif
      endif
    endif
    let i=i+1
  endwhile
endfun


"fun! s:UpdateStatusFor( bufname, force )
"  if !a:force || !g:scMaintainStatus  | return | endif
"  let cur=winnr()
"  let destbuf=bufnr(a:bufname)
"  if destbuf!=-1
"    let dest=bufwinnr(destbuf)
"    if cur!= destbuf | exe destbuf.'winc w' | endif
"    call s:UpdateStatus()
"    if cur!= destbuf | exe cur.'winc w' | endif
"  endif
"endfun

" The file where the comment is stored!
let s:commenttmp=$TEMP.'/ss/comment.@@@'

" Get a comment
fun! s:SCGetComment( prjname, useLast )
  let useLast=a:useLast
  let comment=''
  if filereadable(s:commenttmp)
     let comment=substitute(substitute(s:Cat(s:commenttmp),'^\s*','',''),'\s*$','','')
  endif
  if comment=='' || comment=='.'
    let useLast=0
  endif

  let editcomment=1
  if useLast
    let res = confirm("Reuse comment:\n\n".comment,"&Yes\n&No\n&Edit",1)
    if res==0
      return ':cancel:'
    elseif res==1
      let editcomment=0
    elseif res==2
      let useLast=0
    endif
  endif

  call s:CheckTemp()
  if filereadable(s:commenttmp)
    call rename(s:commenttmp,s:commenttmp.'.1')
  endif
  if useLast
    if editcomment
      " Bring up the old comment to be edited
      call s:system($VIMRUNTIME.'/gvim','-f -u NONE -U NONE -c "set go=aiMr co=30 lines=10"  -c "set nomod" -c "silent e '.s:commenttmp.'.1" -c "silent f %:r" -c "set mod"')
    else
      " Reuse the old comment
      call rename(s:commenttmp.'.1', s:commenttmp)
    endif
  else
    " Create a new comment
    call s:system($VIMRUNTIME.'/gvim',' -f -u NONE -U NONE -c "set go=aiMr co=30 lines=10" "'.s:commenttmp.'"')
  endif
  if filereadable(s:commenttmp)
    return '@'.s:commenttmp
  endif
  " Restore old comment and cancel
  if filereadable(s:commenttmp.'.1')
    call rename(s:commenttmp.'.1', s:commenttmp)
  endif
  return ':cancel:'
endfun

fun! s:system(cmd,args)
"  echo '+'.a:cmd.'+'.a:args.'+'
  return system('""'.a:cmd.'" '.a:args.'"')
endfun

" fun! s:SSUpdate( filename, copyold, extras)
"   call s:CheckTemp()
"   if filereadable(s:commenttmp)
"     call rename(s:commenttmp,s:commenttmp.'.1')
"   endif
"   if a:copyold=='e'
"     " Bring up the old comment to be edited
"     call s:system($VIMRUNTIME.'\gvim.exe','-f -u NONE -U NONE -c "set go=aiMr co=30 lines=10"  -c "set nomod" -c "silent e '.s:commenttmp.'.1" -c "silent f %:r" -c "set mod"')
"   elseif a:copyold=='r'
"     " Reuse the old comment
"       call rename(s:commenttmp.'.1', s:commenttmp)
"   else
"     " Create a new comment
"     call s:system($VIMRUNTIME.'\gvim.exe',' -f -u NONE -U NONE -c "set go=aiMr co=30 lines=10" "'.s:commenttmp.'"')
"   endif
"   if filereadable(s:commenttmp)
"     let cmt=s:Cat(s:commenttmp)
"     if cmt != ''
"       "call input(' update '.s:commenttmp.' '.a:filename)
"       call s:SSCmd('Update '.a:extras.' -C@'.s:commenttmp.' ', a:filename,0, 'o')
"       return
"     endif
"   endif
"   echo 'Cancelled'
"   if filereadable(s:commenttmp.'.1')
"       call rename(s:commenttmp.'.1', s:commenttmp)
"   endif
" endfun

" Get the string identifier for the project.
fun! s:GetProjectIdent(filename)
  " Check for URLs - which usually get read by plugins.
  if a:filename =~ '^[a-z]\{2,10}:[/\\]\{2}'
    return ''
  endif
  let bufnr=bufnr(a:filename)
  " Speedup - make sure we aren't looking at a special buffer
  if bufnr >= 0 
    let buftype= getbufvar(bufnr, '&buftype')
    if buftype != '' && buftype != 'nowrite' | return '' | endif
  endif

  let fname=fnamemodify(a:filename,':p:gs+\\+/+')
  let projdir=s:CheckDirForFile(fnamemodify(fname, ':h'),'.project')

  if projdir==''
    " Backwards compatible, use ssLocalTree for source-safe local database
    if exists('g:ssLocalTree')
      let localtree=fnamemodify( g:ssLocalTree, ':p:gs+\\+/+')
      if localtree ==? strpart( fname, 0, strlen(localtree) )
        return 'vss:$'.strpart( fname, strlen(localtree))
      endif
    endif
    return ''
  endif

  let proj=projdir.'.project'
  let filecontent=s:Cat(proj)
  let ssfile=substitute(filecontent,"^[  \n\r]*".'\(.\{-}\)'."[  \n\r]*$",'\1','')
  if ssfile==''
    return ssfile
  endif
  if ssfile !~ '[\\/]$'
    let ssfile=ssfile.'/'
  endif
  " Backwards compatible, assume SourceSafe if prefix is $
  if ssfile =~ '^\$' || ssfile=~ '^@.*@\$'
     let ssfile='vss:'.ssfile
  endif
 " echo 'found: '.ssfile.strpart(fname,strlen(projdir))
  return ssfile.strpart(fname,strlen(projdir))
endfun

" Load module as appropriate, also check for function existance
fun! s:has_function( fn, ptype)
  if exists( '*'.a:fn.a:ptype ) | return 1 | endif
  if exists( 'g:srcctl_version_'.a:ptype ) | return 0 | endif
  exe 'runtime srcctl_'.a:ptype.'.vim srcctl/'.a:ptype.'.vim srcctl/srcctl_'.a:ptype.'.vim'
  return exists( '*'.a:fn.a:ptype )
endfun

" View checked out files.
fun! s:DoCheckedOutFiles(bang, ...)
  " Todo: Make generic
    let bang=a:bang.'' =='!'
    if a:0==0
      let f1=getcwd()
    else
      let f1=a:1
    endif
    let prjid=s:GetProjectIdent(f1)
    let ptype=matchstr(prjid,'^[^:]\+\ze:')
    let prj=matchstr(prjid,'^[^:]\+:\zs.*$')
    if ptype == '' || !s:has_function('SourceControlCheckedOutFiles_',ptype)
      echoerr 'Not supported for "'.ptype.'"'
      return
    endif

    if a:0 <= 1
        call SourceControlCheckedOutFiles_{ptype}( f1, prj, '', bang)
    elseif a:0 == 1
        call SourceControlCheckedOutFiles_{ptype}( f1, prj, '', bang)
    elseif a:0 == 2
        call SourceControlCheckedOutFiles_{ptype}( f1, prj, a:2, bang)
    else
        echoerr 'SPLocked: Too many arguments!'
    endif
endfun

" Do a SS difference using vim diff.
fun! s:SCDiff(sctype, filename, project, Ver1, Ver2)
  "TODO: Handle diff on directories.
  if isdirectory(a:filename) || a:filename=='' | return | endif

  call s:CheckTemp()
  let ssorig=a:filename
  let ssfile=$TEMP."\\ss\\". fnamemodify(a:filename, ':t')
  if ! s:has_function( 'SourceControlDo_', a:sctype )
    echoerr 'Source Control not supported: "'.a:sctype.'"'
    return
  endif
  if a:Ver1 > 0
    call SourceControlDo_{a:sctype}( 'Checkout', ':overwrite:noupdate:', a:Ver1, ssfile, a:project, '')
    let ssorig=ssfile.'.'.a:Ver1
    call rename(ssfile, ssorig)
    exe 'above new '.ssorig
  endif

  let res=SourceControlDo_{a:sctype}( 'Checkout', ':overwrite:noupdate:forcedir:', a:Ver2, ssfile, a:project, '')

  if filereadable(ssfile)
    call s:SetDiffSplit(ssfile, 1)
    exe "norm \<C-W>\<C-X>"
  else
    echo confirm("Could not get file.", "OK",0)
  endif
endfun

" Create a checked-out list info line
fun! SourceControl_CreateInfoLine( currentproj, filename, user, fversion, which_user, recurse)
    let fname=( a:filename)
    let brief=fnamemodify(fname, ':t')
    let briefcol=30
    if a:recurse
        if a:currentproj== '' || a:currentproj =~ '/$'
            let brief=a:currentproj.brief
        else
            let brief=a:currentproj.'/'.brief
        endif
        let briefcol=50
    endif
    if a:which_user == '*'
        let b:filenamecolumn=(briefcol+5+10)
        return escape(substitute(s:TabColumns(briefcol,brief, 'r5',a:fversion,10,a:user, 0,fname),'\s*$','',''),'\&')
    else
        let b:filenamecolumn=(briefcol+5)
        return escape(substitute(s:TabColumns( briefcol,brief, 'r5', a:fversion,0,fname ),'\s*$','',''),'\&')
    endif
endfun

let s:spaces_str='     '
let s:spaces_n=5
fun! s:Spaces( n)
  if a:n <= s:spaces_n
    return strpart(s:spaces_str,0,a:n)
  endif
  let s:spaces_str=s:spaces_str.s:spaces_str
  let s:spaces_n=s:spaces_n*2
  return s:Spaces(a:n)
endfun

fun! s:TabColumns( ...)
    let c=1
    let ret=''
    while c+1 <= a:0
        let txt=a:{c+1}
        let txtlen=strlen(txt)
        let rjust=a:{c}[0] ==?'r'
        if rjust
            let width=strpart(a:{c},1)
        elseif a:{c} == 0
            let ret=ret.a:{c+1}
            let c=c+2
            continue
        else
            let width=a:{c}
        endif
        if txtlen>=width
            if width>5
                let txt='..'.strpart(txt,(txtlen-width)+3)
                let txtlen=width-1
            elseif width <= 2
                let txt=strpart(txt,0,width)
                let txtlen=width
            else
                let txt=strpart('.....',0,width-1)
                let txtlen=width-1
            endif
        endif
        let diff=width-txtlen
        let prespace =s:Spaces( rjust?((width>2)?(diff-1):(diff)):0)
        let postspace=s:Spaces( rjust?((width>2)?1:0):(diff))

        let ret=ret.prespace.txt.postspace
        let c=c+2
    endwhile
    return ret
endfun

fun! s:ExplorerMode()
  nnoremap <script> <buffer> <CR> :call <SID>OpenFile('')<CR>
  nnoremap <script> <buffer> o :call <SID>OpenFile('o')<CR>
  nnoremap <script> <buffer> O :call <SID>OpenFile('O')<CR>
  nnoremap <script> <buffer> p :call <SID>OpenFile('p')<CR>
  nnoremap <script> <buffer> f :call <SID>OpenFile('f')<CR>
  nnoremap <script> <buffer> <localleader>sf :call <SID>OpenFile('f')<CR>
  nnoremap <script> <buffer> K :call <SID>OpenFile('K')<CR>
  nnoremap <script> <buffer> <localleader>sk :call <SID>OpenFile('K')<CR>
  nnoremap <script> <buffer> L :call <SID>OpenFile('L')<CR>
  nnoremap <script> <buffer> <localleader>s<c-k> :call <SID>OpenFile('L')<CR>
  nnoremap <script> <buffer> <localleader>sl :call <SID>OpenFile('L')<CR>
  nnoremap <script> <buffer> u :call <SID>OpenFile('u')<CR>
  nnoremap <script> <buffer> <localleader>su :call <SID>OpenFile('u')<CR>
  nnoremap <script> <buffer> <localleader>sU :call <SID>OpenFile('ul')<CR>
  nnoremap <script> <buffer> U :call <SID>OpenFile('U')<CR>
  nnoremap <script> <buffer> <localleader><localleader>u :call <SID>OpenFile('U')<CR>
  nnoremap <script> <buffer> <localleader><localleader>U :call <SID>OpenFile('Ul')<CR>
endfun

"
fun! s:OpenFile( openmode )
  if !exists('b:filenamecolumn')
    call confirm('No column width specified!??')
    return
  endif
  let fname=strpart(getline('.'),b:filenamecolumn)
  if !filereadable(fname)
    call confirm("Can't find: '".fname."'")
    return
  endif
  if a:openmode==''
    exe 'e '.fname
  elseif a:openmode==#'O'
    exe 'new '.fname
  elseif a:openmode==#'o'
    let win=bufwinnr(bufnr(fname))
    if win != -1
      exe win.'winc w'
    else
      exe 'new '.fname
    endif
  elseif a:openmode==#'p'
    exe 'pedit '.fname
  elseif a:openmode==#'f'
    let win=bufwinnr(bufnr(fname))
    if win != -1
      exe win.'winc w'
    else
      exe 'new '.fname
    endif
    call s:SCDoCommand('',v:count, 'Diff' )
  elseif a:openmode==#'K'
    call s:SCDoCommand('',v:count, 'Checkout', fname )
  elseif a:openmode==#'L'
    call s:SCDoCommand('',v:count, 'Lock', fname )
  elseif a:openmode==#'u'
    call s:SCDoCommand('',v:count, 'Update', fname )
  elseif a:openmode==#'U'
    call s:SCDoCommand('',v:count, 'RepeatUpdate', fname )
  elseif a:openmode==#'ul'
    call s:SCDoCommand('',v:count, 'UpdateLocked', fname )
  elseif a:openmode==#'Ul'
    call s:SCDoCommand('',v:count, 'RepeatUpdateLocked', fname )
  endif
endfun

" Coppied from the scripts of David Eggum.
function! SCGetLockStatus()
   if exists("b:checked_out_status")
      return b:checked_out_status
   else
      return ""
   endif
endfunction

" Coppied from the scripts of David Eggum.
function! SCLockStat()
   if exists("b:checked_out_status_brief")
      if b:checked_out_status_brief != ''
        return '<'.b:checked_out_status_brief.'>'
      else
        return b:checked_out_status_brief
      endif
   else
      return ""
   endif
endfunction

if (strlen(&rulerformat) == 0) && (scSetRuler == 1)
   set rulerformat=%60(%=%{SCGetLockStatus()}%)\ %4l,%-3c\ %3p%%
endif
augroup srcctlstatus
au!
if g:scMaintainStatus
  au BufRead * call <SID>UpdateStatus()
endif
aug END

fun! s:SetStatus( bufident, filename, sctype, projname, opts)
  if s:has_function('SourceControlDo_', a:sctype)
    call s:UpdateStatusVars( SourceControlDo_{a:sctype}('Status',a:opts.':return:',0,a:filename, a:projname, ''), a:bufident)
  endif
endfun
fun! s:UpdateStatusVars( result, bufident)
    let ret=matchstr(a:result,"^[^\n]*")
    if (a:bufident != -1 )
      call setbufvar(a:bufident, 'checked_out_status',ret)
      call setbufvar(a:bufident, 'checked_out_status_brief',matchstr(a:result,"\n\\@<=.*$"))
    endif
    return ret
endfun

" get the current lock status from VSS and place it in b:checked_out_status
function! s:UpdateStatus()
  if &buftype != '' || ! &buflisted | return |endif
  let prjid=s:GetProjectIdent(@%)
  if prjid=='' | return | endif

  let typ1=matchstr(prjid,'^[^:]\+\ze:')
  let prj1=matchstr(prjid,'^[^:]\+:\zs.*$')
  call s:SetStatus( winbufnr(winnr()), @%, typ1, prj1, (g:scShowExtra?':extras:':'').(g:scShowAllLocks?':locks:':''))
endfunction

fun! s:pushLeader()
    let restore=''
    if exists("g:scMapLeader")
      if exists("g:maplocalleader")
        let restore='let g:maplocalleader="'.escape(maplocalleader,"\\".'"'
      else
        let restore='unlet maplocalleader'
      endif
      let maplocalleader=g:scMapLeader
    endif
    return restore
endfun

" Add menu seperator at the end of the SourceControl menu
fun! SrcCtl_addMenuSep()
  if !has('menu') | return | endif
  if !exists('s:scMenuWhere') | let s:scMenuWhere = 0 | endif

  let s:scMenuWhere=s:scMenuWhere+5
  let menu_desc=g:scMenuPlace.'.'.s:scMenuWhere.' '.escape(g:scMenuPath.'.-s'.s:scMenuWhere.'-',"\\ \<tab>")
  exe 'menu '.menu_desc.' <nul>'
endfun

" Add mapping, command and menu entry for SourceControl command
"   cmd  : Name of the :command
"   item : Menu item description
"   desc : Prefix for the key description (for {count}) in the menu.
"   keys : Key mapping (prefixed by <localleader>.
"   a{1} : Override for menu/key mapping. 
"
" You can also use <localleader> in the mappings.
"
fun! SrcCtl_addMenuMapping( cmd, item, desc, keys, ...)
    if !exists('s:scMenuWhere') | let s:scMenuWhere = 0 | endif
    let s:scMenuWhere=s:scMenuWhere+5
    let menu_desc=g:scMenuPlace.'.'.s:scMenuWhere.' '.escape(g:scMenuPath.'.'.a:item,"\\ \<tab>")

    if a:0==0
      let mapping=':<c-u>call <SID>SCDoCommand("", v:count, "'.a:cmd.'")<cr>'
    else
      let mapping=a:1
    endif

    let attr = s:SCCommandAttrib( a:cmd)
    if g:scCommandPrefix =~ '^[A-Z]' || g:scCommandPrefix == ''
      let complete=(attr =~':directory:') ? 'dir' : 'file'
      exe 'command! -complete='.complete' -nargs=* -count=0 -bang '.g:scCommandPrefix.a:cmd.' call s:SCDoCommand(<q-bang>,<count>, "'.a:cmd.'",<f-args>)'
    endif

    if a:keys==''
      " menu/command only mapping
      if has('menu') | exe 'menu '.menu_desc.' '.mapping | endif
      return
    endif

    let restore=s:pushLeader()
    if exists('g:maplocalleader')
      let leaderdesc=g:maplocalleader
    else
      let leaderdesc='\'
    endif
    let keys='<localleader>'.a:keys
    let keysdesc=escape(substitute(keys,'<localleader>',escape(leaderdesc,"\\"), 'g'),"\\")

    if !hasmapto( mapping)
      exe 'nnoremap '.keys.' '.mapping
    endif
    if has('menu') | exe 'amenu '.menu_desc.'<tab>'.a:desc.keysdesc.' '.mapping | endif

    exe restore
endfun

" Dump the currently loaded modules.
fun! s:DumpModules()
  if exists('g:srcctl_loaded_modules')
    let lm=g:srcctl_loaded_modules
    echo "Loaded modules for SourceControl version ".g:srcctl_version
    echo s:TabColumns( 5, 'Id', 'r5', 'Ver', 0,'Module Name')
    while lm=~'^:.' 
      let module=matchstr(lm,':\zs[^:]*')
      if module != '' && exists( 'g:srcctl_version_'.module) && exists( 'g:srcctl_name_'.module)
        echo s:TabColumns( 5, module,'r5', g:srcctl_version_{module}, 0, g:srcctl_name_{module})
      endif
      let lm=substitute(lm,'^:[^:]\+','','')
    endwhile
  else
    echo 'None Loaded'
  endif
endfun

command! -complete=file -bang -nargs=+ -count=0 SS call s:DoSrcSafe(<q-bang>,<count>,<f-args>)

command! -complete=dir -bang -nargs=* -count=0 SDeploy call s:DoSrcSafe(<q-bang>,<count>, 'Deploy',<f-args>)
command! -complete=dir -bang -nargs=* -count=0 SPLocked call s:DoCheckedOutFiles(<q-bang>, <f-args>)

command! -complete=file -bang -nargs=1 -count=0 VDiff call s:SetDiffSplit(<f-args>, <q-bang> != '!')


let s:scMenuWhere=0
"  call SrcCtl_addMenuMapping('Chec&kout', '{count}', 'sk', )
"
call SrcCtl_addMenuMapping('Get',                '&Get',                   '{count}',  'sg')
"call SrcCtl_addMenuMapping('Merge',              '&Merge',                 '',         'sg')
call SrcCtl_addMenuMapping('GetForced',          'Get &Forced',            '',         'sG')
call SrcCtl_addMenuMapping('Checkout',           'Check&out (locked)',     '{count}',  'sk')
call SrcCtl_addMenuMapping('Lock',               '&Lock',                  '',         's<c-k>')
call SrcCtl_addMenuMapping('Unlock',             '&Uncheckout',            '',         's<c-u>')
call SrcCtl_addMenuSep()
call SrcCtl_addMenuMapping('Update',             'Check&in',               '',         'su')
call SrcCtl_addMenuMapping('RepeatUpdate',       'Checkin Simila&r',       '',         '<localleader>u')
call SrcCtl_addMenuMapping('UpdateLocked',       'Checkin Locked',         '',         'sU')
call SrcCtl_addMenuMapping('RepeatUpdateLocked', 'Checkin Similar Locked', '',         '<localleader>U')
call SrcCtl_addMenuSep()
call SrcCtl_addMenuMapping('Directory',          'D&irectory',             '',         's.')
call SrcCtl_addMenuMapping('RawStatus',          'S&tatus',                '',         'SS')
call SrcCtl_addMenuMapping('Status',             '&Show Status',           '',         'ss')
call SrcCtl_addMenuMapping('Summary',            '&History',               '{count}',  'sh')
call SrcCtl_addMenuMapping('History',            '&View history',          '{count}',  'sH')
call SrcCtl_addMenuSep()
call SrcCtl_addMenuMapping('TDiff',              '&Text Diff',             '{count}',  'sd')
call SrcCtl_addMenuMapping('Diff',               '&Diff',                  '{count}',  'sf')
call SrcCtl_addMenuMapping('DiffClose',          'Diff C&lose',            '',         'sF', ':<c-u>call <SID>ClearDiffMode()<cr>')
call SrcCtl_addMenuSep()
call SrcCtl_addMenuMapping('Add',                '&Add File',              '',         'sa')

" Add at end.
let s:save_menuwhere=s:scMenuWhere
let s:scMenuWhere=500
call SrcCtl_addMenuSep()
call SrcCtl_addMenuMapping('Modules',             'Show Modules',         '',         '', ':<c-u>call <SID>DumpModules()<cr>')

" Restore to allow other modules to add special entries
let s:scMenuWhere=s:save_menuwhere
unlet s:save_menuwhere


" --------------------------- Utility ----------------------------------
"
" Handle auto-closing of Diff windows.
"
"
augroup difwin
au!
au BufHidden,BufDelete,BufUnload * call <SID>DiffBufHide(expand('<afile>'))
au BufEnter * call <SID>ReclearDiffMode()
aug END

fun! s:ReclearDiffMode()
  if exists('b:srcsafe_diffmode') && !b:srcsafe_diffmode && exists('b:srcsafe_diffbuffvars')
    exe b:srcsafe_diffbuffvars
    unlet b:srcsafe_diffbuffvars
  endif
endfun

" 
fun! s:ClearDiffMode()
  if exists('g:srcsafe_diffbuffnr')
    let diffbuffnr=g:srcsafe_diffbuffnr
    unlet g:srcsafe_diffbuffnr
    let win=bufwinnr(diffbuffnr)
    while winbufnr(win) != -1 && !getwinvar(win,'&diff')
      let win=win+1
      while winbufnr(win) != -1 && winbufnr(win) != diffbuffnr
        let win=win+1
      endwhile
    endwhile
    let curwin=winnr()
    if winbufnr(win)!= -1
      exe win.'winc w'
      exe b:srcsafe_diffbuffvars
      if &fdc==0
        norm zn
      endif
      exe curwin.'winc w'
    endif
    if g:srcsafe_diffbuff_delbuffer
      exe 'bdel '.g:srcsafe_diffbuff_difnr
    else
      call setbufvar(g:srcsafe_diffbuff_delbuffer, 'srcsafe_diffmode', 0)
    endif
    unlet g:srcsafe_diffbuff_delbuffer

    let b:srcsafe_diffmode=0
    "unlet b:srcsafe_diffbuffvars
    unlet g:srcsafe_diffbuff_difnr
  endif
endfun

fun! s:SetDiffSplit( ssfile, delbuffer )
    call s:ClearDiffMode()
    let diffs=''
    let win=1
    while winbufnr(win) != -1
        if getwinvar(win, '&diff')
            call setwinvar(win, '&diff',0)
            let diffs='|call setbufvar('.winbufnr(win).',"&diff",1)'
        endif
        let win=win+1
    endwhile
    let b:srcsafe_diffmode=1
    let vars=s:GetDiffSplitVars()
    let b:srcsafe_diffbuffvars=vars
    let g:srcsafe_diffbuffnr=winbufnr(winnr())

    " Vertical diff split
    exe (g:scDiffVertical?'vert': '').' diffsplit '.a:ssfile

    " Make the defaults the same as the file we are comparing against
    if !a:delbuffer
      let b:srcsafe_diffbuffvars=vars
      let b:srcsafe_diffmode=1
    endif
    let g:srcsafe_diffbuff_difnr=winbufnr(winnr())
    let g:srcsafe_diffbuff_delbuffer=a:delbuffer
endfun

fun! s:GetDiffSplitVars()
    return 'set nodiff '. (&scb?'': 'no').'scb sbo='.&sbo.' '.(&wrap?'': 'no' ).'wrap fdm='.&fdm.' fdc='.&fdc.' '.(&fen?'': 'no').'fen '.(&scb?'': 'no').'scb fdl='.&fdl
endfun

" Hide a diff buffer
fun! s:DiffBufHide( filename)
    if exists('g:srcsafe_diffbuff_difnr')
        let buf=bufnr(a:filename)
        if g:srcsafe_diffbuff_difnr==buf
            call s:ClearDiffMode()
        elseif exists('g:srcsafe_diffbuffnr') && g:srcsafe_diffbuffnr==buf
            let curbuf=winbufnr(winnr())
            if bufexists(buf)
                exe 'buf '.buf
                set diff
                call s:ClearDiffMode()
                exe 'buf '.curbuf
            else
                call s:ClearDiffMode()
            endif
        endif
    endif
endfun
" Check a directory for the specified file
function! s:CheckDirForFile(directory,file)
    let aborted=0
    let cur=substitute(a:directory,'\\\+','/','g')
    while !filereadable(cur.'/'.a:file) && (aborted==0)
        if ( cur =~ '^\(.:\)\=//\=$') 
            let aborted=1
        elseif ( cur =~ '^//[^/]\+/[^/]\+$' )
            let aborted=2
        else
            let cur=fnamemodify(cur,':h')
        endif
    endwhile
    " Check the two cases we haven't tried
    if (aborted == 1) && filereadable(cur.a:file)
        let aborted=0
    elseif (aborted == 2) && filereadable(cur.'/'.a:file)
        let aborted=0
    endif
    if !(cur =~ '/$')
        let cur = cur.'/'
    endif 
    if aborted
      return ''
    else
      return cur
    endif
endfun

if !has('unix') && &shell !~ 'sh[a-z.]*$'
" && system('cat xxy__yzz') =~ "is not recognized"
    fun! s:Cat( filename )
        return system( 'type "'.substitute(a:filename,'/', '\\', 'g').'"')
    endfun
else
    fun! s:Cat( filename )
        return system( 'cat "'.a:filename.'"')
    endfun
endif
fun! s:CheckOverWrite(bufname)
  let destbuf=bufnr(a:bufname)
  if destbuf!=-1
    " make sure we're ok to overwrite a modified file!
    if getbufvar( a:bufname, '&modified' )
      return confirm('File Modified:'. a:bufname."\nContinue?", "&Ok\n&Cancel", 1) == 1
    endif
  endif
  return 1
endfun


fun! s:CheckWrite(bufname)
  " make sure we're ok to write a modified file!
  let destbuf=bufnr(a:bufname)
  if destbuf!=-1
    " make sure we're ok to overwrite a modified file!
    if getbufvar( a:bufname, '&modified' )
      let res=confirm('File Modified:'. a:bufname."\nWrite?", "&Ok\n&Ignore\n&Cancel", 1) 
      if res==3 || res==0
        return 0
      endif
      let cur=winnr()
      let dest=bufwinnr(destbuf)
      exe dest.'winc w'
      if res==1
        write
      elseif res==2
        edit!
      endif
      exe cur.'winc w'
    endif
  endif
  return 1
endfun

fun! SrcCtl_RegisterModule( module, modver, ... ) " Ignore extra args
  if !exists('g:srcctl_loaded_modules') | let g:srcctl_loaded_modules = ':' | endif
  let g:srcctl_loaded_modules=substitute(g:srcctl_loaded_modules,':'.a:module.':', ':', 'g').a:module.':'
  let g:srcctl_version_{a:module}=a:modver
  if !exists('g:srcctl_name_'.a:module)
    echo 'Please register source control module '.a:module.' with the script writer!'
  endif
  if a:0 >=1
     let g:srcctl_name_{a:module}=a:1
  endif
endfun

" com! -nargs=+ SrcCtlRegister call <SID>SrcCtl_RegisterModule( <f-args> )


" vim: ts=2 et sw=2
