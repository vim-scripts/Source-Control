" Source Control module for Visual Source Safe
"
" --------------------------- vss : Visual Source Safe ------------------------------
"
" The scripts require the sourcesafe command-line to be working without
" prompting for a password.  To do this, there are two main methods. The first
" is to set the the %SSDIR% enviornment variable.
" SSDIR=\\server\share\ssdir
"
" The second is to put the following line in your local srcsafe.ini:
"     #include \\server\share\ssdir\srcsafe.ini
"
" The script relies on an environment variable $SS to be set to point to
" ss.exe, and on a '.project' file specifying which srcsafe database to use.
"
" This file should contain the source-safe path
" eg:
" $/MyProj/
" Version 1.6 has improved support for multiple databases. 
" A sourcesafe path (or inifile) can precede this, inclosed with '@' symbols : 
" eg:
" @\\file\srcsafe@$/MyProj/
" This works by setting (& restoring) $SSDIR before calling the
" command-line.
"
" The script does its  the user to respond to prompts.  It does
" this by always answering 'no', then parsing the response, and asking the
" user 'yes/no', and then re-executing the command always answering 'yes'.
" It is possible that there are some special cases that need handling.
" 
" g:ssExecutable     ($SS)       : May NOT have spaces - use short-filenames.
" g:ssDeployFile     (0)         : Allow deploying of files.
"
"* If the directory $TEMP/SS  (which is required by the script) does not
"  exist, then create it / warn the user if $TEMP doesn't exist.
"
"Add ourselves to the loaded modules
call SrcCtl_RegisterModule( 'vss', 1.1, 'Visual Source Safe')
" History:
" 1.1:
"   - Fix up adding comments.


fun! SourceControlAttrib_vss( attrib )
  return substitute(a:attrib, ':?lock:', ':lock:', '')
endfun

fun! SSDlet(lhs,rhs)
  if a:lhs !~ '^[gs]:' | let lhs='g:'.a:lhs | else | let lhs=a:lhs | endif
  if !exists(lhs) | exe 'let '.lhs.' = "'.escape(a:rhs,"\\\"").'"' | endif
endfun

call SSDlet('ssDeployFile', 0)     "Allow deployment of files.

delfun SSDlet

call SrcCtl_addMenuSep()
call SrcCtl_addMenuMapping('', '&Explore', '','',':call <SID>SSRun("ssexp.exe")<CR>')
call SrcCtl_addMenuMapping('', 'Admin', '','',':call <SID>SSRun("ssadmin.exe")<CR>')

fun! s:system(cmd,args)
"  echo a:cmd.' '.a:args
  return system('""'.a:cmd.'" '.a:args.'"')
endfun


" SourceControlDo_vss
" a:cmd                 The command
" a:opts                options
"    :self:                Command was specified without a name
"    :recurse:             Command should be recursive
"    :text:                Text output (Diff)
"    :lock:                Lock the file
"    :overwrite:           Overwrite the file
"    :locks:               Show locks on status
"    :extras:              Show extras on status
" a:file1               The original filename specification 
" a:repositoryfile1     The repository filename specification
" a:comment             Comment for operation.  '@' prefix specified filename
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
"
fun! SourceControlDo_vss(cmd, opts, count, file1, repositoryfile1, comment)
  let comment=' -C-'
  if a:comment != ''
    if a:comment !~ '\s'
      let comment= ' -C'.a:comment
    else
      if a:comment =~ '^@'
        if a:comment =~ '\s'
          let comment=' "-C@'.substitute(a:comment,'^@','','').'"'
        end 
      else
        let comment=' "-C'.a:comment.'"'
      endif
    endif
  endif

  if a:cmd ==? 'Diff'
    if a:opts =~ ':text:'
      call s:SSCmd('Diff', a:file1, a:repositoryfile1, a:count, 'd')
"    else
"      call s:SSDiff(f{i},a:count)
    endif
    return 0
  elseif a:cmd==? 'Checkout'
    let opts='o'
    if a:opts =~ ':forcedir:'
      let opts=opts.'af'
    endif
    if a:opts =~ ':lock:'
      call s:SSCmd('Checkout -GWA'.comment, a:file1, a:repositoryfile1, a:count,  opts)
    else
      let cmdopts = 'Get '
      let ow = (a:opts =~ ':overwrite:')
      if ow | let cmdopts=cmdopts.'-GWM ' | endif
      if a:opts =~ ':recurse:' 
        let cmdopts=cmdopts.'-R '
      elseif !ow
          let cmdopts=cmdopts.' -GWA'
      endif
      call s:SSCmd(cmdopts, a:file1, a:repositoryfile1, a:count,  opts)
"    else
"      call s:SSCmd('Get -GWM', a:file1, a:repositoryfile1, a:count, 'o')
    endif
    return 1
  elseif a:cmd==? 'Lock'
    call s:SSCmd('Checkout -G-'.comment, a:file1, a:repositoryfile1, a:count,  'o')
    return 1
  elseif a:cmd==? 'Unlock'
    call s:SSCmd('UnCheckout ', a:file1, a:repositoryfile1, a:count,  'o')
    return 1
  elseif a:cmd==?'Checkin'
    let cmdopts = 'Update'
    if a:opts =~ ':lock:' | let cmdopts=cmdopts.' -K' |endif
    call s:SSCmd( cmdopts.comment, a:file1, a:repositoryfile1, 0, 'o')
    return 1
  elseif a:cmd==?'Status'
    if a:opts =~ ':return:'
      return s:SSGetStatus( a:file1, a:repositoryfile1, 1, a:opts =~ ':extras:', a:opts =~ ':locks:')
    elseif a:opts =~ ':text:'
      call s:SSCmd( 'Status', a:file1, a:repositoryfile1, 0, '')
    else
      call confirm( s:SSGetStatus(a:file1, a:repositoryfile1, 0, 1, 1))
    endif
    return 0
  elseif a:cmd==?'History'
    if a:opts =~ ':text:'
      call s:SSCmd( 'History -#'.((a:count)?(a:count):3), a:file1, a:repositoryfile1, 0, '')
    else
      call s:DoHistoryWithSyntax( a:file1, a:repositoryfile1)
    endif
    return 0
  elseif a:cmd==? 'Add'
    call s:SSCmdAdd( a:file1, a:repositoryfile1)
    return 1
  elseif a:cmd ==? 'Deploy'
    if a:opts =~ ':self:' && !g:ssDeployFile
      let f1=fnamemodify(a:file1,':h') " Directory, not file
      let r1=fnamemodify(a:file1,':h')
    else
      let f1=a:file1
      let r1=a:repositoryfile1
    endif
    call s:SSCmd('Deploy'.((a:opts =~ ':recurse:')?' -R': ''), f1, r1, a:count, '')
    return 0
  elseif a:cmd ==? 'Dir'
    call s:SSCmd('Dir'.((a:opts=~':recurse:')?' -R': ''), a:file1, a:repositoryfile1, a:count, '')
    return 0
  else
      call confirm('VSS: Unknown function :"'.a:cmd.'"')
  end if
endfun

"
" Special version for Add, as we need to do a CP to make sure it gets added to
" the correct project.
fun! s:SSCmdAdd( filename, repositoryfile )
  "Check for setup.
  if !s:CheckSS() | return | endif

  if !filereadable( a:filename )
    call confirm( "'".a:filename."' does not exist!")
    return
  endif

  let repfile=SSGetRepFile(repositoryfile)

  let newproj=fnamemodify(repfile,':h')
  if newproj.'' == ''
     echoerr 'Invalid project'
     return
  endif
  " Change project - so the file gets added to the correct one!
  let result=s:system( g:ssExecutable,' CP "'.newproj.'"' )
  call s:Success( result)

  let result=s:system( g:ssExecutable,' Add -I-N "'.a:filename.'"')

  " Clear all the answered y/n questions for the prompt.
  let res=substitute(result,'(Y/N)N', '', 'g')
  if res ==result
    " Successful - finish.
    call s:Success(res)
    return
  else
    " Answer the questions
    if confirm(res, "&Yes\n&No", 1) != 1
        " Don't continue!
        return 
    endif
  endif

  let result=s:system(g:ssExecutable,' '.a:cmd.' -I-Y '.cmdargs)

  " Strip out all the questions answered:
  let result=substitute(substitute(result,"[^\n]*(Y/N)Y", '', 'g'),"\n\\+", "\n", 'g')
  call s:Success(result)

endfun

" Return the SS Filename spec from the 'repository file' spec
fun! s:SSGetRepFile( filename )
  if (a:filename == '') | return a:filename | endif

  " Remember original directory
  if !exists('g:srcsafe_ssdir_orig') | let g:srcsafe_ssdir_orig=$SSDIR | endif
  let ssfile=a:filename

  if ssfile=~'^@'
    let ssproj=substitute(ssfile,'^@\(.*\)@$.*$','\1','')
    let ssfile=substitute(ssfile,'^@.*@\ze\$','','')
    let $SSDIR=substitute(ssproj,'\c[/\\]\k\+\.ini$','','')
  else
    let $SSDIR=g:srcsafe_ssdir_orig
  endif
  return ssfile
endfun

" Execute a sourcesafe command.
"  cmd - the command plus args.
"  filename - the local filename
"  ssVer  - the version in sourcesafe (or 0)
"  opts
"       f - to answer 'y' to all questions.
"       o - specify output directory
"       d - diff mode (specify standard filename as well)
"       a - alternate directory override
fun! s:SSCmd(cmd, filename, repositoryfile, ssVer, opts)
" force, hasoutput)
  let force=(a:opts=~'f')
  let hasoutput=(a:opts=~'o')
  let isdiff=(a:opts=~'d')
  let isalt=(a:opts=~'a')

  "Check for setup.
  if !s:CheckSS() | return 0 | endif

  " make sure we're ok to discard a modified file!
  "if &modified && confirm('File Modified - discard?', "&Ok\n&Cancel", 1) != 1
  "  return
  "endif

  " Make sure the file gets output in the correct spot.
  let outpath=a:filename
  if !isdirectory(outpath)
    let outpath=fnamemodify(outpath, ':p:h')
  endif

  let ssfile = s:SSGetRepFile(a:repositoryfile)
  if ssfile.'' == ''
     return 0
  endif
  let ssfile='"'.ssfile.'"'
  " There is some weirdness that causes the final quote in -GL"s:\" to be escaped!?
  " Make sure this doesn't happen
  let outpath=substitute(outpath,'\\$','\\\\','')
  " Build up the basic arguments.
  let cmdargs=(isalt?'-GF- ':'').(hasoutput?'-GL"'.outpath.'" ' : '').s:SSVersion(a:ssVer).ssfile

  if isdiff
    let cmdargs=cmdargs.' "'.a:filename.'"'
  endif

  let autoread=&autoread
  if !force
      " Try and do it once - answering 'no' to all questions.

      " Autoread the file
      setlocal autoread

      echo a:cmd
      " Perform the operation
      " call input('SS '.a:cmd.' -I-N '.cmdargs )
      let result=s:system(g:ssExecutable,' '.a:cmd.' -I-N '.cmdargs)
      " Make sure we know it has been modified.
      checktime
      if !autoread
          setlocal noautoread
      endif

      " Clear all the answered y/n questions for the prompt.
      let res=substitute(result,'(Y/N)N', '', 'g')
      if res ==result
        " Successful - finish.
        "echo a:cmd.' -I-N '.cmdargs
        call s:Success(res)
        return 1
      else
        " Answer the questions
        if confirm(res, "&Yes\n&No", 1) != 1
            " Don't continue!
            return 0
        endif
      endif
  endif 
  " Try again / force 'yes' to questions.
  setlocal autoread
  "call input( 'SS '.a:cmd.' -I-Y '.cmdargs)
  let result=s:system(g:ssExecutable,' '.a:cmd.' -I-Y '.cmdargs)

  " Strip out all the questions answered:
  let result=substitute(substitute(result,"[^\n]*(Y/N)Y", '', 'g'),"\n\\+", "\n", 'g')
  "echo a:cmd.' -I-Y '.cmdargs
  call s:Success(result)
  checktime
  if !autoread
      setlocal noautoread
  endif
  return 1
endfun

fun! s:DoHistoryWithSyntax(filename, repositoryfile)
  if !s:CheckSS() | return | endif

  let prjfile=s:SSGetRepFile(a:repositoryfile)
  if prjfile.'' == ''
     echoerr 'Invalid project'
     return
  endif

  let bname='History!'.fnamemodify(a:filename,':t:gs/ \./_/')
  let hbufnr=bufnr(bname)
  let hwinnr=bufwinnr(hbufnr)
  if hwinnr==-1
    exe 'rightbelow ' . (g:scHistVertical ? 'v' : '') . 'new'
    if hbufnr==-1
        set buftype=nofile

        exec 'f '.bname
    else
        exec 'b '.hbufnr
    endif
  else
    exe hwinnr.'winc w'
  endif
  setlocal modifiable
  1,$d

  exec '.r !""'.g:ssExecutable.'" History "'.prjfile.'""'
  set nomodified
  setlocal nomodifiable
  call s:SSHistorySyntax()
  1
endfun


fun! SourceControlCheckedOutFiles_vss( project, repositoryfile, user, recursive)
  if !s:CheckSS() | return | endif
  let tempfile=fnamemodify(tempname(),':8')
  let ssproj=s:SSGetRepFile(a:repositoryfile)
  if ssproj.'' == ''
     echoerr 'Invalid project'
  endif
  let res=s:system( g:ssExecutable,' Status -I-N -NS -O'.tempfile.' '.(a:recursive?'-R ': '').((a:user=='*')?'':'-U'.a:user.' ').s:SSShortName(ssproj))

  if !filereadable(tempfile)
    echoerr res
  endif

  new

  setlocal modifiable
  let showcmd_save=&showcmd
  let report_save=&report
  set noshowcmd
  set report=9999
  exe 'r '.tempfile
  call delete(tempfile)
  let l=1
  let currprj='$/'
  let thefirst=1
  if line('$')==1
    return
  endif
  while l<= line('$')
    exe l
    let txt=getline('.')
    if txt=~'^\s*$'
      d
      let l=l-1
    elseif txt =~ '^$.*'
      let currprj=substitute(txt,':$','','')
      d
      let l=l-1
      let rootlen=strlen(ssproj)
      if strpart(currprj,0,rootlen-1).'/' ==? ssproj
          let currprj=strpart(currprj,rootlen)
      endif
      let thefirst=0
    else
        s/^\([^ ]*\) \+\<\(\k\+\)\> \+\%(\(v[0-9]\+\) \+\)\=[0-9\/]* \+[0-9:pa]*  \+\(.*\)$/\=
\SourceControl_CreateInfoLine(currprj,s:ExpandFile(submatch(4).'\'.submatch(1)),submatch(2),submatch(3),a:user,a:recursive)/
    endif

    let l=l+1
  endwhile
  call append(0,ssproj.':')
  call append(1,'')
  let ssprojn=substitute(substitute(ssproj, '^"\(.*\)"$', '\1', ''),'[^a-zA-Z0-9]', '_', 'g')
  let bufname='srcsafe:'.ssprojn
  let idx=1
  while bufnr(bufname) != -1
    if bufwinnr(bufnr(bufname))==-1
        exe 'bdel '.bufname
        break
    endif
    let bufname='srcsafe'.idx.':'.ssprojn
    let idx=idx+1
    if idx >10 
        let bufname='??'.winbufnr(winnr())
    endif
  endwhile
  silent exe 'f '.bufname
  let &showcmd=showcmd_save
  let &report=report_save
  set bt=nofile nomodified
  setlocal nomodifiable
  call s:ExplorerMode()
endfun

fun! s:CheckSS()
  if !exists("g:ssExecutable")
    if !filereadable($SS)
      let SS='c:\Program Files\Microsoft Visual Studio\vss\win32\ss.exe'
      if !filereadable(SS)
        let SS='c:\Program Files\Microsoft Visual Studio\Common\vss\win32\ss.exe'
      endif
      if filereadable(SS)
        let g:ssExecutable=SS
      else
        echoe 'Set g:ssExecutable or $SS Variable correctly to point to SS.EXE'
        return 0
      endif
    else
        let g:ssExecutable=$SS
    endif
  endif
  return 1
endfun

" Returns an SS Version string (when using 'count' before a command)
func! s:SSVersion(c)
  return ((a:c==0)?"": ("-V".a:c." "))
endf

" Turns on Sourcesafe history syntax
function! s:SSHistorySyntax()
 syntax region SSHistoryHeader start="^---" skip="^history" end="^" contains=SSHistoryFile
 syntax region SSHistoryFile matchgroup=SSHistoryFileLeader start="^history " matchgroup=NULL end="$"  contained
 syntax match SSHistoryVersion /\<[0-9]*\>/ contained
 syntax match SSHistoryField /\<[a-zA-Z]\+:/  
 syntax region SSHistoryVersionLine start="^\*\+" end="$" contains=SSHistoryVersion
" syntax match SSHistoryCommentLeader "Comment:" contained
" syntax match SSHistoryCommentTrailer "Comment:" contained
 syntax region SSHistoryComment matchgroup=SSHistoryCommentLeader start="^\(Label \)\=[cC]omment:" matchgroup=SSHistoryCommentTrailer end="^\*"he=s-1 contains=SSHistoryCommentLeader
 syntax region SSHistoryAction matchgroup=SSHistoryActionHead start="^\(Checked in\)\|\(Labeled\)" end="$"
endfunction 

highlight default SSHistoryVersionLine guifg=Maroon
highlight default SSHistoryVersion gui=bold guifg=Maroon
highlight default SSHistoryField gui=bold
highlight default SSHistoryActionHead gui=bold guifg=Blue 
highlight default link SSHistoryComment Comment 
highlight default link SSHistoryCommentLeader SSHistoryField
highlight default link SSHistoryCommentTrailer SSHistoryVersionLine
highlight default link SSHistoryAction String 
highlight default link SSHistoryHeader SSHistoryVersionLine
highlight default link SSHistoryFile String
highlight default link SSHistoryFileLeader SSHistoryActionHead

function! s:SSGetStatus(filename, repositoryfile, include_brief, ShowExtra, ShowAllLocks)
  if !s:CheckSS() | return "No VSS\n#" | endif

  let ssname = s:SSGetRepFile(a:repositoryfile)

  if ssname.'' == '' 
    return "Not in VSS!".((a:include_brief)?"\n": '')
  endif

  let sBrief=''

  let sCmd = 'Status -I-N "'.ssname.'"'

  let sFull = s:system(g:ssExecutable, sCmd)
  let sLine = sFull
  if (match(sFull,"No checked out files found.") == 0)
    return "Not Locked".((a:include_brief)?"\n@":'')
  elseif (match(sFull,"is not valid SourceSafe syntax") != -1 || 
\          match(sFull,"is not an existing filename or project") != -1 ||
\          match(sFull,"has been deleted") != -1)
    return "Not in VSS".((a:include_brief)?"\n":'')
  elseif (strlen(sFull) == 0)
    return ((a:include_brief)?"\n": "")
  endif

   "Slightly different mode for directory
   let is_dir=isdirectory( a:filename)
   let sep = (is_dir?"\n":', ')
   " Quirk: VSS truncates files over 19 characters long
   let file = (is_dir?'':(strpart(expand("%:t"),0,19)))
   let sUsers = ""
   let sStatus = ""
   let sBrief = ''
   let nCount = 0
   let bMyLock =0
   while (strlen(sLine) != 0)
      let sMatch = matchstr(sLine,".\\{-1,}\n")
      if match(sMatch,'\c^srcsafe:')!=-1 || match(sMatch, '^\s') != -1 || match(sMatch,'^\$') != -1
        "
      elseif is_dir || match(sMatch,'\c^'.file) == 0
        if is_dir 
          let file = substitute(matchstr(sMatch, '^.\{18}'),'\s*$','','') 
        endif
        let sUser = matchstr(sMatch, ' \@<=\w\+')
        let bExclusive = match(sMatch,'\w\+\s\+\w\+\s\+Exc') > -1
        let bIsMe = sUser ==? g:scUserName
        let sOld = matchstr(sMatch,'\(\w\+\s\+\w\+\s\+v\)\@<=\d\+')
        let bOld = (sOld!='')
        let sExtras = ''

        if bExclusive
          let sBrief=(bIsMe? 'X' : 'x')
        else
          let sBrief=sBrief.(bIsMe? (bOld? 'O' : 'L') : 'l')
        endif

        if a:ShowExtra
          let sExtras = (bExclusive?' (exc)': '').(bOld? ('('.(bIsMe?sOld : 'old').')'): '')
        endif
        if is_dir
          let sUser = file.":\<tab>".sUser
        endif
        if a:ShowAllLocks == 1 || is_dir
           let sUsers = sUsers.((nCount==0)?'':sep).sUser.sExtras
        else
          let bMyLock = bMyLock || bIsMe
          if nCount==0
            let sUsers = sUser.sExtras
          endif
        endif
        let nCount=nCount+1
        if bExclusive | break | endif
      endif

      let iLen = strlen(sMatch)
      let sLine = strpart(sLine,iLen,strlen(sLine)-iLen)
   endwhile

   if a:ShowAllLocks || is_dir
     if strlen(sUsers) > 0
        let sStatus=(is_dir?"Locks:\n": 'Locked by ').sUsers
     endif
   else
     let sStatus=(bMyLock?"Locked":("Locked by ".sUsers))
     let sStatus=sStatus.((nCount>1)?'..':'')
   endif

   if nCount == 0
     echom "VSS plugin: Unrecognised output:" sFull
   endif

   return sStatus.((a:include_brief)?("\n".sBrief): '')
endfun
" Get the short-form-name of a filename (the whole directory path)
fun! s:SSShortName( filename )
"  if v:version >= 602
"    return fnamemodify( a:filename, ':8')
"  endif
  let fn=a:filename
  if !isdirectory(fn) && ! filereadable(fn)
    return fn
  endif

  let base=fnamemodify(fn,':h')
  let file=fnamemodify(fn,':t')
  if base==fn
    return base
  endif
  let base=s:SSShortName( base )
  if base !~ '[/\\]$'
    let base=base.'\'
  endif
  if isdirectory(base)
    if file=~' ' || file =~ '\..*\.' || file =~ '\.[^.]\{4,}$' || file=~'^[^.]\{9}'

      let short=substitute(file,' ', '', 'g')
      let ext=matchstr(short,'\.[^\.]\{3}\([^\.]*$\)\@=')
      let short=substitute(short,'\..*$','','')

      let orig=expand(base.file,':p')
      if orig != ''
        let c=1
        while c<20
          let file=strpart(short,0,(7-strlen(c))).'~'.c.ext
          if expand( base.file,':p' ) == orig
            break
          endif
          let c=c+1
        endwhile
      endif
    endif
  endif

  return base.file
endfun

" Expand the filename
fun! s:ExpandFile( filename)
    let fname = expand(a:filename)
    if fname==''
        return a:filename
    endif
    return fname
endfun

fun! s:SSRun( prog)
  if !s:CheckSS() | return | endif
  exe '!start "'.fnamemodify(g:ssExecutable,':h').'\'.a:prog
endfun

fun! s:Success( msg )
  if a:msg =~ '^\s*$'
    echo 'srcsafe: Success'
  else
    echo 'srcsafe: '.substitute(a:msg,"[\n\r ]*$",'','')
  endif
endfun

" vim: ts=2 et sw=2
