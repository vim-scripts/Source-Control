" SourceControl module for Star Team
" --------------------------- st : Star Team
" :help srcctl-st
" The .project format is:
"   st:username:password@host:port/Project/View
"     Specify username and password (no prompting required)
"
"   st:username:@password_file_path@host:port/Project/View
"     Specify username and a password file (no prompting required)
"
"   st:[username]@host:port/Project/View
"     Optionally specify a username (prompt for username once) and no password
"     specified (prompt for password once per username)
"     The username can also be specified in g:st_username.
"     The password can be specified per username : g:st_password_{username} 
"       or globaly:  g:st_password
"
" In order for the project status to work properly, you should define
"  g:st_user_{username} to be your real name.

" TODO:
"   * Add Support for History with hilighting
"   * Add support for 'checked out files' browsing

" Add ourselves to the loaded modules
call SrcCtl_RegisterModule( 'st', 1.5, 'Star Team' )

" History:
"     1.1:
"       - Fix up comments
"       - Various fixes
"     1.2:
"       - Fix 'Out of Date' status.
"       - Fix updating of status for file.
"     1.3:
"       - Fix history window syntax hilighting
"       - Give message on missing files for history
"     1.4:
"       - Make it run
"     1.5:
"       - Auto-Update status when checking out an 'Unknown' file.
"       - Preserve file status


fun! s:Error(msg)
  echoerr g:srcctl_name_st.': '.a:msg
endfun

" Responds to query attributes, asking questions as appropriate
fun! SourceControlAttrib_st( attrib )
  if a:attrib =~ ':?lock:'
    let res=confirm('Lock:', "&Unlock\n&Exclusive\n&Non-Exclusive\nCancel", 3)
    if res ==0 || res==4 | return ':cancel:' | endif
    if res ==1
      return substitute(a:attrib, ':?lock:', ':unlock:', '')
    elseif res==2
      return substitute(a:attrib, ':?lock:', ':lock:exclusive:', '')
    else
      return substitute(a:attrib, ':?lock:', ':lock:', '')
    endif
  endif
  return a:attrib
endfun

" Main loop for handling commands
fun! SourceControlDo_st(cmd, opts, count, file1, repositoryfile1, comment)
  let comment=''
  if a:comment != ''
    if a:comment =~ '^@'
      let comment = ' -rf "'.strpart(a:comment,1).'"'
    else
      let comment = ' -r "'.a:comment.'"'
    endif
  endif

  let lock=((a:opts =~ ':lock:')?((a:opts =~ ':exclusive:')?' -l':' -nel'):(a:opts =~ ':unlock:')?' -u':'')
  if a:opts =~ ':unlock:'
    let lock=lock.' -ro'
  elseif a:opts =~ ':lock:'
    let lock=lock.' -rw'
  endif
  if a:cmd ==? 'Diff'
    if a:opts =~ ':text:'
      call s:st_cmd('diff', a:file1, a:repositoryfile1, a:count,'d')
    endif
  elseif a:cmd==? 'Checkout'
    let opts='o'.((a:opts =~ ':forcedir:')?'af':'')
    let extra=((a:opts =~ ':merge:')?' -merge':'').((a:opts =~ ':recurse:')? ' -is':'').((a:opts =~ ':overwrite:')?' -o':'')

    " Maintain current status
    if a:opts =~ ':overwrite:' && a:opts !~ ':lock:' && a:opts !~ ':unlock:'
"      let lock=lock.filereadable(a:file1)?(filewritable(a:file1)?' -rw':' -u -ro'):' -u -ro'
"      if !filereadable(a:file1) || !filewritable(a:file1)
        let lock=lock.' -u -ro'
"      endif
    endif

    "echo 'options '.a:opts
    let ret=s:st_cmd('co'.lock.extra.comment, a:file1, a:repositoryfile1, a:count,  opts)
    if ret=~': skipped\>' && (a:opts =~ ':lock:' || a:opts =~ ':exclusive:')
      if ret =~ 'file status is Unknown\>'
        let ret=s:st_Exec(a:repositoryfile1, 'update-status -q', 0)
      endif
      call s:st_cmd('lck'.lock.comment, a:file1, a:repositoryfile1, a:count,  'o')
    endif

  elseif a:cmd==? 'Lock'
    call s:st_cmd('lck'.lock.comment, a:file1, a:repositoryfile1, a:count,  'o')

  elseif a:cmd==? 'Unlock'
    call s:st_cmd('lck -ro -u', a:file1, a:repositoryfile1, a:count,  'o')

  elseif a:cmd==?'Checkin'
    let cmdopts = 'ci'
    let extra=((a:opts =~ ':merge:')?'-merge ':'').((a:opts =~ ':complete:')?'-mark':'').((a:opts =~ ':pinactive:')?'-active':'')
    " .((a:opts =~ ':recurse:')? '-is':'')
    call s:st_cmd( cmdopts.extra.lock.comment, a:file1, a:repositoryfile1, 0, 'o')
  elseif a:cmd==?'Status'
    if a:opts =~ ':return:'
      return s:st_GetStatus( a:file1, a:repositoryfile1, 1, a:opts =~ ':force:' ) ", a:opts =~ ':extras:', a:opts =~ ':locks:')
    elseif a:opts =~ ':text:'
      call s:st_cmd( 'list', a:file1, a:repositoryfile1, 0, '')
    else
      call confirm( s:RepFilePrintable(a:repositoryfile1).': '.s:st_GetStatus(a:file1, a:repositoryfile1, 0, 1)) ", 1, 1))
    endif
  elseif a:cmd==?'History'
    if a:opts =~ ':text:'
      call s:st_cmd( 'hist', a:file1, a:repositoryfile1, 0, '')
    else
      call s:DoHistoryWithSyntax( a:file1, a:repositoryfile1)
    endif
    return 0
  elseif a:cmd==? 'Add'
    call s:Error('Add is Unimplemented!')
  "  call s:SSCmdAdd( a:file1, a:repositoryfile1)
  elseif a:cmd ==? 'Deploy'
    call s:Error('Deploy not supported!')
  elseif a:cmd ==? 'Dir'
    call s:st_cmd('list'.((a:opts=~':recurse:')?' -is': ''), a:file1, a:repositoryfile1, 0, '')
  else
      call confirm(g:srcctl_name_st.': Unknown function "'.a:cmd.'"')
  endif
endfun

fun! s:RepFilePrintable(repfile)
  return substitute( a:repfile, '\v^%(\k+%(:%(\@=[^@]+))=)=\@([^@:]+):\d+/(.*)$', '\1:\2', '' )
endfun

" Starteam Exec function - work out password/repository and filename commands.
fun! s:st_Exec( repositoryfile, cmd, passive )
  let lhs=fnamemodify( a:repositoryfile, ':h')
  let rhs=fnamemodify( a:repositoryfile, ':t')
  let setuser=0
  let setpass=0

  " Work out username and password to use
  if lhs =~ '^\k\+:\k\+@[^@:]\+:' " Username:password@host:port
    let args = '-p "'.lhs.'"'
    let username=matchstr(lhs,'^\k\+')
  elseif lhs =~ '^\k\+:@[^@]+@[^@:]\+:' " Username:@password/file@host:port
    let args = '-p "'.substitute(lhs,':@[^@]\+',':','').'" -pwdfile "'.matchstr(lhs, ':\zs@[^@]\+').'" '
    let username=matchstr(lhs,'^\k\+')
  elseif lhs =~ '^\k*@[^@:]\+:'  "[username]@host:port
    let username=matchstr(lhs,'^\k\+')
    if username==''
      if exists('s:st_username')
        let username=s:st_username
      elseif exists('g:st_username')
        let username=g:st_username
      else
        " Already disabled
        if exists('s:st_username_disable') | return | endif

        if a:passive | return | endif
        let username=input('Star Team User Name:')
        if username==''
          echo 'Cancelled'
          return
        endif
        let setuser = 1
        " let s:st_username=username
      endif
    endif
    if exists('s:st_password_'.username.'_disable') | return | endif

    if exists('s:st_password_'.username)
      let pass=s:st_password_{username}
    elseif exists('g:st_password_'.username)
      let pass=g:st_password_{username}
    elseif exists('g:st_password')
      let pass=g:st_password
    else
      if a:passive | return '?' | endif
      let pass=inputsecret('Star Team password for '.username.':')
      let setpass=1
      "let s:st_password_{username}=pass
    endif
    if pass=~'^@' && filereadable( strpart(pass,1) )
      let args= '-p "'.username.substitute(lhs,'^\k*@','@','').'" -pwdfile "'.strpart(pass,1).'"'
    else
      let args= '-p "'.username.':'.pass.substitute(lhs,'^\k*@','@','').'"'
    endif
  else
    call s:Error("Project format not recognized: '".lhs."'")
    return ''
  endif
  let b:st_username=username " For status scripts
  " echo 'stcmd '.a:cmd.' -x -nologo '.args.' '.rhs

  " Execute the command.
  let exec=s:st_GetExe()
  if exec != ''
    if has('win16') || has('win32') 
      " Need to put quotes right around the command 'cause they get stripped!
      let ret=system('""'.exec.'" '.a:cmd.' -x -nologo '.args.' '.rhs.'"')
    else
      let ret=system('"'.exec.'" '.a:cmd.' -x -nologo '.args.' '.rhs)
    endif
    " echo ret
    if ret =~ '\<Logon failed.'
      if confirm(ret, "&Ok\n&Disable",1) ==2
        if setuser 
          let s:st_username_disable=1
        else
          let s:st_password_{username}_disable=1
        endif
      endif
    else
      if setuser | let s:st_username=username | endif
      if setpass | let s:st_password_{username}=pass | endif
    endif

    return ret
  endif
endfun

" Execute a star-team command.
fun! s:st_cmd(cmd, file1, repositoryfile1, count, opts)
  let ver=''
  if a:count != 0
    let ver=' -vn '.a:count
  endif
  let force=''
  if a:opts =~ 'f'
    let force=' -fs -fp "'.fnamemodify(a:file1,':h').'"'
  endif

  let repfile=a:repositoryfile1.((isdirectory(a:file1) && match(a:repositoryfile1,'[^\\/]$'))?'\':'')
  let autoread=&autoread
  setlocal autoread
  let ret= s:st_Exec(repfile, a:cmd.ver.force, 0)
  checktime
  if !autoread | setlocal noautoread | endif
  let ret=substitute(ret,"^[^\n]*\n",'','')
  if a:opts !~ ':return:'
    echo 'st: '.ret
  endif
  return ret
endfun

" Get and interpret a status from a listing.
fun! s:st_GetStatus( file1, repositoryfile1, include_brief, force)
  if a:force 
    call s:st_Exec(a:repositoryfile1, 'update-status -q', 0)
  elseif exists('b:zz_st_not_in_view')
    return "Not in View".((a:include_brief)?"\n": "")
  endif
  let res=s:st_Exec(a:repositoryfile1, 'list', 1)
  if res == '?' | return ((a:include_brief)?"?\n?": "?") | endif

  let res=substitute(res,"^[^\n]*\n",'','') " remove folder name

  let restat='^\%(Out of Date\|\k\+\)\>'

  let stat=matchstr(res,restat)
  let locked=matchstr(res,restat.'\s\+\zs.\{-}\ze\s\+[-r][-w] ')
  let attr=matchstr(res,restat.'\s\+.\{-}\s\+\zs[-r][-w]\ze ')

  if stat == 'Not' || stat == 'subfolder'
    let b:zz_st_not_in_view=1
    return "Not in View".((a:include_brief)?"\n": "")
  endif

  let bExcLock=0
  let bIsMe=0
  let lockdesc=''
  let sBrief=''
  if stat == 'Current' && attr[1]=='-' && locked==''
    let sBrief='@'
  elseif locked != ''
    let lckre='\<'.substitute(locked, '\.\.\.', '.*','').'\>'
    if exists('b:st_username') && exists( 'g:st_user_'.b:st_username) && g:st_user_{b:st_username} =~ lckre
      let sBrief='X'
      let lockdesc='Exclusive Locked'
    else
      let sBrief='l'
      let lockdesc='Exclusive Locked:'.locked
      if attr[1]=='w'
        let sBrief='Ll'
        let lockdesc='Locked '.lockdesc
      endif
    endif
  elseif attr[1]=='w'
    let sBrief='L'
    let lockdesc='Locked'
  endif
  if stat != 'Current'
    let sBrief=strpart(stat,0,3).((sBrief=='')?'':(':'.sBrief))
  endif
  if lockdesc!='' | let lockdesc=lockdesc.' ' | endif

  return stat.' '.lockdesc.((a:include_brief)?("\n".sBrief): '')
endfun

" Find the stcmd executable
fun! s:st_GetExe()
  if exists('g:stExecuteable') | return g:stExecutable | endif

  if has('unix')
    let stcmd='stcmd'
  else
    if $STARTEAMAPP == '' 
      let $STARTEAMAPP='C:\Program Files\Starbase\StarTeam 5.2' 
      if isdirectory($STARTEAMAPP)
        let $STARTEAMAPP='C:\Program Files\Starbase\StarTeam 5.3' 
      endif
    endif
    let stcmd=$STARTEAMAPP.'\stcmd.exe'
  endif
  if executable(stcmd)
    let g:stExecutable=stcmd
    return g:stExecutable
  endif
  echoe 'Set g:stExecutable to point to stcmd, or $STARTEAMAPP Variable to point to the directory where it exists.'
  return ''
endfun

fun! s:DoHistoryWithSyntax(filename, repositoryfile)
  let res=s:st_cmd( 'hist', a:filename, a:repositoryfile, 0, ':return:')
  if res =~ ': No files matching ".*" were found'  
    echo substitute(substitute(res, '^\_s*','',''), '\_s*$','','')
    return 
  elseif res =~ '^\_s*$'
    return
  endif
  let bname='History!'.fnamemodify(a:filename,':t:gs/[ \.]/_/')
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

  exe "norm \"=res\<cr>p"

"  let prjfile=s:SSGetRepFile(a:repositoryfile)
"  if prjfile.'' == ''
"     echoerr 'Invalid project'
"     return
"  endif
"  exec '.r !""'.g:ssExecutable.'" History "'.prjfile.'""'
  set nomodified
  setlocal nomodifiable
  call s:STHistorySyntax()
  1
endfun

" Turns on Sourcesafe history syntax
function! s:STHistorySyntax()
"fun! ST__HistorySyntax()
  syn clear
  hi default link STHeaderLabel Title
  hi default link STHistoryField Title
  hi default link STHistoryHeader Special
  hi default link STDateField Title
  hi default link STHistoryComment Comment
  hi default link STHeaderSep NonText

  syntax region STFileHeader start="\%^" end="^---*$" contains=STHeaderLabel,STHeaderSep keepend
  syntax match STHeaderSep "^---*" contained
  syntax match STHeaderLabel '^\<[^:]\+:' contained
  syntax region STHistoryHeader start="^---" skip="^history" end="^" contains=STHistoryFile
  syntax region STHistoryFile matchgroup=STHistoryFileLeader start="^history " matchgroup=NULL end="$"  contained
  syntax match STHistoryVersion /\<[0-9]*\>/ contained
  syntax match STHistoryField /\<\(\(Branch \)\=Revision\|View\|Author\)\>:/
  syntax match STDateField /\<Date:/ nextgroup=STDate
  syntax match STDate contained /.*$/ nextgroup=STHistoryComment skipnl
  syntax region STHistoryComment contained start='^' end='^---*$' contains=STHeaderSep keepend
 
  syntax region STHistoryVersionLine start="^\*\+" end="$" contains=STHistoryVersion
endfunction

" vim: ts=2 et sw=2
