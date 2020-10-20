*---------------------------------------------------------*
function NewPay
private old_select,var_retr,var_fld,fil_string,type_var,new_var,i,j,fld
if is_arhiv
 return .F.
endif
save screen
if Yes_Or_No(' С О З Д А Т Ь ','Новый лицевой счет?',1)
  MsgWrite("Создаю новый ЛС...")
  var_retr=.T.
  append blank
  if SIZE_FILTER!=0.and.(.not.empty(I->N_FILTER))  && Знач. фильтра в ЛС
    fil_string=alltrim(I->L_FILTER)
    fil_string=strtran(fil_string,".and.","     ")
    for i=1 to SIZE_FILTER
      new_var=psubstr(fil_string,i)
      new_var=strtran(new_var,filter_fld[i])
      new_var=strtran(new_var,"=")
      var_fld=filter_fld[i]
      type_var=&var_fld
      do case
        case type("type_var")="C"
          replace &var_fld with strtran(new_var,'"')
        case type("type_var")="N"
          replace &var_fld with val(new_var)
        case type("type_var")="D"
          new_var=strtran(new_var,"ctod()")
          new_var=strtran(new_var,'"')
          replace &var_fld with ctod(new_var)
        case type("type_var")="L"
          if new_var="()"     && "(.not.)"
            replace &var_fld with .T.
          else
            replace &var_fld with .F.
          endif
      endcase
    next
  endif
  j=0
  for i=1 to I->MAX_COME
    fld=ltrim(str(i))
    if I->COD&fld != 0
      var=ltrim(str(I->COD&fld))
      if type("M->cod&var") != "U"
        M->cod&var.=I->COD&fld
        j=j+1
      endif
    else
      exit
    endif
  next
  if j != 0
    old_select=select()
    select 8
    use come.pay
    j=1
    do while .not.eof()
      var=ltrim(str(H->COME_COD))
      if M->cod&var != 0
        fld=ltrim(str(j))
        repl A->DAY&fld with H->COME_DAY,A->CLO&fld with H->COME_CLO,;
             A->BAZ&fld with H->COME_BAZ,A->SUM&fld with H->COME_SUM,;
             A->COD&fld with H->COME_COD
        M->cod&var.=nul_cod
        j=j+1
      endif
      skip
    enddo
    use
    select(old_select)
  endif
  RETR = 0
else
  var_retr=.F.
endif
restore screen
return var_retr
*---------------------------------------------------------*
function InpAcco
private old_color,old_var,InpPage,var_fld,var_prm,otwet
old_color = setcolor()
save screen to InpPage
set color to &c
DrawBox(Y_min,X_min,Y_max,X_max,ibox)
set color to &cl
MSG=' Л И Ц Е В О Й  С Ч Е Т '
@ Y_min,int((X_min+X_max)/2-len(MSG)/2) say MSG
MenuAcco()
set color to &c
otwet=1
do while otwet != 0
  if SIZE_ACCO != 0
    declare come_edit[SIZE_ACCO]
    for i=1 to SIZE_ACCO
      var_fld=acco_fld[i]
      do case
      case .not.empty(acco_dbf[i])
        var_file=acco_dbf[i]
        if file(var_file)
          var_prm=mDict(acco_scr[i],&var_fld,acco_cod[i],var_file,acco_ntx[i])
        else
          var_prm=space(21)
        endif
      othe
        var_prm=&var_fld
      endcase
      come_edit[i]=rtrim(acco_nam[i])+" "+varstr(var_prm)
    next
  else
    declare come_edit[1]
    come_edit[1]=" "
  endif
  otwet=achoice(Y_min+1,X_min+1,Y_max-1,X_max-1,come_edit,"","AccoFunc",otwet)
  release come_edit  && очистить
enddo
Nz=recno()
skip
commit
go Nz
restore screen from InpPage
set color to &old_color
return " "
*---------------------------------------------------------*
function MenuAcco
set color to &cl
@ Y_max,X_min+24 say ' М Е Н Ю: '
set color to &cmenu
MSG='Enter'
@ row(),col() get MSG
@ row(),col() say 'Ввод'
MSG='ESC'
@ row(),col() get MSG
@ row(),col() say 'Выход'
clear gets
return " "
*---------------------------------------------------------*
function AccoFunc
parameters mode,ptr
private old_color,var,fld,old_select
key_last=LASTKEY()
do case
  case mode=0
    return 2
  case lastkey()=27
    return 0
  case lastkey()=13.or.isdata(key_last)
    if SIZE_ACCO != 0.and.(.not.is_arhiv)
      if key_last <> 13
        keyboard chr(key_last)
      endif
      var_fld=acco_fld[ptr]
      old_var=&var_fld
      do case
      case .not.empty(acco_dbf[ptr])
        var_file=acco_dbf[ptr]
        if file(var_file)
          repl &var_fld with DictFind(&var_fld,acco_cod[ptr],acco_scr[ptr],acco_nam[ptr],var_file,acco_ntx[ptr])
        endif
      othe
        set cursor on
        @ row(),X_min+1 say rtrim(acco_nam[ptr])
        @ row(), col()+1 get &var_fld
        read
        set cursor off
      endcase
      if old_var != &var_fld
        return 1
      endif
      set cursor off
    endif
endcase
return 2
*---------------------------------------------------------*
function InpCome
private old_color,InpPage,otwet,var,fld,nucount,log_clr
old_color = setcolor()
save screen to InpPage
set color to &c
DrawBox(Y_min,X_min,Y_max,X_max,ibox)
set color to &cl
MSG=' Н А Ч И С Л Е Н И Я / У Д Е Р Ж А Н И Я '
@ Y_min,int((X_min+X_max)/2-len(MSG)/2) say MSG
MenuCome()
set color to &c
if (.not.empty(I->L_FILTER)).and.(SIZE_FILTER!=0)
  MSG=substr(alltrim(I->N_FILTER),1,X_max-X_min-2)
  @ Y_min+1,int((X_min+X_max)/2-len(MSG)/2) say MSG
endif
if (SIZE_JOUR > 1)
  MSG=""
  for i=2 to SIZE_JOUR
    var=jour_fld[i]
    MSG=MSG+jour_nam[i]+" "+ltrim(varstr(&var))+"   "
  next
  MSG=substr(alltrim(MSG),1,X_max-X_min-2)
  @ Y_min+2,int((X_min+X_max)/2-len(MSG)/2) say MSG
endif
@ Y_min+3,X_min+1 to Y_min+3,X_max-1
@ Y_min+4,X_min+1 say "  Дни      Часы     База         Сумма          Код   Название"
@ Y_min+5,X_min+1 to Y_min+5,X_max-1
MsgWrite("Секунду... Загружаю...")
nucount=0  && Счетчик Н/У в pay.dbf
for i=0 to 11
  fld=ltrim(str(i))
  ms&fld.=A->MONSUM&fld
  md&fld.=A->MONDAT&fld
next
i=1
do while i <= I->MAX_COME
  fld=ltrim(str(i))
  if A->COD&fld != 0
    var=ltrim(str(A->COD&fld))
    if type("M->cod&var") != "U"
      B&var.=DAY&fld
      C&var.=CLO&fld
      D&var.=BAZ&fld
      F&var.=SUM&fld
      M->cod&var.=A->COD&fld
      nucount=nucount+1
    else
      if is_arhiv
       B&var.=DAY&fld
       C&var.=CLO&fld
       D&var.=BAZ&fld
       F&var.=SUM&fld
       M->codn&var.=A->COD&fld
       nucount=nucount+1
      else
       DelNU(i)  && Удал. неизв. Н/У
       loop
      endif
    endif
  else
    exit
  endif
  i=i+1
enddo
log_clr=.T.
otwet=1
do while otwet != 0
  if nucount != 0
    declare come_edit[nucount]
    for i=1 to nucount
      fld=ltrim(str(i))
      var=ltrim(str(A->COD&fld))
      if (.not.is_arhiv).and.(.not.(empty(frm&var)))
        F&var.=formula(frm&var)
        repl I->FORMAT with F&var
        F&var.=I->FORMAT
      endif
      come_edit[i]="  "+str(B&var)+"   "+str(C&var)+"   "+;
        str(D&var)+"   "+str(F&var)+"   "+;
        iif(type("M->cod&var")!="U",;
            str(M->cod&var)+"   "+nam&var,;
            str(M->codn&var)+"   "+space(20))
    next
  else
    declare come_edit[1]
    come_edit[1]=" "
  endif
  if log_clr
    log_clr=.F.
    @ Y_min+6,X_min+1 clear to Y_max-1,X_max-1
  endif
  otwet=achoice(Y_min+6,X_min+1,Y_max-1,X_max-1,come_edit,"","ComeFunc",otwet)
  release come_edit  && очистить
enddo
if nucount!=0
  for i=1 to nucount
    fld=ltrim(str(i))
    var=ltrim(str(A->COD&fld))
    if .not.is_arhiv
     repl DAY&fld with B&var,CLO&fld with C&var,;
          BAZ&fld with D&var,SUM&fld with F&var,A->COD&fld with M->cod&var
    endif
    B&var.=nul_day
    C&var.=nul_clo
    D&var.=nul_baz
    F&var.=nul_sum
    M->cod&var.=nul_cod
  next
endif
commit
restore screen from InpPage
set color to &old_color
return " "
*---------------------------------------------------------*
function MenuCome
set color to &cl
@ Y_max,X_min+10 say ' М Е Н Ю: '
set color to &cmenu
MSG='Enter'
@ row(),col() get MSG
@ row(),col() say 'Ввод'
MSG='INS'
@ row(),col() get MSG
@ row(),col() say 'Вставить'
MSG='DEL'
@ row(),col() get MSG
@ row(),col() say 'Удалить'
MSG='F4'
@ row(),col() get MSG
@ row(),col() say 'Суммы'
MSG='ESC'
@ row(),col() get MSG
@ row(),col() say 'Выход'
clear gets
return " "
*---------------------------------------------------------*
function ComeFunc
parameters mode,ptr
private old_color,var,fld,old_select
do case
  case mode=0
    return 2
  case lastkey()=27
    return 0
  case lastkey()=13
    if nucount != 0.and.(.not.is_arhiv)
      fld=ltrim(str(ptr))
      var=ltrim(str(A->COD&fld))
      set cursor on
      @ row(),col()+2 get B&var
      @ row(),col()+3 get C&var
      @ row(),col()+3 get D&var
      @ row(),col()+3 get F&var
      @ row(),col()+3 say M->cod&var
      @ row(),col()+3 say nam&var
      read
      set cursor off
      if updated()
        return 1
      endif
    endif
  case lastkey()=22   && INS
    if nucount < I->MAX_COME.and.(.not.is_arhiv)
      old_select=select()
      select 8
      use come.pay
      save screen
      old_color=setcolor(cdict)
      BoxTenj(Y_min+3,X_srm-10,Y_max-3,X_srm+11,ibox)
      declare name_var[1],name_fld[1]
      name_var[1]=" Список Н/У"
      name_fld[1]="COME_NAM"
      dbedit(Y_min+4,X_srm-9,Y_max-4,X_srm+10,name_fld,"dbFind","",name_var)
      setcolor(old_color)
      restore screen
      if lastkey()=13.and.H->COME_COD != 0
        var=ltrim(str(H->COME_COD))
        if M->cod&var. = 0
          log_clr=.T.
          MsgWrite("Ждите... Вставляю Н/У.")
          B&var.=H->COME_DAY
          C&var.=H->COME_CLO
          D&var.=H->COME_BAZ
          F&var.=H->COME_SUM
          M->cod&var.=H->COME_COD
          InsNU()
          nucount=nucount+1
          var=1
        else
          var=2
        endif
      else
        var=2
      endif
      use
      select(old_select)
      return var
    endif
  case lastkey()=7    && DEL
    if nucount != 0.and.(.not.is_arhiv)
      if Fragen('','Удалить строку Н/У?')
        log_clr=.T.
        MsgWrite("Ждите... Удаляю Н/У.")
        fld=ltrim(str(ptr))
        var=ltrim(str(A->COD&fld))
        B&var.=nul_day
        C&var.=nul_clo
        D&var.=nul_baz
        F&var.=nul_sum
        M->cod&var.=nul_cod  && 0 - Н/У нет
        DelNU(ptr)
        nucount=nucount-1
        return 1
      endif
    endif
  case lastkey()=-3
    old_color=setcolor(cdict)
    save screen
    BoxTenj(Y_srm-6,X_srm-22,Y_srm+7,X_srm+22,ibox)
    setcolor(cl)
    MSG="Суммы за 12 месяцев:"
    @ Y_srm-6,X_srm-len(MSG)/2+1 say MSG
    setcolor(cdict)
    for i=0 to 11
     fld=ltrim(str(i))
     if .not.is_arhiv
      @ Y_srm-5+i,X_srm-19 say 'ms'+fld+iif(i<10,' ','')+':'
      @ row(),col()+1 get MONSUM&fld
      @ row(),col()+1 say ' │  '+'md'+fld+iif(i<10,' ','')+':'
      @ row(),col()+1 get MONDAT&fld
     else
      @ Y_srm-5+i,X_srm-19 say 'ms'+fld+iif(i<10,' ','')+':'
      @ row(),col()+1 say MONSUM&fld
      @ row(),col()+1 say ' │  '+'md'+fld+iif(i<10,' ','')+':'
      @ row(),col()+1 say MONDAT&fld
     endif
    next
    if .not.is_arhiv
     set cursor on
     read
     set cursor off
    else
     inkey(0)
    endif
    restore screen
    setcolor(old_color)
    if updated()
     for i=0 to 11
      fld=ltrim(str(i))
      ms&fld.=A->MONSUM&fld
      md&fld.=A->MONDAT&fld
     next
     return 1
    endif
endcase
return 2
*---------------------------------------------------------*
procedure InsNU
private ii,var,fld
ii=1
go top
do while .not.eof()
  var=ltrim(str(H->COME_COD))
  if M->cod&var. != 0
    fld=ltrim(str(ii))
    repl A->COD&fld with H->COME_COD
    ii=ii+1
  endif
  skip
enddo
return
*---------------------------------------------------------*
procedure DelNU
parameters i
private ii,var,fld
for ii=i to I->MAX_COME
  var=ltrim(str(ii))
  if (ii+1) <= I->MAX_COME
    fld=ltrim(str(ii+1))
    repl DAY&var with DAY&fld,CLO&var with CLO&fld,;
         BAZ&var with BAZ&fld,SUM&var with SUM&fld,A->COD&var with A->COD&fld
  else
    repl A->COD&var with nul_cod
  endif
next
return
*---------------------------------------------------------*
procedure InpMonth
private old_color,ch,otwet,ptr,old_date,old_select,old_filter,j,i,new_ptr
if is_arhiv
 return
endif
old_color=setcolor(cw)
save screen to InpPage
DrawBox(Y_min,X_max-24,Y_min+2,X_max,ibox)
setcolor(cl)
MSG=' ТЕКУЩАЯ ДАТА '
@ Y_min,int((X_max-24+X_max)/2-len(MSG)/2) say MSG
??chr(7)
old_date=I->T_DATE
ptr=month(I->T_DATE)
ch=str(year(I->T_DATE),4)
otwet=1
do while otwet!=0
  setcolor(cw)
  @ Y_min+1,X_max-19 prompt Month[ptr]
  @ row(),col()+1 prompt ch
  @ row(),col() say " г."
  menu to otwet
  if otwet != 0
    do case
    case otwet=1
      save screen
      setcolor(cdict)
      BoxTenj(Y_min+1,X_max-20,Y_min+14,X_max-11,ibox)
      new_ptr=achoice(Y_min+2,X_max-19,Y_min+13,X_max-12,month,"","",ptr)
      if new_ptr != 0
        ptr=new_ptr
      endif
      rest screen
    case otwet=2
      set cursor on
      @ Y_min+1,X_max-19+len(Month[month(I->T_DATE)])+1 get ch picture '####'
      read
      set cursor off
      ch=iif(val(ch)<1980.or.val(ch)>2080,str(year(I->T_DATE),4),ch)
    endcase
  endif
enddo
if old_date <> ctod("01."+str(ptr,2)+"."+ch)
 setcolor(cw)
 BoxTenj(Y_srm-3,X_srm-30,Y_srm+3,X_srm+30,ibox)
 set cursor on
 @ Y_srm-1,X_srm-27 say "Введите для нового месяца:"
 @ Y_srm+1,X_srm-27 say "минимум -" get mo
 @ row(),col() say " ;  дни -" get wd
 @ row(),col() say " ;  часы -" get wc
 read
 set cursor off
 if lastkey() != 27
  CloseDBF()  && закрываю старый месяц
  select 9
  use &cur_config
  repl I->T_DATE with ctod("01."+str(ptr,2)+"."+ch)
  old_path=bal_path
  bal_path=cur_path+"\"+str(year(I->T_DATE),4)+"."+strtran(str(month(I->T_DATE),2)," ","0")
  use_path=bal_path+"\"
  set path to &bal_path
  mkdir(bal_path)
  deles("*.*",bal_path)
  copys("*.*",old_path,bal_path)
  new_config=bal_path+"\config.pay"
  use &new_config
  repl I->T_DATE with ctod("01."+str(ptr,2)+"."+ch)
  nm=month(I->T_DATE)
  repl I->N_MO with mo
  repl I->N_DAY with wd
  repl I->N_CLOCK with wc
  OpenDBF()   && откpываю новый месяц
  setcolor(cw)
  BoxTenj(Y_srm-2,X_srm-30,Y_srm+2,X_srm+30,ibox)
  setcolor(cl)
  MSG=' ПЕРЕВОД HА HОВЫЙ МЕСЯЦ '
  @ Y_srm-2,X_srm-len(MSG)/2 say MSG
  setcolor(cw)
  old_select=select()
  Nz=recno()
  old_filter=dbfilter()
  set filter to
  go top
  @ Y_srm-1,X_srm-28 say "Количество лицевых счетов: "+ltrim(str(lastrec()))
  select 8
  use come.pay                        && Начисления/Удержания
  do while .not.eof()
   var=ltrim(str(H->COME_COD))  && Взять Н/У
   private per&var
   per&var.=upper(strtran(H->COME_PER," "))
   if COME_DAY != 0
     repl COME_DAY with wd
   endif
   if COME_CLO != 0
     repl COME_CLO with wc
   endif
   skip
  enddo
  use
  select(old_select)
  ncnt=0
  do while .not.eof()
    @ Y_srm-0,X_srm-28 say "Обpаботано лицевых счетов: "+ltrim(str(ncnt))
    ncnt++
    @ Y_srm+1,X_srm-28 say "Обpабатываю ЛС: "+A->PAY2+"N "+ltrim(str(A->PAY1))
    for j=1 to I->MAX_COME
      fld=ltrim(str(j))
      if DAY&fld.!=0
        repl DAY&fld with I->N_DAY
      endif
      if CLO&fld.!=0
        repl CLO&fld with I->N_CLOCK
      endif
    next
    nucount=0  && Счетчик Н/У в pay.dbf
    i=1
    do while i <= I->MAX_COME
      fld=ltrim(str(i))
      if A->COD&fld != 0
        var=ltrim(str(A->COD&fld))
        if type("M->cod&var") != "U"
          B&var.=DAY&fld
          C&var.=CLO&fld
          D&var.=BAZ&fld
          F&var.=SUM&fld
          M->cod&var.=A->COD&fld
          nucount=nucount+1
        else
          DelNU(i)  && Удал. неизв. Н/У
          loop
        endif
      else
        exit
      endif
      i=i+1
    enddo
    for i=0 to 11
     fld=ltrim(str(i))
     ms&fld.=A->MONSUM&fld
     md&fld.=A->MONDAT&fld
    next
    for i=10 to 0 step -1               && сдвинуть
      ptr=ltrim(str(i+1))
      var=ltrim(str(i))
      repl MONSUM&ptr with MONSUM&var,MONDAT&ptr with MONDAT&var
    next
    if .not.(empty(summaf))
      repl MONSUM&var with formula(summaf)
    else
      repl MONSUM&var with 0
    endif
    if .not.(empty(summad))
      repl MONDAT&var with formula(summad)
    else
      repl MONDAT&var with 0
    endif
    for i=0 to 11
     fld=ltrim(str(i))
     ms&fld.=A->MONSUM&fld
     md&fld.=A->MONDAT&fld
    next
    if nucount != 0
      for i=1 to nucount
        fld=ltrim(str(i))
        var=ltrim(str(A->COD&fld))
        if .not.(empty(per&var))
          F&var.=formula(per&var)
          repl I->FORMAT with F&var
          F&var.=I->FORMAT
        endif
      next
      for i=1 to nucount
        fld=ltrim(str(i))
        var=ltrim(str(A->COD&fld))
        if .not.(empty(frm&var))
          F&var.=formula(frm&var)
          repl I->FORMAT with F&var
          F&var.=I->FORMAT
        endif
      next
      for i=1 to nucount
        fld=ltrim(str(i))
        var=ltrim(str(A->COD&fld))
        repl DAY&fld with B&var,CLO&fld with C&var,;
             BAZ&fld with D&var,SUM&fld with F&var,A->COD&fld with M->cod&var
        B&var.=nul_day
        C&var.=nul_clo
        D&var.=nul_baz
        F&var.=nul_sum
        M->cod&var.=nul_cod
      next
    endif
    skip
  enddo
  InitFilter()
  go Nz
 endif
endif
restore screen from InpPage
DrawDate(Y_min,X_max-24,Y_min+2,X_max)
DrawMenu(Y_min+2,X_max-24,Y_max,X_max)
setcolor(old_color)
redraw=.T.
RETR=0
return
*---------------------------------------------------------*
procedure Kalkulat
private Nz,i,var,fld
if is_arhiv
 return
endif
save screen to InpPage
if Yes_Or_No('','Произвести перерасчет?',1)
  MsgWrite("Считаю...")
  Nz=recno()
  if Mark != 0
    go top
  endif
  do while .not.eof()
    if delete().or.Mark = 0
      nucount=0
      for i=0 to 11
       fld=ltrim(str(i))
       ms&fld.=A->MONSUM&fld
       md&fld.=A->MONDAT&fld
      next
      i=1
      do while i <= I->MAX_COME
        fld=ltrim(str(i))
        if A->COD&fld != 0
          var=ltrim(str(A->COD&fld))
          if type("M->cod&var") != "U"
            B&var.=DAY&fld
            C&var.=CLO&fld
            D&var.=BAZ&fld
            F&var.=SUM&fld
            M->cod&var.=A->COD&fld
            nucount=nucount+1
          else
            DelNU(i)  && Удал. неизв. Н/У
            loop
          endif
        else
          exit
        endif
        i=i+1
      enddo
      if nucount != 0
        for i=1 to nucount
          fld=ltrim(str(i))
          var=ltrim(str(A->COD&fld))
          if .not.(empty(frm&var))
            F&var.=formula(frm&var)
            repl I->FORMAT with F&var
            F&var.=I->FORMAT
          endif
        next
        for i=1 to nucount
          fld=ltrim(str(i))
          var=ltrim(str(A->COD&fld))
          repl DAY&fld with B&var,CLO&fld with C&var,;
               BAZ&fld with D&var,SUM&fld with F&var,A->COD&fld with M->cod&var
          B&var.=nul_day
          C&var.=nul_clo
          D&var.=nul_baz
          F&var.=nul_sum
          M->cod&var.=nul_cod
        next
      endif
    endif
    if Mark != 0
      skip
    else
      exit
    endif
  enddo
  go Nz
endif
restore screen from InpPage
return
*---------------------------------------------------------*
procedure Chastnoe
private old_color,InpPage,otwet,var,fld,nucount,log_clr
var=ltrim(str(I->CHASTN_NU))
if type("M->cod&var") = "U".or.I->CHASTN_NU = 0
  WarnWrite("Hеизвестное H/У...")
  return
endif
old_color = setcolor()
save screen to InpPage
set color to &c
DrawBox(Y_min,X_min,Y_max,X_max,ibox)
set color to &cl
var=ltrim(str(I->CHASTN_NU))
MSG=str(I->CHASTN_NU)+" "+iif(type("cod&var")!="U",nam&var,nul_nam)
@ Y_min,int((X_min+X_max)/2-len(MSG)/2) say MSG
MenuChas()
set color to &c
private B0,C0,D0,F0,ptr_NU,nucount
declare decl_fld[1],decl_nam[1]
decl_fld[1]="draw_sum()"
decl_nam[1]="Таб N ФИО                                Дни    Часы   База       Сумма"
dbedit(Y_min+1,X_min+1,Y_max-1,X_max-1,decl_fld,"ChasFunc","",decl_nam)
commit
restore screen from InpPage
set color to &old_color
return
*---------------------------------------------------------*
procedure MenuChas
set color to &cl
@ Y_max,X_min+14 say ' М Е Н Ю: '
set color to &cmenu
MSG='Enter'
@ row(),col() get MSG
@ row(),col() say 'Ввод'
MSG='INS'
@ row(),col() get MSG
@ row(),col() say 'Вставить'
MSG='DEL'
@ row(),col() get MSG
@ row(),col() say 'Удалить'
MSG='ESC'
@ row(),col() get MSG
@ row(),col() say 'Выход'
clear gets
return
*---------------------------------------------------------*
function ChasFunc
parameters mode,fld_ptr
private cur_field,key_last
key_last=LASTKEY()
do case
case mode <= 3
  return 1
case LASTKEY() = 27
  return 0
case LASTKEY() = 13.or.isdata(key_last)
  if ptr_NU != 0.and.(.not.is_arhiv)
    set cursor on
    @ row(),col()+41 get B0
    @ row(),col()+ 1 get C0
    @ row(),col()+ 1 get D0
    @ row(),col()+ 1 get F0
    read
    set cursor off
    if updated()
      Inp_Sum()
    endif
  endif
case lastkey()=22   && INS
  if ptr_NU = 0 .and. nucount < I->MAX_COME.and.(.not.is_arhiv)
    if Fragen('','Вставить строку Н/У?')
      save screen
      MsgWrite("Ждите... Вставляю Н/У.")
      old_select=select()
      select 8
      use come.pay
      i=1
      fld=ltrim(str(i))
      s_day=A->DAY&fld
      s_clo=A->CLO&fld
      s_baz=A->BAZ&fld
      s_sum=A->SUM&fld
      s_cod=A->COD&fld
      d_day=nul_day
      d_clo=nul_clo
      d_baz=nul_baz
      d_sum=nul_sum
      d_cod=nul_cod
      do while .not.eof()
        if i <= I->MAX_COME
          fld=ltrim(str(i))
          if H->COME_COD = A->COD&fld..or. H->COME_COD = I->CHASTN_NU .or. s_cod = d_cod
            if H->COME_COD = I->CHASTN_NU
              B0=H->COME_DAY
              C0=H->COME_CLO
              D0=H->COME_BAZ
              F0=H->COME_SUM
              d_day=A->DAY&fld
              d_clo=A->CLO&fld
              d_baz=A->BAZ&fld
              d_sum=A->SUM&fld
              d_cod=A->COD&fld
              s_day=H->COME_DAY
              s_clo=H->COME_CLO
              s_baz=H->COME_BAZ
              s_sum=H->COME_SUM
              s_cod=H->COME_COD
              ptr_NU = i
            else
              if s_cod = d_cod
                d_day=A->DAY&fld
                d_clo=A->CLO&fld
                d_baz=A->BAZ&fld
                d_sum=A->SUM&fld
                d_cod=A->COD&fld
              else
                s_day=A->DAY&fld
                s_clo=A->CLO&fld
                s_baz=A->BAZ&fld
                s_sum=A->SUM&fld
                s_cod=A->COD&fld
                d_day=s_day
                d_clo=s_clo
                d_baz=s_baz
                d_sum=s_sum
                d_cod=s_cod
              endif
            endif
            repl A->DAY&fld with s_day
            repl A->CLO&fld with s_clo
            repl A->BAZ&fld with s_baz
            repl A->SUM&fld with s_sum
            repl A->COD&fld with s_cod
            if s_cod = d_cod
              d_day=nul_day
              d_clo=nul_clo
              d_baz=nul_baz
              d_sum=nul_sum
              d_cod=nul_cod
            else
              s_day=d_day
              s_clo=d_clo
              s_baz=d_baz
              s_sum=d_sum
              s_cod=d_cod
            endif
            i=i+1
          endif
        else
          exit
        endif
        skip
      enddo
      use
      select(old_select)
      rest screen
      Inp_Sum()
    endif
  endif
case lastkey()=7    && DEL
  if ptr_NU != 0 .and. nucount != 0.and.(.not.is_arhiv)
    if Fragen('','Удалить строку Н/У?')
      save screen
      MsgWrite("Ждите... Удаляю Н/У.")
      for i=ptr_NU to I->MAX_COME
        var=ltrim(str(i))
        if (i+1) <= I->MAX_COME
          fld=ltrim(str(i+1))
          repl DAY&var with DAY&fld,CLO&var with CLO&fld,;
               BAZ&var with BAZ&fld,SUM&var with SUM&fld,;
               A->COD&var with A->COD&fld
        else
          repl A->COD&var with nul_cod
        endif
      next
      ptr_NU = 0
      rest screen
      Inp_Sum()
    endif
  endif
endcase
return 1
*---------------------------------------------------------*
function Draw_Sum
private draw
B0=nul_day
C0=nul_clo
D0=nul_baz
F0=nul_sum
nucount=0
ptr_NU=0
i=1
do while i <= I->MAX_COME
  fld=ltrim(str(i))
  if A->COD&fld != 0
    var=ltrim(str(A->COD&fld))
    if type("M->cod&var") != "U"
      if A->COD&fld. = I->CHASTN_NU
        B0=DAY&fld
        C0=CLO&fld
        D0=BAZ&fld
        F0=SUM&fld
        ptr_NU=i
      endif
      nucount=nucount+1
    else
     if .not.is_arhiv
      DelNU(i)  && Удал. неизв. Н/У
      loop
     endif
    endif
  else
    exit
  endif
  i=i+1
enddo
if ptr_NU != 0
  draw=str(PAY1)+" "+PAY2+str(B0)+" "+str(C0)+" "+str(D0)+" "+str(F0)
else
  draw=str(PAY1)+" "+PAY2+strnum(B0)+" "+strnum(C0)+" "+strnum(D0)+" "+strnum(F0)
endif
return draw
*---------------------------------------------------------*
procedure Inp_Sum
save screen
MsgWrite("Считаю...")
nucount=0  && Счетчик Н/У в pay.dbf
for i=0 to 11
  fld=ltrim(str(i))
  ms&fld.=A->MONSUM&fld
  md&fld.=A->MONDAT&fld
next
i=1
do while i <= I->MAX_COME
  fld=ltrim(str(i))
  if A->COD&fld != 0
    var=ltrim(str(A->COD&fld))
    if type("M->cod&var") != "U"
      if A->COD&fld. = I->CHASTN_NU
        B&var.=B0
        C&var.=C0
        D&var.=D0
        F&var.=F0
      else
        B&var.=DAY&fld
        C&var.=CLO&fld
        D&var.=BAZ&fld
        F&var.=SUM&fld
      endif
      M->cod&var.=A->COD&fld
      nucount=nucount+1
    else
      DelNU(i)  && Удал. неизв. Н/У
      loop
    endif
  else
    exit
  endif
  i=i+1
enddo
if nucount != 0
  for i=1 to nucount
    fld=ltrim(str(i))
    var=ltrim(str(A->COD&fld))
    if .not.(empty(frm&var))
      F&var.=formula(frm&var)
      repl I->FORMAT with F&var
      F&var.=I->FORMAT
    endif
  next
  for i=1 to nucount
    fld=ltrim(str(i))
    var=ltrim(str(A->COD&fld))
    repl DAY&fld with B&var,CLO&fld with C&var,;
         BAZ&fld with D&var,SUM&fld with F&var,A->COD&fld with M->cod&var
    B&var.=nul_day
    C&var.=nul_clo
    D&var.=nul_baz
    F&var.=nul_sum
    M->cod&var.=nul_cod
  next
endif
rest screen
return
