*---------------------------------------------------------*
function InpFilter
parameters Y1,X1,Y2,X2
private var_fld,var_prm
private fil_string,type_var
private otwet
private old_color
private del_filter
del_filter=.F.
old_color = setcolor()
save screen to InpPage
set color to &cfr
BoxTenj(Y1,X1,Y2,X2,ibox)
set color to &cl
MSG=' В Ы Б О Р К А '
@ Y1,int((X1+X2)/2-len(MSG)/2) say MSG
set color to &cl
@ Y2,X1+10 say ' М Е Н Ю: '
set color to &cmenu
MSG='Enter'
@ row(),col() get MSG
@ row(),col() say 'Ввод'
MSG='DEL'
@ row(),col() get MSG
@ row(),col() say 'Удалить'
MSG='ESC'
@ row(),col() get MSG
@ row(),col() say 'Выход'
clear gets
declare var_filter[SIZE_FILTER]  && Массив ключей фильтра
fil_string=alltrim(I->L_FILTER)  && Разбор строки фильтра
fil_string=strtran(fil_string,".and.","     ")
for i=1 to SIZE_FILTER
  var_filter[i]=psubstr(fil_string,i)
  var_filter[i]=strtran(var_filter[i],filter_fld[i])
  var_filter[i]=strtran(var_filter[i],"=")
  var_fld=filter_fld[i]
  type_var=&var_fld
  do case
    case type("type_var")="C"
      var_filter[i]=strtran(var_filter[i],'"')
    case type("type_var")="N"
      var_filter[i]=val(var_filter[i])
      var_filter[i]=val(transform(var_filter[i],pictn(type_var)))  && для форматирования
    case type("type_var")="D"
      var_filter[i]=strtran(var_filter[i],"ctod()")
      var_filter[i]=strtran(var_filter[i],'"')
      var_filter[i]=ctod(var_filter[i])
    case type("type_var")="L"
      if var_filter[i]="()"     && "(.not.)"
        var_filter[i]=strtran(filter_iif[i],filter_fld[i],".T.")
      else
        var_filter[i]=strtran(filter_iif[i],filter_fld[i],".F.")
      endif
  endcase
next
otwet=1
do while otwet!=0   && Собственно меню выбора нового фильтра
  set color to &cfr
  for i=1 to SIZE_FILTER
    @ Y1+i+1,X1+2 say filter_nam[i]
    do case
      case .not.empty(filter_dbf[i])
        var_prm=mDict(filter_scr[i],var_filter[i],filter_cod[i],filter_dbf[i],filter_ntx[i])
      case .not.empty(filter_iif[i])
        var_fld=var_filter[i]
        var_prm=&var_fld
      othe
        var_prm=var_filter[i]
    endcase
    @ row(),col()+1 prompt varstr(var_prm)
  next
  set key 7 to DelFilter
  menu to otwet
  set key 7 to
  if otwet != 0
    do case
      case .not.empty(filter_dbf[otwet])
        var_filter[otwet]=DictFind(var_filter[otwet],filter_cod[otwet],filter_scr[otwet],;
                          filter_nam[otwet],filter_dbf[otwet],filter_ntx[otwet])
      case .not.empty(filter_iif[otwet])
        if var_filter[otwet]="iif(.T.,"
          var_filter[otwet]=strtran(var_filter[otwet],"iif(.T.,","iif(.F.,")
        else
          var_filter[otwet]=strtran(var_filter[otwet],"iif(.F.,","iif(.T.,")
        endif
      othe
        set cursor on
        @ Y1+otwet+1,X1+3+len(filter_nam[otwet]) get var_filter[otwet]
        read
        set cursor off
    endcase
  endif
enddo
MSG=""          && Строка названия фильтра
fil_string=""   && Строка логического выражения фильтра
if .not.del_filter
for i=1 to SIZE_FILTER
  if i != 1
     fil_string=fil_string+".and."
     MSG=MSG+"  "
  endif
  var_fld=filter_fld[i]
  type_var=&var_fld
  do case
    case type("type_var")="C"
      fil_string=fil_string+filter_fld[i]+"="+'"'+var_filter[i]+'"'
    case type("type_var")="N"
      fil_string=fil_string+filter_fld[i]+"="+ltrim(str(var_filter[i]))
    case type("type_var")="D"
      fil_string=fil_string+filter_fld[i]+"="+"ctod("+"'"+dtoc(var_filter[i])+"'"+")"
    case type("type_var")="L"
      if var_filter[i]="iif(.T.,"
        fil_string=fil_string+"("+filter_fld[i]+")"
      else
        fil_string=fil_string+"(.not."+filter_fld[i]+")"
      endif
  endcase
  do case
    case .not.empty(filter_dbf[i])
      var_fld=mDict(filter_scr[i],var_filter[i],filter_cod[i],filter_dbf[i],filter_ntx[i])
    case .not.empty(filter_iif[i])
      var_fld=var_filter[i]
      var_fld=&var_fld
    othe
      var_fld=var_filter[i]
  endcase
  do case
    case type("var_fld")="C"
      MSG=MSG+'"'+alltrim(var_fld)+'"'
    case type("var_fld")="N"
      MSG=MSG+ltrim(str(var_fld))
    case type("var_fld")="D"
      MSG=MSG+dtoc(var_fld)
    case type("var_fld")="L"
      MSG=MSG+'"'+alltrim(var_fld)+'"'
  endcase
next
endif
restore screen from InpPage
if I->L_FILTER != fil_string.or.del_filter  && Если фильтр изменен
  replace I->L_FILTER with fil_string
  replace I->N_FILTER with MSG
  MarkRecall()
  DrawFilter(Y_max-2,X_min,Y_max,X_max-24)
  RETR=0
endif
set color to &old_color
return " "
*---------------------------------------------------------*
procedure DelFilter
del_filter=.T.
keyboard chr(27)
return
*---------------------------------------------------------*
function FindPay
parameters fld_ptr,Y1,X1,Y2,X2
private var_find,var_fld,otwet
private old_color,old_index
old_color = setcolor()
save screen to InpPage
set color to &cfr
BoxTenj(Y1,X1,Y2,X2,ibox)
set color to &cl
MSG='  И С К А Т Ь '
@ Y1,int((X1+X2)/2-len(MSG)/2) say MSG
set color to &cfr
var_fld=jour_fld[fld_ptr]
var_find=nulvar(&var_fld)
@ Y1+2,X1+2 say jour_nam[fld_ptr]
do case
  case .not.empty(jour_iif[fld_ptr])
    var_fld=jour_jou[fld_ptr]
    var_fld=strtran(var_fld,jour_fld[fld_ptr],"var_find")
    otwet=1
    var_find=.T.
    @ row(),col()+2 prompt &var_fld
    var_find=.F.
    @ row(),col()+2 prompt &var_fld
    menu to otwet
    if otwet=1
      var_find=.T.
    else
      var_find=.F.
    endif
  othe
    set cursor on
    @ row(),col()+2 get var_find
    read
    set cursor off
endcase
if lastkey() != 27
  MsgWrite("Сейчас поищем...")
  Nz=recno()
  log=0   && Есть ли индекс по этому полю
  if SIZE_INDX != 0
    for i=1 to SIZE_INDX
      if jour_fld[fld_ptr]==index_key[i]
        log=i
      endif
    next
  endif
  old_index=indexord()
  set order to log
  var_fld=jour_fld[fld_ptr]
  if indexord() != 0
    seek var_find
  else
    if type("var_find")="L"
      if var_find
        locate for &var_fld
      else
        locate for .not.&var_fld
      endif
    else
      locate for var_find=&var_fld
    endif
  endif
  if found()
    RETR=2
  else
    ??chr(7)
    WarnWrite("Поиск не удачен...")
    go Nz
  endif
  set order to old_index
endif
restore screen from InpPage
set color to &old_color
return " "
*---------------------------------------------------------*
procedure DelPay
if is_arhiv
 return
endif
save screen
if Yes_Or_No(' У Д А Л И Т Ь ','Лицевые счета?',2)
  MsgWrite("Ждите... Удаляю...")
  if Mark=0
    delete
  else
    Mark=0
  endif
  pack
  go top
  RETR=2
endif
restore screen
return
*---------------------------------------------------------*
procedure SelePay
parameters log,fld_ptr,Y1,X1,Y2,X2
if fld_ptr = 1
  WarnWrite("По полю метки селектирование не осуществляется...")
  return
endif
private var_sele1,var_sele2,var_fld,var_num,otwet
private old_color
old_color = setcolor()
save screen to InpPage
set color to &cfr
BoxTenj(Y1,X1,Y2,X2,ibox)
set color to &cl
if log
  MSG=' З А С Е Л Е К Т И Р О В А Т Ь '
else
  MSG=' Р А З С Е Л Е К Т И Р О В А Т Ь '
endif
@ Y1,int((X1+X2)/2-len(MSG)/2) say MSG
set color to &cfr
var_fld=jour_fld[fld_ptr]
var_sele1=nulvar(&var_fld)
var_sele2=var_sele1
do case
  case .not.empty(jour_iif[fld_ptr])
    @ Y1+2,X1+2 say jour_nam[fld_ptr]+" для "
    var_fld=jour_jou[fld_ptr]
    var_fld=strtran(var_fld,jour_fld[fld_ptr],"var_sele1")
    otwet=1
    var_sele1=.T.
    @ row(),col()+2 prompt &var_fld
    var_sele1=.F.
    @ row(),col()+2 prompt &var_fld
    menu to otwet
    if otwet=1
      var_sele1=.T.
    else
      var_sele1=.F.
    endif
  othe
    set cursor on
    @ Y1+2,X1+2 say jour_nam[fld_ptr]+" с "
    @ row(),col() get var_sele1
    @ Y1+4,X1+2+len(jour_nam[fld_ptr]) say "по "
    @ row(),col() get var_sele2
    read
    set cursor off
endcase
if lastkey() != 27
  if log
    MsgWrite("Сейчас заселектируем...")
  else
    MsgWrite("Сейчас разселектируем...")
  endif
  Nz=recno()
  go top
  var_fld=jour_fld[fld_ptr]
  if empty(var_sele2)
    var_sele2=var_sele1
  endif
  if log
    if type("var_sele1")="L"
      do while .not.eof()
        if iif(var_sele1,&var_fld.,.not.&var_fld)
          if .not.delete()
            delete
            Mark=Mark+1
          endif
        endif
        skip
      enddo
    else
      do while .not.eof()
        if &var_fld.>=var_sele1.and.&var_fld.<=var_sele2
          if .not.delete()
            delete
            Mark=Mark+1
          endif
        endif
        skip
      enddo
    endif
  else
    if type("var_sele1")="L"
      do while .not.eof()
        if iif(var_sele1,&var_fld.,.not.&var_fld)
          if delete()
            recall
            Mark=Mark-1
          endif
        endif
        skip
      enddo
    else
      do while .not.eof()
        if &var_fld.>=var_sele1.and.&var_fld.<=var_sele2
          if delete()
            recall
            Mark=Mark-1
          endif
        endif
        skip
      enddo
    endif
  endif
  go Nz
  RETR=2
endif
restore screen from InpPage
set color to &old_color
return
*---------------------------------------------------------*
function MenuIndex
parameters Y1,X1,Y2,X2
private otwet,old_color
old_color = setcolor()
save screen to OnePage
setcolor(cmenu)
BoxTenj(Y1,X1,Y2,X2,ibox)
setcolor(cl)
MSG=' СОРТИРОВАТЬ '
@ Y1,int((X1+X2)/2-len(MSG)/2) say MSG
setcolor(cmenu)
@ Y1,X1+1 say ''
@ row()+1,X1+1 prompt " Сортировать все ...  "
@ row()+1,X1+1 say    "──────────────────────"
@ row()+1,X1+1 prompt " "+iif(indexord()=0,"√","-")+" поступлению        "
if SIZE_INDX != 0
  for i=1 to SIZE_INDX
    @ row()+1,X1+1 prompt " "+iif(indexord()=i,"√","-")+index_prm[i]
  next
endif
menu to otwet
Save Screen to TwoPage
do case
  case otwet=1
    OpenDBF(.T.)
  othe
    if otwet != 0
      repl I->REGIM_SORT with (otwet-2)
      set order to I->REGIM_SORT
    endif
endcase
Restore Screen from TwoPage
if otwet != 0
  RETR=2
endif
restore screen from OnePage
set color to &old_color
return " "
*---------------------------------------------------------*
function MenuDict
parameters Y1,X1,Y2,X2,pth
private otwet,old_color,old_select,name_str,name_dbf,name_ntx
old_color = setcolor()
save screen to OnePage
setcolor(cmenu)
BoxTenj(Y1,X1,Y2,X2,ibox)
setcolor(cl)
MSG=' С Л О В А Р Ь '
@ Y1,int((X1+X2)/2-len(MSG)/2) say MSG
otwet=1
do while (otwet != 0)
   setcolor(cmenu)
   @ Y1,X1+1 say ''
   for i=1 to SIZE_DICT
     @ row()+1,X1+1 prompt dict_prm[i]
   next
   menu to otwet
   Save Screen to TwoPage
   if otwet!=0
     old_select=select()
     set color to &c
     DrawBox(Y_min,X_min,Y_max,X_max,ibox)
     set color to &cl
     TITL=" С Л О В А Р Ь "+'"'+alltrim(dict_prm[otwet])+'"'
     @ Y_min,int((X_min+X_max)/2-len(TITL)/2) say TITL
     MenuGloss()
     set color to &c
     select 8
     name_str=dict_str[otwet]
     use &name_str
     j=lastrec()
     declare dict_fld[j]
     declare dict_nam[j]
     for i=1 to j
       dict_fld[i]=alltrim(H->FIELD_NAME)
       dict_nam[i]=alltrim(H->NAME_FIELD)
       skip
     next
     name_dbf=iif(empty(pth),'',cur_path+pth)+dict_dbf[otwet]
     use &name_dbf
     if .not.empty(dict_ntx[otwet])
       name_ntx=dict_ntx[otwet]
       set index to &name_ntx
     endif
     dbedit(Y_min+1,X_min+1,Y_max-1,X_max-1,dict_fld,"GlossFunc","",dict_nam,.T.,.T.,"─┴─")
     use
     select(old_select)
   endif
   Restore Screen from TwoPage
enddo
restore screen from OnePage
set color to (old_color)
return " "
*---------------------------------------------------------*
function GlossFunc
parameters mode,fld_ptr
private cur_field,key_last
key_last=LASTKEY()
do case
case mode <= 3
  return 1
case LASTKEY() = 27
  return 0
case LASTKEY() = 13.or.isdata(key_last)
  if key_last <> 13
    keyboard chr(key_last)
  endif
  cur_field=dict_fld[fld_ptr]
  set cursor on
  if type('&cur_field')=='C'
   if len(&cur_field) > 78
    @ row(),col() get &cur_field picture "@S78"
   else
    @ row(),col() get &cur_field
   endif
  else
   @ row(),col() get &cur_field
  endif
  read
  set cursor off
  return 2
case lastkey() = 22        && Ins
 if .not.is_arhiv
  if insert()
   return 2
  endif
 endif
case lastkey() = -4        && F5
  dblpt1(dict_fld,dict_nam,TITL)
case lastkey() = -5        && F6
  FindGloss(fld_ptr,Y_srm-2,X_srm-30,Y_srm+2,X_srm+30)
  return 2
case lastkey() = 7         && Del
  DelGloss()
  return 2
endcase
return 1
*---------------------------------------------------------*
procedure MenuGloss
set color to &cl
@ Y_max,X_min+8 say ' М Е Н Ю: '
set color to &cmenu
MSG='INS'
@ row(),col() get MSG
@ row(),col() say 'Создать'
MSG='Enter'
@ row(),col() get MSG
@ row(),col() say 'Ввод'
MSG='F5'
@ row(),col() get MSG
@ row(),col() say 'Печать'
MSG='F6'
@ row(),col() get MSG
@ row(),col() say 'Искать'
MSG='DEL'
@ row(),col() get MSG
@ row(),col() say 'Удалить'
MSG='ESC'
@ row(),col() get MSG
@ row(),col() say 'Выход'
clear gets
return
*---------------------------------------------------------*
procedure FindGloss
parameters fld_ptr,Y1,X1,Y2,X2
private var_find,var_fld,otwet
private old_color
old_color = setcolor()
save screen to InpPage
set color to &cfr
BoxTenj(Y1,X1,Y2,X2,ibox)
set color to &cl
MSG=' И С К А Т Ь '
@ Y1,int((X1+X2)/2-len(MSG)/2) say MSG
set color to &cfr
var_fld=dict_fld[fld_ptr]
var_find=nulvar(&var_fld)
@ Y1+2,X1+2 say dict_nam[fld_ptr]
set cursor on
@ row(),col()+2 get var_find
read
set cursor off
if lastkey() != 27
  MsgWrite("Сейчас поищем...")
  Nz=recno()
  if type("var_find")="L"
    if var_find
      locate for &var_fld
    else
      locate for .not.&var_fld
    endif
  else
    locate for var_find=&var_fld
  endif
  if .not.found()
    ??chr(7)
    WarnWrite("Поиск не удачен...")
    go Nz
  endif
endif
restore screen from InpPage
set color to &old_color
return
*---------------------------------------------------------*
procedure DelGloss
if is_arhiv
 return
endif
save screen
if Yes_Or_No(' У Д А Л И Т Ь ','Текущую запись?',2)
  MsgWrite("Ждите... Удаляю...")
  delete
  pack
  go top
endif
restore screen
return
*---------------------------------------------------------*
function WhoKW
private old_color,ptr,currprow
old_color=setcolor(cw)
DrawBox(Y_srm-3,X_srm-5,Y_srm+3,X_srm+5,ibox)
setcolor(cl)
MSG=' КВАРТАЛ '
@ Y_srm-3,int(X_srm-len(MSG)/2)+1 say MSG
kv=1
setcolor(cw)
@ Y_srm-2,X_srm-4 prompt "первый   "
@ row()+1,X_srm-4 prompt "второй   "
@ row()+1,X_srm-4 prompt "третий   "
@ row()+1,X_srm-4 prompt "четвертый"
@ row()+1,X_srm-4 prompt "текущий  "
menu to kv
do case
case kv=1
 bm=1
 em=3
case kv=2
 bm=4
 em=6
case kv=3
 bm=7
 em=9
case kv=4
 bm=10
 em=12
case kv=5
 bm=1
 em=month(I->T_DATE)
endcase
setcolor(old_color)
return kv
*---------------------------------------------------------*
procedure svedproc
  select 1
  use
  for i=bm to em
   acco_path=cur_path+"\"+str(year(cur_date),4)+"."+strtran(str(i,2)," ","0")+"\"
   use_dbf=acco_path+MainDBF
   if file(use_dbf)
    select 9
    use_pay=acco_path+MainPAY
    use &use_pay
    InitConfig()
    select 1
    use &use_dbf
    use_ntx=acco_path+index_ntx[2]
    set index to &use_ntx
    seek acco_PAY2
    if found()
     period=period+iif(empty(period),""," ")+strtran(str(i,2)," ","0")
     nucount=0
     for j=1 to I->MAX_COME
       fld=ltrim(str(j))
       if A->COD&fld != 0
         var=ltrim(str(A->COD&fld))
         if type("M->cod&var") != "U"
           B&var.=DAY&fld
           C&var.=CLO&fld
           D&var.=BAZ&fld
           F&var.=SUM&fld
           M->cod&var.=A->COD&fld
           nucount=nucount+1
         endif
       else
         exit
       endif
     next
     if .not.empty(sovoku)
      vas="MESZ"+ltrim(str(i))
      &vas.=formula(sovoku)
     endif
     for iii=1 to LEN7
      vas="d37"+ltrim(str(iii))
      if .not.empty(&vas)
       var=formula(&vas.)
      else
       var=0
      endif
      if var!=0
       for ii=1 to LEN_DOHOD
        vac="CODD"+ltrim(str(ii))
        vas="SUMD"+ltrim(str(ii))
        vad="d17"+ltrim(str(iii))
        if empty(&vac)
         &vac.=&vad
         &vas.=var
         exit
        else
         if &vac.==&vad.
          &vas.=&vas.+var
          exit
         endif
        endif
       next
       if ii=LEN_DOHOD
        exit
       endif
      endif
     next
     for iii=1 to LEN8
      vas="d38"+ltrim(str(iii))
      if .not.empty(&vas)
       var=formula(&vas.)
      else
       var=0
      endif
      if var!=0
       for ii=1 to LEN_RASOD
        vac="CODC"+ltrim(str(ii))
        vas="SUMC"+ltrim(str(ii))
        vad="d18"+ltrim(str(iii))
        if empty(&vac)
         &vac.=&vad
         &vas.=var
         exit
        else
         if &vac.==&vad.
          &vas.=&vas.+var
          exit
         endif
        endif
       next
       if ii=LEN_RASOD
        exit
       endif
      endif
     next
     for iii=1 to LEN9
      vas="d39"+ltrim(str(iii))
      if .not.empty(&vas)
       var=formula(&vas.)
      else
       var=0
      endif
      if var!=0
       for ii=1 to LEN_DSXTN
        vac="CODV"+ltrim(str(ii))
        vas="SUMV"+ltrim(str(ii))
        vad="d19"+ltrim(str(iii))
        if empty(&vac)
         &vac.=&vad
         &vas.=var
         exit
        else
         if &vac.==&vad.
          &vas.=&vas.+var
          exit
         endif
        endif
       next
       if ii=LEN_DSXTN
        exit
       endif
      endif
     next
     for iii=1 to LEN12
      vas="d312"+ltrim(str(iii))
      if .not.empty(&vas)
       var=formula(&vas.)
      else
       var=0
      endif
      vas="SUMI"+ltrim(str(iii))
      &vas.=&vas.+var
     next
     for ii=1 to SIZE_COME
      var=cod[ii]
      dF&var.=dF&var.+(F&var.-dF&var.)
     next
     if nucount != 0
       for j=1 to nucount
         fld=ltrim(str(j))
         var=ltrim(str(A->COD&fld))
         B&var.=nul_day
         C&var.=nul_clo
         D&var.=nul_baz
         F&var.=nul_sum
         M->cod&var.=nul_cod
       next
     endif
    endif
    select 1
    use
   endif
  next
return
