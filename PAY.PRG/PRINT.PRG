*---------------------------------------------------------*
procedure MenuPrint
parameters Y1,X1,Y2,X2
private otwet,old_color
old_color = setcolor()
save screen to OnePage
setcolor(cmenu)
BoxTenj(Y1,X1,Y2,X2,ibox)
setcolor(cl)
MSG=' П Е Ч А Т Ь '
@ Y1,int((X1+X2)/2-len(MSG)/2) say MSG
otwet=1
do while (otwet != 0)
   setcolor(cmenu)
   @ Y1+ 1,X1+1 prompt ' Журнал                '
   @ Y1+ 2,X1+1 prompt ' Лицевой счет          '
   @ Y1+ 3,X1+1 prompt ' Hалоговая каpточка    '
   @ Y1+ 4,X1+1 prompt ' Квиток                '
   @ Y1+ 5,X1+1 prompt ' Ведомость             '
   @ Y1+ 6,X1+1 prompt ' Кассовый ордер        '
   @ Y1+ 7,X1+1 prompt ' Сводная ведомость     '
   @ Y1+ 8,X1+1 prompt ' Раздаточная ведомость '
   @ Y1+ 9,X1+1 prompt ' Жуpнал-оpдеp          '
   @ Y1+10,X1+1 prompt ' Сведения              '
   @ Y1+11,X1+1 prompt ' Реестp                '
   @ Y1+12,X1+1 prompt ' Итоговый свод         '
   @ Y1+13,X1+1 prompt ' Итоговая каpточка     '
   menu to otwet
   Save Screen to TwoPage
   if otwet!=0
     do case
     case otwet=1
       dblpt1(jour_jou,jour_nam,"СПИСОК ЛИЦЕВЫХ СЧЕТОВ "+'"'+alltrim(I->N_FILTER)+'"')
     case otwet=2
       AccoLPT1()
     case otwet=3
       AccounLPT1()
     case otwet=4
       ComeLPT1()
     case otwet=5
       VedoLPT1()
     case otwet=6
       KassLPT1()
     case otwet=7
       SvodLPT1()
     case otwet=8
       RvedLPT1()
     case otwet=9
       OrderLPT1()
     case otwet=10
       WhoLPT1(1)
     case otwet=11
       WhoLPT1(2)
     case otwet=12
       WhoLPT1(3)
     case otwet=13
       WhoLPT1(4)
     endcase
   endif
   Restore Screen from TwoPage
enddo
restore screen from OnePage
setcolor(old_color)
return
*---------------------------------------------------------*
procedure AccoLPT1
private str_file,str_prn
if .not.QuesPrint()
  return
endif
MsgWrite(msgprn)
str_file=memoread("acco.prn")
if empty(str_file)
  return
endif
NN=1
Nz=recno()
if Mark != 0
  go top
endif
do while .not.eof()
  if delete().or.Mark = 0
    str_prn=strfrm(str_file)
    if .not.mprints(str_prn)
      exit
    endif
  endif
  if Mark != 0
    skip
  else
    exit
  endif
  NN=NN+1
enddo
go Nz
return
*---------------------------------------------------------*
procedure AccounLPT1
private str_file,str_prn,is_prn,Nz
if .not.QuesPrint()
  return
endif
MsgWrite(msgprn)
str_file=memoread("account.prn")
if empty(str_file)
  return
endif
NN=1
Nz=recno()
if Mark != 0
  go top
endif
nLines=mlcount(str_file,I->LEN_LPT1)
for j=2 to karta_len
 vac="A"+ltrim(str(j))
 &vac.=0
next
select 8
use come.pay				&& Начисления/Удержания
SIZE_COME=reccount()
decl cod[SIZE_COME]
for i=1 to SIZE_COME
 cod[i]=ltrim(str(H->COME_COD))
 skip
next
use
cur_date=I->T_DATE
select 1
is_prn=.T.
do while (.not.eof()).and.is_prn
 if delete().or.Mark = 0
  MsgWrite("Обpабатываю лицевой счет - "+PAY2)
  acco_PAY2=PAY2
  fld="M0"
  for j=2 to karta_len
   var=ltrim(str(j))
   ptr=fld+"C"+var
   &ptr.=nul_sum
  next
  for ii=1 to SIZE_COME
   var=cod[ii]
   dF&var.=nul_sum
  next
  cur_Nz=recno()
  CloseDBF()
  acco_path=cur_path+"\"+str(year(cur_date)-1,4)+".12"+"\"
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
    fld="M0"
    for j=2 to karta_len
     var=ltrim(str(j))
     if .not.(empty(M->EARTA&var))
      ptr=fld+"C"+var
      repl I->FORMAT with formula(M->EARTA&var)
      &ptr.=I->FORMAT
     endif
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
  for ii=1 to SIZE_COME
   var=cod[ii]
   dF&var.=nul_sum
  next
  for i=1 to 12
   fld="M"+ltrim(str(i))
   for j=2 to karta_len
    var=ltrim(str(j))
    ptr=fld+"C"+var
    &ptr.=nul_sum
   next
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
     fld="M"+ltrim(str(i))
     for j=2 to karta_len
      var=ltrim(str(j))
      vac="A"+ltrim(str(j))
      if .not.(empty(M->KARTA&var))
       ptr=fld+"C"+var
       repl I->FORMAT with formula(M->KARTA&var)
       &ptr.=I->FORMAT
       &vac.=I->FORMAT
      endif
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
  select 9
  use &new_config
  for j=2 to karta_len
   var="C"+ltrim(str(j))
   itogo=0
   for i=1 to 12
    ptr="M"+ltrim(str(i))+var
    itogo=itogo+&ptr.
   next
   ptr="M13"+var
   repl I->FORMAT with itogo
   &ptr.=I->FORMAT
  next
  OpenDBF()
  go cur_Nz
  for nCurLine=1 to nLines
   str_prn=strfrm(rtrim(memoline(str_file,I->LEN_LPT1,nCurLine))+chr(13)+chr(10))
   if .not.mprints(str_prn)
    is_prn=.F.
    exit
   endif
  next
 endif
 if Mark != 0
   skip
 else
   exit
 endif
 NN=NN+1
enddo
select 9
use &new_config
InitConfig()
select 1
go Nz
return
*---------------------------------------------------------*
procedure ComeLPT1
private str_file,i,j,i_min,i_max,Nz,NN,var,fld,str_prn
if .not.QuesPrint()
  return
endif
declare format_str[3]   && 1-заголовок; 2-текст; 3-окончание
MsgWrite(msgprn)
str_file=memoread("come.prn")
if empty(str_file)
  return
endif
for i=1 to 3
  i_min=at(chr(i),str_file)
  i_max=rat(chr(i),str_file)
  if i_min != i_max.and.i_min != (i_max - 1)
    format_str[i]=substr(str_file,i_min+1,i_max-i_min-1)
  else
    format_str[i]=""
  endif
next
Nz=recno()
if Mark != 0
  go top
endif
NN=1
do while .not.eof()
 if delete().or.Mark = 0
   str_prn=strfrm(format_str[1])
   for j=1 to I->MAX_COME
     fld=ltrim(str(j))
     var=ltrim(str(A->COD&fld))
     if A->COD&fld != 0.and.type("M->cod&var") != "U"
       B0=DAY&fld
       C0=CLO&fld
       D0=BAZ&fld
       F0=SUM&fld
       cod0=A->COD&fld
       nam0=nam&var
       if F0 != 0
        if prn&var
         str_prn=str_prn+strfrm(format_str[2])
        endif
       endif
       NN=NN+1
     endif
   next
   str_prn=str_prn+strfrm(format_str[3])
   if .not.mprints(str_prn)
     exit
   endif
 endif
 if Mark != 0
   skip
 else
   exit
 endif
enddo
go Nz
return
*---------------------------------------------------------*
procedure KassLPT1
private str_file,var,NN,Nz,str_prn
var=ltrim(str(I->KASSA_PRN))
if type("M->cod&var") = "U"
  WarnWrite("Неизвестное Н/У...")
  return
endif
save screen
BoxTenj(Y_srm-2,X_srm-15,Y_srm+2,X_srm+15,ibox)
set cursor on
NN=0
@ Y_srm,X_srm-7 say "Номер:" get NN picture '99999'
read
set cursor off
rest screen
if lastkey() = 27
  return
endif
if .not.QuesPrint()
  return
endif
MsgWrite(msgprn)
str_file=memoread("kassa.prn")
if empty(str_file)
  return
endif
Nz=recno()
if Mark != 0
  go top
endif
do while .not.eof()
  if delete().or.Mark = 0
    for j=1 to I->MAX_COME
      fld=ltrim(str(j))
      if A->COD&fld. = I->KASSA_PRN
        B0=DAY&fld
        C0=CLO&fld
        D0=BAZ&fld
        F0=okrugl(SUM&fld,I->NUM_DIV)
        cod0=A->COD&fld
        nam0=nam&var
        exit
      endif
    next
    if A->COD&fld. = I->KASSA_PRN.and.SUM&fld. != 0
      str_prn=strfrm(str_file)
      if .not.mprints(str_prn)
        exit
      endif
      NN=NN+1
    endif
  endif
  if Mark != 0
    skip
  else
    exit
  endif
enddo
go Nz
return
*---------------------------------------------------------*
procedure VedoLPT1
private str_file,i,j,indx,i_min,i_max,Nz,NN,NL,ITOG,ITOGLIST
private old_color,ptr,currprow
var=ltrim(str(I->COME_PRN1))
if type("M->cod&var") = "U"
  WarnWrite("Неизвестное Н/У...")
  return
endif
if .not.QuesPrint()
  return
endif
declare format_str[5]   && 1-титул; 2-заголовок; 3-текст; 4-окончание; 5-подпись
MsgWrite(msgprn)
str_file=memoread("vedo.prn")
if empty(str_file)
  return
endif
for i=1 to 5
  i_min=at(chr(i),str_file)
  i_max=rat(chr(i),str_file)
  if i_min != i_max.and.i_min != (i_max - 1)
    format_str[i]=substr(str_file,i_min+1,i_max-i_min-1)
  else
    format_str[i]=""
  endif
next
NN=1
ITOG=0
i=1
str_prn=strfrm(format_str[i])
if .not.mprints(str_prn)
  return
endif
Nz=recno()
if Mark != 0
  go top
endif
NL=0
do while (.not.eof())
  ITOGLIST=0
  Currprow=0
  NL=NL+1
  for i=2 to 4
    do case
    case i=2.or.i=4
      str_prn=strfrm(format_str[i])
      if .not.mprints(str_prn)
          go Nz
          return
      endif
    case i=3
      do while (.not.eof())
        if delete().or.Mark = 0
          for j=1 to I->MAX_COME
            fld=ltrim(str(j))
            if A->COD&fld. = I->COME_PRN1
              B0=DAY&fld
              C0=CLO&fld
              D0=BAZ&fld
              F0=okrugl(SUM&fld,I->NUM_DIV)
              cod0=A->COD&fld
              nam0=nam&var
              exit
            endif
          next
          if A->COD&fld. = I->COME_PRN1.and.SUM&fld. != 0
            str_prn=strfrm(format_str[i])
            if .not.mprints(str_prn)
              go Nz
              return
            endif
            NN=NN+1
            ITOGLIST=ITOGLIST+F0
            ITOG=ITOG+F0
            currprow = currprow + 1
          endif
        endif
        if Mark != 0
          skip
        else
          exit
        endif
        if currprow >= I->MAXPROW
          exit
        endif
      enddo
    endcase
  next
  if currprow >= I->MAXPROW
    PrintChar(chr(12))
  endif
  if Mark = 0
    exit
  endif
enddo
go Nz
i=5
str_prn=strfrm(format_str[i])
mprints(str_prn)
return
*---------------------------------------------------------*
procedure SvodLPT1
private old_color,old_select,old_Nz,Nz,ch,otwet,ptr,fld,var,str_prn,ITOG
if .not.Fragen('','Произвести необходимые расчеты?')
  return
endif
MsgWrite("Расчитываю...")
old_color=setcolor(cw)
old_select=select()
old_Nz=recno()
select 8
use come.pay
select 2
create temp.str
use temp.str
append blank
repl FIELD_NAME with I->SVOD_NAME,FIELD_TYPE with I->SVOD_TYPE,;
  FIELD_LEN with I->SVOD_LEN,FIELD_DEC with I->SVOD_DEC
select 8
do while .not.eof()
  select 2
  append blank
  repl FIELD_NAME with "SUM"+ltrim(str(H->COME_COD)),FIELD_TYPE with "N",;
       FIELD_LEN with len(str(I->FORMAT)),;
       FIELD_DEC with iif(at(".",str(I->FORMAT))=0,0,len(substr(str(I->FORMAT),rat(".",str(I->FORMAT))+1)))
  select 8
  skip
enddo
select 8  && надо
use
select 2
use
select 2
create temp.dbf from temp.str
use temp.dbf
select(old_select)
ptr=alltrim(I->SVOD_NAME)
close index
index on &ptr. to temp.ntx
go top
do while .not.eof()
  select 2
  append blank
  repl B->&ptr. with A->&ptr.
  select(old_select)
  do while (B->&ptr.=A->&ptr.).and.(.not.eof())
    for i=1 to I->MAX_COME
      fld=ltrim(str(i))
      var=ltrim(str(A->COD&fld))
      if A->COD&fld. !=0 .and. type("M->cod&var") != "U"
        repl B->SUM&var with B->SUM&var.+A->SUM&fld.
      endif
    next
    skip
  enddo
enddo
select 2
do case
case .not.empty(I->SVOD_IIF)
 var=alltrim(I->SVOD_IIF)
case .not.empty(I->SVOD_DBF)
 var="mdict('"+alltrim(I->SVOD_SCR)+"',B->"+ptr+",'"+alltrim(I->SVOD_COD)+;
     "','"+alltrim(I->SVOD_DBF)+"','')"
othe
 var=ptr
endcase
len_val1=lenvar(&var)
len_val2=lenvar(nul_sum)
len_table=lenvar(nul_nam)+(lastrec()+1)*(iif(len_val1>=len_val2,len_val1,len_val2)+1)
* фоpмиpование таблицы
str_file=memoread("svod.prn")
str_prn=""
i_min=at(chr(1),str_file)
i_max=rat(chr(1),str_file)
if i_min != i_max.and.i_min != (i_max - 1)
  str_prn=strfrm(substr(str_file,i_min+1,i_max-i_min-1))
endif
* 1 строка
str_prn=str_prn+replicate("-",len_table)+chr(13)+chr(10)
* 2 строка
str_prn=str_prn+nul_nam
go top
do while .not.eof()
  str_prn=str_prn+"!"+iif(len_val1>=len_val2,varstr(&var),varstr(&var)+space(len_val2-len_val1))
  skip
enddo
ch="Итого"
str_prn=str_prn+"!"+iif(len(ch)>=len_val2,ch,ch+space(len_val2-len(ch)))
str_prn=str_prn+chr(13)+chr(10)
* 3 строка
str_prn=str_prn+replicate("-",len_table)+chr(13)+chr(10)
* собственно табл.
select 8
use come.pay
do while .not.eof()
  if H->COME_PRN
   var="SUM"+ltrim(str(H->COME_COD))
   select 2
   sum &var to ITOG
   if ITOG != 0
    str_prn=str_prn+H->COME_NAM
    select 2
    go top
    do while .not.eof()
      str_prn=str_prn+"!"+iif(len_val2>=len_val1,strnum(&var),space(len_val1-len_val2)+strnum(&var))
      skip
    enddo
    repl I->FORMAT with ITOG
    str_prn=str_prn+"!"+iif(len_val2>=len_val1,strnum(I->FORMAT),space(len_val1-len_val2)+strnum(I->FORMAT))
    str_prn=str_prn+chr(13)+chr(10)
   endif
   select 8
  endif
  skip
enddo
select 8
use
* строка
str_prn=str_prn+replicate("-",len_table)+chr(13)+chr(10)
*** 3 - END строка
i_min=at(chr(5),str_file)
i_max=rat(chr(5),str_file)
if i_min != i_max.and.i_min != (i_max - 1)
  str_prn=str_prn+strfrm(substr(str_file,i_min+1,i_max-i_min-1))
endif
****
select 2
use
dele file temp.dbf
dele file temp.str
select(old_select)
close index
dele file temp.ntx
***
str_prn=formprn(@str_prn,I->LEN_LPT1,I->IS_LIST)
ed(Y_min,X_min,Y_max,X_max,@str_prn)
***
setcolor(old_color)
select(old_select)
OpenDBF()
InitFilter()
go old_Nz
return
*---------------------------------------------------------*
procedure RvedLPT1
private old_color,old_select,old_Nz,Nz,ch,otwet,ptr,fld,var,str_prn,ITOG
if .not.Fragen('','Произвести необходимые расчеты?')
  return
endif
MsgWrite("Расчитываю...")
old_color=setcolor(cw)
old_select=select()
old_Nz=recno()
select 7
use come.pay
SIZE_COME = lastrec()
decl _COD[SIZE_COME]
i=1
do while .not.eof()
 var=ltrim(str(COME_COD))
 is_prn&var.=iif(COME_PRN,1,0)
 itogo&var.=0
 _COD[i]=COME_COD
 i++
 skip
enddo
use
select 1
if Mark != 0
 go top
endif
do while .not.eof()
 if delete().or.Mark = 0
  i=1
  do while i <= I->MAX_COME
   fld=ltrim(str(i))
   if A->COD&fld != 0
     var=ltrim(str(A->COD&fld))
     if type("M->cod&var") != "U"
       if is_prn&var.=1
        is_prn&var.=iif(SUM&fld.=0,1,2)
       endif
     endif
   else
     exit
   endif
   i=i+1
  enddo
  select 1
 endif
 if Mark != 0
  skip
 else
  exit
 endif
enddo
cnt_come=0
for i=1 to SIZE_COME
 var=ltrim(str(_COD[i]))
 if is_prn&var.=2
  cnt_come++
 endif
next
* фоpмиpование таблицы
cpfopen("rved.txt")
len_val=lenvar(I->FORMAT)
len_table=67+cnt_come*(len_val+1)-1
str_file=memoread("rved.prn")
i_min=at(chr(1),str_file)
i_max=rat(chr(1),str_file)
cpfout(strfrm(substr(str_file,i_min+1,i_max-i_min-1)))
i_min=at(chr(2),str_file)
i_max=rat(chr(2),str_file)
str_2=substr(str_file,i_min+1,i_max-i_min-1)
i_min=at(chr(3),str_file)
i_max=rat(chr(3),str_file)
str_3=substr(str_file,i_min+1,i_max-i_min-1)
* 1 строка
cpfout(replicate("-",len_table)+chr(13)+chr(10))
* 2 строка
cpfout(" Таб !")
cpfout(" Должность!")
cpfout("Стаж!")
cpfout(" Оклад !")
cpfout("Дни!")
for i=1 to SIZE_COME
 var=ltrim(str(_COD[i]))
 if is_prn&var.=2
  cpfout(ssub(alltrim(nam&var),1,len_val)+"!")
 endif
next
cpfout(" Фамилия и инициалы !")
cpfout(" Расписка  ")
cpfout(chr(13)+chr(10))
* 3 строка
cpfout(" NN° !")
cpfout(space(10)+"!")
cpfout(space(4)+"!")
cpfout(space(7)+"!")
cpfout(space(3)+"!")
for i=1 to SIZE_COME
 var=ltrim(str(_COD[i]))
 if is_prn&var.=2
  cpfout(ssub(alltrim(nam&var),len_val+1,len_val)+"!")
 endif
next
cpfout(space(20)+"!")
cpfout("в получении")
cpfout(chr(13)+chr(10))
* 4 строка
cpfout(replicate("-",len_table)+chr(13)+chr(10))
* собственно табл.
select 1
if Mark != 0
  go top
endif
NN=1
do while .not.eof()
 if delete().or.Mark = 0
 nucount=0
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
     endif
   else
     exit
   endif
   i=i+1
 enddo
 cpfout(strfrm(str_2))
 for i=1 to SIZE_COME
  var=ltrim(str(_COD[i]))
  if is_prn&var.=2
   cpfout(strnum(F&var)+"!")
   itogo&var.=itogo&var.+F&var
  endif
 next
 if nucount!=0
   for i=1 to nucount
     fld=ltrim(str(i))
     var=ltrim(str(A->COD&fld))
     if type("M->cod&var") != "U"
      B&var.=nul_day
      C&var.=nul_clo
      D&var.=nul_baz
      F&var.=nul_sum
      M->cod&var.=nul_cod
     endif
   next
 endif
 cpfout(strfrm(str_3))
 cpfout("___________")
 cpfout(chr(13)+chr(10))
 endif
 if Mark != 0
   skip
 else
   exit
 endif
 NN=NN+1
enddo
* строка
cpfout(replicate("-",len_table)+chr(13)+chr(10))
* итого
cpfout("И Т О Г О"+space(24)+"!")
for i=1 to SIZE_COME
 var=ltrim(str(_COD[i]))
 if is_prn&var.=2
  repl I->FORMAT with itogo&var
  cpfout(strnum(I->FORMAT)+"!")
 endif
next
cpfout(chr(13)+chr(10))
* строка
cpfout(replicate("-",len_table)+chr(13)+chr(10))
*** 3 - END строка
i_min=at(chr(4),str_file)
i_max=rat(chr(4),str_file)
cpfout(strfrm(substr(str_file,i_min+1,i_max-i_min-1)))
***
cpfclose()
***
setcolor(old_color)
select(old_select)
OpenDBF()
InitFilter()
go old_Nz
return
*---------------------------------------------------------*
procedure OrderLPT1
private old_color,old_select,old_Nz,Nz,ch,otwet,ptr,fld,var,str_prn,ITOG
if .not.Fragen('','Произвести необходимые расчеты?')
  return
endif
MsgWrite("Расчитываю...")
old_color=setcolor(cw)
old_select=select()
old_Nz=recno()
go top
do while .not.eof()               && Суммы по Л/С
 for i=1 to I->MAX_COME
  fld=ltrim(str(i))
  if COD&fld != 0
   var=ltrim(str(COD&fld))
   if type("cod&var") != "U"
    F&var.=F&var.+SUM&fld
   endif
  else
   exit
  endif
 next
 skip
enddo
select 8
use come.pay
do while .not.eof()
 if .not.(empty(H->COME_ORD))
  var=ltrim(str(H->COME_COD))
  F&var.=formula(upper(strtran(H->COME_ORD," ")))
 endif
 skip
enddo
* фоpмиpование таблицы
str_file=memoread("order.prn")
str_prn=""
i_min=at(chr(1),str_file)
i_max=rat(chr(1),str_file)
if i_min != i_max.and.i_min != (i_max - 1)
  str_prn=strfrm(substr(str_file,i_min+1,i_max-i_min-1))
endif
* собственно табл.
select 8
go top
do while .not.eof()
 if H->DEBET!=0.and.H->KREDIT!=0
  var=ltrim(str(H->COME_COD))
  if F&var.!=0
   repl I->FORMAT with F&var.
   str_prn=str_prn+H->COME_NAM+"! "+strroun(H->DEBET)+"! "+strroun(H->KREDIT)+"!"+strnum(I->FORMAT)+chr(13)+chr(10)
  endif
 endif
 skip
enddo
*** 3 - END строка
i_min=at(chr(5),str_file)
i_max=rat(chr(5),str_file)
if i_min != i_max.and.i_min != (i_max - 1)
  str_prn=str_prn+strfrm(substr(str_file,i_min+1,i_max-i_min-1))
endif
****
select 8
go top
do while .not.eof()
 var=ltrim(str(H->COME_COD))
 F&var.=nul_sum
 skip
enddo
use
***
str_prn=formprn(@str_prn,I->LEN_LPT1,I->IS_LIST)
ed(Y_min,X_min,Y_max,X_max,@str_prn)
***
setcolor(old_color)
select(old_select)
OpenDBF()
InitFilter()
go old_Nz
return
*---------------------------------------------------------*
procedure WhoLPT1
parameters whohin
private str_file,i,j,indx,i_min,i_max,Nz,NN,NL
bm=0
em=0
kv=0
if WhoKW()!=0
 Nz=recno()
 do case
 case whohin=1
  SvedLPT1()
 case whohin=2
  ReesLPT1()
 case whohin=3
  Otch1LPT1()
 case whohin=4
  Otch2LPT1()
 endcase
 select 9
 use &new_config
 InitConfig()
 OpenDBF()
 InitFilter()
 go Nz
endif
return
*---------------------------------------------------------*
procedure SvedLPT1
private str_file,str_prn,old_Nz
MsgWrite(msgprn)
str_file=memoread("sved.prn")
if empty(str_file)
  return
endif
old_Nz=recno()
NN=1
if Mark != 0
  go top
endif
cur_date=I->T_DATE	&& Имеется в виду ГОД
sovoku=upper(strtran(I->SOVOKUP," "))
select 8
name_dbf="dict7.dbf"
use &name_dbf
LEN7=reccount()
for i=1 to LEN7
 vac="d17"+ltrim(str(i))
 &vac.=D1
 vas="d37"+ltrim(str(i))
 &vas.=upper(strtran(D3," "))
 skip
next
use
select 8
name_dbf="dict8.dbf"
use &name_dbf
LEN8=reccount()
for i=1 to LEN8
 vac="d18"+ltrim(str(i))
 &vac.=D1
 vas="d38"+ltrim(str(i))
 &vas.=upper(strtran(D3," "))
 skip
next
use
select 8
name_dbf="dict9.dbf"
use &name_dbf
LEN9=reccount()
for i=1 to LEN9
 vac="d19"+ltrim(str(i))
 &vac.=D1
 vas="d39"+ltrim(str(i))
 &vas.=upper(strtran(D3," "))
 skip
next
use
select 8
name_dbf="dict12.dbf"
use &name_dbf
LEN12=reccount()
for i=1 to LEN12
 vac="d112"+ltrim(str(i))
 &vac.=D1
 vas="d312"+ltrim(str(i))
 &vas.=upper(strtran(D3," "))
 skip
next
use
use come.pay				&& Начисления/Удержания
SIZE_COME=reccount()
decl cod[SIZE_COME]
for i=1 to SIZE_COME
 cod[i]=ltrim(str(H->COME_COD))
 skip
next
use
period=""
is_prn=.T.
select 1
do while (.not.eof()).and.is_prn
 if delete().or.Mark = 0
  MsgWrite("Обpабатываю лицевой счет - "+PAY2)
  period=""
  acco_PAY2=PAY2
  DOC1=padr(psubstr(PAY6,1),9)
  DOC2=padr(psubstr(PAY6,2),9)
  for i=1 to LEN_MESEZ
   vas="MESZ"+ltrim(str(i))
   &vas.=0
  next
  for i=1 to LEN_DOHOD
   vac="CODD"+ltrim(str(i))
   &vac.='     '
   vas="SUMD"+ltrim(str(i))
   &vas.=0
  next
  for i=1 to LEN_RASOD
   vac="CODC"+ltrim(str(i))
   &vac.='     '
   vas="SUMC"+ltrim(str(i))
   &vas.=0
  next
  for i=1 to LEN_DSXTN
   vac="CODV"+ltrim(str(i))
   &vac.='     '
   vas="SUMV"+ltrim(str(i))
   &vas.=0
  next
  for i=1 to LEN_ISPOLN
   vas="SUMI"+ltrim(str(i))
   &vas.=0
  next
  for ii=1 to SIZE_COME
   var=cod[ii]
   dF&var.=nul_sum
  next
  cur_Nz=recno()
  svedproc()
  period=padr(period,35)
  ISUMD=0
  for ii=1 to LEN_MESEZ
   vas="MESZ"+ltrim(str(ii))
   repl I->FORMAT with &vas.
   &vas.=strnum(I->FORMAT)
  next
  for ii=1 to LEN_DOHOD
   vas="SUMD"+ltrim(str(ii))
   repl I->FORMAT with &vas.
   &vas.=strnum(I->FORMAT)
   ISUMD=ISUMD+I->FORMAT
  next
  repl I->FORMAT with ISUMD
  ISUMD=strnum(I->FORMAT)
  ISUMC=0
  for ii=1 to LEN_RASOD
   vas="SUMC"+ltrim(str(ii))
   repl I->FORMAT with &vas.
   &vas.=strnum(I->FORMAT)
   ISUMC=ISUMC+I->FORMAT
  next
  repl I->FORMAT with ISUMC
  ISUMC=strnum(I->FORMAT)
  ISUMV=0
  for ii=1 to LEN_DSXTN
   vas="SUMV"+ltrim(str(ii))
   repl I->FORMAT with &vas.
   &vas.=strnum(I->FORMAT)
   ISUMV=ISUMV+I->FORMAT
  next
  ISUMI=0
  for ii=1 to LEN12
   vas="SUMI"+ltrim(str(ii))
   repl I->FORMAT with &vas.
   &vas.=strnum(I->FORMAT)
   ISUMI=ISUMI+I->FORMAT
  next
  repl I->FORMAT with ISUMV
  ISUMV=strnum(I->FORMAT)
  select 9
  use &new_config
  OpenDBF()
  go cur_Nz
  str_prn=strfrm(str_file)
  if .not.mprints(str_prn)
   is_prn=.F.
   exit
  endif
 endif
 if Mark != 0
   skip
 else
   exit
 endif
 NN=NN+1
enddo
go old_Nz
return
*---------------------------------------------------------*
procedure ReesLPT1
declare format_str[5]   && 1-титул; 2-заголовок; 3-текст; 4-окончание; 5-подпись
MsgWrite(msgprn)
str_file=memoread("rees.prn")
if empty(str_file)
  return
endif
for i=1 to 5
  i_min=at(chr(i),str_file)
  i_max=rat(chr(i),str_file)
  if i_min != i_max.and.i_min != (i_max - 1)
    format_str[i]=substr(str_file,i_min+1,i_max-i_min-1)
  else
    format_str[i]=""
  endif
next
NN=1
ii=1
str_prn=strfrm(format_str[ii])
if .not.mprints(str_prn)
  return
endif
if Mark != 0
  go top
endif
select 8
use come.pay				&& Начисления/Удержания
SIZE_COME=reccount()
decl cod[SIZE_COME]
for i=1 to SIZE_COME
 cod[i]=ltrim(str(H->COME_COD))
 skip
next
use
cur_date=I->T_DATE
NL=0
select 1
do while (.not.eof())
 Currprow=0
 NL=NL+1
 for ii=2 to 4
  do case
  case ii=2.or.ii=4
   str_prn=strfrm(format_str[ii])
   if .not.mprints(str_prn)
    return
   endif
  case ii=3
   do while (.not.eof())
    if delete().or.Mark = 0
     MsgWrite("Обpабатываю лицевой счет - "+PAY2)
     FIO1=padr(psubstr(PAY2,1)+" "+psubstr(PAY2,2),25)
     FIO2=padr(psubstr(PAY2,3),25)
     ADR1=padr(psubstr(PAY5,1),25)
     ADR2=padr(psubstr(PAY5,2)+" "+psubstr(PAY5,3),25)
     PAS1=padr(psubstr(PAY6,1)+" "+psubstr(PAY6,2),20)
     PAS2=padr(psubstr(PAY6,3)+" "+psubstr(PAY6,4),20)
     acco_PAY2=PAY2
     for iii=1 to SIZE_COME
      var=cod[iii]
      dF&var.=nul_sum
     next
     cur_Nz=recno()
     select 1
     use
     for i=bm to em
      fld="M"+ltrim(str(i))
      for j=2 to karta_len
       var=ltrim(str(j))
       ptr=fld+"C"+var
       &ptr.=nul_sum
      next
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
        fld="M"+ltrim(str(i))
        for j=2 to karta_len
         var=ltrim(str(j))
         if .not.(empty(M->KARTA&var))
          ptr=fld+"C"+var
          &ptr.=formula(M->KARTA&var)
         endif
        next
        for iii=1 to SIZE_COME
         var=cod[iii]
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
     select 9
     use &new_config
     for j=2 to karta_len
      var="C"+ltrim(str(j))
      itogo=0
      for i=bm to em
       ptr="M"+ltrim(str(i))+var
       itogo=itogo+&ptr.
      next
      ptr="M13"+var
      repl I->FORMAT with itogo
      &ptr.=I->FORMAT
     next
     OpenDBF()
     go cur_Nz
     str_prn=strfrm(format_str[ii])
     if .not.mprints(str_prn)
      go Nz
      return
     endif
    endif
    NN++
    if Mark != 0
      skip
    else
      exit
    endif
    if currprow >= I->MAXPROW
      exit
    endif
   enddo
  endcase
 next
 if currprow >= I->MAXPROW
   PrintChar(chr(12))
 endif
 if Mark = 0
   exit
 endif
enddo
go Nz
ii=5
str_prn=strfrm(format_str[ii])
mprints(str_prn)
return
*---------------------------------------------------------*
procedure Otch1LPT1
private old_color,old_select,old_Nz,Nz,ch,otwet,ptr,fld,var,str_prn,ITOG
MsgWrite("Расчитываю...")
select 8
use come.pay
select 2
create temp.str
use temp.str
append blank
repl FIELD_NAME with I->SVOD_NAME,FIELD_TYPE with I->SVOD_TYPE,;
  FIELD_LEN with I->SVOD_LEN,FIELD_DEC with I->SVOD_DEC
select 8
do while .not.eof()
  select 2
  append blank
  repl FIELD_NAME with "SUM"+ltrim(str(H->COME_COD)),FIELD_TYPE with "N",;
       FIELD_LEN with len(str(I->FORMAT)),;
       FIELD_DEC with iif(at(".",str(I->FORMAT))=0,0,len(substr(str(I->FORMAT),rat(".",str(I->FORMAT))+1)))
  select 8
  skip
enddo
select 8  && надо
use
select 2
use
select 2
create temp.dbf from temp.str
use temp.dbf
cur_date=I->T_DATE
for i=bm to em
 acco_path=cur_path+"\"+str(year(cur_date),4)+"."+strtran(str(i,2)," ","0")+"\"
 use_dbf=acco_path+MainDBF
 if file(use_dbf)
  select 1
  use &use_dbf
  ptr=alltrim(I->SVOD_NAME)
  index on &ptr. to temp.ntx
  name_filter=alltrim(I->L_FILTER)
  if .not.empty(name_filter)
   set filter to &name_filter
  endif
  go top
  do while .not.eof()
   select 2
   locate for B->&ptr. = A->&ptr.
   if .not.found()
    append blank
    repl B->&ptr. with A->&ptr.
   endif
   select 1
   do while (B->&ptr.=A->&ptr.).and.(.not.eof())
    for ii=1 to I->MAX_COME
     fld=ltrim(str(ii))
     var=ltrim(str(A->COD&fld))
     if A->COD&fld. !=0 .and. type("M->cod&var") != "U"
      repl B->SUM&var with B->SUM&var.+A->SUM&fld.
     endif
    next
    skip
   enddo
  enddo
  select 1
  use
 endif
next
select 2
do case
case .not.empty(I->SVOD_IIF)
 var=alltrim(I->SVOD_IIF)
case .not.empty(I->SVOD_DBF)
 var="mdict('"+alltrim(I->SVOD_SCR)+"',B->"+ptr+",'"+alltrim(I->SVOD_COD)+;
     "','"+alltrim(I->SVOD_DBF)+"','')"
othe
 var=ptr
endcase
len_val1=lenvar(&var)
len_val2=lenvar(nul_sum)
len_table=lenvar(nul_nam)+(lastrec()+1)*(iif(len_val1>=len_val2,len_val1,len_val2)+1)
* фоpмиpование таблицы
str_file=memoread("otch1.prn")
str_prn=""
i_min=at(chr(1),str_file)
i_max=rat(chr(1),str_file)
if i_min != i_max.and.i_min != (i_max - 1)
  str_prn=strfrm(substr(str_file,i_min+1,i_max-i_min-1))
endif
* 1 строка
str_prn=str_prn+replicate("-",len_table)+chr(13)+chr(10)
* 2 строка
str_prn=str_prn+nul_nam
go top
do while .not.eof()
  str_prn=str_prn+"!"+iif(len_val1>=len_val2,varstr(&var),varstr(&var)+space(len_val2-len_val1))
  skip
enddo
ch="Итого"
str_prn=str_prn+"!"+iif(len(ch)>=len_val2,ch,ch+space(len_val2-len(ch)))
str_prn=str_prn+chr(13)+chr(10)
* 3 строка
str_prn=str_prn+replicate("-",len_table)+chr(13)+chr(10)
* собственно табл.
select 8
use come.pay
do while .not.eof()
  if H->COME_PRN
   var="SUM"+ltrim(str(H->COME_COD))
   select 2
   sum &var to ITOG
   if ITOG != 0
    str_prn=str_prn+H->COME_NAM
    select 2
    go top
    do while .not.eof()
      str_prn=str_prn+"!"+iif(len_val2>=len_val1,strnum(&var),space(len_val1-len_val2)+strnum(&var))
      skip
    enddo
    repl I->FORMAT with ITOG
    str_prn=str_prn+"!"+iif(len_val2>=len_val1,strnum(I->FORMAT),space(len_val1-len_val2)+strnum(I->FORMAT))
    str_prn=str_prn+chr(13)+chr(10)
   endif
   select 8
  endif
  skip
enddo
select 8
use
* строка
str_prn=str_prn+replicate("-",len_table)+chr(13)+chr(10)
*** 3 - END строка
i_min=at(chr(5),str_file)
i_max=rat(chr(5),str_file)
if i_min != i_max.and.i_min != (i_max - 1)
  str_prn=str_prn+strfrm(substr(str_file,i_min+1,i_max-i_min-1))
endif
****
select 2
use
select 1
use
dele file temp.dbf
dele file temp.str
dele file temp.ntx
***
str_prn=formprn(@str_prn,I->LEN_LPT1,I->IS_LIST)
ed(Y_min,X_min,Y_max,X_max,@str_prn)
***
return
*---------------------------------------------------------*
procedure Otch2LPT1
private str_file,str_prn,is_prn,Nz
MsgWrite("Расчитываю...")
str_file=memoread("otch2.prn")
if empty(str_file)
  return
endif
NN=1
Nz=recno()
if Mark != 0
  go top
endif
for j=2 to karta_len
 vac="A"+ltrim(str(j))
 &vac.=0
next
select 8
use come.pay				&& Начисления/Удержания
SIZE_COME=reccount()
decl cod[SIZE_COME]
for i=1 to SIZE_COME
 cod[i]=ltrim(str(H->COME_COD))
 skip
next
use
fld="M0"
for j=2 to karta_len
 var=ltrim(str(j))
 ptr=fld+"C"+var
 &ptr.=nul_sum
next
for i=1 to 12
 fld="M"+ltrim(str(i))
 for j=2 to karta_len
  var=ltrim(str(j))
  ptr=fld+"C"+var
  &ptr.=nul_sum
 next
next
cur_date=I->T_DATE
select 1
name_filter=alltrim(I->L_FILTER)
go top
do while (.not.eof())
  MsgWrite("Обpабатываю лицевой счет - "+PAY2)
  acco_PAY2=PAY2
  cur_Nz=recno()
  CloseDBF()
  for ii=1 to SIZE_COME
   var=cod[ii]
   dF&var.=nul_sum
  next
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
     fld="M"+ltrim(str(i))
     for j=2 to karta_len
      var=ltrim(str(j))
      vac="A"+ltrim(str(j))
      if .not.(empty(M->KARTA&var))
       ptr=fld+"C"+var
       repl I->FORMAT with &ptr.+formula(M->KARTA&var)
       &ptr.=I->FORMAT
       &vac.=I->FORMAT
      endif
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
  select 9
  use &new_config
  for j=2 to karta_len
   var="C"+ltrim(str(j))
   itogo=0
   for i=1 to 12
    ptr="M"+ltrim(str(i))+var
    itogo=itogo+&ptr.
   next
   ptr="M13"+var
   repl I->FORMAT with itogo
   &ptr.=I->FORMAT
  next
  OpenDBF()
  InitFilter()
  go cur_Nz
 skip
 NN=NN+1
enddo
select 9
use &new_config
InitConfig()
select 1
go Nz
***
MsgWrite("С о з д а ю ...")
str_prn=strfrm(@str_file)
str_prn=formprn(@str_prn,I->LEN_LPT1,I->IS_LIST)
ed(Y_min,X_min,Y_max,X_max,@str_prn)
***
return
