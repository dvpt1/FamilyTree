*----------------------------------------Конюхов Д.Л.-----*
*                   З А Р П Л А Т А                       *
*---------------------------------------------------------*
*=========================================================*
*                     CLIPPER  2.0                        *
*=========================================================*
external dbfind,psubstr,f_i_o
external mdict,strnum,strnuma,snum,dnum
*---------------------------------------------------------*
#define F_NAME 1
#define F_SIZE 2
#define F_DATE 3
#define F_TIME 4
#define F_ATT  5
*---------------------------------------------------------*
public MainDBF,MainSTR,MainPAY
store 'pay.dbf' to MainDBF
store 'pay.str' to MainSTR
store 'config.pay' to MainPAY
public wd
public wc
public nm
public mo
public ok
public summaf
public summad
public pic_sum
public nul_sum
public is_arhiv
*---------------------------------------------------------*
parameters param1
if type("param1") <> "C"
  param1 = ""
endif
set decimals to 2
**********************
***InitTest()
**********************
karta_len=14
OpenMouse()
InitSets()
InitVars()
InitColor(param1)
InitFormul()
InitCalc()
InitPays()
********************************
*name_firm = __pinit(direxe()+'PAY.',__panti())
********************************
cur_path=getdir()
cur_config=cur_path+"\config.pay"
redraw=.T.
PrgExit=.T.
do while PrgExit
 if redraw
  select 9
  use &cur_config
  is_arhiv=IsArhiv()
  bal_path=cur_path+"\"+str(year(I->T_DATE),4)+"."+strtran(str(month(I->T_DATE),2)," ","0")
  use_path=bal_path+"\"
  set path to &bal_path
  mkdir(bal_path)
  new_config=bal_path+"\config.pay"
  if .not.file(new_config)
   copy file &cur_config to &new_config
  endif
  use &new_config
  InitConfig()
  OpenDBF(upper(param1)="/A")
  DrawPay(Y_min,X_min,Y_max-2,X_max-24)
  DrawDate(Y_min,X_max-24,Y_min+2,X_max)
  DrawMenu(Y_min+2,X_max-24,Y_max,X_max)
  DrawFilter(Y_max-2,X_min,Y_max,X_max-24)
  redraw=.F.
 endif
 set color to &c
 dbedit(Y_min+1,X_min+1,Y_max-3,X_max-25,jour_jou,"JourFunc","",jour_nam)
enddo
CloseDBF()         && закрыть
set color to w/n
clear
set cursor On
CloseMouse()
Quit    && Выход из программы

*---------------------------------------------------------*
function JourFunc
parameters mode,fld_ptr
private var_fld    && Редактируемое поле
RETR=1
do case
case mode = 0
  @ Y_max, 2 say '██████████████'
  if mark != 0
    setcolor(cl)
    @ Y_max, 2 say "Помечено "+ltrim(str(Mark))
    setcolor(c)
  endif
case mode = 3
case mode = 4
do case
case (lastkey() = -4)       && F5
  ComeTable()
case (lastkey() = -5)       && F6
  MenuDict(Y_srm-6,X_srm-11,Y_srm+SIZE_DICT-5,X_srm+10,'\GNI\')
case (lastkey() = 32)       && SPACE
  if delete()
    recall
    Mark=Mark-1
  else
    delete
    Mark=Mark+1
  endif
  @ Y_max, 2 say '██████████████'
  if mark != 0
    setcolor(cl)
    @ Y_max, 2 say "Помечено "+ltrim(str(Mark))
    setcolor(c)
  endif
case (lastkey() = 45)       && -
  SelePay(.F.,fld_ptr,Y_srm-3,X_srm-30,Y_srm+3,X_srm+30)
case (lastkey() = 43)       && +
  SelePay(.T.,fld_ptr,Y_srm-3,X_srm-30,Y_srm+3,X_srm+30)
case (lastkey() = -9)       && F10
  Tune(Y_min,X_min,Y_max,X_max)
case (lastkey() = -8)       && F9
  save screen to OnePage
  InGNI()
  restore screen from OnePage
case (lastkey() = 96).or.(lastkey() = 126)       && ` or ~
  save screen to OnePage
  OldArhiv()
  restore screen from OnePage
case (lastkey() = 27)       && ESC
  if Fragen(' В Ы Х О Д ','Завершить работу с программой?')
    PrgExit=.F.
    return 0
  endif
endcase
endcase
return RETR
*---------------------------------------------------------*
procedure OpenDBF
parameters log
private old_color,old_select
if pcount()=0
 log=.F.
endif
old_color = setcolor(cw)
save screen
for i=1 to SIZE_DICT
 name_dbf=use_path+dict_dbf[i]
 if .not.file(name_dbf)
  name_str=dict_str[i]
  select 1
  creat &name_dbf from &name_str
  use
 endif
next
for i=1 to SIZE_DICT
 if .not.empty(dict_ntx[i])
   if .not.file(dict_ntx[i]).or.log
     name_dbf=use_path+dict_dbf[i]
     name_ntx=use_path+dict_ntx[i]
     name_key=dict_key[i]
     BoxTenj(Y_srm-2,X_srm-25,Y_srm+2,X_srm+25,ibox)
     @ Y_srm-2,X_srm-5 say 'Сортирую...'
     @ Y_srm,int(X_srm-len(name_ntx)/2) say name_ntx
     select 8
     use &name_dbf
     index on &name_key to &name_ntx
     use
   endif
  endif
next
select 1
use_dbf=use_path+MainDBF
if file(use_dbf)
 use &use_dbf
else
 creat &use_dbf from &MainSTR
endif
for i=1 to SIZE_INDX
  use_ntx=use_path+index_ntx[i]
  if (.not.file(use_ntx)).or.log
    BoxTenj(Y_srm-2,X_srm-25,Y_srm+2,X_srm+25,ibox)
    @ Y_srm-2,X_srm-5 say 'Сортирую...'
    @ Y_srm,int(X_srm-len(use_ntx)/2) say use_ntx
    key_ntx = index_key[i]
    index on &key_ntx to &use_ntx
  endif
next
go top
for i=1 to SIZE_INDX
  ch = ltrim(str(i))
  Fl_NTX&ch = use_path+index_ntx[i]
next
do case
   case SIZE_INDX=1
     set index to &Fl_NTX1
   case SIZE_INDX=2
     set index to &Fl_NTX1,&Fl_NTX2
endcase
set order to I->REGIM_SORT
restore screen
setcolor(old_color)
return
*---------------------------------------------------------*
function DrawPay
parameters Y1,X1,Y2,X2
set color to &c
DrawBox(Y1,X1,Y2,X2,ibox)
set color to &cl
MSG=' Ж У Р Н А Л  Л И Ц Е В Ы Х  С Ч Е Т О В '
@ Y1,int((X1+X2)/2-len(MSG)/2) say MSG
return " "
*---------------------------------------------------------*
function DrawDate
parameters Y1,X1,Y2,X2
set color to &c
DrawBox(Y1,X1,Y2,X2,ibox)
set color to &cl
MSG=' ТЕКУЩАЯ ДАТА '
@ Y1,int((X1+X2)/2-len(MSG)/2) say MSG
set color to &c
MSG=alltrim(Month[month(I->T_DATE)])+" "+str(year(I->T_DATE),4)+" г."
@ Y1+1,int((X1+X2)/2-len(MSG)/2)+1 say MSG
return " "
*---------------------------------------------------------*
function DrawMenu
parameters Y1,X1,Y2,X2
set color to &c
DrawBox(Y1,X1,Y2,X2,'████████░')
set color to &cl
MSG=' М Е Н Ю '
@ Y1,int((X1+X2)/2-len(MSG)/2) say MSG
set color to &cmenu
MSG='F5'
@ row()+1,X1+2 get MSG
@ row(),col()  say '  Таблица Н/У      '
MSG='F6'
@ row()+1,X1+2 get MSG
@ row(),col()  say '  Словарь          '
MSG='F10'
@ row()+1,X1+2 get MSG
@ row(),col()  say ' Настройка        '
MSG='F9'
@ row()+1,X1+2 get MSG
@ row(),col()  say '  В ГНИ            '
MSG='SPACE'
@ row()+1,X1+2 get MSG
@ row(),col()  say ' Пометить       '
MSG='+'
@ row()+1,X1+2 get MSG
@ row(),col()  say '   Селектировать    '
MSG='-'
@ row()+1,X1+2 get MSG
@ row(),col()  say '   Разселектировать '
MSG='Tab'
@ row()+1,X1+2 get MSG
@ row(),col()  say ' Калькулятор      '
MSG='`~'
@ row()+1,X1+2 get MSG
@ row(),col()  say '  Переключить      '
MSG='ESC'
@ row()+1,X1+2 get MSG
@ row(),col()  say ' Выход            '
clear gets
return " "
*---------------------------------------------------------*
function DrawFilter
parameters Y1,X1,Y2,X2
set color to &c
DrawBox(Y1,X1,Y2,X2,ibox)
set color to &cl
MSG=' П О Д Р А З Д Е Л Е H И Е '
@ Y1,int((X1+X2)/2-len(MSG)/2) say MSG
InitFilter()
MSG=alltrim(I->N_FILTER)
set color to &c
if len(MSG) < (X2-X1-1)
  @ Y1+1,int((X1+X2)/2-len(MSG)/2) say MSG
else
  @ Y1+1,X1+2 say substr(MSG,1,X2-X1-1)
endif
return " "
*---------------------------------------------------------*
procedure InitFilter
name_filter=alltrim(I->L_FILTER)
if empty(name_filter).or.SIZE_FILTER=0
  replace I->N_FILTER with " "
  set filter to
else
  set filter to &name_filter
endif
go top
return
*---------------------------------------------------------*
function CloseDBF
MarkRecall()
select 1
use
return " "
*---------------------------------------------------------*
function MarkRecall
if Mark # 0
  save screen
  MsgWrite(' М и н у т к у ...  Удаляю метки ... ')
  recall for delete()
  Mark = 0
  restore screen
endif
return " "
*---------------------------------------------------------*
procedure inGNI
bm=0
em=0
kv=0
if WhoKW()=0
 return
endif
disk=diskABC()
if empty(disk)
  return
endif
old_Nz=recno()
nnNN=1
if Mark != 0
  go top
endif
cur_date=I->T_DATE	&& Имеется в виду ГОД
#define LEN_MESEZ 12
#define LEN_DOHOD 8
#define LEN_RASOD 8
#define LEN_DSXTN 12
#define LEN_ISPOLN 15
handle=fopen("gni.srl",0)
buffer=space(8)
count=fread(handle,@buffer,8)
if count=0
 fcreate("gni.srl")
 buffer=space(8)
endif
gg=val(substr(buffer,1,4))
mm=val(substr(buffer,5,2))
nn=val(substr(buffer,7,2))
if year(I->T_DATE)=gg
 if month(I->T_DATE)!=mm
  nn++
 endif
else
 gg=year(I->T_DATE)
 mm=month(I->T_DATE)
 nn=1
endif
fclose(handle)
handle=fopen("gni.srl",1)
buffer=str(gg,4)+str(mm,2)+str(nn,2)+chr(13)+chr(10)
fwrite(handle,buffer)
fclose(handle)
sovoku=upper(strtran(I->SOVOKUP," "))
f_name=iif(disk=="C:",'',disk+"\")+'S'+substr(I->INN,5,5)+strtran(str(nn,2),' ','0')+'.txt'
handle=fcreate(f_name)
buffer='ИдФайл:'+strtran(I->INN,' ','*')+strtran(time(),':','')+;
       strtran(substr(str(year(date()),4),3,2)+;
       str(month(date()),2)+str(day(date()),2),' ','0')+;
       chr(13)+chr(10)
fwrite(handle,buffer)
buffer='ТипИнф:'+'ДОХОД'+chr(13)+chr(10)
fwrite(handle,buffer)
buffer='НаимОтпрЮЛ:'+alltrim(I->W_NAME)+chr(13)+chr(10)
fwrite(handle,buffer)
buffer='ТелОтпр:'+alltrim(I->TELEFON)+chr(13)+chr(10)
fwrite(handle,buffer)
buffer='АдрОтпр:'+alltrim(I->ADDRES)+chr(13)+chr(10)
fwrite(handle,buffer)
buffer='ДолжнОтпр:'+'БУХГАЛТЕР'+chr(13)+chr(10)
fwrite(handle,buffer)
buffer='ФИООтпр:'+alltrim(I->G_BUH)+chr(13)+chr(10)
fwrite(handle,buffer)
buffer='КолДок:'+alltrim(str(mark))+chr(13)+chr(10)
fwrite(handle,buffer)
buffer='ВерсПрог:'+'КОАПП-ЗАРПЛАТА (с) 10.1998'+chr(13)+chr(10)
fwrite(handle,buffer)
select 8
name_dbf=cur_path+"\GNI\"+"dict7.dbf"
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
name_dbf=cur_path+"\GNI\"+"dict8.dbf"
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
name_dbf=cur_path+"\GNI\"+"dict9.dbf"
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
name_dbf=cur_path+"\GNI\"+"dict12.dbf"
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
  select 9
  use &new_config
  OpenDBF()
  go cur_Nz
  buffer='ИдДок:'+strtran(I->INN,' ','*')+;
       strtran(substr(str(year(date()),4),3,2)+str(nnNN,8),' ','0')+;
       chr(13)+chr(10)
  fwrite(handle,buffer)
  buffer='ДатаДок:'+strtran(str(day(date()),2)+'.'+str(month(date()),2)+'.'+str(year(date()),4),' ','0')+chr(13)+chr(10)
  fwrite(handle,buffer)
  buffer='ИННФЛ:'+PAY30+chr(13)+chr(10)
  fwrite(handle,buffer)
  buffer='ФИО:'+strtran(upper(alltrim(PAY2))," ",",")+chr(13)+chr(10)
  fwrite(handle,buffer)
  buffer='УдЛичн:'+alltrim(PAY23)+","+psubstr(PAY6,1)+" "+psubstr(PAY6,2)+chr(13)+chr(10)
  fwrite(handle,buffer)
  buffer='ДатаРожд:'+strtran(str(day(PAY4),2)+'.'+str(month(PAY4),2)+'.'+str(year(PAY4),4),' ','0')+chr(13)+chr(10)
  fwrite(handle,buffer)
  buffer='АдрМЖ:'+upper(alltrim(PAY5))+chr(13)+chr(10)
  fwrite(handle,buffer)
  buffer='СтатусФЛ:'+alltrim(PAY27)+chr(13)+chr(10)
  fwrite(handle,buffer)
  buffer='МестоДох:'+alltrim(str(PAY21))+chr(13)+chr(10)
  fwrite(handle,buffer)
  ch='000000000000'
  for ii=1 to 12
   nn=val(psubstr(period,ii))
   if nn!=0
    ch=stuff(ch,nn,1,'1')
   endif
  next
  buffer='ПериодДох:'+ch+chr(13)+chr(10)
  fwrite(handle,buffer)
  buffer='ДоходМес:'
  for ii=1 to LEN_MESEZ
   vas="MESZ"+ltrim(str(ii))
   repl I->FORMAT with &vas.
   buffer=buffer+iif(ii=1,'',',')+ltrim(str(I->FORMAT))
  next
  buffer=buffer+chr(13)+chr(10)
  fwrite(handle,buffer)
  buffer='ДоходВид:'
  ISUMD=0
  for ii=1 to LEN_DOHOD
   vas="SUMD"+ltrim(str(ii))
   if &vas.!=0
    vac="CODD"+ltrim(str(ii))
    buffer=buffer+alltrim(&vac)+','
    repl I->FORMAT with &vas.
    buffer=buffer+iif(ii=1,'',',')+ltrim(str(I->FORMAT))
    ISUMD=ISUMD+&vas
   endif
  next
  buffer=buffer+chr(13)+chr(10)
  fwrite(handle,buffer)
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
  buffer='Вычет:'
  ISUMV=0
  iil=.F.
  for ii=1 to LEN_DSXTN
   vas="SUMV"+ltrim(str(ii))
   if &vas.!=0
    if iil
     buffer=buffer+';'
    else
     iil=.T.
    endif
    vac="CODV"+ltrim(str(ii))
    buffer=buffer+alltrim(&vac)+','
    repl I->FORMAT with &vas.
    buffer=buffer+ltrim(str(I->FORMAT))
    ISUMV=ISUMV+I->FORMAT
   endif
  next
  buffer=buffer+chr(13)+chr(10)
  fwrite(handle,buffer)
  ISUMI=0
  for ii=1 to LEN12
   vas="SUMI"+ltrim(str(ii))
   repl I->FORMAT with &vas.
   &vas.=strnum(I->FORMAT)
   ISUMI=ISUMI+I->FORMAT
  next
  repl I->FORMAT with ISUMV
  ISUMV=strnum(I->FORMAT)
  buffer='СкидСумм:'+alltrim(ISUMC)+chr(13)+chr(10)
  fwrite(handle,buffer)
  buffer='ВычСумм:'+alltrim(ISUMV)+chr(13)+chr(10)
  fwrite(handle,buffer)
  buffer='ВалСумм:'+alltrim(SUMI3)+chr(13)+chr(10)
  fwrite(handle,buffer)
  buffer='ОблСумм:'+alltrim(SUMI4)+chr(13)+chr(10)
  fwrite(handle,buffer)
  buffer='ОблСуммНалИс:'+alltrim(SUMI5)+chr(13)+chr(10)
  fwrite(handle,buffer)
  buffer='ОблСуммНалУд:'+alltrim(SUMI6)+chr(13)+chr(10)
  fwrite(handle,buffer)
  buffer='@@@'+chr(13)+chr(10)
  fwrite(handle,buffer)
 endif
 if Mark != 0
   skip
 else
   exit
 endif
 nnNN=nnNN+1
enddo
buffer='==='+chr(13)+chr(10)
fwrite(handle,buffer)
fclose(handle)
go old_Nz
return
