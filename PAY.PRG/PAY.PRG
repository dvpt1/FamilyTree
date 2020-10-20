*----------------------------------------Конюхов Д.Л.-----*
*                   З А Р П Л А Т А                       *
*---------------------------------------------------------*
*=========================================================*
*                     CLIPPER  2.0                        *
*=========================================================*
external draw_sum,dbfind,psubstr,f_i_o
external mdict,strnum,strnuma,snum,dnum
*---------------------------------------------------------*
#define F_NAME 1
#define F_SIZE 2
#define F_DATE 3
#define F_TIME 4
#define F_ATT  5
*---------------------------------------------------------*
#define LEN_MESEZ 12
#define LEN_DOHOD 8
#define LEN_RASOD 8
#define LEN_DSXTN 12
#define LEN_ISPOLN 15
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
case (lastkey() = 13)       && Enter
  InpCome()
case (lastkey() =  7)       && DEL
  DelPay()
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
case (lastkey() = 22)       && INS
  if NewPay()
    InpAcco()
  endif
case (lastkey() = -1)       && F2
  InpAcco()
case (lastkey() = -2)       && F3
  InpFilter(Y_srm-3,X_srm-30,Y_srm+SIZE_FILTER,X_srm+30)
case (lastkey() = -3)       && F4
  FindPay(fld_ptr,Y_srm-2,X_srm-30,Y_srm+2,X_srm+30)
case (lastkey() = -4)       && F5
  ComeTable()
case (lastkey() = -5)       && F6
  MenuDict(Y_srm-6,X_srm-11,Y_srm+SIZE_DICT-5,X_srm+10,'')
case (lastkey() = -6)       && F7
  MenuIndex(Y_srm-4,X_srm-12,Y_srm+SIZE_INDX,X_srm+11)
case (lastkey() = -7)       && F8
  MenuPrint(Y_srm-7,X_srm-12,Y_srm+7,X_srm+12)
case (lastkey() = -8)       && F9
  InpMonth()
case (lastkey() = -9)       && F10
  Tune(Y_min,X_min,Y_max,X_max)
case (lastkey() = 61)       && =
  Kalkulat()
case (lastkey() = 42)       && *
  Chastnoe()
case (lastkey() = 45)       && -
  SelePay(.F.,fld_ptr,Y_srm-3,X_srm-30,Y_srm+3,X_srm+30)
case (lastkey() = 43)       && +
  SelePay(.T.,fld_ptr,Y_srm-3,X_srm-30,Y_srm+3,X_srm+30)
case (lastkey() = 63)       && ?
  DrawTitle()
case (lastkey() = 96).or.(lastkey() = 126)       && ` or ~
  MenuArhiv(Y_srm-3,X_srm-8,Y_srm+2,X_srm+7)
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
MSG='INS'
@ Y1+ 1,X1+2   get MSG
@ row(),col()  say ' Создать          '
MSG='F2'
@ row()+1,X1+2 get MSG
@ row(),col()  say '  Корректировать   '
MSG='F3'
@ row()+1,X1+2 get MSG
@ row(),col()  say '  Подpазделение    '
MSG='F4'
@ row()+1,X1+2 get MSG
@ row(),col()  say '  Искать           '
MSG='F5'
@ row()+1,X1+2 get MSG
@ row(),col()  say '  Таблица Н/У      '
MSG='F6'
@ row()+1,X1+2 get MSG
@ row(),col()  say '  Словарь          '
MSG='F7'
@ row()+1,X1+2 get MSG
@ row(),col()  say '  Сортировать      '
MSG='F8'
@ row()+1,X1+2 get MSG
@ row(),col()  say '  Печать           '
MSG='F9'
@ row()+1,X1+2 get MSG
@ row(),col()  say '  Сменить месяц    '
MSG='F10'
@ row()+1,X1+2 get MSG
@ row(),col()  say ' Настройка        '
MSG='Enter'
@ row()+1,X1+2 get MSG
@ row(),col()  say '  Расчет H/У    '
MSG='*'
@ row()+1,X1+2 get MSG
@ row(),col()  say '   Частное H/У      '
MSG='='
@ row()+1,X1+2 get MSG
@ row(),col()  say '   Перерасчет H/У   '
MSG='DEL'
@ row()+1,X1+2 get MSG
@ row(),col()  say ' Удалить          '
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
@ row(),col()  say '  Аpхив            '
MSG='?'
@ row()+1,X1+2 get MSG
@ row(),col()  say '   Hемного обо мне..'
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
