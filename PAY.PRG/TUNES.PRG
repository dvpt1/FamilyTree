*---------------------------------------------------------*
function Tune
parameters Y1,X1,Y2,X2
private otwet,old_color
old_color = setcolor()
save screen to OnePage
setcolor(cmenu)
DrawBox(Y1,X1,Y2,X2,ibox)
setcolor(cl)
MSG=' Н А С Т Р О Й К А '
@ Y1,int((X1+X2)/2-len(MSG)/2) say MSG
keyboard chr(27)
TuneNU(Y1+9,X_srm+2,Y2-1,X2-2)
otwet=1
do while (otwet != 0)
   setcolor(cmenu)
   @ Y1+1,X1+2 say "Hазвание пpедпpиятия:"
   @ row(),col() prompt I->W_NAME
   @ Y1+2,X1+2 say "Адpес пpедпpиятия:"
   @ row(),col() prompt I->ADDRES
   @ Y1+3,X1+2 say "Диpектоp:"
   @ row(),col() prompt I->G_DIR
   @ Y1+3,X1+31 say "Бухгалтеp:"
   @ row(),col() prompt I->G_BUH
   @ Y1+3,X1+61 say "Телефон:"
   @ row(),col() prompt I->TELEFON
   @ Y1+4,X1+2 say "ИHH:"
   @ row(),col() prompt I->INN
   @ Y1+4,X1+20 say "Л/С:"
   @ row(),col() prompt I->N_LS
   @ Y1+4,X1+50 say "P/С:"
   @ row(),col() prompt I->N_RS
   @ Y1+5,X1+2 say "Код стpаны:"
   @ row(),col() prompt I->CODSTR
   @ Y1+5,X1+31 say "Код pегиона:"
   @ row(),col() prompt I->CODREG
   @ Y1+6,X1+2 say "Банк:"
   @ row(),col() prompt I->N_BANK
   @ Y1+6,X1+38 say "Hалоговая:"
   @ row(),col() prompt I->N_NALOG
   @ Y1+7,X1+2 say "Hеоблагаемый минимум:"
   @ row(),col() prompt varstr(I->N_MO)
   @ Y1+7,X1+36 say "Дни:"
   @ row(),col() prompt varstr(I->N_DAY)
   @ Y1+7,X1+52 say "Часы:"
   @ row(),col() prompt varstr(I->N_CLOCK)
   @ Y1+8,X1+2 say "Платежная ведомость:"
   var=ltrim(str(I->COME_PRN1))
   @ Y1+9,X1+2 prompt iif(type("cod&var")!="U",nam&var,nul_nam)
   @ Y1+10,X1+2 say "Расходный кассовый ордер:"
   var=ltrim(str(I->KASSA_PRN))
   @ Y1+11,X1+2 prompt iif(type("cod&var")!="U",nam&var,nul_nam)
   @ Y1+12,X1+2 say "Сводная ведомость:"
   @ Y1+13,X1+2 prompt I->NAME_SVOD
   @ Y1+14,X1+2 say "Суммы за 12 месяцев:"
   @ Y1+15,X1+2 prompt substr(I->FSUMMA,1,30)
   @ Y1+16,X1+2 prompt substr(I->DSUMMA,1,30)
   @ Y1+17,X1+2 say "Совокупный доход:"
   @ row(),col() prompt substr(I->SOVOKUP,1,20)
   @ Y1+18,X1+2 say "Частное H/У:"
   var=ltrim(str(I->CHASTN_NU))
   @ row(),col() prompt iif(type("cod&var")!="U",nam&var,nul_nam)
   @ Y1+19,X1+2 say "Окpуглить до:"
   @ row(),col()+1 prompt varstr(I->NUM_DIV)
   @ Y1+20,X1+2 say "Количество строк:"
   @ row(),col()+1 prompt varstr(I->MAXPROW)
   @ Y1+21,X1+2 say "Шиpина печати:"
   @ row(),col()+1 prompt varstr(I->LEN_LPT1)
   @ Y1+22,X1+2 prompt "Перевод листа:"
   @ row(),col()+1 say iif(I->IS_LIST,"ДА ","НЕТ")
   @ Y1+23,X1+2 say "Pежим pаботы"
   @ row(),col()+1 prompt iif(is_arhiv,"СМОТPЕТЬ","ВВОДИТЬ ")
   @ Y1+8,X_srm+2 prompt "Список общих Н/У"
   menu to otwet
   if otwet!=0
     do case
       case otwet=1
         set cursor on
         @ Y1+1,X1+23 get I->W_NAME
         read
         set cursor off
       case otwet=2
         set cursor on
         @ Y1+2,X1+20 get I->ADDRES
         read
         set cursor off
       case otwet=3
         set cursor on
         @ Y1+3,X1+11 get I->G_DIR
         read
         set cursor off
       case otwet=4
         set cursor on
         @ Y1+3,X1+41 get I->G_BUH
         read
         set cursor off
       case otwet=5
         set cursor on
         @ Y1+3,X1+69 get I->TELEFON
         read
         set cursor off
       case otwet=6
         set cursor on
         @ Y1+4,X1+6 get I->INN
         read
         set cursor off
       case otwet=7
         set cursor on
         @ Y1+4,X1+24 get I->N_LS
         read
         set cursor off
       case otwet=8
         set cursor on
         @ Y1+4,X1+54 get I->N_RS
         read
         set cursor off
       case otwet=9
         set cursor on
         @ Y1+5,X1+13 get I->CODSTR
         read
         set cursor off
       case otwet=10
         set cursor on
         @ Y1+5,X1+43 get I->CODREG
         read
         set cursor off
       case otwet=11
         set cursor on
         @ Y1+6,X1+7 get I->N_BANK
         read
         set cursor off
       case otwet=12
         set cursor on
         @ Y1+6,X1+48 get I->N_NALOG
         read
         set cursor off
       case otwet=13
         set cursor on
         @ Y1+7,X1+23 get I->N_MO
         read
         mo=I->N_MO
         set cursor off
       case otwet=14
         set cursor on
         @ Y1+7,X1+40 get I->N_DAY
         read
         wd=I->N_DAY
         set cursor off
       case otwet=15
         set cursor on
         @ Y1+7,X1+57 get I->N_CLOCK
         read
         wd=I->N_CLOCK
         set cursor off
       case otwet=16
         repl I->COME_PRN1 with FindCome(I->COME_PRN1)
       case otwet=17
         repl I->KASSA_PRN with FindCome(I->KASSA_PRN)
       case otwet=18
         FindAcco()
       case otwet=19
         set cursor on
         @ Y1+15,X1+2 get I->FSUMMA picture "@S30"
         read
         set cursor off
         if updated()
          summaf=upper(strtran(I->FSUMMA," "))
         endif
       case otwet=20
         set cursor on
         @ Y1+16,X1+2 get I->DSUMMA picture "@S30"
         read
         set cursor off
         if updated()
          summad=upper(strtran(I->DSUMMA," "))
         endif
       case otwet=21
         set cursor on
         @ Y1+17,X1+19 get I->SOVOKUP picture "@S20"
         read
         set cursor off
       case otwet=22
         repl I->CHASTN_NU with FindCome(I->CHASTN_NU)
       case otwet=23
         set cursor on
         @ Y1+19,X1+16 get I->NUM_DIV
         read
         ok=I->NUM_DIV
         set cursor off
       case otwet=24
         set cursor on
         @ Y1+20,X1+20 get I->MAXPROW
         read
         set cursor off
       case otwet=25
         set cursor on
         @ Y1+21,X1+17 get I->LEN_LPT1
         read
         set cursor off
       case otwet=26
         repl I->IS_LIST with .not.(I->IS_LIST)
       case otwet=27
         is_arhiv=.not.(is_arhiv)
       case otwet=28
         TuneNU(Y1+9,X_srm+2,Y2-1,X2-2)
     endcase
   endif
enddo
restore screen from OnePage
set color to (old_color)
return " "
*---------------------------------------------------------*
function FindCome
parameters retr_var
private old_color,old_select
save screen
old_color=setcolor(cdict)
BoxTenj(Y_min+3,X_srm-11,Y_max-3,X_srm+11,ibox)
declare name_var[1],name_fld[1]
name_var[1]="Список Н/У"
name_fld[1]="COME_NAM"
old_select=select()
select 8
use come.pay
dbedit(Y_min+4,X_srm-10,Y_max-4,X_srm+10,name_fld,"dbFind","",name_var)
retr_var=iif(lastkey()=13,H->COME_COD,retr_var)
use
select(old_select)
setcolor(old_color)
restore screen
return retr_var
*---------------------------------------------------------*
function FindAcco
private old_color,old_select
save screen
old_color=setcolor(cdict)
BoxTenj(Y_min+3,X_srm-16,Y_max-3,X_srm+16,ibox)
declare name_var[1],name_fld[1]
name_var[1]="Список ЛС"
name_fld[1]="NAME_FIELD"
old_select=select()
select 8
use account.pay
dbedit(Y_min+4,X_srm-15,Y_max-4,X_srm+15,name_fld,"dbFind","",name_var)
if lastkey()=13
  repl I->SVOD_NAME with H->FIELD_NAME,I->NAME_SVOD with H->NAME_FIELD,;
       I->SVOD_TYPE with H->FIELD_TYPE,I->SVOD_LEN with H->FIELD_LEN,;
       I->SVOD_DEC with H->FIELD_DEC
  repl I->SVOD_IIF with H->IIF_STR,I->SVOD_DBF with H->DICT_DBF,;
       I->SVOD_NTX with H->DICT_NTX,I->SVOD_COD with H->FIELD_COD,;
       I->SVOD_SCR with H->FIELD_SCR
endif
use
select(old_select)
setcolor(old_color)
restore screen
return " "
*---------------------------------------------------------*
function TuneNU
parameters Y11,X11,Y22,X22
private otwet,var,fld,pic,nucount,log_clr
set color to &cdict
nucount=0
i=1
do while i <= I->MAX_COME
  fld=ltrim(str(i))
  if I->COD&fld != 0
    var=ltrim(str(I->COD&fld))
    if type("cod&var") != "U"
      cod&var.=I->COD&fld
      nucount=nucount+1
    else
      iDelNU(i)  && Удал. неизв. Н/У
      loop
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
      var=ltrim(str(I->COD&fld))
      come_edit[i]=" "+str(I->COD&fld)+" "+nam&var
    next
  else
    declare come_edit[1]
    come_edit[1]=" "
  endif
  if log_clr
    log_clr=.F.
    @ Y11,X11 clear to Y22,X22
  endif
  otwet=achoice(Y11,X11,Y22,X22,come_edit,"","NuFunc",otwet)
  release come_edit  && очистить
enddo
if nucount != 0
  for i=1 to nucount
    fld=ltrim(str(i))
    var=ltrim(str(I->COD&fld))
    cod&var.=nul_cod
  next
endif
return " "
*---------------------------------------------------------*
function NuFunc
parameters mode,ptr
private old_color,var,fld,old_select
do case
  case mode=0
    return 2
  case lastkey()=27
    return 0
  case lastkey()=22   && INS
    if nucount < I->MAX_COME
      old_select=select()
      select 8
      use come.pay
      save screen
      old_color=setcolor(cdict)
      BoxTenj(Y_min+3,X_srm-11,Y_max-3,X_srm+11,ibox)
      declare name_var[1],name_fld[1]
      name_var[1]="Список Н/У"
      name_fld[1]="COME_NAM"
      dbedit(Y_min+4,X_srm-10,Y_max-4,X_srm+10,name_fld,"dbFind","",name_var)
      setcolor(old_color)
      restore screen
      if lastkey()=13.and.H->COME_COD != 0
        var=ltrim(str(H->COME_COD))
        if cod&var. = 0
          log_clr=.T.
          cod&var.=H->COME_COD
          iInsNU()
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
    if nucount != 0
      if Fragen('','Удалить строку Н/У?')
        log_clr=.T.
        fld=ltrim(str(ptr))
        var=ltrim(str(I->COD&fld))
        cod&var.=nul_cod
        iDelNU(ptr)
        nucount=nucount-1
        return 1
      endif
    endif
endcase
return 2
*---------------------------------------------------------*
function iInsNU
private ii,var,fld
ii=1
go top
do while .not.eof()
  var=ltrim(str(H->COME_COD))
  if cod&var. != 0
    fld=ltrim(str(ii))
    repl I->COD&fld with H->COME_COD
    ii=ii+1
  endif
  skip
enddo
return " "
*---------------------------------------------------------*
function iDelNU
parameters i
private ii,var,fld
for ii=i to I->MAX_COME
  var=ltrim(str(ii))
  if (ii+1) <= I->MAX_COME
    fld=ltrim(str(ii+1))
    repl I->COD&var with I->COD&fld
  else
    repl I->COD&var with nul_cod
  endif
next
return " "
