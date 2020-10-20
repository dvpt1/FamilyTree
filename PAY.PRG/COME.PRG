*---------------------------------------------------------*
function ComeTable
private old_color,old_select,InpPage,otwet,DoneCome,old_otwet,log_clear
old_color = setcolor()
save screen to InpPage
old_select = select()
select 8
use come.pay
setcolor(c)
DrawBox(Y_min,X_min,Y_max,X_max,ibox)
setcolor(cl)
TITL=' Т А Б Л И Ц А  Н А Ч И С Л Е Н И Й / У Д Е Р Ж А Н И Й '
@ Y_min,int((X_min+X_max)/2-len(TITL)/2) say TITL
MenuTable()
setcolor(c)
declare name_var[9],name_fld[9]
name_var[1]="Дни;B№"
name_var[2]="Часы;C№"
name_var[3]="Базовое;D№"
name_var[4]="Сумма;F№"
name_var[5]="Код;№№"
name_var[6]="Название; "
name_var[7]="Дебет; "
name_var[8]="Кpедит; "
name_var[9]="Печ;ать"
name_fld[1]="COME_DAY"
name_fld[2]="COME_CLO"
name_fld[3]="COME_BAZ"
name_fld[4]="COME_SUM"
name_fld[5]="COME_COD"
name_fld[6]="COME_NAM"
name_fld[7]="DEBET"
name_fld[8]="KREDIT"
name_fld[9]="iif(COME_PRN,'ДА ','HЕТ')"
dbedit(Y_min+1,X_min+1,Y_max-4,X_max-1,name_fld,"TableFunc","",name_var,"───","  ","───")
use
select(old_select)
restore screen from InpPage
setcolor(old_color)
return " "
*---------------------------------------------------------*
function MenuTable
setcolor(cl)
@ Y_max,X_min+1 say 'МЕНЮ:'
setcolor(cmenu)
MSG='Enter'
@ row(),col() get MSG
@ row(),col() say 'Ввод'
MSG='INS'
@ row(),col() get MSG
@ row(),col() say 'Вставь'
MSG='DEL'
@ row(),col() get MSG
@ row(),col() say 'Удали'
MSG='F2'
@ row(),col() get MSG
@ row(),col() say 'Формула'
MSG='F3'
@ row(),col() get MSG
@ row(),col() say 'Оpдеp'
MSG='F4'
@ row(),col() get MSG
@ row(),col() say 'Перевод'
MSG='F5'
@ row(),col() get MSG
@ row(),col() say 'Печать'
MSG='F6'
@ row(),col() get MSG
@ row(),col() say 'Каpта'
MSG='ESC'
@ row(),col() get MSG
@ row(),col() say 'Выход'
clear gets
return " "
*---------------------------------------------------------*
function TableFunc
parameters mode,ptr
private old_color,old_select,var,new_cod,log_prn,key_last
key_last=LASTKEY()
do case
case mode <= 3
  @ Y_max-3,X_min+1 say substr(COME_FRM,1,78)
  @ Y_max-2,X_min+1 say substr(COME_ORD,1,78)
  @ Y_max-1,X_min+1 say substr(COME_PER,1,78)
case lastkey()=27
  return 0
case lastkey()=-1      && F2
  if H->COME_COD != 0
    set cursor on
    @ Y_max-3,X_min+1 get COME_FRM picture "@S78"
    read
    set cursor off
    if updated()
      var=ltrim(str(H->COME_COD))
      frm&var.=upper(strtran(H->COME_FRM," "))
      return 2
    endif
  endif
case lastkey()=-2      && F3
  if H->COME_COD != 0
    set cursor on
    @ Y_max-2,X_min+1 get COME_ORD picture "@S78"
    read
    set cursor off
  endif
case lastkey()=-3      && F4
  if H->COME_COD != 0
    set cursor on
    @ Y_max-1,X_min+1 get COME_PER picture "@S78"
    read
    set cursor off
  endif
case lastkey()=-5      && F6
  old_color = setcolor()
  save screen
  old_select = select()
  select 7
  use karta.pay
  setcolor(cdict)
  BoxTenj(Y_srm-7,X_srm-35,Y_srm+7,X_srm+36,ibox)
  for i=2 to karta_len
   @ Y_srm-8+i,X_srm-34 say str(i,3)+" │"
   var="G->KARTA"+ltrim(str(i))
   @ row(),col() get &var picture "@S30"
   @ row(),col() say str(i,3)+" │"
   var="G->EARTA"+ltrim(str(i))
   @ row(),col() get &var picture "@S30"
  next
  set cursor on
  read
  set cursor off
  if updated()
   for i=2 to karta_len
    var=ltrim(str(i))
    M->KARTA&var.=upper(strtran(G->KARTA&var," "))
    M->EARTA&var.=upper(strtran(G->EARTA&var," "))
   next
  endif
  use
  select(old_select)
  restore screen
  setcolor(old_color)
case LASTKEY() = 13.or.isdata(key_last)
  if ptr != 5.and.H->COME_COD != 0
  do case
  case ptr=9
    repl H->COME_PRN with .not.(H->COME_PRN)
    var=ltrim(str(H->COME_COD))
    prn&var.=H->COME_PRN
  othe
    if key_last <> 13
      keyboard chr(key_last)
    endif
    var=name_fld[ptr]
    set cursor on
    @ row(),col() get &var
    read
    set cursor off
    if updated().and.ptr=6
      var=ltrim(str(H->COME_COD))
      nam&var.=H->COME_NAM
    endif
  endcase
  endif
case lastkey()=22   && INS
  save screen
  old_color=setcolor(cfr)
  BoxTenj(Y_srm-2,X_srm-25,Y_srm+2,X_srm+25,ibox)
  MSG='Введите новый номер начисления/удержания'
  new_cod=nul_cod
  set cursor on
  @ Y_srm,X_srm-23 say MSG get new_cod
  read
  set cursor off
  setcolor(old_color)
  restore screen
  if lastkey() != 27.and.(.not.empty(new_cod))
    var=ltrim(str(new_cod))
    if type("G&var") = "U"
     Insert()
     if lastkey()<>27
      repl H->COME_COD with new_cod
      var=ltrim(str(H->COME_COD))
      public B&var,C&var,D&var,F&var,cod&var,nam&var,prn&var,frm&var
      B&var.=nul_day
      C&var.=nul_clo
      D&var.=nul_baz
      F&var.=nul_sum
      cod&var.=nul_cod                && 0 - Н/У нет
      nam&var.=H->COME_NAM
      prn&var.=H->COME_PRN
      frm&var.=upper(strtran(H->COME_FRM," "))
      return 2
     endif 
    else
     WarnWrite("Н/У с номером "+ltrim(str(new_cod))+" уже существует...")
    endif
  endif
case lastkey()=7    && DEL
  if H->COME_COD != 0
    if Fragen('','Удалить строку Н/У')
      save screen
      MsgWrite("Удаляю текущее начисление/удержание...")
      var=ltrim(str(H->COME_COD))
      rele B&var,C&var,D&var,F&var,cod&var,nam&var,prn&var,frm&var
      delete
      pack
      rest screen
      return 2
    endif
  endif
case lastkey()=-4    && F5
  dblpt1(name_fld,name_var,TITL)
endcase
return 1
