*-------------Инициализация конфигурации системы----------*
procedure InitConfig
wd=I->N_DAY
wc=I->N_CLOCK
nm=month(I->T_DATE)
mo=I->N_MO
ok=I->NUM_DIV
summaf=upper(strtran(I->FSUMMA," "))
summad=upper(strtran(I->DSUMMA," "))
pic_sum=pictn(I->FORMAT)	&& для форматирования
repl I->FORMAT with 0
nul_sum=I->FORMAT
return
*-------------Инициализация массива лицевого счета--------*
procedure InitPays
select 8
use journal.pay			&& Журнал
public SIZE_JOUR
SIZE_JOUR = lastrec()+1
public jour_jou[SIZE_JOUR]	&& Макро поле журнала
public jour_fld[SIZE_JOUR]	&& Имя  поля
public jour_nam[SIZE_JOUR]	&& Название поля
public jour_iif[SIZE_JOUR]	&& Название логического поля
i=1
jour_jou[i]="iif(delete(),chr(16),' ')"
jour_fld[i]="delete()"
jour_nam[i]=""
jour_iif[i]="iif(delete(),chr(16),' ')"
do while .not.eof()
  i=i+1
  do case
    case .not.empty(H->IIF_STR)
      jour_jou[i]=alltrim(H->IIF_STR)
    othe
      jour_jou[i]=alltrim(H->FIELD_NAME)
  endcase
  jour_fld[i]=alltrim(H->FIELD_NAME)
  jour_nam[i]=alltrim(H->NAME_FIELD)
  jour_iif[i]=alltrim(H->IIF_STR)
  skip
enddo
use filter.pay				&& Фильтр
public SIZE_FILTER
SIZE_FILTER = lastrec()
if SIZE_FILTER != 0
  public filter_fld[SIZE_FILTER]   && Имя  поля
  public filter_nam[SIZE_FILTER]   && Название поля
  public filter_iif[SIZE_FILTER]   && Название логического поля
  public filter_dbf[SIZE_FILTER]   && Название файла словаря
  public filter_ntx[SIZE_FILTER]   && и его файла индекса
  public filter_cod[SIZE_FILTER]   && Код из словаря
  public filter_scr[SIZE_FILTER]   && Листинг словаря
  i=0
  do while .not.eof()
    i=i+1
    filter_fld[i]=alltrim(H->FIELD_NAME)
    filter_nam[i]=alltrim(H->NAME_FIELD)
    filter_iif[i]=alltrim(H->IIF_STR)
    filter_dbf[i]=alltrim(H->DICT_DBF)
    filter_ntx[i]=alltrim(H->DICT_NTX)
    filter_cod[i]=alltrim(H->FIELD_COD)
    filter_scr[i]=alltrim(H->FIELD_SCR)
    skip
  enddo
endif
use account.pay				&& Лицевой счет
public SIZE_ACCO
SIZE_ACCO = lastrec()
if SIZE_ACCO != 0
  public acco_fld[SIZE_ACCO]   && Имя  поля
  public acco_nam[SIZE_ACCO]   && Название поля
  public acco_iif[SIZE_ACCO]   && Название логического поля
  public acco_row[SIZE_ACCO]   && Курсор Y
  public acco_col[SIZE_ACCO]   && Курсор X
  public acco_dbf[SIZE_ACCO]   && Название файла словаря
  public acco_ntx[SIZE_ACCO]   && и его файла индекса
  public acco_cod[SIZE_ACCO]   && Код из словаря
  public acco_scr[SIZE_ACCO]   && Листинг словаря
  i=0
  do while .not.eof()
    i=i+1
    acco_fld[i]=alltrim(H->FIELD_NAME)
    acco_nam[i]=alltrim(H->NAME_FIELD)
    acco_iif[i]=alltrim(H->IIF_STR)
    acco_row[i]=H->CUR_ROW
    acco_col[i]=H->CUR_COL
    acco_dbf[i]=alltrim(H->DICT_DBF)
    acco_ntx[i]=alltrim(H->DICT_NTX)
    acco_cod[i]=alltrim(H->FIELD_COD)
    acco_scr[i]=alltrim(H->FIELD_SCR)
    skip
  enddo
endif
use index.pay				&& Индекс pay.dbf
public SIZE_INDX
SIZE_INDX = lastrec()
if SIZE_INDX != 0
  public index_prm[SIZE_INDX]
  public index_ntx[SIZE_INDX]
  public index_key[SIZE_INDX]
  i=0
  do while .not.eof()
    i=i+1
    index_prm[i] = H->INDEX_PRM
    index_ntx[i] = alltrim(H->INDEX_NTX)
    index_key[i] = alltrim(H->INDEX_KEY)
    skip
  enddo
endif
use diction.pay				&& список словарей
public SIZE_DICT
SIZE_DICT = lastrec()
if SIZE_DICT != 0
  public dict_prm[SIZE_DICT]
  public dict_dbf[SIZE_DICT]
  public dict_ntx[SIZE_DICT]
  public dict_key[SIZE_DICT]
  public dict_str[SIZE_DICT]
  i=0
  do while .not.eof()
    i=i+1
    dict_prm[i] = H->DICT_PRM
    dict_dbf[i] = alltrim(H->DICT_DBF)
    dict_ntx[i] = alltrim(H->DICT_NTX)
    dict_key[i] = alltrim(H->DICT_KEY)
    dict_str[i] = alltrim(H->DICT_STR)
    skip
  enddo
endif
use come.pay				&& Начисления/Удержания
go bottom
skip
public nul_day
public nul_clo
public nul_baz
public nul_sum
public nul_cod
public nul_nam
nul_day=H->COME_DAY
nul_clo=H->COME_CLO
nul_baz=H->COME_BAZ
nul_sum=H->COME_SUM
nul_cod=H->COME_COD
nul_nam=H->COME_NAM
go top
do while .not.eof()
  var=ltrim(str(H->COME_COD))  && Взять все Н/У
  public B&var,C&var,D&var,F&var,cod&var,nam&var,sch&var,prn&var,frm&var
  B&var.=nul_day
  C&var.=nul_clo
  D&var.=nul_baz
  F&var.=nul_sum
  cod&var.=nul_cod                && 0 - Н/У нет
  nam&var.=H->COME_NAM
  prn&var.=H->COME_PRN
  frm&var.=upper(strtran(H->COME_FRM," "))
  skip
enddo

use karta.pay				&& Hалоговая каpточка
for i=2 to karta_len
 var=ltrim(str(i))
 public KARTA&var
 M->KARTA&var.=upper(strtran(H->KARTA&var," "))
 public EARTA&var
 M->EARTA&var.=upper(strtran(H->EARTA&var," "))
next
use
return
