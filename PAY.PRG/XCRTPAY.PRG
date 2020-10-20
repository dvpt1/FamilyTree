select 9
use config.pay

select 1
create pay.str
use pay.str

select 2
use account.pay
do while .not.eof()
  select 1
  append blank
  repl FIELD_NAME with B->FIELD_NAME
  repl FIELD_TYPE with B->FIELD_TYPE
  repl FIELD_LEN  with B->FIELD_LEN
  repl FIELD_DEC  with B->FIELD_DEC
  select 2
  skip
enddo
select 2
use

select 1
i=0
do while i<I->MAX_COME
  i=i+1
  var=ltrim(str(i))
  append blank
  repl FIELD_NAME with "DAY"+var,FIELD_TYPE with "N",FIELD_LEN with  6,FIELD_DEC with 2
  append blank
  repl FIELD_NAME with "CLO"+var,FIELD_TYPE with "N",FIELD_LEN with  6,FIELD_DEC with 2
  append blank
  repl FIELD_NAME with "BAZ"+var,FIELD_TYPE with "N",FIELD_LEN with 10,FIELD_DEC with 2
  append blank
  repl FIELD_NAME with "SUM"+var,FIELD_TYPE with "N",FIELD_LEN with 12,FIELD_DEC with 2
  append blank
  repl FIELD_NAME with "COD"+var,FIELD_TYPE with "N",FIELD_LEN with  3,FIELD_DEC with 0
enddo

i=0
do while i<=I->NUMMON
  var=ltrim(str(i))
  append blank
  repl FIELD_NAME with "MONSUM"+var,FIELD_TYPE with "N",FIELD_LEN with 12,FIELD_DEC with 2
  append blank
  repl FIELD_NAME with "MONDAT"+var,FIELD_TYPE with "N",FIELD_LEN with 12,FIELD_DEC with 2
  i=i+1
enddo

use

select 1
create karta.str
use karta.str

j=2
do while j<=14
 append blank
 repl FIELD_NAME with "KARTA"+ltrim(str(j)),FIELD_TYPE with "C",FIELD_LEN with  250,FIELD_DEC with 0
 append blank
 repl FIELD_NAME with "EARTA"+ltrim(str(j)),FIELD_TYPE with "C",FIELD_LEN with  250,FIELD_DEC with 0
 j=j+1
enddo
use

*create karta.pay from karta.str
*use karta.pay
*append blank
*use

select 9
use
quit
