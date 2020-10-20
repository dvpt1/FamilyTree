copy pay.prg pay_b.prg
type func.prg >> pay_b.prg
type inp.prg >> pay_b.prg
type come.prg >> pay_b.prg
type print.prg >> pay_b.prg
type arhivs.prg >> pay_b.prg
type tunes.prg >> pay_b.prg
type title.prg >> pay_b.prg
type vars.prg >> pay_b.prg
clipper pay_b -m -q -l
rtlink FI pay_b OUT pay LIB clipper,extend,mcl
del pay_b.*
