copy gni.prg pay_b.prg
type func.prg >> pay_b.prg
type come.prg >> pay_b.prg
type arhivs.prg >> pay_b.prg
type tunes.prg >> pay_b.prg
type vars.prg >> pay_b.prg
clipper pay_b -m -q -l
rtlink FI pay_b OUT gni LIB clipper,extend,mcl
del pay_b.*
