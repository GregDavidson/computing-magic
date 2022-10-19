%.pdf: %.me
	pdfroff --no-toc -me --pdf-output=$@ $< 
%.pdf: %.tbl,me
	pdfroff --no-toc -t -me --pdf-output=$@ $< 
%.pdf: %.eqn,tbl,me
	pdfroff --no-toc -e -t -me --pdf-output=$@ $< 
PDFs = ed+sed+ex.pdf reg-exp.pdf vi.pdf bash-metas.pdf
# DST = ngender.net:/WebPages/ngender.net/public/jgd/gnu
GNU = $(PDFs) index.html
ALL= $(PDFs)
all: $(ALL)
# public: $(GNU)
# 	rsync --rsh=ssh $(GNU) $(DST)
ed+sed+ex.pdf: ed+sed+ex.tbl,me
reg-exp.pdf: reg-exp.eqn,tbl,me
vi.pdf: vi.tbl,me
bash-metas.pdf: bash-metas.eqn,tbl,me
