SRCS = $(wildcard Templates/*.hs)
OBJS = $(patsubst Templates/%.hs, templates/%.html, $(SRCS))

templates/%.html: Templates/%.hs
	runghc $< > $@

index.html: Templates/Index.hs
	runghc $< > $@

all: $(OBJS) index.html
