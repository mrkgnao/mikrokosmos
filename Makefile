SRCS = $(wildcard lucid/*.hs)
OBJS = $(patsubst lucid/%.hs, templates/%.html, $(SRCS))

templates/%.html: lucid/%.hs
	runghc $< > $@

index.html: index.hs
	runghc $< > $@

all: $(OBJS) index.html
