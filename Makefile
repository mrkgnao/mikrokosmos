SRCS = $(wildcard lucid/*.hs)
OBJS = $(patsubst lucid/%.hs, templates/%.html, $(SRCS))

templates/%.html: lucid/%.hs
	runghc $< > $@

all: $(OBJS)
