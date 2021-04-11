build_dir := ./build
build_dir_test := ./build/test
target := $(build_dir)/cscam
target_test := $(build_dir)/cscam.test

compile := g++ --std=c++11
link := g++

CAIRO_LDFLAGS := $(shell pkg-config --libs cairomm-1.0)
CAIRO_CFLAGS := $(shell pkg-config --cflags cairomm-1.0)
RSVG_CFLAGS := $(shell pkg-config --cflags librsvg-2.0)
RSVG_LDFLAGS := $(shell pkg-config --libs librsvg-2.0)

compile_flags := -frounding-math -fopenmp $(CAIRO_CFLAGS) $(RSVG_CFLAGS)
link_flags := -lm -lrt -lfftw3 -lgsl -lgslcblas -lgmp -lboost_thread -lmpfr -fopenmp $(CAIRO_LDFLAGS) $(RSVG_LDFLAGS)

cc_files = $(shell find ./src -name *.cc)
obj_files = $(cc_files:%.cc=$(build_dir)/%.o)
test_obj_files = $(cc_files:%.cc=$(build_dir_test)/%.o)
dep_files = $(obj_files:%.o=%.d)
test_dep_files = $(test_obj_files:%.o=%.d)

.PHONY: all clean test

all: $(target)

test: $(target_test)
	cp test/stanford_bunny.ply build
	cd build && ./cscam.test test

-include $(dep_files)
-include $(test_dep_files)

$(build_dir_test)/%.o: %.cc
	@mkdir -p $(@D)
	$(compile) $(compile_flags) -DUNITTEST -g -O1 -MMD -c $< -o $@

$(build_dir)/%.o: %.cc
	@mkdir -p $(@D)
	$(compile) $(compile_flags) -O3 -MMD -c $< -o $@

$(target): $(obj_files)
	@mkdir -p $(@D)
	$(link) $^ $(link_flags) -o $@

$(target_test): $(test_obj_files)
	@mkdir -p $(@D)
	$(link) -Wl,--start-group $^ -Wl,--end-group $(link_flags) -o $@

clean:
	rm -rf build

